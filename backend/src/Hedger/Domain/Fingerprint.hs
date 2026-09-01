{- | Content-addressed fingerprints of a journal.

The point of this module is to answer \"which months of derived history survived
this edit?\" without recomputing any of them. It does so by hashing /semantic/
journal content only: two journals that differ in whitespace, alignment,
comment layout or source positions produce byte-identical fingerprints, so a
reformat — or a revert to an earlier state — is a cache hit rather than a full
invalidation.
-}
module Hedger.Domain.Fingerprint (
    ChainDiff (..),
    GlobalFingerprint (..),
    Hash (..),
    PrefixChain (..),
    chainHashAt,
    combineHash,
    diffChains,
    globalFingerprint,
    hashText,
    prefixChain,

    -- * Per-family chains
    ReportFamily (..),
    FamilyChains (..),
    everyFamily,
    familyChains,
    chainFor,
    diffFamilyChains,
)
where

import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Builder.Extra qualified as BuilderExtra
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Decimal (Decimal)
import Data.Decimal qualified as Decimal
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time.Calendar (Day)
import Data.Time.Calendar qualified as Calendar

import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Journal qualified as HledgerJournal
import Hledger.Data.Types qualified as Hledger
import Hledger.Utils.Parse qualified as HledgerParse

import Hedger.Config (Config (..))

-- | A hex-encoded MD5 digest.
newtype Hash = Hash Text
    deriving stock (Eq, Ord, Show)

{- | The journal-global values that no per-month hash can protect: a change to
any of them can alter arbitrarily old derived output, so it invalidates the
whole chain.
-}
data GlobalFingerprint = GlobalFingerprint
    { files :: !Hash
    , parseOptions :: !Hash
    , commodityStyles :: !Hash
    , accountTypes :: !Hash
    , configVersion :: !Hash
    }
    deriving stock (Eq, Show)

{- | A Merkle chain over journal history: one entry per month that contains at
least one entry, oldest first, where each hash covers every entry dated at or
before that month end.

@monthContents@ is the same history one level down: the digest of each month's
/own/ entries, before the rolling fold turns it into a prefix hash. It is
retained because it is what the next reload's per-file pre-filter reuses — a
month whose files are byte-identical keeps its content digest and pays nothing,
while the fold that turns contents into prefix hashes is re-run in full and so
still propagates every earlier change. Keeping /content/ digests rather than
prefix hashes is what makes that sound: a month appearing or vanishing shifts
every later prefix hash automatically.

Because each month's prefix hash is a function of the content digests up to it,
the two fields agree by construction and 'Eq' over both is the same relation as
'Eq' over either.
-}
data PrefixChain = PrefixChain
    { global :: !GlobalFingerprint
    , months :: ![(Day, Hash)]
    , monthContents :: ![(Day, Text)]
    }
    deriving stock (Eq, Show)

-- | How far back an edit reached.
data ChainDiff
    = -- | Something not date-scoped changed; no checkpoint is reusable.
      GlobalChanged
    | -- | Checkpoints from this month end onwards must be discarded.
      DivergesFrom !Day
    | Identical
    deriving stock (Eq, Show)

hashText :: Text -> Hash
hashText = Hash . hexEncode . MD5.hash . TextEncoding.encodeUtf8

-- | Extend a chain hash with the next period's content.
combineHash :: Hash -> Text -> Hash
combineHash previous content = hashTagged "chain-v2" [unHash previous, content]

globalFingerprint :: Hledger.Journal -> Config -> GlobalFingerprint
globalFingerprint journal config =
    GlobalFingerprint
        { files = hashTagged "files-v1" (Text.pack . fst <$> journal.jfiles)
        , parseOptions = hashTagged "parse-options-v1" ["definputopts-v1"]
        , commodityStyles = commodityStylesHash (HledgerJournal.journalCommodityStyles journal)
        , accountTypes = accountTypesHash journal.jdeclaredaccounttypes
        , configVersion = configHash config
        }

{- | Build the whole chain in one pass over date-sorted entries. The inputs are
sorted here rather than by the caller, because @jpricedirectives@ is in parse
order.
-}
prefixChain ::
    GlobalFingerprint ->
    [Hledger.Transaction] ->
    [Hledger.PriceDirective] ->
    PrefixChain
prefixChain fingerprint transactions directives =
    chainFrom
        fingerprint
        [ (day, monthContentDigest (entryDigest <$> entries))
        | (day, entries) <- monthGroups (sortedEntries transactions directives)
        ]

-- | Compare an older chain against a newer one.
diffChains :: PrefixChain -> PrefixChain -> ChainDiff
diffChains old new
    | old.global /= new.global = GlobalChanged
    | otherwise = maybe Identical DivergesFrom (firstDivergentMonth old.months new.months)

{- | The chain hash covering every entry up to the end of the month containing
this day, if that month has an entry.
-}
chainHashAt :: PrefixChain -> Day -> Maybe Hash
chainHashAt chain day = lookup (monthEnd day) chain.months

-- * Per-family chains and the per-file pre-filter

{- | The analytics families whose checkpoints are invalidated independently.

A change at month /m/ only truncates a family that the change can actually
reach: recategorising one expense to another cannot move an asset column or an
investment scope scan, and a transfer between two bank accounts cannot move an
income or expense column.
-}
data ReportFamily
    = NetWorthFamily
    | CashflowFamily
    | InvestmentFamily
    deriving stock (Eq, Ord, Show, Enum, Bounded)

everyFamily :: [ReportFamily]
everyFamily = [minBound .. maxBound]

{- | The whole-journal chain, one chain per family, and the two things the /next/
reload needs in order to skip work: a content hash per included journal file, and
a hash of each month's entry layout.

Every family chain carries an entry for every month the whole chain has, even
when the family has no relevant entry in it — a constant digest then stands in.
That keeps all four chains aligned month for month, so a divergence date means
the same thing in each, and it makes a reused month's lookup in the previous
chain unconditional.
-}
data FamilyChains = FamilyChains
    { whole :: !PrefixChain
    , netWorth :: !PrefixChain
    , cashflow :: !PrefixChain
    , investments :: !PrefixChain
    , files :: !(Map FilePath Hash)
    , monthShapes :: !(Map Day Hash)
    }
    deriving stock (Eq, Show)

{- | A family's own chain.

One field per family rather than a @Map ReportFamily PrefixChain@, so that this
is total without a default. A map would need one, and the only defensible default
— the whole-journal chain — is unreachable code that reads exactly like a safety
property: it would silently absorb a family someone forgot to populate, and no
test could tell. With fields, forgetting one does not compile.
-}
chainFor :: ReportFamily -> FamilyChains -> PrefixChain
chainFor family chains = case family of
    NetWorthFamily -> chains.netWorth
    CashflowFamily -> chains.cashflow
    InvestmentFamily -> chains.investments

-- | Compare one family's chain across two reloads.
diffFamilyChains :: FamilyChains -> FamilyChains -> ReportFamily -> ChainDiff
diffFamilyChains old new family = diffChains (chainFor family old) (chainFor family new)

-- | The included files whose contents changed between two reloads.
changedFiles :: Map FilePath Hash -> Map FilePath Hash -> Set FilePath
changedFiles old new =
    Map.keysSet (Map.filterWithKey (\path hash -> Map.lookup path new /= Just hash) old)

{- | Build the whole-journal chain and every family chain in one pass, reusing
the previous reload's month digests wherever the files behind a month are
byte-identical.

The pre-filter in front of the hashing is what makes a year-split journal cheap:
editing @2024.journal@ leaves every month sourced only from earlier years with
its digest intact, and those months are never re-encoded. It is conservative by
construction — see 'reusableMonths' for every condition that gives up — because
a wrongly reused digest is a silently stale journal with no other symptom.
-}
familyChains ::
    {- | Which families a transaction can affect; see
    "Hedger.Domain.Internal.FamilyRelevance".
    -}
    (Hledger.Transaction -> [ReportFamily]) ->
    -- | The previous reload's chains, if there are any to reuse from.
    Maybe FamilyChains ->
    Hledger.Journal ->
    GlobalFingerprint ->
    [Hledger.Transaction] ->
    [Hledger.PriceDirective] ->
    FamilyChains
familyChains relevance previous journal fingerprint transactions directives =
    FamilyChains
        { whole = chainOf Nothing
        , netWorth = chainOf (Just NetWorthFamily)
        , cashflow = chainOf (Just CashflowFamily)
        , investments = chainOf (Just InvestmentFamily)
        , files = currentFiles
        , monthShapes = Map.fromList [(group.day, group.shape) | group <- groups]
        }
  where
    currentFiles = Map.fromList [(path, hashText contents) | (path, contents) <- journal.jfiles]

    -- \| Files are identified by a small integer for the rest of this pass. It is
    --    worth the indirection: the layout hash and the per-month contributor set are
    --    both computed for every entry on every reload, including the reloads that go
    --    on to hash nothing, and a 'FilePath' is a 'String' whose every use would
    --    otherwise mean a fresh 'Text' or a chain of character comparisons.
    --    'unknownFile' is reserved for a file that is not in @jfiles@ at all.
    --
    fileIndexes = Map.fromList (zip (Map.keys currentFiles) [unknownFile + 1 ..])
    fileIndex path = Map.findWithDefault unknownFile path fileIndexes

    groups = monthGroupsFor relevance previous currentFiles fileIndex entryStream
    entryStream = sortedEntries transactions directives

    chainOf selector = chainFrom fingerprint [(group.day, contentOf group) | group <- groups]
      where
        stored = Map.fromList (maybe [] ((.monthContents) . selectedChain selector) previous)
        contentOf group
            | group.reusable, Just content <- Map.lookup group.day stored = content
            | otherwise =
                monthContentDigest
                    [digest | (digest, families) <- group.classified, selects selector families]

selectedChain :: Maybe ReportFamily -> FamilyChains -> PrefixChain
selectedChain selector chains = maybe chains.whole (`chainFor` chains) selector

selects :: Maybe ReportFamily -> [ReportFamily] -> Bool
selects selector families = maybe True (`elem` families) selector

{- | One month of entries, with the pre-filter's verdict attached.

@classified@ is deliberately lazy. It is the expensive field — one digest per
entry, which is the whole cost of the chain pass — and a month that every chain
reuses never forces it. A month that any chain has to recompute forces it once
and all four chains share the result.
-}
data MonthGroup = MonthGroup
    { day :: !Day
    , shape :: !Hash
    , reusable :: !Bool
    , classified :: [(Digest, [ReportFamily])]
    }

monthGroupsFor ::
    (Hledger.Transaction -> [ReportFamily]) ->
    Maybe FamilyChains ->
    Map FilePath Hash ->
    (FilePath -> Int) ->
    [Entry] ->
    [MonthGroup]
monthGroupsFor relevance previous currentFiles fileIndex entries =
    [ MonthGroup
        { day = day
        , shape = shape
        , reusable = reuse day shape (Set.fromList (snd <$> labels))
        , classified = [(entryDigest entry, entryFamilies relevance entry) | entry <- monthEntries]
        }
    | (day, monthEntries) <- monthGroups entries
    , let labels = entryLabel fileIndex <$> monthEntries
    , let shape = monthShape labels
    ]
  where
    reuse = reusableMonths previous currentFiles fileIndex

{- | Price directives are relevant to every family: each family values its
columns through the journal's price oracle, so a directive dated inside a month
can restate that month's output whatever accounts it mentions.
-}
entryFamilies :: (Hledger.Transaction -> [ReportFamily]) -> Entry -> [ReportFamily]
entryFamilies relevance entry = case entry of
    TransactionEntry transaction -> relevance transaction
    PriceEntry _ -> everyFamily

{- | What an entry contributes to its month's layout: whether it is a transaction
or a price directive, and which file it came from.
-}
entryLabel :: (FilePath -> Int) -> Entry -> (Int, Int)
entryLabel fileIndex entry = (kind, fileIndex (entryFile entry))
  where
    kind = case entry of
        TransactionEntry _ -> 0
        PriceEntry _ -> 1

{- | A hash of a month's /layout/: the kind and source file of each of its
entries, in order, without their contents.

This is what makes reuse exact rather than merely plausible. A month's digest is
over its entries in stream order, and stream order across two files is fixed by
the journal's include structure — which lives in the text of some third file that
may itself contribute no entry to this month. Comparing the layout catches any
such reordering directly.

Run-length encoded, which is injective for a sequence and is what keeps the
layout hash cheap enough to compute on a reload that hashes nothing else: a month
whose entries all come from one file is three builder writes rather than two per
entry, and the encoding stays exact because consecutive equal labels can only be
recovered one way.
-}
monthShape :: [(Int, Int)] -> Hash
monthShape labels =
    Hash . hexDigest . digestOf $ listPart "month-shape-v2" runPart (runLengths labels)
  where
    runPart ((kind, file), count) =
        Builder.word8 (fromIntegral kind)
            <> Builder.word64LE (fromIntegral file)
            <> Builder.word64LE (fromIntegral count)

runLengths :: (Eq label) => [label] -> [(label, Int)]
runLengths [] = []
runLengths (first : rest) =
    let (same, others) = span (== first) rest
     in (first, 1 + length same) : runLengths others

{- | Whether one month's stored content digests may be reused, given the
previous reload's chains and this reload's file hashes.

Every clause is a way of giving up, and the order they are written in is the
argument for correctness:

* No previous chains means there is nothing to compare against. Note that this
  clause is not what makes a boot reload safe: 'familyChains' also has no stored
  month digests to look up in that case, so 'contentOf' recomputes regardless.
  Mutating it to @True@ provably changes no behaviour. It is stated here so the
  clause is read as the obvious value and not as a guard carrying weight.
* A changed set of file /paths/ means a file was added, removed or renamed and
  the include structure moved with it; the paths are also a component of
  'GlobalFingerprint', so this case invalidates anyway. An /empty/ previous file
  map is covered by the same comparison, since @jfiles@ is never empty for a
  journal that parsed.
* A month containing an entry sourced from a file that is not in @jfiles@ — file
  index 0 — cannot be judged: that entry's dependence on file contents is
  unaccounted for, so its month is recomputed.
* A month absent from the previous reload, or one whose layout hash moved, is
  recomputed: either its entries are new or they were reordered.
* Finally, a month is reused only when /none/ of the files its entries came from
  changed. Contributing files being byte-identical fixes each file's own
  entries and their order; the layout hash fixes how those files interleave; so
  the month's entry sequence, and therefore its digest, is unchanged.

Note what the layout clause buys in the deletion case: removing a transaction
from an otherwise unrelated file shortens that month's layout, so the month is
recomputed even though the file that lost the entry no longer appears among the
month's contributors.
-}
reusableMonths ::
    Maybe FamilyChains -> Map FilePath Hash -> (FilePath -> Int) -> Day -> Hash -> Set Int -> Bool
reusableMonths previous currentFiles fileIndex = case previous of
    Nothing -> \_ _ _ -> False
    Just prior
        | Map.keysSet prior.files /= Map.keysSet currentFiles -> \_ _ _ -> False
        | otherwise ->
            let changed = Set.map fileIndex (changedFiles prior.files currentFiles)
             in \day shape monthFiles ->
                    Map.lookup day prior.monthShapes == Just shape
                        && not (Set.member unknownFile monthFiles)
                        && Set.disjoint changed monthFiles

-- | The file index standing for "not among @jfiles@"; see 'familyChains'.
unknownFile :: Int
unknownFile = 0

{- | Hash a tagged list of parts. Every part is length-delimited so that
concatenation is injective: without it @["ab", "c"]@ and @["a", "bc"]@ would
share a digest, and an account name could be confused with a description.
-}
hashTagged :: Text -> [Text] -> Hash
hashTagged tag parts = hashText (Text.concat (lengthDelimited <$> tag : parts))

unHash :: Hash -> Text
unHash (Hash value) = value

-- | The last day of the month containing this day; the chain's checkpoint key.
monthEnd :: Day -> Day
monthEnd day =
    let (year, month, _) = Calendar.toGregorian day
     in Calendar.fromGregorian year month (Calendar.gregorianMonthLength year month)

{- | Semantic content of a transaction.

@tsourcepos@, @tindex@ and @tprecedingcomment@ are excluded on purpose. They all
shift when unrelated lines move within the file, so hashing them would make
inserting a comment at the top of a journal invalidate every month of history —
the cache would never hit. @ptransaction@ and @poriginal@ are excluded too: the
former closes a cycle back to this transaction, the latter is a parse artefact.

Postings, amounts and tags are encoded /inline/ rather than hashed one level at a
time: nothing outside this module ever asks for a sub-transaction hash, so the
intermediate digests only cost work. One digest per transaction replaces roughly
fifteen.
-}
transactionPart :: Hledger.Transaction -> Builder
transactionPart transaction =
    header "transaction-v2" 8
        <> dayPart transaction.tdate
        <> optionalPart dayPart transaction.tdate2
        <> statusPart transaction.tstatus
        <> textPart transaction.tcode
        <> textPart transaction.tdescription
        <> textPart transaction.tcomment
        <> tagsPart transaction.ttags
        <> listPart "postings-v2" postingPart transaction.tpostings

{- | Semantic content of a price directive, keyed by the directive's own date so
that inserting a historical price invalidates exactly the months from that date
onwards. @pdsourcepos@ is excluded, as in 'transactionPart'.
-}
priceDirectivePart :: Hledger.PriceDirective -> Builder
priceDirectivePart directive =
    header "price-directive-v2" 4
        <> dayPart directive.pddate
        <> textPart directive.pdcommodity
        <> textPart directive.pdamount.acommodity
        <> quantityPart directive.pdamount.aquantity

postingPart :: Hledger.Posting -> Builder
postingPart posting =
    header "posting-v2" 9
        <> textPart posting.paccount
        <> statusPart posting.pstatus
        <> postingTypePart posting.ptype
        <> textPart posting.pcomment
        <> optionalPart dayPart posting.pdate
        <> optionalPart dayPart posting.pdate2
        <> mixedAmountPart posting.pamount
        <> optionalPart balanceAssertionPart posting.pbalanceassertion
        <> tagsPart posting.ptags

mixedAmountPart :: Hledger.MixedAmount -> Builder
mixedAmountPart = listPart "mixed-amount-v2" amountPart . HledgerAmount.amountsRaw

{- | @astyle@ is absent because display styles are journal-global and therefore
belong to 'GlobalFingerprint', not to per-month content.
-}
amountPart :: Hledger.Amount -> Builder
amountPart amount =
    header "amount-v2" 4
        <> textPart amount.acommodity
        <> quantityPart amount.aquantity
        <> optionalPart costPart amount.acost
        <> optionalPart costBasisPart amount.acostbasis

costPart :: Hledger.AmountCost -> Builder
costPart cost = case cost of
    Hledger.UnitCost amount -> header "unit-cost-v2" 1 <> amountPart amount
    Hledger.TotalCost amount -> header "total-cost-v2" 1 <> amountPart amount

costBasisPart :: Hledger.CostBasis -> Builder
costBasisPart basis =
    header "cost-basis-v2" 3
        <> optionalPart amountPart basis.cbCost
        <> optionalPart dayPart basis.cbDate
        <> optionalPart textPart basis.cbLabel

-- | @baposition@ is a source position; see the note on 'transactionPart'.
balanceAssertionPart :: Hledger.BalanceAssertion -> Builder
balanceAssertionPart assertion =
    header "balance-assertion-v2" 3
        <> amountPart assertion.baamount
        <> boolPart assertion.batotal
        <> boolPart assertion.bainclusive

tagsPart :: [Hledger.Tag] -> Builder
tagsPart = listPart "tags-v2" (\(name, value) -> textPart name <> textPart value)

commodityStylesHash :: Map Hledger.CommoditySymbol Hledger.AmountStyle -> Hash
commodityStylesHash styles =
    hashTagged
        "commodity-styles-v1"
        (concat [[symbol, textShow style] | (symbol, style) <- Map.toAscList styles])

{- | Only /declared/ account types are hashed. @jaccounttypes@ additionally
holds types inferred from account names, which are already covered by the
account names inside the transaction hashes; hashing it would make every
append that introduces a new account a full rebuild.
-}
accountTypesHash :: Map Hledger.AccountType [Hledger.AccountName] -> Hash
accountTypesHash declared =
    hashList
        "account-types-v1"
        [ hashTagged "account-type-v1" (textShow accountType : accounts)
        | (accountType, accounts) <- Map.toAscList declared
        ]

configHash :: Config -> Hash
configHash config =
    hashTagged
        "config-v1"
        [ config.defaultDisplayCommodity
        , textShow config.fiscalYearStartMonth
        , textShow config.investmentMappings
        , textShow config.assetClassMappings
        ]

{- | One hashable journal entry, kept unhashed so that the per-file pre-filter
can decide whether the month it belongs to needs hashing at all.
-}
data Entry
    = TransactionEntry !Hledger.Transaction
    | PriceEntry !Hledger.PriceDirective

entryDay :: Entry -> Day
entryDay entry = case entry of
    TransactionEntry transaction -> transaction.tdate
    PriceEntry directive -> directive.pddate

{- | The file the entry was parsed from. Read from the source position, which is
never /hashed/ — hashing it would make an unrelated inserted line invalidate all
of history — but is exactly the right thing to /group/ by, because it says which
file's contents an entry's digest depends on.
-}
entryFile :: Entry -> FilePath
entryFile entry = case entry of
    TransactionEntry transaction -> HledgerParse.sourceName (fst transaction.tsourcepos)
    PriceEntry directive -> HledgerParse.sourceName directive.pdsourcepos

entryDigest :: Entry -> Digest
entryDigest entry = case entry of
    TransactionEntry transaction -> digestOf (transactionPart transaction)
    PriceEntry directive -> digestOf (priceDirectivePart directive)

{- | Transactions and price directives merged into one date-ordered stream. The
sort is stable, so entries sharing a date keep their original relative order —
which is itself semantic, because balance assertions depend on it.
-}
sortedEntries :: [Hledger.Transaction] -> [Hledger.PriceDirective] -> [Entry]
sortedEntries transactions directives =
    List.sortOn entryDay $
        (TransactionEntry <$> transactions) <> (PriceEntry <$> directives)

-- | The date-ordered stream cut at month boundaries, oldest month first.
monthGroups :: [Entry] -> [(Day, [Entry])]
monthGroups [] = []
monthGroups entries@(first : _) =
    let boundary = monthEnd (entryDay first)
        (thisMonth, laterMonths) = span ((== boundary) . monthEnd . entryDay) entries
     in (boundary, thisMonth) : monthGroups laterMonths

monthContentDigest :: [Digest] -> Text
monthContentDigest digests = hexDigest (digestOf (listPart "month-v2" digestPart digests))

-- | Roll per-month content digests up into the prefix hash chain.
chainFrom :: GlobalFingerprint -> [(Day, Text)] -> PrefixChain
chainFrom fingerprint contents =
    PrefixChain{global = fingerprint, months = go absentHash contents, monthContents = contents}
  where
    go _ [] = []
    go previous ((day, content) : rest) =
        let !boundaryHash = combineHash previous content
         in (day, boundaryHash) : go boundaryHash rest

firstDivergentMonth :: [(Day, Hash)] -> [(Day, Hash)] -> Maybe Day
firstDivergentMonth [] [] = Nothing
firstDivergentMonth ((day, _) : _) [] = Just day
firstDivergentMonth [] ((day, _) : _) = Just day
firstDivergentMonth (oldEntry : oldRest) (newEntry : newRest)
    | oldEntry == newEntry = firstDivergentMonth oldRest newRest
    -- A month appearing or disappearing mid-chain shifts the alignment, so the
    -- earlier of the two boundaries is the first one that cannot be reused.
    | otherwise = Just (min (fst oldEntry) (fst newEntry))

hashList :: Text -> [Hash] -> Hash
hashList tag hashes = hashTagged tag (unHash <$> hashes)

absentHash :: Hash
absentHash = hashTagged "absent-v1" []

lengthDelimited :: Text -> Text
lengthDelimited value = textShow (Text.length value) <> ":" <> value

textShow :: (Show value) => value -> Text
textShow = Text.pack . show

hexEncode :: ByteString.ByteString -> Text
hexEncode =
    TextEncoding.decodeUtf8
        . LazyByteString.toStrict
        . Builder.toLazyByteString
        . Builder.byteStringHex

-- * Binary entry encoding

{- | The raw 16 bytes of an MD5 digest.

Digests are composed as bytes rather than as hex text. The previous encoding
rendered every intermediate digest to a 32-character hex 'Text' and concatenated
those, so a transaction's bytes were re-encoded once per level of nesting; that
was the dominant cost of building the chain.
-}
newtype Digest = Digest ByteString.ByteString

digestOf :: Builder -> Digest
digestOf = Digest . MD5.hashlazy . buildSmall
{-# INLINE digestOf #-}

hexDigest :: Digest -> Text
hexDigest (Digest bytes) = hexEncode bytes

{- | The default builder strategy allocates a 4 kB chunk per run and then copies
the used prefix out of it. At one run per transaction that allocation dwarfed the
few hundred bytes an entry actually encodes to, so ask for a small untrimmed
buffer instead.
-}
buildSmall :: Builder -> LazyByteString.ByteString
buildSmall =
    BuilderExtra.toLazyByteStringWith
        (BuilderExtra.untrimmedStrategy 1024 BuilderExtra.smallChunkSize)
        LazyByteString.empty

{- | A byte string, preceded by its length.

Length-delimiting is what makes the encoding injective, and it is the same
guarantee 'hashTagged' gives in text form: every field boundary is recoverable
from the bytes alone, so @["ab", "c"]@ and @["a", "bc"]@ cannot collide and an
account name cannot be read as a description.
-}
bytesPart :: ByteString.ByteString -> Builder
bytesPart value =
    Builder.word64LE (fromIntegral (ByteString.length value)) <> Builder.byteString value
{-# INLINE bytesPart #-}

textPart :: Text -> Builder
textPart = bytesPart . TextEncoding.encodeUtf8
{-# INLINE textPart #-}

digestPart :: Digest -> Builder
digestPart (Digest bytes) = bytesPart bytes
{-# INLINE digestPart #-}

{- | The start of a node: its tag, then the number of fields that follow. The
field count keeps a list's extent explicit, so a node whose last field is a list
cannot absorb whatever comes after it, and the tag keeps two structurally
identical types (a unit cost and a total cost, say) apart.

Fields are appended with '<>' rather than collected into a list, because at one
node per posting and per amount the list of parts allocated more than the bytes
it described.
-}
header :: ByteString.ByteString -> Int -> Builder
header tag arity = bytesPart tag <> Builder.word64LE (fromIntegral arity)
{-# INLINE header #-}

listPart :: ByteString.ByteString -> (item -> Builder) -> [item] -> Builder
listPart tag encode items = header tag (length items) <> foldMap encode items
{-# INLINE listPart #-}

-- | A present/absent marker, so @Nothing@ cannot be confused with any encoding.
optionalPart :: (value -> Builder) -> Maybe value -> Builder
optionalPart encode = maybe (Builder.word8 0) ((Builder.word8 1 <>) . encode)
{-# INLINE optionalPart #-}

-- | Fixed width, so no delimiter is needed.
dayPart :: Day -> Builder
dayPart = Builder.int64LE . fromIntegral . Calendar.toModifiedJulianDay
{-# INLINE dayPart #-}

{- | Normalising keeps hash equality in step with 'Eq': the representation
distinguishes @1.5@ from @1.50@ (different mantissa and scale), whereas 'Eq' on
'Decimal' does not.
-}
quantityPart :: Decimal -> Builder
quantityPart quantity =
    let normalized = Decimal.normalizeDecimal quantity
     in Builder.word8 normalized.decimalPlaces <> integerPart normalized.decimalMantissa
{-# INLINE quantityPart #-}

{- | Decimal digits followed by a terminator. @integerDec@ emits only digits and
a leading @-@, neither of which is the terminator, so the boundary is
unambiguous — and an 'Integer' has no fixed width to delimit it by.
-}
integerPart :: Integer -> Builder
integerPart value = Builder.integerDec value <> Builder.word8 terminator
  where
    terminator = fromIntegral (fromEnum ';')
{-# INLINE integerPart #-}

statusPart :: Hledger.Status -> Builder
statusPart = Builder.word8 . fromIntegral . fromEnum
{-# INLINE statusPart #-}

postingTypePart :: Hledger.PostingType -> Builder
postingTypePart postingType =
    Builder.word8 $ case postingType of
        Hledger.RegularPosting -> 0
        Hledger.VirtualPosting -> 1
        Hledger.BalancedVirtualPosting -> 2
{-# INLINE postingTypePart #-}

boolPart :: Bool -> Builder
boolPart value = Builder.word8 (if value then 1 else 0)
{-# INLINE boolPart #-}
