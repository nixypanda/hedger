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
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time.Calendar (Day)
import Data.Time.Calendar qualified as Calendar

import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Journal qualified as HledgerJournal
import Hledger.Data.Types qualified as Hledger

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
-}
data PrefixChain = PrefixChain
    { global :: !GlobalFingerprint
    , months :: ![(Day, Hash)]
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
    PrefixChain
        { global = fingerprint
        , months = monthChain (sortedEntries transactions directives)
        }

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

{- | Transactions and price directives merged into one date-ordered stream. The
sort is stable, so entries sharing a date keep their original relative order —
which is itself semantic, because balance assertions depend on it.
-}
sortedEntries :: [Hledger.Transaction] -> [Hledger.PriceDirective] -> [(Day, Digest)]
sortedEntries transactions directives =
    List.sortOn fst $
        [(transaction.tdate, digestOf (transactionPart transaction)) | transaction <- transactions]
            <> [(directive.pddate, digestOf (priceDirectivePart directive)) | directive <- directives]

monthChain :: [(Day, Digest)] -> [(Day, Hash)]
monthChain = go absentHash
  where
    go _ [] = []
    go previous entries@((day, _) : _) =
        let boundary = monthEnd day
            (thisMonth, laterMonths) = span ((== boundary) . monthEnd . fst) entries
            monthDigest = digestOf (listPart "month-v2" (digestPart . snd) thisMonth)
            !boundaryHash = combineHash previous (hexDigest monthDigest)
         in (boundary, boundaryHash) : go boundaryHash laterMonths

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
