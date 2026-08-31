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
import Data.Word (Word8)
import Numeric (showHex)

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
combineHash previous content = hashTagged "chain-v1" [unHash previous, content]

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

-- | The chain hash covering every entry up to the end of the month containing
-- this day, if that month has an entry.
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
-}
transactionHash :: Hledger.Transaction -> Hash
transactionHash transaction =
    hashTagged
        "transaction-v1"
        [ dayPart transaction.tdate
        , textShow transaction.tdate2
        , statusPart transaction.tstatus
        , transaction.tcode
        , transaction.tdescription
        , transaction.tcomment
        , unHash (tagsHash transaction.ttags)
        , unHash (hashList "postings-v1" (postingHash <$> transaction.tpostings))
        ]

{- | Semantic content of a price directive, keyed by the directive's own date so
that inserting a historical price invalidates exactly the months from that date
onwards. @pdsourcepos@ is excluded, as in 'transactionHash'.
-}
priceDirectiveHash :: Hledger.PriceDirective -> Hash
priceDirectiveHash directive =
    hashTagged
        "price-directive-v1"
        [ dayPart directive.pddate
        , directive.pdcommodity
        , directive.pdamount.acommodity
        , quantityPart directive.pdamount.aquantity
        ]

postingHash :: Hledger.Posting -> Hash
postingHash posting =
    hashTagged
        "posting-v1"
        [ posting.paccount
        , statusPart posting.pstatus
        , textShow posting.ptype
        , posting.pcomment
        , textShow posting.pdate
        , textShow posting.pdate2
        , unHash (mixedAmountHash posting.pamount)
        , unHash (maybe absentHash balanceAssertionHash posting.pbalanceassertion)
        , unHash (tagsHash posting.ptags)
        ]

mixedAmountHash :: Hledger.MixedAmount -> Hash
mixedAmountHash = hashList "mixed-amount-v1" . fmap amountHash . HledgerAmount.amountsRaw

-- | @astyle@ is absent because display styles are journal-global and therefore
-- belong to 'GlobalFingerprint', not to per-month content.
amountHash :: Hledger.Amount -> Hash
amountHash amount =
    hashTagged
        "amount-v1"
        [ amount.acommodity
        , quantityPart amount.aquantity
        , unHash (maybe absentHash costHash amount.acost)
        , unHash (maybe absentHash costBasisHash amount.acostbasis)
        ]

costHash :: Hledger.AmountCost -> Hash
costHash cost = case cost of
    Hledger.UnitCost amount -> hashTagged "unit-cost-v1" [unHash (amountHash amount)]
    Hledger.TotalCost amount -> hashTagged "total-cost-v1" [unHash (amountHash amount)]

costBasisHash :: Hledger.CostBasis -> Hash
costBasisHash basis =
    hashTagged
        "cost-basis-v1"
        [ unHash (maybe absentHash amountHash basis.cbCost)
        , textShow basis.cbDate
        , textShow basis.cbLabel
        ]

-- | @baposition@ is a source position; see the note on 'transactionHash'.
balanceAssertionHash :: Hledger.BalanceAssertion -> Hash
balanceAssertionHash assertion =
    hashTagged
        "balance-assertion-v1"
        [ unHash (amountHash assertion.baamount)
        , textShow assertion.batotal
        , textShow assertion.bainclusive
        ]

tagsHash :: [Hledger.Tag] -> Hash
tagsHash tags = hashTagged "tags-v1" (concat [[name, value] | (name, value) <- tags])

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
sortedEntries :: [Hledger.Transaction] -> [Hledger.PriceDirective] -> [(Day, Hash)]
sortedEntries transactions directives =
    List.sortOn fst $
        [(transaction.tdate, transactionHash transaction) | transaction <- transactions]
            <> [(directive.pddate, priceDirectiveHash directive) | directive <- directives]

monthChain :: [(Day, Hash)] -> [(Day, Hash)]
monthChain = go absentHash
  where
    go _ [] = []
    go previous entries@((day, _) : _) =
        let boundary = monthEnd day
            (thisMonth, laterMonths) = span ((== boundary) . monthEnd . fst) entries
            boundaryHash = combineHash previous (unHash (hashList "month-v1" (snd <$> thisMonth)))
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
lengthDelimited value = Text.pack (show (Text.length value)) <> ":" <> value

dayPart :: Day -> Text
dayPart = Text.pack . Calendar.showGregorian

-- | Normalising keeps hash equality in step with 'Eq': @Show@ distinguishes
-- @1.5@ from @1.50@, whereas 'Eq' on 'Decimal' does not.
quantityPart :: Decimal -> Text
quantityPart = textShow . Decimal.normalizeDecimal

-- | @Show Status@ renders @Unmarked@ as the empty string, which would collide
-- with an absent field.
statusPart :: Hledger.Status -> Text
statusPart = textShow . fromEnum

textShow :: (Show value) => value -> Text
textShow = Text.pack . show

hexEncode :: ByteString.ByteString -> Text
hexEncode = Text.pack . concatMap byteHex . ByteString.unpack

byteHex :: Word8 -> String
byteHex byte =
    case showHex byte "" of
        [digit] -> ['0', digit]
        digits -> digits
