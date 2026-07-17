module Hedger.Domain.Transaction (
    CommodityQuantity (..),
    TransactionPresentation (..),
    newestFirst,
    presentationFor,
)
where

import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (Down))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Hledger.Data.Amount qualified as Hledger
import Hledger.Data.Types qualified as Hledger

data CommodityQuantity = CommodityQuantity
    { commodity :: Text
    , quantity :: Text
    }
    deriving (Eq, Show)

data TransactionPresentation = TransactionPresentation
    { classification :: Text
    , fromAccounts :: [Text]
    , toAccounts :: [Text]
    , totalAmounts :: [CommodityQuantity]
    , openingAccountCount :: Maybe Int
    }
    deriving (Eq, Show)

newestFirst :: [Hledger.Transaction] -> [Hledger.Transaction]
newestFirst = List.sortOn (\transaction -> (Down transaction.tdate, Down transaction.tindex))

data Entry = Entry
    { account :: Text
    , sourceAccount :: Text
    , amount :: Hledger.Amount
    }

data FlowTone = Positive | Negative | Neutral
    deriving (Eq)

presentationFor :: Hledger.Transaction -> TransactionPresentation
presentationFor transaction =
    TransactionPresentation
        { classification = classification
        , fromAccounts = fromAccounts
        , toAccounts = toAccounts
        , totalAmounts = aggregateAmounts selectedEntries
        , openingAccountCount = if opening then Just (accountCount debits) else Nothing
        }
  where
    entries = postingEntries transaction.tpostings
    debits = filter ((> 0) . (.amount.aquantity)) entries
    credits = filter ((< 0) . (.amount.aquantity)) entries
    rawFromAccounts = sortedAccounts credits
    rawToAccounts = sortedAccounts debits
    opening = isOpeningSetup credits debits
    fromAccounts = if opening then ["opening"] else rawFromAccounts
    toAccounts = if opening then rawToAccounts else dominantToAccounts rawFromAccounts rawToAccounts debits
    tone = toneFromFlow fromAccounts toAccounts
    classification
        | opening = "opening_balance"
        | tone == Positive = "income"
        | tone == Negative = "expense"
        | otherwise = "transfer"
    selectedEntries
        | opening = []
        | otherwise = relevantEntries tone fromAccounts toAccounts credits debits

postingEntries :: [Hledger.Posting] -> [Entry]
postingEntries =
    concatMap
        ( \posting ->
            [ Entry
                { account = topLevel posting.paccount
                , sourceAccount = posting.paccount
                , amount = amount
                }
            | amount <- Hledger.amountsRaw posting.pamount
            , amount.aquantity /= 0
            ]
        )

topLevel :: Text -> Text
topLevel = Text.toLower . Text.takeWhile (/= ':')

sortedAccounts :: [Entry] -> [Text]
sortedAccounts = List.sortOn accountOrder . Set.toList . Set.fromList . fmap (.account)

accountOrder :: Text -> Int
accountOrder account =
    case List.elemIndex account ["assets", "income", "equity", "liabilities", "expenses", "opening", "other"] of
        Just index -> index
        Nothing -> 7

toneFromFlow :: [Text] -> [Text] -> FlowTone
toneFromFlow fromAccounts toAccounts
    | not (Set.null fromSet) && fromSet == toSet = Neutral
    | Set.member "expenses" toSet = Negative
    | Set.member "assets" toSet && not (Set.null (Set.delete "assets" fromSet)) = Positive
    | otherwise = Neutral
  where
    fromSet = Set.fromList fromAccounts
    toSet = Set.fromList toAccounts

dominantToAccounts :: [Text] -> [Text] -> [Entry] -> [Text]
dominantToAccounts fromAccounts toAccounts debitEntries
    | length fromAccounts == 1 && length toAccounts > 1 =
        case sortedTotals of
            [] -> toAccounts
            (account, _) : _ -> [account]
    | otherwise = toAccounts
  where
    totals =
        Map.fromListWith
            (+)
            [(entry.account, abs entry.amount.aquantity) | entry <- debitEntries]
    sortedTotals = List.sortBy compareTotals (Map.toList totals)
    compareTotals (leftAccount, leftTotal) (rightAccount, rightTotal)
        | leftTotal == rightTotal = compare (accountOrder leftAccount) (accountOrder rightAccount)
        | otherwise = compare rightTotal leftTotal

relevantEntries :: FlowTone -> [Text] -> [Text] -> [Entry] -> [Entry] -> [Entry]
relevantEntries tone fromAccounts toAccounts credits debits
    | null selected = debits <> credits
    | otherwise = selected
  where
    selected
        | tone == Neutral && not (null fromAccounts) && Set.fromList fromAccounts == Set.fromList toAccounts =
            filter ((`elem` fromAccounts) . (.account)) credits
        | otherwise = filter ((`elem` toAccounts) . (.account)) debits

aggregateAmounts :: [Entry] -> [CommodityQuantity]
aggregateAmounts entries =
    [ CommodityQuantity
        { commodity = commodity
        , quantity = Text.pack (show quantity)
        }
    | (commodity, quantity) <- Map.toAscList totals
    ]
  where
    totals :: Map Text Hledger.Quantity
    totals =
        Map.fromListWith
            (+)
            [(entry.amount.acommodity, abs entry.amount.aquantity) | entry <- entries]

isOpeningSetup :: [Entry] -> [Entry] -> Bool
isOpeningSetup credits debits =
    Set.member "equity" accounts
        && Set.null (Set.intersection accounts (Set.fromList ["income", "expenses"]))
        && not (null debits)
        && any hasOpeningSource credits
  where
    accounts = Set.fromList ((.account) <$> credits <> debits)
    hasOpeningSource entry =
        any (`Text.isInfixOf` Text.toLower entry.sourceAccount) ["opening", "open", "balance", "balances", "conversion", "migration", "initial"]

accountCount :: [Entry] -> Int
accountCount = Set.size . Set.fromList . fmap (.sourceAccount)
