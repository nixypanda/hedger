module Hedger.Application.Investments (
    InvestmentsRequest (..),
    InvestmentCompleteness (..),
    InvestmentSummary (..),
    InvestmentPoint (..),
    InvestmentBreadcrumb (..),
    InvestmentChild (..),
    InvestmentWarning (..),
    InvestmentsReport (..),
    investments,
    investmentSummaryAt,
)
where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Decimal (Decimal)
import Data.List qualified as List
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, addDays, addGregorianMonthsClip, fromGregorian, toGregorian)

import Hledger.Data.AccountName qualified as HledgerAccountName
import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Period qualified as HledgerPeriod
import Hledger.Data.Types qualified as Hledger
import Hledger.Query qualified as HledgerQuery
import Hledger.Reports.BalanceReport qualified as HledgerBalance
import Hledger.Reports.ReportOptions qualified as HledgerReportOptions

import Hedger.Application.Analytics (Granularity (..))
import Hedger.Application.AppM (AppM, Env (..), requireSnapshot)
import Hedger.Application.Common (SnapshotMetadata, metadataFor)
import Hedger.Config (Config (..))
import Hedger.Domain.Account qualified as Account
import Hedger.Domain.Error (AppError (..))
import Hedger.Domain.Investment qualified as Investment
import Hedger.Domain.Snapshot (Snapshot (..))

data InvestmentsRequest = InvestmentsRequest
    { granularity :: Granularity
    , displayCommodity :: Maybe Text
    , accountPrefix :: Maybe Text
    }

data InvestmentCompleteness = Complete | Partial | Unavailable
    deriving stock (Eq, Show)

data InvestmentSummary = InvestmentSummary
    { marketValue :: Maybe Decimal
    , purchaseCost :: Maybe Decimal
    , gainLoss :: Maybe Decimal
    , returnPercentage :: Maybe Decimal
    , attributedIncome :: Maybe Decimal
    , realisedGain :: Maybe Decimal
    , totalProfit :: Maybe Decimal
    , completeness :: InvestmentCompleteness
    }

data InvestmentPoint = InvestmentPoint
    { period :: Text
    , startDate :: Day
    , summary :: InvestmentSummary
    }

data InvestmentBreadcrumb = InvestmentBreadcrumb
    { account :: Text
    , label :: Text
    }

data InvestmentChild = InvestmentChild
    { account :: Text
    , label :: Text
    , summary :: InvestmentSummary
    , hasChildren :: Bool
    }

data InvestmentWarning = InvestmentWarning
    { kind :: Text
    , account :: Maybe Text
    , message :: Text
    }
    deriving stock (Eq, Show)

data InvestmentsReport = InvestmentsReport
    { metadata :: SnapshotMetadata
    , displayCommodity :: Text
    , granularity :: Granularity
    , accountPrefix :: Maybe Text
    , summary :: InvestmentSummary
    , timeline :: [InvestmentPoint]
    , breadcrumbs :: [InvestmentBreadcrumb]
    , children :: [InvestmentChild]
    , warnings :: [InvestmentWarning]
    }

data SummaryResult = SummaryResult
    { summary :: InvestmentSummary
    , warnings :: [InvestmentWarning]
    }

investments :: InvestmentsRequest -> AppM InvestmentsReport
investments request = do
    snapshot <- requireSnapshot
    config <- asks (.config)
    scope <- either throwError pure (validateScope config.investmentMappings request.accountPrefix)
    responseMetadata <- metadataFor snapshot
    let commodity = fromMaybe config.defaultDisplayCommodity (request.displayCommodity >>= nonEmpty)
        latest = fromMaybe epoch snapshot.latestJournalDate
        current = summaryAt snapshot config.investmentMappings commodity scope latest
        hasInvestments = current.summary.marketValue /= Nothing || current.summary.purchaseCost /= Nothing
        points =
            if hasInvestments
                then
                    [ InvestmentPoint
                        key
                        start
                        (summaryAt snapshot config.investmentMappings commodity scope end).summary
                    | (key, start, end) <- reportPeriods request.granularity snapshot latest
                    ]
                else []
        childAccounts = immediateChildren snapshot config.investmentMappings scope
        childResults =
            [ (child, summaryAt snapshot config.investmentMappings commodity (Just child) latest)
            | child <- childAccounts
            ]
        children =
            [ InvestmentChild
                { account = child
                , label = accountLabel child
                , summary = result.summary
                , hasChildren = not (null (immediateChildren snapshot config.investmentMappings (Just child)))
                }
            | (child, result) <- childResults
            ]
        allWarnings = current.warnings
    pure
        InvestmentsReport
            { metadata = responseMetadata
            , displayCommodity = commodity
            , granularity = request.granularity
            , accountPrefix = scope
            , summary = current.summary
            , timeline = points
            , breadcrumbs = breadcrumbsFor scope
            , children
            , warnings = allWarnings
            }

summaryAt :: Snapshot -> Investment.InvestmentMappings -> Text -> Maybe Text -> Day -> SummaryResult
summaryAt snapshot mappings commodity scope day =
    let holdings = holdingsAt snapshot mappings scope day
        costHoldings =
            holdingsAtWithJournal
                (journalWithHistoricalCosts snapshot commodity)
                mappings
                scope
                day
        costRaw = HledgerAmount.mixedAmountCost costHoldings
        valueRaw = valueAt snapshot commodity day holdings
        costValued = valueAt snapshot commodity day costRaw
        hasHoldings = any ((>= 0.000001) . abs . (.aquantity)) (HledgerAmount.amountsRaw holdings)
        market = completeDecimal commodity valueRaw
        rawMissingCost =
            Set.fromList
                [ item.acommodity
                | item <- HledgerAmount.amountsRaw holdings
                , abs item.aquantity >= 0.000001
                , item.acommodity /= commodity
                , item.acost == Nothing
                ]
        foreignCost =
            Set.fromList
                [ item.acommodity
                | item <- HledgerAmount.amountsRaw costRaw
                , abs item.aquantity >= 0.000001
                , item.acommodity /= commodity
                ]
        reportedCost =
            if Set.null rawMissingCost && Set.null foreignCost
                then completeDecimal commodity costValued
                else Nothing
        relevantAccounts = investmentAccountsAt snapshot mappings scope day
        mappingComplete =
            not (null relevantAccounts)
                && all (hasAttributableMapping mappings scope) relevantAccounts
        retained =
            if mappingComplete
                then retainedPerformanceAt snapshot mappings commodity scope day
                else Just 0
        cost = (-) <$> reportedCost <*> retained
        calculatedGain = (-) <$> market <*> reportedCost
        attributed = if mappingComplete then attributedAt snapshot mappings commodity scope day else Nothing
        income = attributed >>= (Just . (\(value, _, _) -> value))
        realised = attributed >>= (Just . (\(_, value, _) -> value))
        fees = attributed >>= (Just . (\(_, _, value) -> value))
        total = (+) <$> calculatedGain <*> ((+) <$> ((+) <$> income <*> realised) <*> fees)
        percent = do
            profit <- total
            basis <- cost
            if abs basis < 0.000001 then Nothing else Just (profit / abs basis * 100)
        missingValue = missingCommodities commodity valueRaw
        missingCost = missingCommodities commodity costValued <> rawMissingCost
        warnings =
            [ warning "missing-price" Nothing ("No " <> commodity <> " price for " <> symbol <> ".")
            | symbol <- Set.toList missingValue
            ]
                <> [ warning "missing-cost" Nothing ("No complete acquisition cost for " <> symbol <> ".")
                   | symbol <- Set.toList missingCost
                   ]
                <> [ warning
                        "missing-performance-mapping"
                        scope
                        ( "Income and realised gains are unavailable because "
                            <> "an investment account mapping is missing."
                        )
                   | not (null relevantAccounts) && not mappingComplete
                   ]
                <> [ warning
                        "foreign-cost"
                        scope
                        ( "Acquisition cost in "
                            <> symbol
                            <> " cannot be reported as historical "
                            <> commodity
                            <> " cost."
                        )
                   | symbol <- Set.toList foreignCost
                   ]
        complete =
            if market == Nothing && cost == Nothing
                then Unavailable
                else if null warnings && total /= Nothing then Complete else Partial
     in if not hasHoldings
            then SummaryResult{summary = emptySummary, warnings = []}
            else
                SummaryResult
                    { summary =
                        InvestmentSummary
                            { marketValue = market
                            , purchaseCost = cost
                            , gainLoss = calculatedGain
                            , returnPercentage = percent
                            , attributedIncome = income
                            , realisedGain = realised
                            , totalProfit = total
                            , completeness = complete
                            }
                    , warnings
                    }

investmentSummaryAt ::
    Snapshot ->
    Investment.InvestmentMappings ->
    Text ->
    Maybe Text ->
    Day ->
    InvestmentSummary
investmentSummaryAt snapshot mappings commodity scope day =
    (summaryAt snapshot mappings commodity scope day).summary

holdingsAt :: Snapshot -> Investment.InvestmentMappings -> Maybe Text -> Day -> Hledger.MixedAmount
holdingsAt snapshot = holdingsAtWithJournal snapshot.journal

holdingsAtWithJournal ::
    Hledger.Journal ->
    Investment.InvestmentMappings ->
    Maybe Text ->
    Day ->
    Hledger.MixedAmount
holdingsAtWithJournal journal mappings scope day =
    foldl HledgerAmount.maPlus HledgerAmount.nullmixedamt reports
  where
    reports = case scope of
        Just selected -> [balanceFor scope (isExcluded selected)]
        Nothing ->
            balanceFor Nothing False
                : [balanceFor (Just mapped) True | mapped <- minimalExcludedMappedRoots]
    balanceFor selected includeExcluded =
        snd (HledgerBalance.balanceReport (spec selected includeExcluded) journal)
    period = Hledger.PeriodTo (addDays 1 day)
    options =
        HledgerReportOptions.defreportopts
            { HledgerReportOptions.period_ = period
            , HledgerReportOptions.balanceaccum_ = HledgerReportOptions.Historical
            , HledgerReportOptions.accountlistmode_ = HledgerReportOptions.ALFlat
            }
    spec selected includeExcluded =
        HledgerReportOptions.defreportspec
            { HledgerReportOptions._rsReportOpts = options
            , HledgerReportOptions._rsDay = day
            , HledgerReportOptions._rsQuery =
                HledgerQuery.And
                    [ investmentQuery selected includeExcluded
                    , HledgerQuery.Date (HledgerPeriod.periodAsDateSpan period)
                    ]
            }
    isExcluded name = any (`accountWithin` name) excludedRoots
    minimalExcludedMappedRoots =
        [ name
        | name <- Investment.mappedAssetAccounts mappings
        , isExcluded name
        , not
            ( any
                (\other -> other /= name && other `accountWithin` name)
                (Investment.mappedAssetAccounts mappings)
            )
        ]

journalWithHistoricalCosts :: Snapshot -> Text -> Hledger.Journal
journalWithHistoricalCosts snapshot commodity =
    snapshot.journal
        { Hledger.jtxns = convertTransaction <$> snapshot.journal.jtxns
        }
  where
    convertTransaction transaction =
        transaction
            { Hledger.tpostings = convertPosting transaction.tdate <$> transaction.tpostings
            }
    convertPosting day posting =
        posting
            { Hledger.pamount = HledgerAmount.mapMixedAmount (convertAmount day) posting.pamount
            }
    convertAmount day amount = amount{Hledger.acost = convertCost day <$> amount.acost}
    convertCost day acquisitionCost = case acquisitionCost of
        Hledger.UnitCost amount -> Hledger.UnitCost (convertCostAmount day amount)
        Hledger.TotalCost amount -> Hledger.TotalCost (convertCostAmount day amount)
    convertCostAmount day amount =
        fromMaybe
            amount
            ( HledgerAmount.unifyMixedAmount
                (valueAt snapshot commodity day (HledgerAmount.mixedAmount amount))
            )

investmentQuery :: Maybe Text -> Bool -> HledgerQuery.Query
investmentQuery scope includeExcluded =
    HledgerQuery.And
        ( [HledgerQuery.Type [Hledger.Asset]]
            <> (if includeExcluded then [] else defaultExclusions)
            <> scopeQuery
        )
  where
    scopeQuery =
        maybe
            []
            (\name -> [HledgerQuery.Acct (HledgerAccountName.accountNameToAccountRegex name)])
            scope
    defaultExclusions =
        [ HledgerQuery.Not (HledgerQuery.Acct (HledgerAccountName.accountNameToAccountRegex name))
        | name <- excludedRoots
        ]

excludedRoots :: [Text]
excludedRoots = ["assets:cash", "assets:property", "assets:receivables"]

valueAt :: Snapshot -> Text -> Day -> Hledger.MixedAmount -> Hledger.MixedAmount
valueAt snapshot commodity day = Account.valueAt (Account.mkValuationContext snapshot.journal commodity day)

completeDecimal :: Text -> Hledger.MixedAmount -> Maybe Decimal
completeDecimal commodity amount
    | Set.null missing = Just (sum [item.aquantity | item <- projected])
    | otherwise = Nothing
  where
    projected = filter ((>= 0.000001) . abs . (.aquantity)) (HledgerAmount.amountsRaw amount)
    missing = Set.fromList [item.acommodity | item <- projected, item.acommodity /= commodity]

missingCommodities :: Text -> Hledger.MixedAmount -> Set.Set Text
missingCommodities commodity amount =
    Set.fromList
        [ item.acommodity
        | item <- HledgerAmount.amountsRaw amount
        , abs item.aquantity >= 0.000001
        , item.acommodity /= commodity
        ]

attributedAt ::
    Snapshot ->
    Investment.InvestmentMappings ->
    Text ->
    Maybe Text ->
    Day ->
    Maybe (Decimal, Decimal, Decimal)
attributedAt snapshot mappings commodity scope day =
    foldl combine (Just (0, 0, 0)) contributions
  where
    selectedMappings = case scope of
        Nothing -> Investment.mappingsForScope "" mappings
        Just selected -> Investment.mappingsForScope selected mappings
    accounts = concatMap (.performanceAccounts) selectedMappings
    contributions =
        [ do
            quantity <-
                completeDecimal
                    commodity
                    (valueAt snapshot commodity transaction.tdate posting.pamount)
            pure $ case performance.role of
                Investment.RealisedGain -> (0, negate quantity, 0)
                Investment.Fee -> (0, 0, negate quantity)
                _ -> (negate quantity, 0, 0)
        | transaction <- snapshot.transactions
        , transaction.tdate <= day
        , posting <- transaction.tpostings
        , performance <- accounts
        , performance.account `accountWithin` posting.paccount
        ]
    combine Nothing _ = Nothing
    combine _ Nothing = Nothing
    combine (Just (incomeA, gainA, feeA)) (Just (incomeB, gainB, feeB)) =
        Just (incomeA + incomeB, gainA + gainB, feeA + feeB)

retainedPerformanceAt ::
    Snapshot ->
    Investment.InvestmentMappings ->
    Text ->
    Maybe Text ->
    Day ->
    Maybe Decimal
retainedPerformanceAt snapshot mappings commodity scope day = fmap sum (traverse id retainedAmounts)
  where
    selectedMappings = case scope of
        Nothing -> Investment.mappingsForScope "" mappings
        Just selected -> Investment.mappingsForScope selected mappings
    retainedAmounts =
        [ fmap
            (max 0)
            (completeDecimal commodity (valueAt snapshot commodity transaction.tdate posting.pamount))
        | mapping <- selectedMappings
        , transaction <- snapshot.transactions
        , transaction.tdate <= day
        , any
            ( \performance ->
                incomeLike performance.role
                    && any
                        (\candidate -> performance.account `accountWithin` candidate.paccount)
                        transaction.tpostings
            )
            mapping.performanceAccounts
        , posting <- transaction.tpostings
        , mapping.assetAccount `accountWithin` posting.paccount
        ]
    incomeLike role = case role of
        Investment.Interest -> True
        Investment.Dividend -> True
        Investment.OtherIncome -> True
        Investment.RealisedGain -> False
        Investment.Fee -> False

hasAttributableMapping :: Investment.InvestmentMappings -> Maybe Text -> Text -> Bool
hasAttributableMapping mappings _ asset = case Investment.mappingForAccount asset mappings of
    Nothing -> False
    Just _ -> True

investmentAccountsAt :: Snapshot -> Investment.InvestmentMappings -> Maybe Text -> Day -> [Text]
investmentAccountsAt snapshot mappings scope day =
    List.nub
        [ posting.paccount
        | transaction <- snapshot.transactions
        , transaction.tdate <= day
        , posting <- transaction.tpostings
        , isInvestmentAccount mappings scope posting.paccount
        ]

isInvestmentAccount :: Investment.InvestmentMappings -> Maybe Text -> Text -> Bool
isInvestmentAccount mappings scope name =
    "assets" `accountWithin` name
        && case scope of
            Just selected -> selected `accountWithin` name && allowed
            Nothing -> allowed
  where
    allowed =
        not (any (`accountWithin` name) excludedRoots)
            || Investment.mappingForAccount name mappings /= Nothing

immediateChildren :: Snapshot -> Investment.InvestmentMappings -> Maybe Text -> [Text]
immediateChildren snapshot mappings scope =
    List.sort . Set.toList . Set.fromList $
        mapMaybe
            childOf
            (investmentAccountsAt snapshot mappings scope (fromMaybe epoch snapshot.latestJournalDate))
  where
    parentParts = maybe ["assets"] HledgerAccountName.accountNameComponents scope
    childOf name =
        let parts = HledgerAccountName.accountNameComponents name
         in if length parts > length parentParts
                then Just (Text.intercalate ":" (take (length parentParts + 1) parts))
                else Nothing

breadcrumbsFor :: Maybe Text -> [InvestmentBreadcrumb]
breadcrumbsFor Nothing = []
breadcrumbsFor (Just scope) =
    [ InvestmentBreadcrumb (Text.intercalate ":" (take index parts)) part
    | (index, part) <- zip [1 ..] parts
    ]
  where
    parts = HledgerAccountName.accountNameComponents scope

accountLabel :: Text -> Text
accountLabel name = fromMaybe name (lastMay (HledgerAccountName.accountNameComponents name))

reportPeriods :: Granularity -> Snapshot -> Day -> [(Text, Day, Day)]
reportPeriods granularity snapshot latest = case granularity of
    Monthly ->
        [ let start = addGregorianMonthsClip offset finalStart
              end = min latest (addDays (-1) (addGregorianMonthsClip 1 start))
           in (monthKey start, start, end)
        | offset <- [-23 .. 0]
        ]
    Yearly ->
        [ let start = fromGregorian year 1 1
              end = min latest (fromGregorian year 12 31)
           in (Text.pack (show year), start, end)
        | year <- [firstYear .. finalYear]
        ]
  where
    (finalYear, finalMonth, _) = toGregorian latest
    finalStart = fromGregorian finalYear finalMonth 1
    firstYear =
        maybe
            finalYear
            (\day -> let (year, _, _) = toGregorian day in year)
            (minimumMay (fmap (.tdate) snapshot.transactions))

monthKey :: Day -> Text
monthKey day =
    let (year, month, _) = toGregorian day
     in Text.pack (show year <> "-" <> if month < 10 then "0" <> show month else show month)

warning :: Text -> Maybe Text -> Text -> InvestmentWarning
warning kind account message = InvestmentWarning{kind, account, message}

nonEmpty :: Text -> Maybe Text
nonEmpty value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just (Text.strip value)

validateScope :: Investment.InvestmentMappings -> Maybe Text -> Either AppError (Maybe Text)
validateScope mappings requested = case requested >>= nonEmpty of
    Nothing -> Right Nothing
    Just scope
        | "assets" `accountWithin` scope
            && ( not (any (`accountWithin` scope) excludedRoots)
                    || not (null (Investment.mappingsForScope scope mappings))
                    || Investment.mappingForAccount scope mappings /= Nothing
               ) ->
            Right (Just scope)
        | otherwise ->
            Left
                ( InvalidRequest
                    ( "Account prefix must be an investment asset account outside "
                        <> "cash, property, and receivables."
                    )
                )

emptySummary :: InvestmentSummary
emptySummary =
    InvestmentSummary
        { marketValue = Nothing
        , purchaseCost = Nothing
        , gainLoss = Nothing
        , returnPercentage = Nothing
        , attributedIncome = Nothing
        , realisedGain = Nothing
        , totalProfit = Nothing
        , completeness = Unavailable
        }

epoch :: Day
epoch = fromGregorian 1970 1 1

accountWithin :: Text -> Text -> Bool
accountWithin prefix account = prefix == account || (prefix <> ":") `Text.isPrefixOf` account

lastMay :: [value] -> Maybe value
lastMay [] = Nothing
lastMay values = Just (last values)

minimumMay :: (Ord value) => [value] -> Maybe value
minimumMay [] = Nothing
minimumMay values = Just (minimum values)
