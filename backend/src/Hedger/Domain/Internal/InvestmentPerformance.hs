module Hedger.Domain.Internal.InvestmentPerformance (
    -- * Value objects
    InvestmentCompleteness (..),
    InvestmentSummary (..),
    InvestmentWarning (..),
    WarningKind (..),
    SummaryResult (..),

    -- * Performance calculation
    summaryAt,
    investmentSummaryAt,
    emptySummary,

    -- * Ledger policies and derivations
    validateScope,
    isInvestmentAccount,
    investmentAccountsAt,
    investmentDataTransactions,
    immediateChildren,
    reportPeriods,
    excludedRoots,

    -- * Building blocks (exposed for property tests)
    holdingsAt,
    holdingsAtWithJournal,
    journalWithHistoricalCosts,
    investmentQuery,
    attributedAt,
    retainedPerformanceAt,
    hasAttributablePerformance,
    accountUsesExplicitCost,
)
where

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

import Hedger.Domain.Error (AppError (..))
import Hedger.Domain.Investment (accountWithin)
import Hedger.Domain.Investment qualified as Investment
import Hedger.Domain.Period (Granularity (..), epoch, minimumMaybe, monthKey, monthStart, takeLast)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (completeDecimal, missingCommodities, valueMixedAt)

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

{- | A data-quality condition detected while computing an investment summary.
The wording shown to a user is a boundary concern; the domain only records the
structured fact.
-}
data InvestmentWarning = InvestmentWarning
    { kind :: WarningKind
    , account :: Maybe Text
    }
    deriving stock (Eq, Show)

-- | The kind of data-quality gap, carrying the commodity/symbol it concerns.
data WarningKind
    = -- | No display-commodity price for the symbol (display commodity, symbol).
      MissingPrice Text Text
    | -- | No complete acquisition cost for the symbol.
      MissingCost Text
    | -- | An investment account mapping is missing, so income/gains are unavailable.
      MissingPerformanceMapping
    | -- | Acquisition cost is in a foreign commodity (display commodity, symbol).
      ForeignCost Text Text
    deriving stock (Eq, Show)

data SummaryResult = SummaryResult
    { summary :: InvestmentSummary
    , warnings :: [InvestmentWarning]
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
        valueRaw = valueMixedAt snapshot commodity day holdings
        costValued = valueMixedAt snapshot commodity day costRaw
        hasHoldings = any ((>= 0.000001) . abs . (.aquantity)) (HledgerAmount.amountsRaw holdings)
        market = completeDecimal commodity valueRaw
        rawMissingCost =
            Set.fromList
                [ item.acommodity
                | item <- HledgerAmount.amountsRaw costHoldings
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
                && all (hasAttributablePerformance snapshot mappings day) relevantAccounts
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
            [ InvestmentWarning (MissingPrice commodity symbol) Nothing
            | symbol <- Set.toList missingValue
            ]
                <> [ InvestmentWarning (MissingCost symbol) Nothing
                   | symbol <- Set.toList missingCost
                   ]
                <> [ InvestmentWarning MissingPerformanceMapping scope
                   | not (null relevantAccounts) && not mappingComplete
                   ]
                <> [ InvestmentWarning (ForeignCost commodity symbol) scope
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
            { Hledger.pamount =
                HledgerAmount.mapMixedAmount
                    (convertAmount day posting.paccount)
                    posting.pamount
            }
    convertAmount day account amount =
        amount
            { Hledger.acost =
                case amount.acost of
                    Just acquisitionCost -> Just (convertCost day acquisitionCost)
                    Nothing -> inferredCashCost day account amount
            }
    convertCost day acquisitionCost = case acquisitionCost of
        Hledger.UnitCost amount -> Hledger.UnitCost (convertCostAmount day amount)
        Hledger.TotalCost amount -> Hledger.TotalCost (convertCostAmount day amount)
    convertCostAmount day amount =
        fromMaybe
            amount
            ( HledgerAmount.unifyMixedAmount
                (valueMixedAt snapshot commodity day (HledgerAmount.mixedAmount amount))
            )
    inferredCashCost day account amount
        | "assets:cash" `accountWithin` account
            && amount.acommodity /= commodity =
            Hledger.UnitCost <$> historicalUnitPrice day amount
        | otherwise = Nothing
    historicalUnitPrice day amount = do
        valued <-
            HledgerAmount.unifyMixedAmount
                ( valueMixedAt
                    snapshot
                    commodity
                    day
                    ( HledgerAmount.mixedAmount
                        amount
                            { Hledger.aquantity = 1
                            , Hledger.acost = Nothing
                            }
                    )
                )
        if valued.acommodity == commodity then Just valued else Nothing

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
    contributions =
        [ do
            quantity <-
                completeDecimal
                    commodity
                    (valueMixedAt snapshot commodity transaction.tdate posting.pamount)
            pure $ case performance.role of
                Investment.RealisedGain -> (0, negate quantity, 0)
                Investment.Fee -> (0, 0, negate quantity)
                _ -> (negate quantity, 0, 0)
        | transaction <- snapshot.transactions
        , transaction.tdate <= day
        , mapping <- selectedMappings
        , transactionMatchesScope mapping transaction
        , posting <- transaction.tpostings
        , performance <- mapping.performanceAccounts
        , performance.account `accountWithin` posting.paccount
        ]
    transactionMatchesScope mapping transaction = case scope of
        Just selected
            | mapping.assetAccount /= selected
                && mapping.assetAccount `accountWithin` selected ->
                any
                    (\candidate -> selected `accountWithin` candidate.paccount)
                    transaction.tpostings
        _ -> True
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
            (completeDecimal commodity (valueMixedAt snapshot commodity transaction.tdate posting.pamount))
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
        , mappedAssetScope mapping `accountWithin` posting.paccount
        ]
    mappedAssetScope mapping = case scope of
        Just selected
            | mapping.assetAccount `accountWithin` selected -> selected
        _ -> mapping.assetAccount
    incomeLike role = case role of
        Investment.Interest -> True
        Investment.Dividend -> True
        Investment.OtherIncome -> True
        Investment.RealisedGain -> False
        Investment.Fee -> False

hasAttributablePerformance ::
    Snapshot ->
    Investment.InvestmentMappings ->
    Day ->
    Text ->
    Bool
hasAttributablePerformance snapshot mappings day asset =
    case Investment.mappingForAccount asset mappings of
        Just _ -> True
        Nothing -> accountUsesExplicitCost snapshot day asset

accountUsesExplicitCost :: Snapshot -> Day -> Text -> Bool
accountUsesExplicitCost snapshot day asset =
    any
        (\amount -> abs amount.aquantity >= 0.000001 && amount.acost /= Nothing)
        [ amount
        | transaction <- snapshot.transactions
        , transaction.tdate <= day
        , posting <- transaction.tpostings
        , posting.paccount == asset
        , amount <- HledgerAmount.amountsRaw posting.pamount
        ]

-- | The journal transactions that touch an investment account within scope.
investmentDataTransactions ::
    Snapshot -> Investment.InvestmentMappings -> Maybe Text -> [Hledger.Transaction]
investmentDataTransactions snapshot mappings scope =
    filter
        (any (isInvestmentAccount mappings scope . (.paccount)) . (.tpostings))
        snapshot.transactions

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

reportPeriods :: Granularity -> [Hledger.Transaction] -> Day -> [(Text, Day, Day)]
reportPeriods granularity transactions latest = case granularity of
    Monthly ->
        [ let end = min latest (addDays (-1) (addGregorianMonthsClip 1 start))
           in (monthKey start, start, end)
        | start <- monthlyStarts
        ]
    Yearly ->
        [ let start = fromGregorian year 1 1
              end = min latest (fromGregorian year 12 31)
           in (Text.pack (show year), start, end)
        | year <- [firstYear .. finalYear]
        ]
  where
    (finalYear, _, _) = toGregorian latest
    monthlyStarts =
        takeLast
            24
            ( Set.toAscList
                (Set.fromList (fmap (monthStart . (.tdate)) transactions))
            )
    firstYear =
        maybe
            finalYear
            (\day -> let (year, _, _) = toGregorian day in year)
            (minimumMaybe (fmap (.tdate) transactions))

validateScope :: Investment.InvestmentMappings -> Maybe Text -> Either AppError (Maybe Text)
validateScope mappings requested = case requested >>= scopeText of
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

scopeText :: Text -> Maybe Text
scopeText value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just (Text.strip value)

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
