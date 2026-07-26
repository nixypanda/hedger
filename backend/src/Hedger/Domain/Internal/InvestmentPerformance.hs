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

    -- * Building blocks (exposed for property tests)
    holdingsAt,
    holdingsAtWithJournal,
    journalWithHistoricalCosts,
    investmentQuery,
    attributedAt,
    retainedPerformanceAt,
    hasAttributablePerformance,
    accountUsesExplicitCost,
    externalCashFlowsAt,
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
import Hedger.Domain.Period (Granularity (..), epoch, minimumMaybe, monthKey, monthStart)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (completeDecimal, missingCommodities, valueMixedAt)
import Hedger.Domain.Xirr qualified as Xirr

data InvestmentCompleteness = Complete | Partial | Unavailable
    deriving stock (Eq, Show)

data InvestmentSummary = InvestmentSummary
    { marketValue :: Maybe Decimal
    , purchaseCost :: Maybe Decimal
    , gainLoss :: Maybe Decimal
    , xirr :: Maybe Decimal
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
        annualisedReturn =
            if mappingComplete
                then xirrAt snapshot mappings commodity scope day market
                else Nothing
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
                            , xirr = annualisedReturn
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
holdingsAtWithJournal journal _ scope day =
    snd (HledgerBalance.balanceReport (spec scope) journal)
  where
    period = Hledger.PeriodTo (addDays 1 day)
    options =
        HledgerReportOptions.defreportopts
            { HledgerReportOptions.period_ = period
            , HledgerReportOptions.balanceaccum_ = HledgerReportOptions.Historical
            , HledgerReportOptions.accountlistmode_ = HledgerReportOptions.ALFlat
            }
    spec selected =
        HledgerReportOptions.defreportspec
            { HledgerReportOptions._rsReportOpts = options
            , HledgerReportOptions._rsDay = day
            , HledgerReportOptions._rsQuery =
                HledgerQuery.And
                    [ investmentQuery selected
                    , HledgerQuery.Date (HledgerPeriod.periodAsDateSpan period)
                    ]
            }

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

investmentQuery :: Maybe Text -> HledgerQuery.Query
investmentQuery scope =
    HledgerQuery.And
        ( [HledgerQuery.Type [Hledger.Asset]]
            <> scopeQuery
        )
  where
    scopeQuery =
        maybe
            []
            (\name -> [HledgerQuery.Acct (HledgerAccountName.accountNameToAccountRegex name)])
            scope

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
        , transactionMatchesScope scope mapping transaction
        , posting <- transaction.tpostings
        , performance <- mapping.performanceAccounts
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

{- | External investor cash flows for an investment scope, valued on each
transaction date. Contributions are negative; withdrawals and cash-paid income
are positive. The calculation uses postings outside the selected investment
scope and excludes mapped performance postings. This preserves actual purchase
and sale cash, while reinvested income, deducted fees, and transfers wholly
inside the scope cancel to zero.
-}
externalCashFlowsAt ::
    Snapshot ->
    Investment.InvestmentMappings ->
    Text ->
    Maybe Text ->
    Day ->
    Maybe [Xirr.CashFlow]
externalCashFlowsAt snapshot mappings commodity scope day =
    traverse transactionCashFlow relevantTransactions
  where
    selectedMappings = case scope of
        Nothing -> Investment.mappingsForScope "" mappings
        Just selected -> Investment.mappingsForScope selected mappings
    relevantTransactions =
        [ transaction
        | transaction <- snapshot.transactions
        , transaction.tdate <= day
        , transactionTouchesScope transaction || transactionHasSelectedPerformance transaction
        ]
    transactionTouchesScope =
        any (isInvestmentAccount mappings scope . (.paccount)) . (.tpostings)
    transactionHasSelectedPerformance transaction =
        any
            ( \mapping ->
                transactionMatchesScope scope mapping transaction
                    && any
                        ( \performance ->
                            any
                                (\posting -> performance.account `accountWithin` posting.paccount)
                                transaction.tpostings
                        )
                        mapping.performanceAccounts
            )
            selectedMappings
    transactionCashFlow transaction = do
        externalMovement <-
            valuePostings
                transaction
                [ posting
                | posting <- transaction.tpostings
                , not (isInvestmentAccount mappings scope posting.paccount)
                , not (isSelectedPerformancePosting transaction posting)
                ]
        pure (Xirr.cashFlow transaction.tdate externalMovement)
    valuePostings transaction =
        fmap sum
            . traverse
                (completeDecimal commodity . valueMixedAt snapshot commodity transaction.tdate . (.pamount))
    isSelectedPerformancePosting transaction posting =
        any
            ( \mapping ->
                transactionMatchesScope scope mapping transaction
                    && any
                        (\performance -> performance.account `accountWithin` posting.paccount)
                        mapping.performanceAccounts
            )
            selectedMappings

xirrAt ::
    Snapshot ->
    Investment.InvestmentMappings ->
    Text ->
    Maybe Text ->
    Day ->
    Maybe Decimal ->
    Maybe Decimal
xirrAt snapshot mappings commodity scope day terminalMarket = do
    flows <- externalCashFlowsAt snapshot mappings commodity scope day
    terminal <- terminalMarket
    annualRate <- either (const Nothing) Just (Xirr.xirr (flows <> [Xirr.cashFlow day terminal]))
    pure (Xirr.annualRate annualRate)

transactionMatchesScope :: Maybe Text -> Investment.InvestmentMapping -> Hledger.Transaction -> Bool
transactionMatchesScope scope mapping transaction = case scope of
    Just selected
        | mapping.assetAccount /= selected
            && mapping.assetAccount `accountWithin` selected ->
            any
                (\candidate -> selected `accountWithin` candidate.paccount)
                transaction.tpostings
    _ -> True

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
isInvestmentAccount _ scope name =
    "assets" `accountWithin` name
        && case scope of
            Just selected -> selected `accountWithin` name
            Nothing -> True

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
    finalMonth = monthStart latest
    monthlyStarts =
        takeWhile
            (<= finalMonth)
            (iterate (addGregorianMonthsClip 1) firstMonthlyStart)
    firstMonthlyStart =
        max
            (addGregorianMonthsClip (-23) finalMonth)
            (maybe finalMonth monthStart (minimumMaybe (fmap (.tdate) transactions)))
    firstYear =
        maybe
            finalYear
            (\day -> let (year, _, _) = toGregorian day in year)
            (minimumMaybe (fmap (.tdate) transactions))

validateScope :: Investment.InvestmentMappings -> Maybe Text -> Either AppError (Maybe Text)
validateScope _ requested = case requested >>= scopeText of
    Nothing -> Right Nothing
    Just scope
        | "assets" `accountWithin` scope ->
            Right (Just scope)
        | otherwise ->
            Left
                (InvalidRequest "Account prefix must be an asset account.")

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
        , xirr = Nothing
        , attributedIncome = Nothing
        , realisedGain = Nothing
        , totalProfit = Nothing
        , completeness = Complete
        }
