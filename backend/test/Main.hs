module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Hedger.Application.AppMSpec (appMSpec)
import Hedger.Application.EventsSpec (eventsSpec)
import Hedger.Application.TransactionQuerySpec (transactionQuerySpec)
import Hedger.ConfigSpec (configSpec)
import Hedger.Domain.AccountSpec (accountSpec)
import Hedger.Domain.ActivitySpec (activitySpec)
import Hedger.Domain.FingerprintSpec (fingerprintSpec)
import Hedger.Domain.InvestmentPerformanceSpec (investmentsSpec)
import Hedger.Domain.InvestmentSpec (investmentSpec)
import Hedger.Domain.NetWorthSpec (netWorthSpec)
import Hedger.Domain.PeriodReportSpec (periodReportSpec)
import Hedger.Domain.PeriodSpec (periodSpec)
import Hedger.Domain.TransactionSpec (transactionSpec)
import Hedger.Domain.ValuationSpec (valuationSpec)
import Hedger.Domain.XirrSpec (xirrSpec)
import Hedger.Infrastructure.DerivedStoreSpec (derivedStoreSpec)
import Hedger.Infrastructure.Internal.CursorSpec (cursorSpec)
import Hedger.Infrastructure.JournalWatcherSpec (journalWatcherSpec)
import Hedger.Infrastructure.SnapshotStoreSpec (snapshotStoreSpec)
import Hedger.Interface.HTTP.APISpec (apiSpec)
import Hedger.Interface.HTTP.CommonSpec (commonSpec)
import Hedger.Interface.HTTP.EventsSpec (httpEventsSpec)
import Hedger.Runtime.ServerSpec (serverSpec)

main :: IO ()
main =
    defaultMain $
        testGroup
            "hedger"
            [ configSpec
            , fingerprintSpec
            , httpEventsSpec
            , investmentsSpec
            , investmentSpec
            , netWorthSpec
            , periodReportSpec
            , activitySpec
            , snapshotStoreSpec
            , derivedStoreSpec
            , journalWatcherSpec
            , transactionSpec
            , cursorSpec
            , transactionQuerySpec
            , apiSpec
            , commonSpec
            , eventsSpec
            , serverSpec
            , periodSpec
            , accountSpec
            , valuationSpec
            , xirrSpec
            , appMSpec
            ]
