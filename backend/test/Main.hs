module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Hedger.Application.TransactionQuerySpec (transactionQuerySpec)
import Hedger.ConfigSpec (configSpec)
import Hedger.Domain.AccountSpec (accountSpec)
import Hedger.Domain.ActivitySpec (activitySpec)
import Hedger.Domain.InvestmentPerformanceSpec (investmentsSpec)
import Hedger.Domain.InvestmentSpec (investmentSpec)
import Hedger.Domain.PeriodSpec (periodSpec)
import Hedger.Domain.TransactionSpec (transactionSpec)
import Hedger.Domain.ValuationSpec (valuationSpec)
import Hedger.Domain.XirrSpec (xirrSpec)
import Hedger.Infrastructure.Internal.CursorSpec (cursorSpec)
import Hedger.Infrastructure.JournalWatcherSpec (journalWatcherSpec)
import Hedger.Infrastructure.SnapshotStoreSpec (snapshotStoreSpec)
import Hedger.Interface.HTTP.APISpec (apiSpec)
import Hedger.Interface.HTTP.CommonSpec (commonSpec)

main :: IO ()
main =
    defaultMain $
        testGroup
            "hedger"
            [ configSpec
            , investmentsSpec
            , investmentSpec
            , activitySpec
            , snapshotStoreSpec
            , journalWatcherSpec
            , transactionSpec
            , cursorSpec
            , transactionQuerySpec
            , apiSpec
            , commonSpec
            , periodSpec
            , accountSpec
            , valuationSpec
            , xirrSpec
            ]
