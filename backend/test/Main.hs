module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Hedger.Application.InvestmentsSpec (investmentsSpec)
import Hedger.Application.TransactionQuerySpec (transactionQuerySpec)
import Hedger.ConfigSpec (configSpec)
import Hedger.Domain.TransactionSpec (transactionSpec)
import Hedger.Infrastructure.Internal.CursorSpec (cursorSpec)
import Hedger.Infrastructure.SnapshotStoreSpec (snapshotStoreSpec)
import Hedger.Interface.HTTP.APISpec (apiSpec)

main :: IO ()
main =
    defaultMain $
        testGroup
            "hedger"
            [ configSpec
            , investmentsSpec
            , snapshotStoreSpec
            , transactionSpec
            , cursorSpec
            , transactionQuerySpec
            , apiSpec
            ]
