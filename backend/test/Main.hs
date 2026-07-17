module Main (main) where

import Hedger.ConfigSpec (configSpec)
import Hedger.Domain.TransactionSpec (transactionSpec)
import Hedger.Infrastructure.Hledger.SnapshotStoreSpec (snapshotStoreSpec)
import Hedger.Infrastructure.Hledger.TransactionQuerySpec (transactionQuerySpec)
import Hedger.Interface.HTTP.APISpec (apiSpec)
import Hedger.Interface.HTTP.TransactionMapperSpec (transactionMapperSpec)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
        testGroup
            "hedger"
            [ configSpec
            , snapshotStoreSpec
            , transactionSpec
            , transactionMapperSpec
            , transactionQuerySpec
            , apiSpec
            ]
