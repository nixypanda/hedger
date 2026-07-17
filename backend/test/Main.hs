module Main (main) where

import Hedger.Application.TransactionQuerySpec (transactionQuerySpec)
import Hedger.ConfigSpec (configSpec)
import Hedger.Domain.TransactionSpec (transactionSpec)
import Hedger.Infrastructure.Internal.CursorSpec (cursorSpec)
import Hedger.Infrastructure.SnapshotStoreSpec (snapshotStoreSpec)
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
            , cursorSpec
            , transactionQuerySpec
            , apiSpec
            ]
