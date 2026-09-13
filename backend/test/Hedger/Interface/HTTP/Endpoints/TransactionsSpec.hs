module Hedger.Interface.HTTP.Endpoints.TransactionsSpec (transactionsSpec) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Hledger.Data.Transaction (nulltransaction)

import Hedger.Application.Transactions (TransactionItem (..))
import Hedger.Domain.Transaction (TransactionClassification (AssetAcquisition))
import Hedger.Interface.HTTP.Endpoints.Transactions (
    TransactionDto (..),
    TransactionPresentationDto (..),
    toTransactionDto,
 )

transactionsSpec :: TestTree
transactionsSpec =
    testGroup
        "HTTP transaction projections"
        [ testCase "encodes the application-projected classification" encodesProjectedClassification
        ]

encodesProjectedClassification :: IO ()
encodesProjectedClassification = do
    let item = TransactionItem{transaction = nulltransaction, classification = AssetAcquisition}
        dto = toTransactionDto (const Nothing) item
    assertEqual "" "asset_acquisition" dto.presentation.classification
