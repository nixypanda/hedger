module Hedger.Application.TransactionQuerySpec (transactionQuerySpec) where

import Data.Text qualified as Text
import Data.Time (fromGregorian)
import Hedgehog (Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

import Hledger.Data.Posting (nullposting)
import Hledger.Data.Transaction (nulltransaction)
import Hledger.Data.Types qualified as Hledger

import Hedger.Application.Error (AppError (..))
import Hedger.Application.TransactionQuery (
    TransactionPage (..),
    TransactionPageRequest (..),
    transactionPage,
 )
import Hedger.Domain.Internal.Transaction (newestFirst)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Generators (genTransactions)
import Hedger.SyntheticTransactions (mkSnapshot)

transactionQuerySpec :: TestTree
transactionQuerySpec =
    testGroup
        "hledger transaction queries"
        [ testCase "uses pinned hledger transaction matching for payee and note" matchesPayeeAndNote
        , testProperty
            "concatenates cursor pages without gaps or duplicates for generated limits"
            paginationProperty
        , testCase
            "binds cursors to query, revision, ordering, and relative-date reference"
            bindsCursorsCorrectly
        ]

matchesPayeeAndNote :: IO ()
matchesPayeeAndNote = do
    let template =
            nulltransaction
                { Hledger.tdescription = "Corner Shop | Weekly groceries"
                , Hledger.tpostings = [nullposting]
                }
        snapshot = mkSnapshot [template]
    assertEqual
        ""
        (Right 1)
        ( fmap
            (length . (.transactions))
            ( transactionPage
                snapshot
                TransactionPageRequest
                    { query = Just "payee:Corner"
                    , limit = 50
                    , cursor = Nothing
                    }
            )
        )
    assertEqual
        ""
        (Right 1)
        ( fmap
            (length . (.transactions))
            ( transactionPage
                snapshot
                TransactionPageRequest
                    { query = Just "note:Weekly"
                    , limit = 50
                    , cursor = Nothing
                    }
            )
        )

bindsCursorsCorrectly :: IO ()
bindsCursorsCorrectly = do
    case transactionPage
        syntheticSnapshot
        TransactionPageRequest
            { query = Just "date:thisyear"
            , limit = 1
            , cursor = Nothing
            } of
        Left pageError -> assertFailure (show pageError)
        Right first -> case first.nextCursor of
            Nothing -> assertFailure "The relative-date query should have another page."
            Just cursor -> do
                assertEqual
                    ""
                    (Right 1)
                    ( fmap
                        (length . (.transactions))
                        ( transactionPage
                            syntheticSnapshot
                            TransactionPageRequest
                                { query = Just "date:thisyear"
                                , limit = 1
                                , cursor = Just cursor
                                }
                        )
                    )
                assertBool
                    "should reject cursor for different query"
                    ( isInvalidCursor
                        ( transactionPage
                            syntheticSnapshot
                            TransactionPageRequest
                                { query = Just "desc:Salary"
                                , limit = 1
                                , cursor = Just cursor
                                }
                        )
                    )
                assertBool
                    "should reject cursor for different revision"
                    ( isInvalidCursor
                        ( transactionPage
                            syntheticSnapshot{revision = "md5:different"}
                            TransactionPageRequest
                                { query = Just "date:thisyear"
                                , limit = 1
                                , cursor = Just cursor
                                }
                        )
                    )
                assertBool
                    "should reject cursor for different date"
                    ( isInvalidCursor
                        ( transactionPage
                            syntheticSnapshot{latestJournalDate = Just (fromGregorian 2025 4 2)}
                            TransactionPageRequest
                                { query = Just "date:thisyear"
                                , limit = 1
                                , cursor = Just cursor
                                }
                        )
                    )
                assertBool
                    "should reject tampered cursor"
                    ( isInvalidCursor
                        ( transactionPage
                            syntheticSnapshot
                            TransactionPageRequest
                                { query = Just "date:thisyear"
                                , limit = 1
                                , cursor = Just (cursor <> "tampered")
                                }
                        )
                    )

paginationProperty :: Hedgehog.Property
paginationProperty = property $ do
    transactions <- forAll (genTransactions (Range.linear 1 20))
    let snapshot = mkSnapshot transactions
    limit <- forAll (Gen.int (Range.linear 1 (min 13 (length transactions))))
    let expected = fmap (.tdescription) (newestFirst snapshot.transactions)
        actual = fmap (fmap (.tdescription)) (collectPages snapshot limit Nothing)
    actual === Right expected

collectPages :: Snapshot -> Int -> Maybe Text.Text -> Either AppError [Hledger.Transaction]
collectPages snapshot limit cursor = do
    page <- transactionPage snapshot TransactionPageRequest{query = Nothing, limit, cursor}
    rest <- maybe (Right []) (collectPages snapshot limit . Just) page.nextCursor
    pure (page.transactions <> rest)

isInvalidCursor :: Either AppError value -> Bool
isInvalidCursor (Left (InvalidCursor _)) = True
isInvalidCursor _ = False

syntheticSnapshot :: Snapshot
syntheticSnapshot =
    mkSnapshot
        [ nulltransaction
            { Hledger.tindex = 1
            , Hledger.tdate = fromGregorian 2023 3 31
            , Hledger.tdescription = "Opening balances"
            , Hledger.tpostings = [nullposting]
            }
        , nulltransaction
            { Hledger.tindex = 2
            , Hledger.tdate = fromGregorian 2023 4 1
            , Hledger.tdescription = "Opening USD balance"
            , Hledger.tpostings = [nullposting]
            }
        , nulltransaction
            { Hledger.tindex = 3
            , Hledger.tdate = fromGregorian 2023 4 2
            , Hledger.tdescription = "Opening unpriced balance"
            , Hledger.tpostings = [nullposting]
            }
        , nulltransaction
            { Hledger.tindex = 4
            , Hledger.tdate = fromGregorian 2023 4 3
            , Hledger.tdescription = "Salary"
            , Hledger.tpostings = [nullposting]
            }
        , nulltransaction
            { Hledger.tindex = 5
            , Hledger.tdate = fromGregorian 2023 4 5
            , Hledger.tdescription = "Groceries"
            , Hledger.tpostings = [nullposting]
            }
        , nulltransaction
            { Hledger.tindex = 6
            , Hledger.tdate = fromGregorian 2023 4 7
            , Hledger.tdescription = "Cash transfer"
            , Hledger.tpostings = [nullposting]
            }
        , nulltransaction
            { Hledger.tindex = 7
            , Hledger.tdate = fromGregorian 2023 4 10
            , Hledger.tdescription = "Buy shares with unit cost"
            , Hledger.tpostings = [nullposting]
            }
        , nulltransaction
            { Hledger.tindex = 8
            , Hledger.tdate = fromGregorian 2023 4 15
            , Hledger.tdescription = "Buy fund with total cost"
            , Hledger.tpostings = [nullposting]
            }
        , nulltransaction
            { Hledger.tindex = 9
            , Hledger.tdate = fromGregorian 2023 5 1
            , Hledger.tdescription = "Investment with missing cost"
            , Hledger.tpostings = [nullposting]
            }
        , nulltransaction
            { Hledger.tindex = 10
            , Hledger.tdate = fromGregorian 2023 5 10
            , Hledger.tdescription = "Grocery refund"
            , Hledger.tpostings = [nullposting]
            }
        , nulltransaction
            { Hledger.tindex = 11
            , Hledger.tdate = fromGregorian 2023 12 31
            , Hledger.tdescription = "Loan interest"
            , Hledger.tpostings = [nullposting]
            }
        , nulltransaction
            { Hledger.tindex = 12
            , Hledger.tdate = fromGregorian 2024 4 1
            , Hledger.tdescription = "Salary"
            , Hledger.tpostings = [nullposting]
            }
        , nulltransaction
            { Hledger.tindex = 13
            , Hledger.tdate = fromGregorian 2024 4 2
            , Hledger.tdescription = "Utilities"
            , Hledger.tpostings = [nullposting]
            }
        ]
