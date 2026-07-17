module Hedger.Interface.HTTP.TransactionMapperSpec (transactionMapperSpec) where

import Data.Text qualified as Text
import Data.Time (fromGregorian)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Interface.HTTP.TransactionMapper (toTransactions)
import Hedger.Interface.HTTP.Types (
    AmountDto (..),
    CostDto (..),
    PostingDto (..),
    TagDto (..),
    TransactionDto (..),
    TransactionPresentationDto (..),
 )
import Hedger.SyntheticTransactions (mkSnapshot)
import Hledger.Data.Amount (amountWithCommodity, at, mixed, num, (@@))
import Hledger.Data.Posting (nullposting)
import Hledger.Data.Transaction (nulltransaction)
import Hledger.Data.Types qualified as Hledger
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

transactionMapperSpec :: TestTree
transactionMapperSpec =
    testGroup
        "transaction projection"
        [ testCase
            "preserves unit and total cost types without manufacturing missing costs"
            preservesCostTypes
        , testCase
            "preserves optional dates, statuses, codes, comments, and tags"
            preservesMetadataFields
        , testCase
            "derives eLedger list classification and exact headline totals in Haskell"
            derivesClassificationAndTotals
        ]

preservesCostTypes :: IO ()
preservesCostTypes = do
    let costs =
            [ cost.costType
            | transaction <- toTransactions costSnapshot.transactions
            , posting <- transaction.postings
            , amount <- posting.amounts
            , Just cost <- [amount.cost]
            ]
    assertEqual "" ["total", "unit"] costs

preservesMetadataFields :: IO ()
preservesMetadataFields = do
    case toTransactions metadataSnapshot.transactions of
        [] -> assertFailure "The synthetic fixture needs transactions."
        transaction : _ -> do
            assertEqual "" (Just (fromGregorian 2024 4 3)) transaction.secondaryDate
            assertEqual "" "cleared" transaction.status
            assertEqual "" (Just "UTIL") transaction.code
            assertBool
                "comment should contain category: household"
                (maybe False (Text.isInfixOf "category: household") transaction.comment)
            assertEqual "" ["category"] (fmap (.name) transaction.tags)
            assertEqual "" ["cleared", "unmarked"] (fmap (.status) transaction.postings)
            assertEqual "" [["method"], []] (fmap (fmap (.name) . (.tags)) transaction.postings)

derivesClassificationAndTotals :: IO ()
derivesClassificationAndTotals = do
    let projected = toTransactions classificationSnapshot.transactions
    assertEqual
        ""
        ["expense", "income", "expense", "income"]
        (fmap (.presentation.classification) (take 4 projected))

costSnapshot :: Snapshot
costSnapshot =
    mkSnapshot
        [ nulltransaction
            { Hledger.tindex = 2
            , Hledger.tdate = fromGregorian 2024 1 1
            , Hledger.tdescription = "Buy fund with total cost"
            , Hledger.tpostings =
                [ nullposting
                    { Hledger.paccount = "assets:investments:funds"
                    , Hledger.pamount =
                        mixed
                            [ (amountWithCommodity "FUND" (num 2)) @@ amountWithCommodity "INR" (num 5000)
                            ]
                    }
                ]
            }
        , nulltransaction
            { Hledger.tindex = 1
            , Hledger.tdate = fromGregorian 2024 1 1
            , Hledger.tdescription = "Buy shares with unit cost"
            , Hledger.tpostings =
                [ nullposting
                    { Hledger.paccount = "assets:investments:stocks"
                    , Hledger.pamount =
                        mixed
                            [ (amountWithCommodity "AAPL" (num 10)) `at` amountWithCommodity "INR" (num 12000)
                            ]
                    }
                ]
            }
        ]

metadataSnapshot :: Snapshot
metadataSnapshot =
    mkSnapshot
        [ nulltransaction
            { Hledger.tindex = 1
            , Hledger.tdate = fromGregorian 2024 4 2
            , Hledger.tdate2 = Just (fromGregorian 2024 4 3)
            , Hledger.tstatus = Hledger.Cleared
            , Hledger.tcode = "UTIL"
            , Hledger.tdescription = "Utilities"
            , Hledger.tcomment = "category: household"
            , Hledger.ttags = [("category", "household")]
            , Hledger.tpostings =
                [ nullposting
                    { Hledger.pstatus = Hledger.Cleared
                    , Hledger.paccount = "expenses:utilities"
                    , Hledger.pamount = mixed [amountWithCommodity "INR" (num 3000)]
                    , Hledger.ptags = [("method", "")]
                    }
                , nullposting
                    { Hledger.pstatus = Hledger.Unmarked
                    , Hledger.paccount = "assets:cash:checking"
                    , Hledger.pamount = mixed [amountWithCommodity "INR" (num (-3000))]
                    }
                ]
            }
        ]

classificationSnapshot :: Snapshot
classificationSnapshot =
    mkSnapshot
        [ nulltransaction
            { Hledger.tindex = 4
            , Hledger.tdate = fromGregorian 2024 1 1
            , Hledger.tdescription = "Utility bill"
            , Hledger.tpostings =
                [ nullposting
                    { Hledger.paccount = "expenses:utilities"
                    , Hledger.pamount = mixed [amountWithCommodity "INR" (num 3000)]
                    }
                , nullposting
                    { Hledger.paccount = "assets:cash:checking"
                    , Hledger.pamount = mixed [amountWithCommodity "INR" (num (-3000))]
                    }
                ]
            }
        , nulltransaction
            { Hledger.tindex = 3
            , Hledger.tdate = fromGregorian 2024 1 1
            , Hledger.tdescription = "Salary"
            , Hledger.tpostings =
                [ nullposting
                    { Hledger.paccount = "assets:cash:checking"
                    , Hledger.pamount = mixed [amountWithCommodity "INR" (num 50000)]
                    }
                , nullposting
                    { Hledger.paccount = "income:salary"
                    , Hledger.pamount = mixed [amountWithCommodity "INR" (num (-50000))]
                    }
                ]
            }
        , nulltransaction
            { Hledger.tindex = 2
            , Hledger.tdate = fromGregorian 2024 1 1
            , Hledger.tdescription = "Groceries"
            , Hledger.tpostings =
                [ nullposting
                    { Hledger.paccount = "expenses:food:groceries"
                    , Hledger.pamount = mixed [amountWithCommodity "INR" (num 2500)]
                    }
                , nullposting
                    { Hledger.paccount = "assets:cash:checking"
                    , Hledger.pamount = mixed [amountWithCommodity "INR" (num (-2500))]
                    }
                ]
            }
        , nulltransaction
            { Hledger.tindex = 1
            , Hledger.tdate = fromGregorian 2024 1 1
            , Hledger.tdescription = "Freelance payment"
            , Hledger.tpostings =
                [ nullposting
                    { Hledger.paccount = "assets:cash:checking"
                    , Hledger.pamount = mixed [amountWithCommodity "INR" (num 10000)]
                    }
                , nullposting
                    { Hledger.paccount = "income:freelance"
                    , Hledger.pamount = mixed [amountWithCommodity "INR" (num (-10000))]
                    }
                ]
            }
        ]
