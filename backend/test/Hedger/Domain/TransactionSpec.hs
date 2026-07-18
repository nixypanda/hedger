module Hedger.Domain.TransactionSpec (transactionSpec) where

import Data.Time (fromGregorian)
import Hedgehog (Property, assert, forAll, property, (===))
import Hedgehog.Range qualified as Range
import Hedger.Domain.Internal.Transaction (
    TransactionClassification (..),
    classificationFor,
    dominantMovement,
    isOpening,
    newestFirst,
 )
import Hedger.Generators (genTransactions)
import Hledger.Data.Amount (mixed, num)
import Hledger.Data.Posting (nullposting)
import Hledger.Data.Transaction (nulltransaction)
import Hledger.Data.Types qualified as Hledger
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.Hedgehog (testProperty)

transactionSpec :: TestTree
transactionSpec =
    testGroup
        "transaction projection"
        [ testCase "orders newest dates first and reverses journal order within a date" ordersNewestFirst
        , testProperty "result is sorted in descending order by (date, index)" sortedProperty
        , testGroup
            "classificationFor"
            [ testCase "expense transaction" classificationExpense
            , testCase "income transaction" classificationIncome
            , testCase "transfer between same-group accounts" classificationTransfer
            , testCase "opening balance transaction" classificationOpening
            , testGroup
                "new classification types"
                [ testCase "asset acquisition: Cash -> Asset" classificationAssetAcquisition
                , testCase "asset disposal: Asset -> Cash" classificationAssetDisposal
                , testCase "asset reallocation: Asset -> Asset" classificationAssetReallocation
                , testCase "borrowing: Liability -> Asset" classificationBorrowing
                , testCase "repayment: Asset -> Liability" classificationRepayment
                , testCase "unclassified: below 80% threshold" classificationUnclassified
                ]
            , testGroup
                "complex multi-account scenarios"
                [ testCase "SIP: Cash -> multiple Assets + Expense" classificationSIP
                , testCase "loan disbursement" classificationLoanDisbursement
                , testCase "EMI payment" classificationEMIPayment
                , testCase "asset sale with profit" classificationAssetSaleProfit
                , testCase "asset sale with loss" classificationAssetSaleLoss
                , testCase "reallocation with fees" classificationReallocationWithFees
                , testCase "multi-asset acquisition" classificationMultiAssetAcquisition
                , testCase "liability consolidation" classificationLiabilityConsolidation
                , testCase "salary with tax" classificationSalaryWithTax
                , testCase "ambiguous multi-account" classificationAmbiguousMultiAccount
                , testCase "borrowing expense dominant" classificationBorrowingExpenseDominant
                , testCase "repayment interest dominant" classificationRepaymentInterestDominant
                ]
            ]
        , testGroup
            "isOpening"
            [ testCase "equity with opening keyword is detected" openingDetected
            , testCase "no equity is not opening" openingNoEquity
            , testCase "income account disqualifies" openingIncomeDisqualifies
            , testCase "empty debits is not opening" openingEmptyDebits
            ]
        , testGroup
            "dominantMovement"
            [ testCase "single debit single credit" dominantSingleDebit
            , testCase "multiple debits same group" dominantMultipleDebitsSameGroup
            , testCase "two groups clear winner" dominantTwoGroupsClearWinner
            , testCase "two groups exact tie uses alphabetical order" dominantTwoGroupsTie
            , testCase "three groups" dominantThreeGroups
            , testCase "all debits same group" dominantAllDebitsSameGroup
            , testCase "dominant group percentage correct" dominantGroupPercentage
            , testCase "first argument ignored" dominantCreditIgnored
            ]
        ]

-- Test transactions

expenseTransaction :: Hledger.Transaction
expenseTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "assets:checking", Hledger.pamount = mixed [num (-100)]}
            , nullposting{Hledger.paccount = "expenses:food", Hledger.pamount = mixed [num 100]}
            ]
        }

incomeTransaction :: Hledger.Transaction
incomeTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "income:salary", Hledger.pamount = mixed [num (-500)]}
            , nullposting{Hledger.paccount = "assets:checking", Hledger.pamount = mixed [num 500]}
            ]
        }

transferTransaction :: Hledger.Transaction
transferTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "assets:checking", Hledger.pamount = mixed [num (-200)]}
            , nullposting{Hledger.paccount = "assets:savings", Hledger.pamount = mixed [num 200]}
            ]
        }

openingTransaction :: Hledger.Transaction
openingTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "equity:opening", Hledger.pamount = mixed [num (-1000)]}
            , nullposting{Hledger.paccount = "assets:checking", Hledger.pamount = mixed [num 500]}
            , nullposting{Hledger.paccount = "assets:savings", Hledger.pamount = mixed [num 500]}
            ]
        }

-- New classification type test transactions

assetAcquisitionTransaction :: Hledger.Transaction
assetAcquisitionTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "assets:cash:bank", Hledger.pamount = mixed [num (-100)]}
            , nullposting{Hledger.paccount = "assets:investments:stock", Hledger.pamount = mixed [num 100]}
            ]
        }

assetDisposalTransaction :: Hledger.Transaction
assetDisposalTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "assets:investments:stock", Hledger.pamount = mixed [num (-100)]}
            , nullposting{Hledger.paccount = "assets:cash:bank", Hledger.pamount = mixed [num 100]}
            ]
        }

assetReallocationTransaction :: Hledger.Transaction
assetReallocationTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting
                { Hledger.paccount = "assets:investments:stock_a"
                , Hledger.pamount = mixed [num (-100)]
                }
            , nullposting
                { Hledger.paccount = "assets:investments:stock_b"
                , Hledger.pamount = mixed [num 100]
                }
            ]
        }

borrowingTransaction :: Hledger.Transaction
borrowingTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "liability:loan:personal", Hledger.pamount = mixed [num (-100)]}
            , nullposting{Hledger.paccount = "assets:cash:bank", Hledger.pamount = mixed [num 100]}
            ]
        }

repaymentTransaction :: Hledger.Transaction
repaymentTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "assets:cash:bank", Hledger.pamount = mixed [num (-100)]}
            , nullposting{Hledger.paccount = "liabilities:loan:personal", Hledger.pamount = mixed [num 100]}
            ]
        }

unclassifiedTransaction :: Hledger.Transaction
unclassifiedTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "assets:cash:bank", Hledger.pamount = mixed [num (-1000)]}
            , nullposting{Hledger.paccount = "assets:investments:stock", Hledger.pamount = mixed [num 600]}
            , nullposting{Hledger.paccount = "expense:financial:brokerage", Hledger.pamount = mixed [num 400]}
            ]
        }

-- Complex multi-account test transactions

sipTransaction :: Hledger.Transaction
sipTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting
                { Hledger.paccount = "assets:cash:bank:neev"
                , Hledger.pamount = mixed [num (-22435)]
                }
            , nullposting
                { Hledger.paccount = "assets:equity:flexi:mutual:a"
                , Hledger.pamount = mixed [num 3200]
                }
            , nullposting
                { Hledger.paccount = "assets:equity:index:mutual:b"
                , Hledger.pamount = mixed [num 5000]
                }
            , nullposting
                { Hledger.paccount = "assets:equity:large:mutual:c"
                , Hledger.pamount = mixed [num 4500]
                }
            , nullposting
                { Hledger.paccount = "assets:equity:mid:mutual:d"
                , Hledger.pamount = mixed [num 3500]
                }
            , nullposting
                { Hledger.paccount = "assets:equity:small:mutual:e"
                , Hledger.pamount = mixed [num 3200]
                }
            , nullposting
                { Hledger.paccount = "assets:equity:hybrid:mutual:f"
                , Hledger.pamount = mixed [num 3000]
                }
            , nullposting
                { Hledger.paccount = "expenses:financial:brokerage"
                , Hledger.pamount = mixed [num 35]
                }
            ]
        }

loanDisbursementTransaction :: Hledger.Transaction
loanDisbursementTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting
                { Hledger.paccount = "liabilities:loan:personal"
                , Hledger.pamount = mixed [num (-100000)]
                }
            , nullposting
                { Hledger.paccount = "assets:cash:bank"
                , Hledger.pamount = mixed [num 95000]
                }
            , nullposting
                { Hledger.paccount = "expenses:financial:processing"
                , Hledger.pamount = mixed [num 5000]
                }
            ]
        }

emiPaymentTransaction :: Hledger.Transaction
emiPaymentTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "assets:cash:bank", Hledger.pamount = mixed [num (-15000)]}
            , nullposting{Hledger.paccount = "liabilities:loan:personal", Hledger.pamount = mixed [num 12000]}
            , nullposting{Hledger.paccount = "expense:financial:interest", Hledger.pamount = mixed [num 3000]}
            ]
        }

assetSaleProfitTransaction :: Hledger.Transaction
assetSaleProfitTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "assets:investments:stock", Hledger.pamount = mixed [num (-100)]}
            , nullposting{Hledger.paccount = "assets:cash:bank", Hledger.pamount = mixed [num 110]}
            , nullposting{Hledger.paccount = "income:capital:gains", Hledger.pamount = mixed [num 10]}
            ]
        }

assetSaleLossTransaction :: Hledger.Transaction
assetSaleLossTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "assets:investments:stock", Hledger.pamount = mixed [num (-100)]}
            , nullposting{Hledger.paccount = "assets:cash:bank", Hledger.pamount = mixed [num 90]}
            , nullposting{Hledger.paccount = "expenses:capital:loss", Hledger.pamount = mixed [num 10]}
            ]
        }

reallocationWithFeesTransaction :: Hledger.Transaction
reallocationWithFeesTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting
                { Hledger.paccount = "assets:investments:fund_a"
                , Hledger.pamount = mixed [num (-1000)]
                }
            , nullposting
                { Hledger.paccount = "assets:investments:fund_b"
                , Hledger.pamount = mixed [num 950]
                }
            , nullposting
                { Hledger.paccount = "expenses:financial:brokerage"
                , Hledger.pamount = mixed [num 50]
                }
            ]
        }

multiAssetAcquisitionTransaction :: Hledger.Transaction
multiAssetAcquisitionTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting
                { Hledger.paccount = "assets:cash:bank"
                , Hledger.pamount = mixed [num (-10000)]
                }
            , nullposting
                { Hledger.paccount = "assets:investments:stock_a"
                , Hledger.pamount = mixed [num 5000]
                }
            , nullposting
                { Hledger.paccount = "assets:investments:stock_b"
                , Hledger.pamount = mixed [num 4000]
                }
            , nullposting
                { Hledger.paccount = "expenses:financial:brokerage"
                , Hledger.pamount = mixed [num 1000]
                }
            ]
        }

liabilityConsolidationTransaction :: Hledger.Transaction
liabilityConsolidationTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "liabilities:loan:old", Hledger.pamount = mixed [num (-50000)]}
            , nullposting{Hledger.paccount = "liabilities:loan:new", Hledger.pamount = mixed [num 50000]}
            ]
        }

salaryWithTaxTransaction :: Hledger.Transaction
salaryWithTaxTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "income:salary", Hledger.pamount = mixed [num (-30000)]}
            , nullposting{Hledger.paccount = "assets:cash:bank", Hledger.pamount = mixed [num 24000]}
            , nullposting{Hledger.paccount = "liabilities:tax:tds", Hledger.pamount = mixed [num 6000]}
            ]
        }

ambiguousTransaction :: Hledger.Transaction
ambiguousTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "assets:cash:bank", Hledger.pamount = mixed [num (-1000)]}
            , nullposting{Hledger.paccount = "assets:investments:stock", Hledger.pamount = mixed [num 600]}
            , nullposting{Hledger.paccount = "expense:financial:brokerage", Hledger.pamount = mixed [num 400]}
            ]
        }

borrowingExpenseDominantTransaction :: Hledger.Transaction
borrowingExpenseDominantTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "liabilities:loan", Hledger.pamount = mixed [num (-100)]}
            , nullposting{Hledger.paccount = "expense:financial:processing", Hledger.pamount = mixed [num 60]}
            , nullposting{Hledger.paccount = "assets:cash:bank", Hledger.pamount = mixed [num 40]}
            ]
        }

repaymentInterestDominantTransaction :: Hledger.Transaction
repaymentInterestDominantTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "assets:cash:bank", Hledger.pamount = mixed [num (-1000)]}
            , nullposting{Hledger.paccount = "liabilities:loan", Hledger.pamount = mixed [num 600]}
            , nullposting{Hledger.paccount = "expense:financial:interest", Hledger.pamount = mixed [num 400]}
            ]
        }

tieTransaction :: Hledger.Transaction
tieTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "assets:cash:bank", Hledger.pamount = mixed [num (-100)]}
            , nullposting{Hledger.paccount = "assets:investments:stock", Hledger.pamount = mixed [num 50]}
            , nullposting{Hledger.paccount = "expenses:food", Hledger.pamount = mixed [num 50]}
            ]
        }

-- classificationFor tests

classificationExpense :: Assertion
classificationExpense =
    assertEqual "expense" TransactionExpense (classificationFor expenseTransaction)

classificationIncome :: Assertion
classificationIncome =
    assertEqual "income" TransactionIncome (classificationFor incomeTransaction)

classificationTransfer :: Assertion
classificationTransfer =
    assertEqual "transfer" TransactionTransfer (classificationFor transferTransaction)

classificationOpening :: Assertion
classificationOpening =
    assertEqual "opening" OpeningBalance (classificationFor openingTransaction)

-- New classification type tests

classificationAssetAcquisition :: Assertion
classificationAssetAcquisition =
    assertEqual "asset acquisition" AssetAcquisition (classificationFor assetAcquisitionTransaction)

classificationAssetDisposal :: Assertion
classificationAssetDisposal =
    assertEqual "asset disposal" AssetDisposal (classificationFor assetDisposalTransaction)

classificationAssetReallocation :: Assertion
classificationAssetReallocation =
    assertEqual "asset reallocation" AssetReallocation (classificationFor assetReallocationTransaction)

classificationBorrowing :: Assertion
classificationBorrowing =
    assertEqual "borrowing" Borrowing (classificationFor borrowingTransaction)

classificationRepayment :: Assertion
classificationRepayment =
    assertEqual "repayment" Repayment (classificationFor repaymentTransaction)

classificationUnclassified :: Assertion
classificationUnclassified =
    assertEqual "unclassified" Unclassified (classificationFor unclassifiedTransaction)

-- Complex multi-account scenario tests

classificationSIP :: Assertion
classificationSIP =
    assertEqual "sip" AssetAcquisition (classificationFor sipTransaction)

classificationLoanDisbursement :: Assertion
classificationLoanDisbursement =
    assertEqual "loan disbursement" Borrowing (classificationFor loanDisbursementTransaction)

classificationEMIPayment :: Assertion
classificationEMIPayment =
    assertEqual "emi payment" Repayment (classificationFor emiPaymentTransaction)

classificationAssetSaleProfit :: Assertion
classificationAssetSaleProfit =
    assertEqual "asset sale profit" AssetDisposal (classificationFor assetSaleProfitTransaction)

classificationAssetSaleLoss :: Assertion
classificationAssetSaleLoss =
    assertEqual "asset sale loss" AssetDisposal (classificationFor assetSaleLossTransaction)

classificationReallocationWithFees :: Assertion
classificationReallocationWithFees =
    assertEqual
        "reallocation with fees"
        AssetReallocation
        (classificationFor reallocationWithFeesTransaction)

classificationMultiAssetAcquisition :: Assertion
classificationMultiAssetAcquisition =
    assertEqual
        "multi asset acquisition"
        AssetAcquisition
        (classificationFor multiAssetAcquisitionTransaction)

classificationLiabilityConsolidation :: Assertion
classificationLiabilityConsolidation =
    assertEqual
        "liability consolidation"
        TransactionTransfer
        (classificationFor liabilityConsolidationTransaction)

classificationSalaryWithTax :: Assertion
classificationSalaryWithTax =
    assertEqual "salary with tax" TransactionIncome (classificationFor salaryWithTaxTransaction)

classificationAmbiguousMultiAccount :: Assertion
classificationAmbiguousMultiAccount =
    assertEqual
        "ambiguous multi account"
        Unclassified
        (classificationFor ambiguousTransaction)

classificationBorrowingExpenseDominant :: Assertion
classificationBorrowingExpenseDominant =
    assertEqual
        "borrowing expense dominant"
        Unclassified
        (classificationFor borrowingExpenseDominantTransaction)

classificationRepaymentInterestDominant :: Assertion
classificationRepaymentInterestDominant =
    assertEqual
        "repayment interest dominant"
        Unclassified
        (classificationFor repaymentInterestDominantTransaction)

-- isOpening tests

openingDetected :: Assertion
openingDetected =
    assertEqual "equity with opening keyword" True (isOpening openingTransaction)

openingNoEquity :: Assertion
openingNoEquity =
    assertEqual "no equity" False (isOpening transferTransaction)

openingIncomeDisqualifies :: Assertion
openingIncomeDisqualifies =
    assertEqual "income disqualifies" False (isOpening incomeTransaction)

openingEmptyDebits :: Assertion
openingEmptyDebits = do
    let txn =
            nulltransaction
                { Hledger.tpostings =
                    [ nullposting
                        { Hledger.paccount = "equity:opening"
                        , Hledger.pamount = mixed [num (-100)]
                        }
                    ]
                }
    assertEqual "empty debits" False (isOpening txn)

-- dominantMovement tests

dominantSingleDebit :: Assertion
dominantSingleDebit =
    assertEqual "single expense debit" ("expenses", 1.0) (dominantMovement expenseTransaction)

dominantMultipleDebitsSameGroup :: Assertion
dominantMultipleDebitsSameGroup =
    assertEqual "two asset debits" ("assets", 1.0) (dominantMovement openingTransaction)

dominantTwoGroupsClearWinner :: Assertion
dominantTwoGroupsClearWinner =
    assertEqual "assets 600 vs expenses 400" ("assets", 0.6) (dominantMovement unclassifiedTransaction)

dominantTwoGroupsTie :: Assertion
dominantTwoGroupsTie =
    assertEqual "assets 50 vs expenses 50" ("assets", 0.5) (dominantMovement tieTransaction)

dominantThreeGroups :: Assertion
dominantThreeGroups =
    assertEqual
        "liabilities 12000 vs expenses 3000"
        ("liabilities", 0.8)
        (dominantMovement emiPaymentTransaction)

dominantAllDebitsSameGroup :: Assertion
dominantAllDebitsSameGroup =
    assertEqual "both debits are assets" ("assets", 1.0) (dominantMovement transferTransaction)

dominantGroupPercentage :: Assertion
dominantGroupPercentage =
    assertEqual
        "expenses 60 out of 100"
        ("expenses", 0.6)
        (dominantMovement borrowingExpenseDominantTransaction)

dominantCreditIgnored :: Assertion
dominantCreditIgnored = do
    let txFromIncome =
            nulltransaction
                { Hledger.tpostings =
                    [ nullposting{Hledger.paccount = "income:salary", Hledger.pamount = mixed [num (-100)]}
                    , nullposting{Hledger.paccount = "expenses:food", Hledger.pamount = mixed [num 100]}
                    ]
                }
        txFromAsset =
            nulltransaction
                { Hledger.tpostings =
                    [ nullposting{Hledger.paccount = "assets:checking", Hledger.pamount = mixed [num (-100)]}
                    , nullposting{Hledger.paccount = "expenses:food", Hledger.pamount = mixed [num 100]}
                    ]
                }
    assertEqual "different credits same debits" (dominantMovement txFromIncome) (dominantMovement txFromAsset)

-- Sorting tests

syntheticTransactions :: [Hledger.Transaction]
syntheticTransactions =
    [ nulltransaction
        { Hledger.tindex = 1
        , Hledger.tdate = fromGregorian 2024 1 1
        , Hledger.tdescription = "Transaction A"
        , Hledger.tpostings = [nullposting]
        }
    , nulltransaction
        { Hledger.tindex = 2
        , Hledger.tdate = fromGregorian 2024 1 2
        , Hledger.tdescription = "Transaction B"
        , Hledger.tpostings = [nullposting]
        }
    , nulltransaction
        { Hledger.tindex = 3
        , Hledger.tdate = fromGregorian 2024 1 3
        , Hledger.tdescription = "Transaction C"
        , Hledger.tpostings = [nullposting]
        }
    , nulltransaction
        { Hledger.tindex = 4
        , Hledger.tdate = fromGregorian 2024 1 4
        , Hledger.tdescription = "Transaction D"
        , Hledger.tpostings = [nullposting]
        }
    , nulltransaction
        { Hledger.tindex = 5
        , Hledger.tdate = fromGregorian 2024 1 5
        , Hledger.tdescription = "Transaction E"
        , Hledger.tpostings = [nullposting]
        }
    ]

ordersNewestFirst :: Assertion
ordersNewestFirst = do
    let sameDate =
            zipWith
                (\index t -> t{Hledger.tdate = fromGregorian 2024 1 1, Hledger.tindex = index})
                [1 ..]
                (take 3 syntheticTransactions)
        sourceIndexes = (.tindex) <$> sameDate
        orderedIndexes = (.tindex) <$> newestFirst sameDate
    assertEqual "" (reverse sourceIndexes) orderedIndexes

sortedProperty :: Hedgehog.Property
sortedProperty = property $ do
    transactions <- forAll (genTransactions (Range.linear 0 80))
    let result = newestFirst transactions
        keys = (\t -> (t.tdate, t.tindex)) <$> result
    assert (isDescending keys)

-- Internal helpers

isDescending :: (Ord a) => [a] -> Bool
isDescending [] = True
isDescending [_] = True
isDescending (x : y : rest) = x >= y && isDescending (y : rest)
