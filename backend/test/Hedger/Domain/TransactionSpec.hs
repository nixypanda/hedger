module Hedger.Domain.TransactionSpec (transactionSpec) where

import Data.List qualified as List

import Data.Time (fromGregorian)
import Hedgehog (Property, assert, forAll, property, (===))
import Hedgehog.Range qualified as Range
import Hedger.Domain.Internal.Transaction (
    TransactionClassification (..),
    classificationFor,
    isOpening,
    newestFirst,
 )
import Hedger.Generators (genTransactions)
import Hledger.Data.Amount (mixed, num)
import Hledger.Data.Posting (nullposting)
import Hledger.Data.Transaction (nulltransaction)
import Hledger.Data.Types qualified as Hledger
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Hedgehog (testProperty)

transactionSpec :: TestTree
transactionSpec =
    testGroup
        "transaction projection"
        [ testCase "orders newest dates first and reverses journal order within a date" ordersNewestFirst
        , testProperty "result is sorted in descending order by (date, index)" sortedProperty
        , testProperty "same-date transactions reversed from input order" sameDateProperty
        , testGroup
            "classificationFor"
            [ testCase "expense transaction" classificationExpense
            , testCase "income transaction" classificationIncome
            , testCase "transfer between same-group accounts" classificationTransfer
            , testCase "opening balance transaction" classificationOpening
            ]
        , testGroup
            "isOpening"
            [ testCase "equity with opening keyword is detected" openingDetected
            , testCase "no equity is not opening" openingNoEquity
            , testCase "income account disqualifies" openingIncomeDisqualifies
            , testCase "empty debits is not opening" openingEmptyDebits
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

-- classificationFor tests

classificationExpense :: IO ()
classificationExpense = assertEqual "expense" TransactionExpense (classificationFor expenseTransaction)

classificationIncome :: IO ()
classificationIncome = assertEqual "income" TransactionIncome (classificationFor incomeTransaction)

classificationTransfer :: IO ()
classificationTransfer = assertEqual "transfer" TransactionTransfer (classificationFor transferTransaction)

classificationOpening :: IO ()
classificationOpening = assertEqual "opening" OpeningBalance (classificationFor openingTransaction)

-- isOpening tests

openingDetected :: IO ()
openingDetected = assertEqual "equity with opening keyword" True (isOpening openingTransaction)

openingNoEquity :: IO ()
openingNoEquity = assertEqual "no equity" False (isOpening transferTransaction)

openingIncomeDisqualifies :: IO ()
openingIncomeDisqualifies = assertEqual "income disqualifies" False (isOpening incomeTransaction)

openingEmptyDebits :: IO ()
openingEmptyDebits = do
    let txn = nulltransaction{Hledger.tpostings = [nullposting{Hledger.paccount = "equity:opening", Hledger.pamount = mixed [num (-100)]}]}
    assertEqual "empty debits" False (isOpening txn)

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

ordersNewestFirst :: IO ()
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

sameDateProperty :: Hedgehog.Property
sameDateProperty = property $ do
    transactions <- forAll (genTransactions (Range.linear 0 80))
    let result = newestFirst transactions
        inputGroups = groupByDate transactions
        resultGroups = groupByDate result
    length inputGroups === length resultGroups
    assert (all (uncurry sameDateOrder) (zip inputGroups resultGroups))

-- Internal helpers

isDescending :: (Ord a) => [a] -> Bool
isDescending [] = True
isDescending [_] = True
isDescending (x : y : rest) = x >= y && isDescending (y : rest)

groupByDate :: [Hledger.Transaction] -> [[Hledger.Transaction]]
groupByDate = List.groupBy sameDay . List.sortOn (.tdate)
  where
    sameDay a b = a.tdate == b.tdate

sameDateOrder :: [Hledger.Transaction] -> [Hledger.Transaction] -> Bool
sameDateOrder inputGroup resultGroup =
    fmap (.tindex) resultGroup == reverse (fmap (.tindex) inputGroup)
