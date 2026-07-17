module Hedger.Domain.TransactionSpec (transactionSpec) where

import Data.List qualified as List

import Data.Time (fromGregorian)
import Hedgehog (Property, assert, forAll, property, (===))
import Hedgehog.Range qualified as Range
import Hedger.Domain.Internal.Account (Account (..))
import Hedger.Domain.Internal.Transaction (
    TransactionClassification (..),
    classificationFor,
    dominantToAccountFor,
    fromAccountsFor,
    isOpening,
    newestFirst,
    toAccountsFor,
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
        , testGroup
            "fromAccountsFor"
            [ testCase "expense transaction returns credit accounts" fromAccountsExpense
            , testCase "income transaction returns credit accounts" fromAccountsIncome
            ]
        , testGroup
            "toAccountsFor"
            [ testCase "opening transaction returns debit accounts" toAccountsOpening
            , testCase "expense transaction with single from returns debit accounts" toAccountsExpense
            , testCase "income transaction with single from returns dominant account" toAccountsIncome
            , testCase "transfer with multiple to accounts returns all" toAccountsTransferMultiple
            ]
        , testGroup
            "dominantToAccountFor"
            [ testCase "single from with multiple to returns dominant" dominantSingleFromMultipleTo
            , testCase "multiple from returns Nothing" dominantMultipleFrom
            , testCase "single from with single to returns Nothing" dominantSingleFromSingleTo
            , testCase "opening transaction returns Nothing" dominantOpening
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

multiToTransaction :: Hledger.Transaction
multiToTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "assets:checking", Hledger.pamount = mixed [num (-500)]}
            , nullposting{Hledger.paccount = "expenses:food", Hledger.pamount = mixed [num 300]}
            , nullposting{Hledger.paccount = "liabilities:credit", Hledger.pamount = mixed [num 200]}
            ]
        }

multiFromTransaction :: Hledger.Transaction
multiFromTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "assets:checking", Hledger.pamount = mixed [num (-100)]}
            , nullposting{Hledger.paccount = "assets:savings", Hledger.pamount = mixed [num (-100)]}
            , nullposting{Hledger.paccount = "expenses:food", Hledger.pamount = mixed [num 200]}
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

-- fromAccountsFor tests

fromAccountsExpense :: IO ()
fromAccountsExpense = assertEqual "expense returns credit accounts" [Assets ["checking"]] (fromAccountsFor expenseTransaction)

fromAccountsIncome :: IO ()
fromAccountsIncome = assertEqual "income returns credit accounts" [Income ["salary"]] (fromAccountsFor incomeTransaction)

-- toAccountsFor tests

toAccountsOpening :: IO ()
toAccountsOpening =
    assertEqual "opening returns debit accounts (collapsed by group)" [Assets ["checking"]] (toAccountsFor openingTransaction)

toAccountsExpense :: IO ()
toAccountsExpense =
    assertEqual "expense returns debit accounts" [Expenses ["food"]] (toAccountsFor expenseTransaction)

toAccountsIncome :: IO ()
toAccountsIncome =
    assertEqual "income returns dominant account" [Assets ["checking"]] (toAccountsFor incomeTransaction)

toAccountsTransferMultiple :: IO ()
toAccountsTransferMultiple =
    assertEqual "transfer returns all to accounts (different groups)" [Liabilities ["credit"], Expenses ["food"]] (toAccountsFor multiToTransaction)

-- dominantToAccountFor tests

dominantSingleFromMultipleTo :: IO ()
dominantSingleFromMultipleTo =
    assertEqual "returns dominant account by total" (Just (Expenses ["food"])) (dominantToAccountFor multiToTransaction)

dominantMultipleFrom :: IO ()
dominantMultipleFrom = assertEqual "multiple from returns Nothing" Nothing (dominantToAccountFor multiFromTransaction)

dominantSingleFromSingleTo :: IO ()
dominantSingleFromSingleTo = assertEqual "single from single to returns Nothing" Nothing (dominantToAccountFor expenseTransaction)

dominantOpening :: IO ()
dominantOpening = assertEqual "opening returns Nothing" Nothing (dominantToAccountFor openingTransaction)

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
