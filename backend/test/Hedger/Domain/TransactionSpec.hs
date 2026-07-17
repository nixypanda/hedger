module Hedger.Domain.TransactionSpec (transactionSpec) where

import Data.List qualified as List

import Data.Time (fromGregorian)
import Hedgehog (Property, assert, forAll, property, (===))
import Hedgehog.Range qualified as Range
import Hedger.Domain.Transaction (newestFirst)
import Hedger.Generators (genTransactions)
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
        ]

syntheticTransactions :: [Hledger.Transaction]
syntheticTransactions =
    [ nulltransaction{Hledger.tindex = 1, Hledger.tdate = fromGregorian 2024 1 1, Hledger.tdescription = "Transaction A", Hledger.tpostings = [nullposting]}
    , nulltransaction{Hledger.tindex = 2, Hledger.tdate = fromGregorian 2024 1 2, Hledger.tdescription = "Transaction B", Hledger.tpostings = [nullposting]}
    , nulltransaction{Hledger.tindex = 3, Hledger.tdate = fromGregorian 2024 1 3, Hledger.tdescription = "Transaction C", Hledger.tpostings = [nullposting]}
    , nulltransaction{Hledger.tindex = 4, Hledger.tdate = fromGregorian 2024 1 4, Hledger.tdescription = "Transaction D", Hledger.tpostings = [nullposting]}
    , nulltransaction{Hledger.tindex = 5, Hledger.tdate = fromGregorian 2024 1 5, Hledger.tdescription = "Transaction E", Hledger.tpostings = [nullposting]}
    ]

ordersNewestFirst :: IO ()
ordersNewestFirst = do
    let sameDate = zipWith (\index t -> t{Hledger.tdate = fromGregorian 2024 1 1, Hledger.tindex = index}) [1 ..] (take 3 syntheticTransactions)
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

isDescending :: Ord a => [a] -> Bool
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
