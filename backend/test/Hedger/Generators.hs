module Hedger.Generators (
    genTransaction,
    genTransactions,
) where

import Data.Text qualified as Text
import Data.Time (Day (ModifiedJulianDay))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Hledger.Data.Amount (mixed, num)
import Hledger.Data.Posting (nullposting)
import Hledger.Data.Transaction (nulltransaction)
import Hledger.Data.Types qualified as Hledger

genTransaction :: Gen Hledger.Transaction
genTransaction = do
    day <- ModifiedJulianDay <$> Gen.integral (Range.linear 40000 80000)
    index <- Gen.integral (Range.linear 0 100000)
    description <- genText (Range.linear 1 50)
    postings <- Gen.list (Range.linear 1 4) genPosting
    pure
        nulltransaction
            { Hledger.tdate = day
            , Hledger.tindex = index
            , Hledger.tdescription = description
            , Hledger.tpostings = postings
            }

genPosting :: Gen Hledger.Posting
genPosting = do
    account <- genText (Range.linear 1 30)
    amount <- Gen.integral (Range.linear (-1000000) 1000000)
    pure
        nullposting
            { Hledger.paccount = account
            , Hledger.pamount = mixed [num (fromInteger amount)]
            }

genTransactions :: Range.Range Int -> Gen [Hledger.Transaction]
genTransactions range = Gen.list range genTransaction

genText :: Range.Range Int -> Gen Text.Text
genText range = Text.pack <$> Gen.list range Gen.alphaNum
