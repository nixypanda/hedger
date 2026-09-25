module Hedger.Domain.TransactionSpec (
    transactionSpec,
) where

import Control.Monad (when)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (fromGregorian)
import Hedgehog (Gen, Property, PropertyT, assert, footnote, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.Hedgehog (testProperty)

import Hledger.Data.Amount (amountsRaw, mixed, num)
import Hledger.Data.Posting (nullposting)
import Hledger.Data.Transaction (nulltransaction)
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Account (AccountInfo, accountInfo, accountLabel)
import Hedger.Domain.Internal.Account (isAssetOnly, isCashOnly, isLiabilities, isRevenue)
import Hedger.Domain.Internal.Transaction (
    TransactionClassification (..),
    classificationFor,
    dominantMovement,
    isOpening,
    newestFirst,
 )
import Hedger.Generators (
    Intent (..),
    genAccountComponent,
    genIntentTransaction,
    genTransaction,
    genTransactions,
 )

transactionSpec :: TestTree
transactionSpec =
    testGroup
        "transaction projection"
        [ testCase
            "orders newest dates first and reverses journal order within a date"
            ordersNewestFirst
        , testProperty "result is sorted in descending order by (date, index)" sortedProperty
        , testGroup
            "classificationFor"
            [ testCase "unclassified: below 80% threshold" classificationUnclassified
            , testGroup
                "complex multi-account scenarios"
                [ testCase "SIP: Cash -> multiple Assets + Expense" classificationSIP
                , testCase "loan disbursement" classificationLoanDisbursement
                , testCase "EMI payment" classificationEMIPayment
                , testCase "asset sale with profit" classificationAssetSaleProfit
                , testCase "asset sale with loss" classificationAssetSaleLoss
                , testCase "reallocation with fees" classificationReallocationWithFees
                , testCase "ambiguous multi-account" classificationAmbiguousMultiAccount
                , testCase "borrowing expense dominant" classificationBorrowingExpenseDominant
                , testCase "repayment interest dominant" classificationRepaymentInterestDominant
                ]
            ]
        , testGroup
            "classification laws"
            [ testProperty "intent round-trip" intentRoundTrip
            , testProperty "structural postconditions" classificationStructuralPostconditions
            , testProperty "posting order independence" postingOrderIndependence
            , testProperty "totality and determinism" classificationTotality
            ]
        , testGroup
            "isOpening"
            [ testCase "income account disqualifies" openingIncomeDisqualifies
            , testCase "empty debits is not opening" openingEmptyDebits
            ]
        , testGroup
            "dominantMovement"
            [ testCase "two groups exact tie uses alphabetical order" dominantTwoGroupsTie
            , testGroup
                "properties"
                [ testProperty "pct is always between 0 and 1" dominantPctInRange
                , testProperty "no debit postings yields (\"\", 0.0)" dominantEmptyDebits
                , testProperty
                    "the returned group's sum is the largest among all groups"
                    dominantIsMaximal
                , testProperty
                    "splicing in extra credit postings never changes the result"
                    dominantSpliceCreditsInvariant
                , testProperty
                    "dropping all credit postings never changes the result"
                    dominantDropCreditsInvariant
                ]
            ]
        ]

-- Test transactions
incomeTransaction :: Hledger.Transaction
incomeTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting{Hledger.paccount = "income:salary", Hledger.pamount = mixed [num (-500)]}
            , nullposting{Hledger.paccount = "assets:checking", Hledger.pamount = mixed [num 500]}
            ]
        }

-- Unclassified test transaction
unclassifiedTransaction :: Hledger.Transaction
unclassifiedTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting
                { Hledger.paccount = "assets:cash:bank"
                , Hledger.pamount = mixed [num (-1000)]
                }
            , nullposting
                { Hledger.paccount = "assets:investments:stock"
                , Hledger.pamount = mixed [num 600]
                }
            , nullposting
                { Hledger.paccount = "expense:financial:brokerage"
                , Hledger.pamount = mixed [num 400]
                }
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
            [ nullposting
                { Hledger.paccount = "assets:cash:bank"
                , Hledger.pamount = mixed [num (-15000)]
                }
            , nullposting
                { Hledger.paccount = "liabilities:loan:personal"
                , Hledger.pamount = mixed [num 12000]
                }
            , nullposting
                { Hledger.paccount = "expense:financial:interest"
                , Hledger.pamount = mixed [num 3000]
                }
            ]
        }

assetSaleProfitTransaction :: Hledger.Transaction
assetSaleProfitTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting
                { Hledger.paccount = "assets:investments:stock"
                , Hledger.pamount = mixed [num (-100)]
                }
            , nullposting
                { Hledger.paccount = "assets:cash:bank"
                , Hledger.pamount = mixed [num 110]
                }
            , nullposting
                { Hledger.paccount = "income:capital:gains"
                , Hledger.pamount = mixed [num 10]
                }
            ]
        }

assetSaleLossTransaction :: Hledger.Transaction
assetSaleLossTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting
                { Hledger.paccount = "assets:investments:stock"
                , Hledger.pamount = mixed [num (-100)]
                }
            , nullposting
                { Hledger.paccount = "assets:cash:bank"
                , Hledger.pamount = mixed [num 90]
                }
            , nullposting
                { Hledger.paccount = "expenses:capital:loss"
                , Hledger.pamount = mixed [num 10]
                }
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

ambiguousTransaction :: Hledger.Transaction
ambiguousTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting
                { Hledger.paccount = "assets:cash:bank"
                , Hledger.pamount = mixed [num (-1000)]
                }
            , nullposting
                { Hledger.paccount = "assets:investments:stock"
                , Hledger.pamount = mixed [num 600]
                }
            , nullposting
                { Hledger.paccount = "expense:financial:brokerage"
                , Hledger.pamount = mixed [num 400]
                }
            ]
        }

borrowingExpenseDominantTransaction :: Hledger.Transaction
borrowingExpenseDominantTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting
                { Hledger.paccount = "liabilities:loan"
                , Hledger.pamount = mixed [num (-100)]
                }
            , nullposting
                { Hledger.paccount = "expense:financial:processing"
                , Hledger.pamount = mixed [num 60]
                }
            , nullposting
                { Hledger.paccount = "assets:cash:bank"
                , Hledger.pamount = mixed [num 40]
                }
            ]
        }

repaymentInterestDominantTransaction :: Hledger.Transaction
repaymentInterestDominantTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting
                { Hledger.paccount = "assets:cash:bank"
                , Hledger.pamount = mixed [num (-1000)]
                }
            , nullposting
                { Hledger.paccount = "liabilities:loan"
                , Hledger.pamount = mixed [num 600]
                }
            , nullposting
                { Hledger.paccount = "expense:financial:interest"
                , Hledger.pamount = mixed [num 400]
                }
            ]
        }

tieTransaction :: Hledger.Transaction
tieTransaction =
    nulltransaction
        { Hledger.tpostings =
            [ nullposting
                { Hledger.paccount = "assets:cash:bank"
                , Hledger.pamount = mixed [num (-100)]
                }
            , nullposting
                { Hledger.paccount = "assets:investments:stock"
                , Hledger.pamount = mixed [num 50]
                }
            , nullposting
                { Hledger.paccount = "expenses:food"
                , Hledger.pamount = mixed [num 50]
                }
            ]
        }

-- classificationFor tests
classificationUnclassified :: Assertion
classificationUnclassified =
    assertEqual "unclassified" Unclassified (classificationFor unclassifiedTransaction)

-- Complex multi-account scenario tests
classificationSIP :: Assertion
classificationSIP = assertEqual "sip" AssetAcquisition (classificationFor sipTransaction)

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

classificationAmbiguousMultiAccount :: Assertion
classificationAmbiguousMultiAccount =
    assertEqual "ambiguous multi account" Unclassified (classificationFor ambiguousTransaction)

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
openingIncomeDisqualifies :: Assertion
openingIncomeDisqualifies = assertEqual "income disqualifies" False (isOpening incomeTransaction)

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
dominantTwoGroupsTie :: Assertion
dominantTwoGroupsTie =
    assertEqual "assets 50 vs expenses 50" ("assets", 0.5) (dominantMovement tieTransaction)

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
sortedProperty =
    property $ do
        transactions <- forAll (genTransactions (Range.linear 0 80))
        let result = newestFirst transactions
            keys = (\t -> (t.tdate, t.tindex)) <$> result
        assert (isDescending keys)

-- Internal helpers
isDescending :: (Ord a) => [a] -> Bool
isDescending [] = True
isDescending [_] = True
isDescending (x : y : rest) = x >= y && isDescending (y : rest)

-- dominantMovement properties

-- | An account name that classifies into one of the known top-level groups.
genGroupAccountName :: Gen Text
genGroupAccountName = do
    segment <- Gen.element ["assets", "liabilities", "expenses", "income", "equity"]
    subs <- Gen.list (Range.linear 0 3) genAccountComponent
    pure (Text.intercalate ":" (segment : subs))

genGroupPosting :: Gen Hledger.Posting
genGroupPosting = do
    account <- genGroupAccountName
    quantity <- Gen.integral (Range.linearFrom 0 (-1000000) 1000000)
    pure
        nullposting
            { Hledger.paccount = account
            , Hledger.pamount = mixed [num (fromInteger quantity)]
            }

genGroupTransaction :: Gen Hledger.Transaction
genGroupTransaction = do
    year <- Gen.integral (Range.linear 1970 2100)
    month <- Gen.integral (Range.linear 1 12)
    dayOfMonth <- Gen.integral (Range.linear 1 28)
    postings <- Gen.list (Range.linear 1 8) genGroupPosting
    pure
        nulltransaction
            { Hledger.tdate = fromGregorian year month dayOfMonth
            , Hledger.tpostings = postings
            }

-- | A posting with a negative (credit) amount and an arbitrary, unclassified account.
genArbitraryCreditPosting :: Gen Hledger.Posting
genArbitraryCreditPosting = do
    account <- Text.pack <$> Gen.list (Range.linear 1 30) Gen.alphaNum
    quantity <- Gen.integral (Range.linear (-1000000) (-1))
    pure
        nullposting
            { Hledger.paccount = account
            , Hledger.pamount = mixed [num (fromInteger quantity)]
            }

isCredit :: Hledger.Posting -> Bool
isCredit p = any ((< 0) . (.aquantity)) (amountsRaw p.pamount)

dominantPctInRange :: Property
dominantPctInRange =
    property $ do
        tx <- forAll genGroupTransaction
        let (_, pct) = dominantMovement tx
        assert (0 <= pct && pct <= 1)

dominantEmptyDebits :: Property
dominantEmptyDebits =
    property $ do
        postings <- forAll (Gen.list (Range.linear 0 5) genArbitraryCreditPosting)
        let tx = nulltransaction{Hledger.tpostings = postings}
        dominantMovement tx === ("", 0.0)

dominantIsMaximal :: Property
dominantIsMaximal =
    property $ do
        tx <- forAll genGroupTransaction
        let (dominantGroup, _) = dominantMovement tx
            groupSums =
                Map.fromListWith
                    (+)
                    [ (accountLabel (accountInfo p.paccount), abs amount.aquantity)
                    | p <- tx.tpostings
                    , amount <- amountsRaw p.pamount
                    , amount.aquantity > 0
                    ]
            dominantSum = Map.findWithDefault 0 dominantGroup groupSums
        assert (all (<= dominantSum) (Map.elems groupSums))

spliceAt :: Int -> [value] -> [value] -> [value]
spliceAt position extra values =
    let (before, after) = splitAt position values
     in before <> extra <> after

dominantSpliceCreditsInvariant :: Property
dominantSpliceCreditsInvariant =
    property $ do
        tx <- forAll genGroupTransaction
        extras <- forAll (Gen.list (Range.linear 0 5) genArbitraryCreditPosting)
        position <- forAll (Gen.integral (Range.linear 0 (length tx.tpostings)))
        let spliced = tx{Hledger.tpostings = spliceAt position extras tx.tpostings}
        dominantMovement spliced === dominantMovement tx

dominantDropCreditsInvariant :: Property
dominantDropCreditsInvariant =
    property $ do
        tx <- forAll genGroupTransaction
        let onlyNonCredits = tx{Hledger.tpostings = filter (not . isCredit) tx.tpostings}
        dominantMovement onlyNonCredits === dominantMovement tx

-- classificationFor laws

-- | The classification that each intent's generated transaction must receive.
intentClassification :: Intent -> TransactionClassification
intentClassification intent = case intent of
    ExpenseIntent -> TransactionExpense
    IncomeIntent -> TransactionIncome
    TransferIntent -> TransactionTransfer
    OpeningIntent -> OpeningBalance
    AcquisitionIntent -> AssetAcquisition
    DisposalIntent -> AssetDisposal
    ReallocationIntent -> AssetReallocation
    BorrowingIntent -> Borrowing
    RepaymentIntent -> Repayment

intentRoundTrip :: Property
intentRoundTrip =
    property $ do
        intent <- forAll (Gen.element [minBound .. maxBound])
        tx <- forAll (genIntentTransaction intent)
        classificationFor tx === intentClassification intent

{- | Exercise both arbitrary group-shaped accounts and the curated per-intent
recipes, so every classification branch is reachable.
-}
genClassificationTransaction :: Gen Hledger.Transaction
genClassificationTransaction =
    Gen.choice
        [ genGroupTransaction
        , genIntentTransaction =<< Gen.element [minBound .. maxBound]
        ]

isDebit :: Hledger.Posting -> Bool
isDebit p = any ((> 0) . (.aquantity)) (amountsRaw p.pamount)

-- | Postings with a negative quantity; mirrors the domain's internal creditsFor.
creditPostings :: Hledger.Transaction -> [Hledger.Posting]
creditPostings = filter isCredit . (.tpostings)

-- | Postings with a positive quantity; mirrors the domain's internal debitsFor.
debitPostings :: Hledger.Transaction -> [Hledger.Posting]
debitPostings tx = filter isDebit tx.tpostings

-- | Does any posting's inferred account satisfy the predicate?
anyPostingAccount :: (AccountInfo -> Bool) -> [Hledger.Posting] -> Bool
anyPostingAccount predicate = any (predicate . accountInfo . (.paccount))

classificationStructuralPostconditions :: Property
classificationStructuralPostconditions =
    property $ do
        tx <- forAll genClassificationTransaction
        structuralLaws tx

{- | The classification 'classificationFor' returns must satisfy every
structural invariant implied by the branch that produced it.
-}
structuralLaws :: Hledger.Transaction -> PropertyT IO ()
structuralLaws tx = do
    let classification = classificationFor tx
        (dominantGroup, dominantPct) = dominantMovement tx
        credits = creditPostings tx
        debits = debitPostings tx

    footnote "isOpening implies OpeningBalance"
    when (isOpening tx) (classification === OpeningBalance)

    footnote "AssetAcquisition implies a cash credit and an asset debit"
    when (classification == AssetAcquisition) $ do
        assert (anyPostingAccount isCashOnly credits)
        assert (anyPostingAccount isAssetOnly debits)

    footnote "AssetDisposal implies an asset credit and a cash debit"
    when (classification == AssetDisposal) $ do
        assert (anyPostingAccount isAssetOnly credits)
        assert (anyPostingAccount isCashOnly debits)

    footnote "AssetReallocation implies asset credits and asset debits"
    when (classification == AssetReallocation) $ do
        assert (anyPostingAccount isAssetOnly credits)
        assert (anyPostingAccount isAssetOnly debits)

    footnote "Borrowing implies a liability credit"
    when (classification == Borrowing) (assert (anyPostingAccount isLiabilities credits))

    footnote "Repayment implies a liability-dominant debit and a liability debit"
    when (classification == Repayment) $ do
        dominantGroup === "liabilities"
        assert (anyPostingAccount isLiabilities debits)

    footnote "TransactionExpense implies an expense-dominant debit"
    when (classification == TransactionExpense) (dominantGroup === "expenses")

    footnote "TransactionIncome implies an asset-dominant debit and a revenue credit"
    when (classification == TransactionIncome) $ do
        dominantGroup === "assets"
        assert (anyPostingAccount isRevenue credits)

    footnote "below the threshold and not opening implies Unclassified"
    when (not (isOpening tx) && dominantPct < 0.8) (classification === Unclassified)

postingOrderIndependence :: Property
postingOrderIndependence =
    property $ do
        tx <- forAll genGroupTransaction
        shuffled <- forAll (Gen.shuffle tx.tpostings)
        let reordered = tx{Hledger.tpostings = shuffled}
        classificationFor reordered === classificationFor tx
        dominantMovement reordered === dominantMovement tx

classificationTotality :: Property
classificationTotality =
    property $ do
        tx <- forAll genTransaction
        let classification = classificationFor tx
            movement = dominantMovement tx
        classification === classification
        assert (0 <= snd movement && snd movement <= 1)
