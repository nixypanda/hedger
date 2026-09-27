module Hedger.Application.TransactionQuerySpec (
    transactionQuerySpec,
) where

import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Time (fromGregorian)
import Hedgehog (Gen, Property, assert, failure, footnote, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

import Hledger.Data.Posting (nullposting)
import Hledger.Data.Transaction (nulltransaction)
import Hledger.Data.Types qualified as Hledger
import Hledger.Query qualified as HledgerQuery

import Hedger.Application.PageRequest (
    PageLimit,
    defaultPageLimit,
    maximumPageLimit,
    mkPageLimit,
 )
import Hedger.Application.TransactionQuery (
    TransactionPage (..),
    TransactionPageRequest (..),
    parseHledgerQuery,
    transactionPage,
 )
import Hedger.Domain.Error (AppError (..))
import Hedger.Domain.Period (epoch)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Transaction (newestFirst)
import Hedger.Generators (genSnapshot, genTransactions)
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
        , testGroup
            "hledger query filter containment"
            [ testProperty
                "a parsed query selects a subset of the unfiltered view"
                queryFilterContainmentProperty
            , testProperty
                "the empty query is the full view"
                emptyQueryIsFullViewProperty
            , testProperty
                "an over-long query is rejected"
                overLongQueryRejectedProperty
            ]
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
                    , limit = defaultPageLimit
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
                    , limit = defaultPageLimit
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
            , limit = onePageLimit
            , cursor = Nothing
            } of
        Left pageError -> assertFailure (show pageError)
        Right first ->
            case first.nextCursor of
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
                                    , limit = onePageLimit
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
                                    , limit = onePageLimit
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
                                    , limit = onePageLimit
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
                                    , limit = onePageLimit
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
                                    , limit = onePageLimit
                                    , cursor = Just (cursor <> "tampered")
                                    }
                            )
                        )

paginationProperty :: Hedgehog.Property
paginationProperty =
    property $ do
        transactions <- forAll (genTransactions (Range.linear 1 20))
        let snapshot = mkSnapshot transactions
        limit <- forAll (Gen.int (Range.linear 1 (min 13 (length transactions))))
        let expected = fmap (.tdescription) (newestFirst snapshot.transactions)
            actual = fmap (fmap (.tdescription)) (collectPages snapshot limit Nothing)
        actual === Right expected

collectPages :: Snapshot -> Int -> Maybe Text.Text -> Either AppError [Hledger.Transaction]
collectPages snapshot limit cursor = do
    parsedLimit <- mkPageLimit limit
    page <-
        transactionPage
            snapshot
            TransactionPageRequest{query = Nothing, limit = parsedLimit, cursor}
    rest <- maybe (Right []) (collectPages snapshot limit . Just) page.nextCursor
    pure (page.transactions <> rest)

isInvalidCursor :: Either AppError value -> Bool
isInvalidCursor (Left (InvalidCursor _)) = True
isInvalidCursor _ = False

{- | An account term drawn from the generated ledger's vocabulary. Every term
is a valid hledger @acct:@ query, optionally negated with @not:@.
-}
genAccountQueryTerm :: Gen Text.Text
genAccountQueryTerm = do
    negated <- Gen.bool
    account <- Gen.element accountQueryVocabulary
    pure (if negated then "not:" <> account else account)

accountQueryVocabulary :: [Text.Text]
accountQueryVocabulary =
    [ "acct:assets"
    , "acct:income"
    , "acct:expenses"
    , "acct:liabilities"
    , "acct:equity"
    , "acct:investments"
    , "acct:cash"
    , "acct:investments:stock"
    ]

{- | Either one account term, or two of them conjoined with @and@. hledger's
top-level query parser only understands boolean operators inside an
@expr:@ argument, and the whitespace-separated expression must be quoted so
it survives the top-level word split.
-}
genAccountQueryExpression :: Gen Text.Text
genAccountQueryExpression = do
    first <- genAccountQueryTerm
    combined <- Gen.bool
    if combined
        then do
            second <- genAccountQueryTerm
            pure ("expr:\"(" <> first <> " and " <> second <> ")\"")
        else pure first

{- | A parsed query's matches are drawn from the unfiltered transaction view:
the filtered page is no larger than the full one, every transaction in it is
one of the full view's transactions, and each transaction satisfies the parsed
query.
-}
queryFilterContainmentProperty :: Property
queryFilterContainmentProperty = property $ do
    snapshot <- forAll genSnapshot
    rawQuery <- forAll genAccountQueryExpression
    let reference = fromMaybe epoch snapshot.latestJournalDate
    parsedQuery <- case parseHledgerQuery reference rawQuery of
        Left parseError -> do
            footnote ("generated query: " <> show rawQuery)
            footnote ("parse error: " <> show parseError)
            failure
        Right parsed -> pure parsed
    full <- unwrapPage (pageFor snapshot Nothing)
    filtered <- unwrapPage (pageFor snapshot (Just rawQuery))
    footnote ("generated query: " <> show rawQuery)
    footnote ("unfiltered transaction count: " <> show (length full))
    footnote ("filtered transaction count: " <> show (length filtered))
    assert (length filtered <= length full)
    assert (all (`List.elem` full) filtered)
    assert (all (HledgerQuery.matchesTransaction parsedQuery) filtered)
  where
    pageFor snapshot queryText =
        fmap (.transactions) $
            transactionPage
                snapshot
                TransactionPageRequest{query = queryText, limit = fullViewLimit, cursor = Nothing}

    unwrapPage pageResult = case pageResult of
        Left pageError -> do
            footnote ("page error: " <> show pageError)
            failure
        Right transactions -> pure transactions

{- | The empty query is hledger's @Any@ and returns every transaction in the
snapshot, ignoring order.
-}
emptyQueryIsFullViewProperty :: Property
emptyQueryIsFullViewProperty = property $ do
    snapshot <- forAll genSnapshot
    let reference = fromMaybe epoch snapshot.latestJournalDate
    parseHledgerQuery reference "" === Right HledgerQuery.Any
    page <-
        unwrapPage $
            transactionPage
                snapshot
                TransactionPageRequest{query = Just "", limit = fullViewLimit, cursor = Nothing}
    length page.transactions === length snapshot.transactions
    assert (isSameTransactionBag page.transactions snapshot.transactions)
  where
    unwrapPage pageResult = case pageResult of
        Left pageError -> do
            footnote ("page error: " <> show pageError)
            failure
        Right page -> pure page

{- | A query longer than the domain limit is rejected before hledger ever sees
it, regardless of its contents.
-}
overLongQueryRejectedProperty :: Property
overLongQueryRejectedProperty = property $ do
    rawQuery <- forAll genOverLongQuery
    parseHledgerQuery epoch rawQuery === Left (QueryTooLong 2048)

genOverLongQuery :: Gen Text.Text
genOverLongQuery = Text.pack <$> Gen.list (Range.linear 2049 4096) Gen.alphaNum

-- | Multiset equality for transactions, which have 'Eq' but not 'Ord'.
isSameTransactionBag :: [Hledger.Transaction] -> [Hledger.Transaction] -> Bool
isSameTransactionBag left right =
    length left == length right
        && null (List.foldl' (flip List.delete) left right)

-- | The page size used to request an unpaginated view of a generated snapshot.
fullViewLimit :: PageLimit
fullViewLimit = maximumPageLimit

onePageLimit :: PageLimit
onePageLimit = either (error . show) id (mkPageLimit 1)

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
