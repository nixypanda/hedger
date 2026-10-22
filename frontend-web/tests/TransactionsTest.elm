module TransactionsTest exposing (suite)

import Api.Client as Api
import Api.Error as ApiError exposing (ApiError)
import Api.Transaction as Transaction exposing (Response)
import Data.AccountType as AccountType
import Data.Date as Date
import Data.Freshness exposing (Freshness(..))
import Data.Metadata exposing (Metadata)
import Data.Status as Status
import Data.Transaction exposing (Transaction, TransactionEffect(..), effectDecoder)
import Expect
import Fuzz
import Json.Decode as Decode
import Page.Transactions as Page
import Test exposing (Test, describe, fuzz, test)


{-| Two domain rules live on the Transactions page: a selection only ever
references a transaction the page currently holds, and the bounded paging window
never stitches together pages from different journal revisions.
-}
suite : Test
suite =
    describe "Transactions"
        [ test "decodes the backend transaction shape without losing nested postings or costs" <|
            \_ ->
                Decode.decodeString Transaction.decoder transactionGoldenJson
                    |> Result.map
                        (\response ->
                            ( List.length response.transactions
                            , response.transactions
                                |> List.concatMap .postings
                                |> List.length
                            , response.transactions
                                |> List.concatMap .postings
                                |> List.concatMap .amounts
                                |> List.filterMap .cost
                                |> List.map .costType
                            )
                        )
                    |> Expect.equal (Ok ( 13, 27, [ "total", "unit" ] ))
        , test "keeps every effect on a mixed transaction" <|
            \_ ->
                Decode.decodeString Transaction.decoder transactionGoldenJson
                    |> Result.toMaybe
                    |> Maybe.andThen (.transactions >> List.head)
                    |> Maybe.map (.presentation >> .effects)
                    |> Expect.equal (Just [ TransactionExpense, Borrowing ])
        , test "unknown effect labels decode while known labels are preserved" <|
            \_ ->
                Decode.decodeString (Decode.list effectDecoder) """["income","mystery_effect"]"""
                    |> Expect.equal (Ok [ TransactionIncome, Unknown "mystery_effect" ])
        , test "initial load has no stale data" <|
            \_ ->
                let
                    ( loading, request ) =
                        Page.beginLoading Page.init
                in
                ( Page.status loading, Page.visibleResponse loading, Page.requestCursor request )
                    |> Expect.equal ( Page.InitialLoadingStatus, Nothing, Nothing )
        , test "an empty response reaches success without manufacturing data" <|
            \_ ->
                loadInitial { metadata = metadata "revision-a", transactions = [], nextCursor = Nothing }
                    |> Page.visibleResponse
                    |> Maybe.map .transactions
                    |> Expect.equal (Just [])
        , test "apply trims the draft and resets results, paging, and selection" <|
            \_ ->
                let
                    response =
                        sampleResponse "revision-a"

                    ( applied, request ) =
                        loadInitial response
                            |> selectFirst response
                            |> Page.setDraft "  acct:expenses  "
                            |> Page.applyDraft
                in
                { draft = Page.draftQuery applied
                , applied = Page.appliedQuery applied
                , selected = Page.selectedTransaction applied
                , pages = Page.retainedPageCount applied
                , status = Page.status applied
                , requestedQuery = Page.requestQuery request
                }
                    |> Expect.equal
                        { draft = "acct:expenses"
                        , applied = "acct:expenses"
                        , selected = Nothing
                        , pages = 0
                        , status = Page.InitialLoadingStatus
                        , requestedQuery = "acct:expenses"
                        }
        , test "loaded windows own the applied query independently of later drafts" <|
            \_ ->
                let
                    ( loading, applyRequest ) =
                        Page.init |> Page.setDraft "acct:assets" |> Page.applyDraft

                    edited =
                        Page.receive applyRequest (Ok (samplePage 0)) loading
                            |> Page.setDraft "desc:unfinished"

                    ( refreshing, refreshRequest ) =
                        Page.beginLoading edited
                in
                ( Page.appliedQuery refreshing
                , Page.draftQuery refreshing
                , Page.requestQuery refreshRequest
                )
                    |> Expect.equal ( "acct:assets", "desc:unfinished", "acct:assets" )
        , test "query errors decode their safe reason and position" <|
            \_ ->
                ( Decode.decodeString ApiError.decoder
                    """
                    {"error":{"code":"invalid_query",
                    "message":"The query is invalid.",
                    "requestId":"request-1",
                    "details":{"reason":"expected a closing parenthesis",
                    "position":14}}}
                    """
                    |> Result.map ApiError.message
                , Decode.decodeString ApiError.decoder
                    """
                    {"error":{"code":"journal_unavailable",
                    "message":"No snapshot",
                    "requestId":"request-1",
                    "details":null}}
                    """
                    |> Result.map .code
                )
                    |> Expect.equal
                        ( Ok
                            ("The query is invalid. expected a closing parenthesis"
                                ++ " (at character 14) (request request-1)"
                            )
                        , Ok "journal_unavailable"
                        )
        , test "a query error preserves the applied draft for correction" <|
            \_ ->
                let
                    queryError =
                        { code = "invalid_query"
                        , message = "The query is invalid."
                        , requestId = Nothing
                        , details = Just { reason = "expected a term", position = Just 5 }
                        }

                    ( loading, request ) =
                        Page.init
                            |> Page.setDraft "acct:("
                            |> Page.applyDraft

                    failed =
                        Page.receive request (Err queryError) loading
                in
                { draft = Page.draftQuery failed
                , applied = Page.appliedQuery failed
                , visible = Page.visibleResponse failed
                , status = Page.status failed
                }
                    |> Expect.equal
                        { draft = "acct:("
                        , applied = "acct:("
                        , visible = Nothing
                        , status = Page.InitialFailureStatus
                        }
        , test "transaction query and cursor are URL encoded" <|
            \_ ->
                Api.transactionsUrl
                    { query = "acct:food & café"
                    , cursor = Just "page+/="
                    }
                    |> Expect.equal
                        ("/api/v1/transactions"
                            ++ "?query=acct%3Afood%20%26%20caf%C3%A9"
                            ++ "&limit=50"
                            ++ "&cursor=page%2B%2F%3D"
                        )
        , fuzz selectionFuzzer "a selection always references a transaction in the visible window" selectionClosure
        , fuzz pagingFuzzer "the paging window stays bounded and never stitches revisions" pagingLaws
        ]



-- SELECTION


type alias SelectionInput =
    { count : Int
    , pick : Int
    }


selectionFuzzer : Fuzz.Fuzzer SelectionInput
selectionFuzzer =
    Fuzz.intRange 0 8
        |> Fuzz.andThen
            (\count ->
                Fuzz.map (\pick -> { count = count, pick = pick }) (Fuzz.intRange 0 (count + 1))
            )


selectionClosure : SelectionInput -> Expect.Expectation
selectionClosure input =
    let
        response =
            { metadata = metadata "revision-a"
            , transactions = List.range 1 input.count |> List.map sampleTransaction
            , nextCursor = Nothing
            }

        model =
            loadInitial response

        visible =
            response.transactions

        target =
            sampleTransaction
                (if input.pick < input.count then
                    input.pick + 1

                 else
                    999
                )

        outsider =
            sampleTransaction 999

        selected =
            Page.select target model

        chosen =
            Page.selectedTransaction selected

        chosenIsVisible =
            chosen
                |> Maybe.map (\transaction -> List.member transaction visible)
                |> Maybe.withDefault True
    in
    Expect.all
        [ \_ -> Expect.equal True chosenIsVisible
        , \_ -> Expect.equal chosen (Page.selectedTransaction (Page.select target selected))
        , \_ -> Expect.equal chosen (Page.selectedTransaction (Page.select outsider selected))
        , \_ ->
            if input.pick < input.count then
                Expect.equal (Just target) chosen

            else
                Expect.equal Nothing chosen
        ]
        ()



-- PAGING


type alias PagingInput =
    { revision : String
    , appends : List ( String, Int )
    }


pagingFuzzer : Fuzz.Fuzzer PagingInput
pagingFuzzer =
    Fuzz.map2 PagingInput
        (Fuzz.oneOfValues [ "revision-a", "revision-b" ])
        (Fuzz.list (Fuzz.map2 Tuple.pair (Fuzz.oneOfValues [ "revision-a", "revision-b" ]) (Fuzz.intRange 0 8)))


pagingLaws : PagingInput -> Expect.Expectation
pagingLaws input =
    let
        firstPage =
            responseFor 0 input.revision

        starting =
            loadInitial firstPage |> selectFirst firstPage

        ( final, appendChecks ) =
            List.foldl appendStep ( starting, [] ) input.appends

        checks =
            appendChecks ++ retentionChecks final
    in
    Expect.all (List.map (\check _ -> check) checks) ()


appendStep : ( String, Int ) -> ( Page.Model, List Expect.Expectation ) -> ( Page.Model, List Expect.Expectation )
appendStep ( revision, pageIndex ) ( model, checks ) =
    let
        nextPage =
            responseFor pageIndex revision

        after =
            appendResponse nextPage model

        bounded =
            boundedWindow after

        windowCheck =
            case Page.visibleResponse model of
                Nothing ->
                    Expect.fail "Appending requires a visible window."

                Just window ->
                    if window.metadata.revision /= nextPage.metadata.revision then
                        Expect.all
                            [ \_ -> Expect.equal (Just nextPage) (Page.visibleResponse after)
                            , \_ -> Expect.equal True (selectionIsMember after)
                            ]
                            ()

                    else
                        Page.visibleResponse after
                            |> Maybe.andThen lastDescription
                            |> Expect.equal (lastDescription nextPage)
    in
    ( after, checks ++ [ bounded, windowCheck ] )


selectionIsMember : Page.Model -> Bool
selectionIsMember model =
    case Page.selectedTransaction model of
        Nothing ->
            True

        Just transaction ->
            Page.visibleResponse model
                |> Maybe.map (.transactions >> List.member transaction)
                |> Maybe.withDefault False


boundedWindow : Page.Model -> Expect.Expectation
boundedWindow model =
    let
        rows =
            Page.visibleResponse model
                |> Maybe.map (.transactions >> List.length)
                |> Maybe.withDefault 0
    in
    Expect.equal
        { pages = Page.retainedPageCount model <= 5, rows = rows <= 250 }
        { pages = True, rows = True }


retentionChecks : Page.Model -> List Expect.Expectation
retentionChecks model =
    let
        before =
            modelSnapshot model

        ( refreshing, refreshRequest ) =
            Page.beginLoading model

        refreshFailed =
            Page.receive refreshRequest (Err apiError) refreshing

        loadMoreChecks =
            case Page.beginLoadingMore model of
                Just ( loadingMore, request ) ->
                    let
                        failed =
                            Page.receive request (Err apiError) loadingMore
                    in
                    [ Expect.equal
                        { status = Page.LoadMoreFailureStatus, view = before.view, selected = before.selected }
                        (modelSnapshot failed |> (\snapshot -> { status = snapshot.status, view = snapshot.view, selected = snapshot.selected }))
                    ]

                Nothing ->
                    []
    in
    Expect.equal
        { status = Page.RefreshingStatus, view = before.view, selected = before.selected }
        (modelSnapshot refreshing |> (\snapshot -> { status = snapshot.status, view = snapshot.view, selected = snapshot.selected }))
        :: Expect.equal
            { status = Page.RefreshFailureStatus, view = before.view, selected = before.selected }
            (modelSnapshot refreshFailed |> (\snapshot -> { status = snapshot.status, view = snapshot.view, selected = snapshot.selected }))
        :: loadMoreChecks


type alias ModelSnapshot =
    { status : Page.Status
    , view : Maybe Response
    , selected : Maybe Transaction
    , pages : Int
    }


modelSnapshot : Page.Model -> ModelSnapshot
modelSnapshot model =
    { status = Page.status model
    , view = Page.visibleResponse model
    , selected = Page.selectedTransaction model
    , pages = Page.retainedPageCount model
    }


lastDescription : Response -> Maybe String
lastDescription response =
    response.transactions
        |> List.reverse
        |> List.head
        |> Maybe.map .description


responseFor : Int -> String -> Response
responseFor pageIndex revision =
    let
        base =
            samplePage pageIndex
    in
    { base | metadata = metadata revision }


loadInitial : Response -> Page.Model
loadInitial response =
    let
        ( loading, request ) =
            Page.beginLoading Page.init
    in
    Page.receive request (Ok response) loading


appendResponse : Response -> Page.Model -> Page.Model
appendResponse response model =
    case Page.beginLoadingMore model of
        Just ( loading, request ) ->
            Page.receive request (Ok response) loading

        Nothing ->
            model


selectFirst : Response -> Page.Model -> Page.Model
selectFirst response model =
    case response.transactions of
        transaction :: _ ->
            Page.select transaction model

        [] ->
            model


sampleResponse : String -> Response
sampleResponse revision =
    { metadata = metadata revision
    , transactions =
        [ { primaryDate = Date.Date { year = 2024, month = 4, day = 2 }
          , secondaryDate = Nothing
          , status = Status.Unmarked
          , code = Nothing
          , description = "Utilities"
          , comment = Nothing
          , tags = []
          , postings =
                [ { account = "expenses:utilities"
                  , accountType = Just AccountType.Expense
                  , status = Status.Unmarked
                  , comment = Nothing
                  , tags = []
                  , amounts = []
                  }
                ]
          , presentation =
                { classification = TransactionExpense
                , effects = [ TransactionExpense ]
                }
          }
        ]
    , nextCursor = Nothing
    }


samplePage : Int -> Response
samplePage pageIndex =
    { metadata = metadata "revision-a"
    , transactions =
        List.range (pageIndex * 50 + 1) (pageIndex * 50 + 50)
            |> List.map sampleTransaction
    , nextCursor = Just ("cursor-" ++ String.fromInt (pageIndex + 1))
    }


sampleTransaction : Int -> Transaction
sampleTransaction number =
    let
        base =
            sampleResponse "revision-a"
                |> .transactions
                |> List.head
    in
    case base of
        Just transaction ->
            { transaction | description = "Transaction " ++ String.fromInt number }

        Nothing ->
            { primaryDate = Date.Date { year = 2024, month = 4, day = 2 }
            , secondaryDate = Nothing
            , status = Status.Unmarked
            , code = Nothing
            , description = "Transaction " ++ String.fromInt number
            , comment = Nothing
            , tags = []
            , postings = []
            , presentation = { classification = TransactionExpense, effects = [ TransactionExpense ] }
            }


metadata : String -> Metadata
metadata revision =
    { revision = Just revision, freshness = Fresh, warnings = [] }


apiError : ApiError
apiError =
    { code = "network_error", message = "Unavailable", requestId = Nothing, details = Nothing }


transactionGoldenJson : String
transactionGoldenJson =
    """{"metadata":{"revision":"md5:abc123","freshness":"fresh","warnings":[]},"nextCursor":null,"transactions":[{"primaryDate":"2024-04-02","secondaryDate":null,"status":"unmarked","code":null,"description":"Transaction 1","comment":null,"tags":[],"postings":[{"account":"assets:cash","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"1000","cost":null}]},{"account":"expenses:food","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"500","cost":null}]},{"account":"liabilities:card","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"500","cost":{"costType":"total","commodity":"INR","quantity":"500"}}]}],"presentation":{"classification":"expense","effects":["expense","borrowing"]}},{"primaryDate":"2024-04-02","secondaryDate":null,"status":"unmarked","code":null,"description":"Transaction 2","comment":null,"tags":[],"postings":[{"account":"assets:cash","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"2000","cost":null}]},{"account":"expenses:utilities","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"800","cost":{"costType":"unit","commodity":"INR","quantity":"1"}}]}],"presentation":{"classification":"expense","effects":["expense"]}},{"primaryDate":"2024-04-03","secondaryDate":null,"status":"unmarked","code":null,"description":"Transaction 3","comment":null,"tags":[],"postings":[{"account":"assets:cash","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"5000","cost":null}]},{"account":"income:salary","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"5000","cost":null}]}],"presentation":{"classification":"income","effects":["income"]}},{"primaryDate":"2024-04-03","secondaryDate":null,"status":"unmarked","code":null,"description":"Transaction 4","comment":null,"tags":[],"postings":[{"account":"assets:cash","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"300","cost":null}]},{"account":"assets:bank","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"300","cost":null}]}],"presentation":{"classification":"transfer","effects":["transfer"]}},{"primaryDate":"2024-04-04","secondaryDate":null,"status":"unmarked","code":null,"description":"Transaction 5","comment":null,"tags":[],"postings":[{"account":"assets:cash","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"1500","cost":null}]},{"account":"expenses:rent","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"1500","cost":null}]}],"presentation":{"classification":"expense","effects":["expense"]}},{"primaryDate":"2024-04-04","secondaryDate":null,"status":"unmarked","code":null,"description":"Transaction 6","comment":null,"tags":[],"postings":[{"account":"assets:cash","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"400","cost":null}]},{"account":"expenses:transport","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"200","cost":null}]}],"presentation":{"classification":"expense","effects":["expense"]}},{"primaryDate":"2024-04-05","secondaryDate":null,"status":"unmarked","code":null,"description":"Transaction 7","comment":null,"tags":[],"postings":[{"account":"assets:cash","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"800","cost":null}]},{"account":"expenses:entertainment","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"600","cost":null}]}],"presentation":{"classification":"expense","effects":["expense"]}},{"primaryDate":"2024-04-05","secondaryDate":null,"status":"unmarked","code":null,"description":"Transaction 8","comment":null,"tags":[],"postings":[{"account":"assets:cash","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"1200","cost":null}]},{"account":"assets:investments","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"1200","cost":null}]}],"presentation":{"classification":"transfer","effects":["transfer"]}},{"primaryDate":"2024-04-06","secondaryDate":null,"status":"unmarked","code":null,"description":"Transaction 9","comment":null,"tags":[],"postings":[{"account":"assets:cash","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"2500","cost":null}]},{"account":"income:dividends","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"2500","cost":null}]}],"presentation":{"classification":"income","effects":["income"]}},{"primaryDate":"2024-04-06","secondaryDate":null,"status":"unmarked","code":null,"description":"Transaction 10","comment":null,"tags":[],"postings":[{"account":"assets:cash","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"350","cost":null}]},{"account":"expenses:healthcare","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"350","cost":null}]}],"presentation":{"classification":"expense","effects":["expense"]}},{"primaryDate":"2024-04-07","secondaryDate":null,"status":"unmarked","code":null,"description":"Transaction 11","comment":null,"tags":[],"postings":[{"account":"assets:cash","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"900","cost":null}]},{"account":"expenses:education","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"700","cost":null}]}],"presentation":{"classification":"expense","effects":["expense"]}},{"primaryDate":"2024-04-07","secondaryDate":null,"status":"unmarked","code":null,"description":"Transaction 12","comment":null,"tags":[],"postings":[{"account":"assets:cash","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"4500","cost":null}]},{"account":"liabilities:loan","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"4500","cost":null}]}],"presentation":{"classification":"transfer","effects":["transfer"]}},{"primaryDate":"2024-04-08","secondaryDate":null,"status":"unmarked","code":null,"description":"Transaction 13","comment":null,"tags":[],"postings":[{"account":"assets:cash","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"600","cost":null}]},{"account":"expenses:clothing","accountType":null,"status":"unmarked","comment":null,"tags":[],"amounts":[{"commodity":"INR","quantity":"450","cost":null}]}],"presentation":{"classification":"expense","effects":["expense"]}}]}"""
