module TransactionsTest exposing (suite)

import Api.Client as Api
import Api.Error as ApiError exposing (ApiError)
import Api.Metadata exposing (Freshness(..), Metadata)
import Api.Transaction as Transaction exposing (Response)
import Expect
import Json.Decode as Decode
import Page.Transactions as Page
import Test exposing (Test, describe, test)
import TransactionGolden


suite : Test
suite =
    describe "Transactions"
        [ test "decodes the backend transaction golden without losing nested postings or costs" <|
            \_ ->
                Decode.decodeString Transaction.decoder TransactionGolden.json
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
        , test "initial load has no stale data" <|
            \_ ->
                let
                    ( loading, request ) =
                        Page.beginLoading Page.init
                in
                ( Page.status loading, Page.visibleResponse loading, Page.requestCursor request )
                    |> Expect.equal ( Page.InitialLoadingStatus, Nothing, Nothing )
        , test "selection accepts a loaded complete DTO and rejects an outsider" <|
            \_ ->
                let
                    response =
                        sampleResponse "revision-a"

                    model =
                        loadInitial response
                in
                case response.transactions of
                    transaction :: _ ->
                        let
                            selected =
                                Page.select transaction model

                            outsider =
                                { transaction | description = "Not in the window" }
                        in
                        ( Page.selectedTransaction selected |> Maybe.map (.postings >> List.length)
                        , Page.select outsider selected |> Page.selectedTransaction
                        )
                            |> Expect.equal ( Just 1, Just transaction )

                    [] ->
                        Expect.fail "The test response needs one transaction."
        , test "refresh preserves stale data and selection" <|
            \_ ->
                let
                    response =
                        sampleResponse "revision-a"

                    selected =
                        selectFirst response (loadInitial response)

                    ( refreshing, _ ) =
                        Page.beginLoading selected
                in
                ( Page.status refreshing
                , Page.visibleResponse refreshing
                , Page.selectedTransaction refreshing
                )
                    |> Expect.equal
                        ( Page.RefreshingStatus, Just response, Page.selectedTransaction selected )
        , test "refresh retains selection only when it remains in the refreshed window" <|
            \_ ->
                let
                    response =
                        sampleResponse "revision-a"

                    selected =
                        selectFirst response (loadInitial response)

                    ( refreshing, request ) =
                        Page.beginLoading selected
                in
                Page.receive request (Ok response) refreshing
                    |> Page.selectedTransaction
                    |> Expect.equal (Page.selectedTransaction selected)
        , test "refresh structurally removes selection when the row leaves the window" <|
            \_ ->
                let
                    response =
                        samplePage 0

                    selected =
                        selectFirst response (loadInitial response)

                    ( refreshing, request ) =
                        Page.beginLoading selected

                    replacement =
                        { metadata = metadata "revision-a"
                        , transactions = [ sampleTransaction 999 ]
                        , nextCursor = Nothing
                        }
                in
                Page.receive request (Ok replacement) refreshing
                    |> Page.selectedTransaction
                    |> Expect.equal Nothing
        , test "revision change clears selection" <|
            \_ ->
                let
                    first =
                        samplePage 0

                    selected =
                        selectFirst first (loadInitial first)
                in
                appendResponse (sampleResponse "revision-b") selected
                    |> Page.selectedTransaction
                    |> Expect.equal Nothing
        , test "an empty response reaches success without manufacturing data" <|
            \_ ->
                loadInitial { metadata = metadata "revision-a", transactions = [], nextCursor = Nothing }
                    |> Page.visibleResponse
                    |> Maybe.map .transactions
                    |> Expect.equal (Just [])
        , test "refresh failure retains stale response and selection" <|
            \_ ->
                let
                    response =
                        sampleResponse "revision-a"

                    selected =
                        selectFirst response (loadInitial response)

                    ( refreshing, request ) =
                        Page.beginLoading selected

                    failed =
                        Page.receive request (Err apiError) refreshing
                in
                ( Page.status failed, Page.visibleResponse failed, Page.selectedTransaction failed )
                    |> Expect.equal
                        ( Page.RefreshFailureStatus, Just response, Page.selectedTransaction selected )
        , test "fatal failure has no stale response" <|
            \_ ->
                let
                    ( loading, request ) =
                        Page.beginLoading Page.init

                    failed =
                        Page.receive request (Err apiError) loading
                in
                ( Page.status failed, Page.visibleResponse failed )
                    |> Expect.equal ( Page.InitialFailureStatus, Nothing )
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
        , test "paging keeps a bounded five-page window" <|
            \_ ->
                let
                    pages =
                        List.range 0 5 |> List.map samplePage

                    model =
                        case pages of
                            first :: rest ->
                                List.foldl
                                    appendResponse
                                    (loadInitial first)
                                    rest

                            [] ->
                                Page.init
                in
                Page.visibleResponse model
                    |> Maybe.map (\response -> ( List.length response.transactions, response.transactions |> List.head |> Maybe.map .description ))
                    |> (\window -> ( window, Page.retainedPageCount model ))
                    |> Expect.equal ( Just ( 250, Just "Transaction 51" ), 5 )
        , test "a revision change while paging replaces the window and clears selection" <|
            \_ ->
                let
                    first =
                        samplePage 0

                    selected =
                        loadInitial first |> selectFirst first

                    changed =
                        appendResponse (sampleResponse "revision-b") selected
                in
                ( Page.selectedTransaction changed, Page.retainedPageCount changed )
                    |> Expect.equal ( Nothing, 1 )
        , test "query errors decode their safe reason and position" <|
            \_ ->
                Decode.decodeString ApiError.decoder
                    """{"error":{"code":"invalid_query","message":"The query is invalid.","requestId":"request-1","details":{"reason":"expected a closing parenthesis","position":14}}}"""
                    |> Result.map ApiError.message
                    |> Expect.equal
                        (Ok "The query is invalid. expected a closing parenthesis (at character 14) (request request-1)")
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
                ( Page.draftQuery failed, Page.appliedQuery failed, Page.visibleResponse failed )
                    |> Expect.equal ( "acct:(", "acct:(", Nothing )
        , test "a stale request completion cannot overwrite a newer query" <|
            \_ ->
                let
                    ( firstLoading, firstRequest ) =
                        Page.beginLoading Page.init

                    ( newerQuery, _ ) =
                        firstLoading
                            |> Page.setDraft "acct:expenses"
                            |> Page.applyDraft
                in
                Page.receive firstRequest (Ok (sampleResponse "revision-old")) newerQuery
                    |> (\model -> ( Page.status model, Page.draftQuery model ))
                    |> Expect.equal ( Page.InitialLoadingStatus, "acct:expenses" )
        , test "load-more failure retains loaded results and selection for retry" <|
            \_ ->
                let
                    response =
                        samplePage 0

                    selected =
                        loadInitial response |> selectFirst response

                    loading =
                        Page.beginLoadingMore selected

                    failed =
                        loading
                            |> Maybe.map (\( model, request ) -> Page.receive request (Err apiError) model)
                            |> Maybe.withDefault selected

                    retry =
                        Page.beginLoadingMore failed
                in
                { status = Page.status failed
                , response = Page.visibleResponse failed
                , selected = Page.selectedTransaction failed
                , retryCursor = retry |> Maybe.map (Tuple.second >> Page.requestCursor)
                , retryStatus = retry |> Maybe.map (Tuple.first >> Page.status)
                }
                    |> Expect.equal
                        { status = Page.LoadMoreFailureStatus
                        , response = Just response
                        , selected = Page.selectedTransaction selected
                        , retryCursor = Just (Just "cursor-1")
                        , retryStatus = Just Page.LoadingMoreStatus
                        }
        , test "loading more is impossible without a retryable window and cursor" <|
            \_ ->
                let
                    noCursor =
                        loadInitial (sampleResponse "revision-a")

                    withCursor =
                        loadInitial (samplePage 0)

                    loadingAgain =
                        Page.beginLoadingMore withCursor
                            |> Maybe.andThen (Tuple.first >> Page.beginLoadingMore)
                in
                ( Page.beginLoadingMore Page.init, Page.beginLoadingMore noCursor, loadingAgain )
                    |> Expect.equal ( Nothing, Nothing, Nothing )
        , test "first-page and next-page requests cannot complete each other's states" <|
            \_ ->
                let
                    loaded =
                        loadInitial (samplePage 0)

                    ( refreshing, firstRequest ) =
                        Page.beginLoading loaded
                in
                case Page.beginLoadingMore loaded of
                    Just ( loadingMore, nextRequest ) ->
                        ( Page.receive nextRequest (Ok (samplePage 1)) refreshing |> Page.status
                        , Page.receive firstRequest (Ok (samplePage 1)) loadingMore |> Page.status
                        )
                            |> Expect.equal ( Page.RefreshingStatus, Page.LoadingMoreStatus )

                    Nothing ->
                        Expect.fail "The sample page must provide a next cursor."
        , test "every representable lifecycle state is reached through a valid transition" <|
            \_ ->
                let
                    ( initialLoading, initialRequest ) =
                        Page.beginLoading Page.init

                    initialFailure =
                        Page.receive initialRequest (Err apiError) initialLoading

                    showing =
                        loadInitial (samplePage 0)

                    ( refreshing, refreshRequest ) =
                        Page.beginLoading showing

                    refreshFailure =
                        Page.receive refreshRequest (Err apiError) refreshing
                in
                case Page.beginLoadingMore showing of
                    Just ( loadingMore, pageRequest ) ->
                        let
                            loadMoreFailure =
                                Page.receive pageRequest (Err apiError) loadingMore
                        in
                        [ Page.status Page.init
                        , Page.status initialLoading
                        , Page.status initialFailure
                        , Page.status showing
                        , Page.status refreshing
                        , Page.status refreshFailure
                        , Page.status loadingMore
                        , Page.status loadMoreFailure
                        ]
                            |> Expect.equal
                                [ Page.NotAskedStatus
                                , Page.InitialLoadingStatus
                                , Page.InitialFailureStatus
                                , Page.ShowingStatus
                                , Page.RefreshingStatus
                                , Page.RefreshFailureStatus
                                , Page.LoadingMoreStatus
                                , Page.LoadMoreFailureStatus
                                ]

                    Nothing ->
                        Expect.fail "The sample page must provide a next cursor."
        , test "transaction query and cursor are URL encoded" <|
            \_ ->
                Api.transactionsUrl
                    { query = "acct:food & café"
                    , cursor = Just "page+/="
                    }
                    |> Expect.equal
                        "/api/v1/transactions?query=acct%3Afood%20%26%20caf%C3%A9&limit=50&cursor=page%2B%2F%3D"
        ]


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
        [ { primaryDate = "2024-04-02"
          , secondaryDate = Nothing
          , status = "unmarked"
          , code = Nothing
          , description = "Utilities"
          , comment = Nothing
          , tags = []
          , postings =
                [ { account = "expenses:utilities"
                  , status = "unmarked"
                  , comment = Nothing
                  , tags = []
                  , amounts = []
                  }
                ]
          , presentation =
                { classification = "expense"
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


sampleTransaction : Int -> Transaction.Transaction
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
            Debug.todo "sampleResponse must contain a transaction"


metadata : String -> Metadata
metadata revision =
    { revision = Just revision, freshness = Fresh, warnings = [] }


apiError : ApiError
apiError =
    { code = "network_error", message = "Unavailable", requestId = Nothing, details = Nothing }
