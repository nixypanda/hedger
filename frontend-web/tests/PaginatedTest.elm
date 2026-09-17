module PaginatedTest exposing (suite)

import Api.Error exposing (ApiError)
import Data.Paginated as Paginated exposing (Paginated)
import Expect
import Test exposing (Test, describe, test)


{-| A minimal stand-in for a real page payload: a list of row ids, a
continuation cursor, and a revision used to exercise the reset path.
-}
type alias Page =
    { items : List Int
    , cursor : Maybe String
    , revision : String
    }


page : String -> List Int -> Maybe String -> Page
page revision items cursor =
    { items = items, cursor = cursor, revision = revision }


{-| A bounded-window config: append concatenates but keeps at most 4 items, and
a changed revision forces a reset instead of an append.
-}
config : Paginated.Config Page
config =
    { nextCursor = .cursor
    , append =
        \old new ->
            { new | items = List.take 4 (old.items ++ new.items) }
    , shouldReset = \old new -> old.revision /= new.revision
    }


type alias Model =
    Paginated String Page


suite : Test
suite =
    describe "Data.Paginated"
        [ test "starts dormant with no visible page" <|
            \_ ->
                ( Paginated.status Paginated.init, Paginated.currentPage Paginated.init )
                    |> Expect.equal ( Paginated.NotAsked, Nothing )
        , test "a fresh load blanks the window and requests no cursor" <|
            \_ ->
                let
                    ( loading, request ) =
                        Paginated.beginFresh "q1" Paginated.init
                in
                ( Paginated.status loading
                , Paginated.currentPage loading
                , ( Paginated.requestCriteria request, Paginated.requestCursor request )
                )
                    |> Expect.equal ( Paginated.LoadingFirst, Nothing, ( "q1", Nothing ) )
        , test "the active first-page response becomes the window" <|
            \_ ->
                let
                    model =
                        loadFirst "q1" (page "r1" [ 1, 2 ] (Just "c1"))
                in
                ( Paginated.status model
                , Paginated.currentPage model |> Maybe.map .items
                , Paginated.appliedCriteria model
                )
                    |> Expect.equal ( Paginated.Showing_, Just [ 1, 2 ], Just "q1" )
        , test "a stale first-page completion cannot overwrite a newer load" <|
            \_ ->
                let
                    ( firstLoading, firstRequest ) =
                        Paginated.beginFresh "q1" Paginated.init

                    ( newer, _ ) =
                        Paginated.beginFresh "q2" firstLoading
                in
                Paginated.receive config firstRequest (Ok (page "r1" [ 9 ] Nothing)) newer
                    |> (\m -> ( Paginated.status m, Paginated.appliedCriteria m ))
                    |> Expect.equal ( Paginated.LoadingFirst, Just "q2" )
        , test "a first-page failure with no window is fatal" <|
            \_ ->
                let
                    ( loading, request ) =
                        Paginated.beginFresh "q1" Paginated.init
                in
                Paginated.receive config request (Err apiError) loading
                    |> (\m -> ( Paginated.status m, Paginated.currentPage m ))
                    |> Expect.equal ( Paginated.LoadFailure, Nothing )
        , test "refresh keeps the stale window visible while loading" <|
            \_ ->
                let
                    showing =
                        loadFirst "q1" (page "r1" [ 1, 2 ] (Just "c1"))

                    ( refreshing, _ ) =
                        Paginated.beginRefresh "q1" showing
                in
                ( Paginated.status refreshing
                , Paginated.currentPage refreshing |> Maybe.map .items
                )
                    |> Expect.equal ( Paginated.Refreshing_, Just [ 1, 2 ] )
        , test "refresh success replaces the window; refresh failure retains it" <|
            \_ ->
                let
                    showing =
                        loadFirst "q1" (page "r1" [ 1, 2 ] (Just "c1"))

                    ( refreshing, request ) =
                        Paginated.beginRefresh "q1" showing

                    refreshed =
                        Paginated.receive config request (Ok (page "r1" [ 7 ] Nothing)) refreshing

                    failed =
                        Paginated.receive config request (Err apiError) refreshing
                in
                ( Paginated.currentPage refreshed |> Maybe.map .items
                , ( Paginated.status failed, Paginated.currentPage failed |> Maybe.map .items )
                )
                    |> Expect.equal ( Just [ 7 ], ( Paginated.RefreshFailure, Just [ 1, 2 ] ) )
        , test "load-more appends the next page behind the cursor" <|
            \_ ->
                let
                    showing =
                        loadFirst "q1" (page "r1" [ 1, 2 ] (Just "c1"))

                    appended =
                        appendPage (page "r1" [ 3 ] Nothing) showing
                in
                ( Paginated.currentPage appended |> Maybe.map .items
                , Paginated.status appended
                )
                    |> Expect.equal ( Just [ 1, 2, 3 ], Paginated.Showing_ )
        , test "the bounded append drops the oldest rows past the cap" <|
            \_ ->
                let
                    showing =
                        loadFirst "r1" (page "r1" [ 1, 2, 3 ] (Just "c1"))

                    appended =
                        appendPage (page "r1" [ 4, 5 ] (Just "c2")) showing
                in
                Paginated.currentPage appended
                    |> Maybe.map .items
                    -- old [1,2,3] ++ new [4,5] = [1..5], capped to 4 → oldest dropped
                    |> Expect.equal (Just [ 1, 2, 3, 4 ])
        , test "a revision change resets the window instead of appending" <|
            \_ ->
                let
                    showing =
                        loadFirst "q1" (page "r1" [ 1, 2 ] (Just "c1"))

                    reset =
                        appendPage (page "r2" [ 99 ] Nothing) showing
                in
                Paginated.currentPage reset
                    |> Maybe.map .items
                    |> Expect.equal (Just [ 99 ])
        , test "a load-more failure retains the window and cursor for retry" <|
            \_ ->
                let
                    showing =
                        loadFirst "q1" (page "r1" [ 1, 2 ] (Just "c1"))

                    failed =
                        case Paginated.beginLoadMore config showing of
                            Just ( loadingMore, request ) ->
                                Paginated.receive config request (Err apiError) loadingMore

                            Nothing ->
                                showing

                    retryCursor =
                        Paginated.beginLoadMore config failed
                            |> Maybe.map (Tuple.second >> Paginated.requestCursor)
                in
                { status = Paginated.status failed
                , window = Paginated.currentPage failed |> Maybe.map .items
                , hasError = Paginated.loadMoreError failed /= Nothing
                , retryCursor = retryCursor
                }
                    |> Expect.equal
                        { status = Paginated.LoadMoreFailure
                        , window = Just [ 1, 2 ]
                        , hasError = True
                        , retryCursor = Just (Just "c1")
                        }
        , test "load-more is impossible without a retryable window and cursor" <|
            \_ ->
                let
                    noCursor =
                        loadFirst "q1" (page "r1" [ 1 ] Nothing)
                in
                ( Paginated.beginLoadMore config Paginated.init
                , Paginated.beginLoadMore config noCursor
                , Paginated.canLoadMore config noCursor
                )
                    |> Expect.equal ( Nothing, Nothing, False )
        , test "first-page and next-page responses cannot complete each other's states" <|
            \_ ->
                let
                    showing =
                        loadFirst "q1" (page "r1" [ 1, 2 ] (Just "c1"))

                    ( refreshing, firstRequest ) =
                        Paginated.beginRefresh "q1" showing
                in
                case Paginated.beginLoadMore config showing of
                    Just ( loadingMore, nextRequest ) ->
                        ( Paginated.receive
                            config
                            nextRequest
                            (Ok (page "r1" [ 3 ] Nothing))
                            refreshing
                            |> Paginated.status
                        , Paginated.receive
                            config
                            firstRequest
                            (Ok (page "r1" [ 3 ] Nothing))
                            loadingMore
                            |> Paginated.status
                        )
                            |> Expect.equal ( Paginated.Refreshing_, Paginated.LoadingMore_ )

                    Nothing ->
                        Expect.fail "The window must expose a next cursor."
        ]


loadFirst : String -> Page -> Model
loadFirst criteria first =
    let
        ( loading, request ) =
            Paginated.beginFresh criteria Paginated.init
    in
    Paginated.receive config request (Ok first) loading


appendPage : Page -> Model -> Model
appendPage next model =
    case Paginated.beginLoadMore config model of
        Just ( loadingMore, request ) ->
            Paginated.receive config request (Ok next) loadingMore

        Nothing ->
            model


apiError : ApiError
apiError =
    { code = "network_error", message = "Unavailable", requestId = Nothing, details = Nothing }
