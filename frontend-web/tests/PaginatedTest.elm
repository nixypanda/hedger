module PaginatedTest exposing (suite)

import Api.Error exposing (ApiError)
import Data.Paginated as Paginated exposing (Paginated)
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)


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
    Paginated ApiError String Page


{-| The generic paging machine is shared by every windowed list. Its core domain
rule is that only the newest in-flight request may move the machine: a
completion for an older request — or for the other page kind — is stale and must
leave the window and status untouched. The other laws here are that the window
survives refreshes until they succeed, and that a next page is merged or reset
according to policy.
-}
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
        , fuzz (Fuzz.list opFuzzer) "only the newest request may change the machine" paginationLaws
        ]



-- SEQUENTIAL LAW


type Op
    = StartFresh String
    | StartRefresh String
    | StartMore
    | Complete Int Outcome


type Outcome
    = Loaded Page
    | Failed


type alias Trace =
    { machine : Model
    , issued : List (Paginated.Request String)
    }


type alias MachineSnapshot =
    { status : Paginated.Status
    , page : Maybe Page
    , criteria : Maybe String
    , error : Maybe ApiError
    , canLoadMore : Bool
    }


opFuzzer : Fuzz.Fuzzer Op
opFuzzer =
    Fuzz.oneOf
        [ Fuzz.map StartFresh criteriaFuzzer
        , Fuzz.map StartRefresh criteriaFuzzer
        , Fuzz.constant StartMore
        , Fuzz.map2 Complete (Fuzz.intRange 0 30) outcomeFuzzer
        ]


criteriaFuzzer : Fuzz.Fuzzer String
criteriaFuzzer =
    Fuzz.oneOfValues [ "q1", "q2", "q3" ]


outcomeFuzzer : Fuzz.Fuzzer Outcome
outcomeFuzzer =
    Fuzz.oneOf
        [ Fuzz.map Loaded pageFuzzer
        , Fuzz.constant Failed
        ]


pageFuzzer : Fuzz.Fuzzer Page
pageFuzzer =
    Fuzz.map3 Page
        (Fuzz.list (Fuzz.intRange 0 5))
        (Fuzz.maybe Fuzz.string)
        (Fuzz.oneOfValues [ "r1", "r2" ])


paginationLaws : List Op -> Expect.Expectation
paginationLaws ops =
    Expect.all (List.map (\expectation _ -> expectation) (run ops initialTrace)) ()


initialTrace : Trace
initialTrace =
    { machine = Paginated.init, issued = [] }


run : List Op -> Trace -> List Expect.Expectation
run ops trace =
    case ops of
        [] ->
            invariants trace

        op :: rest ->
            let
                ( next, stepExpectations ) =
                    step op trace
            in
            stepExpectations ++ invariants next ++ run rest next


step : Op -> Trace -> ( Trace, List Expect.Expectation )
step op trace =
    case op of
        StartFresh criteria ->
            let
                ( machine, request ) =
                    Paginated.beginFresh criteria trace.machine
            in
            ( { machine = machine, issued = trace.issued ++ [ request ] }
            , [ Expect.equal Paginated.LoadingFirst (Paginated.status machine)
              , Expect.equal Nothing (Paginated.currentPage machine)
              ]
            )

        StartRefresh criteria ->
            let
                ( machine, request ) =
                    Paginated.beginRefresh criteria trace.machine

                before =
                    Paginated.currentPage trace.machine
            in
            ( { machine = machine, issued = trace.issued ++ [ request ] }
            , [ Expect.equal before (Paginated.currentPage machine)
              , Expect.equal
                    (if before == Nothing then
                        Paginated.LoadingFirst

                     else
                        Paginated.Refreshing_
                    )
                    (Paginated.status machine)
              ]
            )

        StartMore ->
            case Paginated.beginLoadMore config trace.machine of
                Just ( machine, request ) ->
                    ( { machine = machine, issued = trace.issued ++ [ request ] }
                    , [ Expect.equal Paginated.LoadingMore_ (Paginated.status machine)
                      , Expect.equal (currentPageOf trace.machine) (currentPageOf machine)
                      ]
                    )

                Nothing ->
                    ( trace, [] )

        Complete index outcome ->
            case requestAt index trace.issued of
                Nothing ->
                    ( trace, [] )

                Just request ->
                    let
                        before =
                            snapshot trace.machine

                        machine =
                            Paginated.receive config request (resultOf outcome) trace.machine

                        after =
                            { trace | machine = machine }

                        lastIndex =
                            List.length trace.issued - 1
                    in
                    if index < lastIndex then
                        ( after, [ snapshot machine |> Expect.equal before ] )

                    else
                        ( after, latestCompletion before outcome machine )


invariants : Trace -> List Expect.Expectation
invariants trace =
    let
        machine =
            trace.machine

        status =
            Paginated.status machine

        hasNoPage =
            Paginated.currentPage machine == Nothing

        pageLessStatus =
            List.member status
                [ Paginated.NotAsked
                , Paginated.LoadingFirst
                , Paginated.LoadFailure
                ]
    in
    [ Expect.equal (Paginated.appliedCriteria machine == Nothing) (status == Paginated.NotAsked)
    , Expect.equal hasNoPage pageLessStatus
    , Expect.equal (canLoadMore machine) (canBeginLoadMore machine)
    ]


latestCompletion : MachineSnapshot -> Outcome -> Model -> List Expect.Expectation
latestCompletion before outcome machine =
    let
        status =
            Paginated.status machine

        currentPage =
            Paginated.currentPage machine
    in
    case outcome of
        Failed ->
            case before.status of
                Paginated.LoadingFirst ->
                    [ Expect.equal Paginated.LoadFailure status
                    , Expect.equal Nothing currentPage
                    ]

                Paginated.Refreshing_ ->
                    [ Expect.equal Paginated.RefreshFailure status
                    , Expect.equal before.page currentPage
                    ]

                Paginated.LoadingMore_ ->
                    [ Expect.equal Paginated.LoadMoreFailure status
                    , Expect.equal before.page currentPage
                    ]

                _ ->
                    [ snapshot machine |> Expect.equal before ]

        Loaded loadedPage ->
            case before.status of
                Paginated.LoadingFirst ->
                    [ Expect.equal Paginated.Showing_ status
                    , Expect.equal (Just loadedPage) currentPage
                    ]

                Paginated.Refreshing_ ->
                    [ Expect.equal Paginated.Showing_ status
                    , Expect.equal (Just loadedPage) currentPage
                    ]

                Paginated.LoadingMore_ ->
                    case before.page of
                        Just window ->
                            [ Expect.equal Paginated.Showing_ status
                            , Expect.equal
                                (Just
                                    (if config.shouldReset window loadedPage then
                                        loadedPage

                                     else
                                        config.append window loadedPage
                                    )
                                )
                                currentPage
                            ]

                        Nothing ->
                            [ Expect.fail "A load-more state must retain its window." ]

                _ ->
                    [ snapshot machine |> Expect.equal before ]


snapshot : Model -> MachineSnapshot
snapshot machine =
    { status = Paginated.status machine
    , page = Paginated.currentPage machine
    , criteria = Paginated.appliedCriteria machine
    , error = Paginated.error machine
    , canLoadMore = Paginated.canLoadMore config machine
    }


currentPageOf : Model -> Maybe Page
currentPageOf =
    Paginated.currentPage


canLoadMore : Model -> Bool
canLoadMore =
    Paginated.canLoadMore config


canBeginLoadMore : Model -> Bool
canBeginLoadMore machine =
    Paginated.beginLoadMore config machine /= Nothing


resultOf : Outcome -> Result ApiError Page
resultOf outcome =
    case outcome of
        Loaded loadedPage ->
            Ok loadedPage

        Failed ->
            Err apiError


requestAt : Int -> List (Paginated.Request String) -> Maybe (Paginated.Request String)
requestAt index requests =
    requests |> List.drop index |> List.head


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
