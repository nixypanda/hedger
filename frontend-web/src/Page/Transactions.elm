module Page.Transactions exposing
    ( Model
    , Msg(..)
    , OutMsg(..)
    , Request
    , Status(..)
    , appliedQuery
    , applyDraft
    , beginLoading
    , beginLoadingMore
    , clearSelection
    , draftQuery
    , fetch
    , init
    , receive
    , requestCursor
    , requestQuery
    , retainedPageCount
    , select
    , selectedTransaction
    , setDraft
    , status
    , transactionDetail
    , update
    , view
    , visibleResponse
    )

import Api.Client as Api
import Api.Error as ApiError exposing (ApiError)
import Api.Metadata exposing (Freshness(..), Metadata)
import Api.Transaction exposing (Amount, Posting, Response, Tag, Transaction)
import Data.Date as Date
import Data.Decimal as Decimal
import Data.Status as Status
import Element exposing (Device, Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Lib.Time as TimeExtra
import RemoteData exposing (RemoteData(..))
import State.Viewport exposing (Viewport)
import Ui
import Ui.Icon as Icon
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import View.AccountPresentation as AccountPresentation



-- MODEL


type Model
    = Model ModelData


type alias ModelData =
    { state : State
    , draft : String
    , nextToken : Int
    }


type State
    = Dormant
    | InitialLoading AppliedQuery Token
    | InitialFailure AppliedQuery ApiError
    | Showing Window
    | RefreshingWindow Window Token
    | RefreshFailedWindow Window ApiError
    | LoadingMoreWindow Window NextAttempt
    | LoadMoreFailedWindow Window NextAttempt ApiError


type AppliedQuery
    = AppliedQuery String


type Token
    = Token Int


type NextAttempt
    = NextAttempt Token String


type Window
    = Window WindowData


type alias WindowData =
    { applied : AppliedQuery
    , metadata : Metadata
    , rows : Rows
    , nextCursor : Maybe String
    , retainedPages : RetainedPages
    }


type Rows
    = Unselected (List Transaction)
    | Selected (List Transaction) Transaction (List Transaction)


type RetainedPages
    = OnePage
    | TwoPages
    | ThreePages
    | FourPages
    | FivePages


type Request
    = FirstPageRequest FirstRequest
    | NextPageRequest NextRequest


type FirstRequest
    = FirstRequest RequestBase


type NextRequest
    = NextRequest RequestBase String


type alias RequestBase =
    { token : Token
    , query : String
    }


type Status
    = NotAskedStatus
    | InitialLoadingStatus
    | InitialFailureStatus
    | ShowingStatus
    | RefreshingStatus
    | RefreshFailureStatus
    | LoadingMoreStatus
    | LoadMoreFailureStatus


init : Model
init =
    Model
        { state = Dormant
        , draft = ""
        , nextToken = 0
        }



-- UPDATE


type Msg
    = Refresh
    | DraftChanged String
    | ApplyDraft
    | LoadMore
    | Select Transaction
    | Back
    | Loaded Request (Result ApiError Response)


type OutMsg
    = NoOutMsg


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg (Model data) =
    case msg of
        Refresh ->
            let
                ( nextModel, request ) =
                    beginLoading (Model data)
            in
            case nextModel of
                Model nextData ->
                    ( Model nextData, fetchTransactions request, NoOutMsg )

        DraftChanged draft ->
            ( Model { data | draft = draft }, Cmd.none, NoOutMsg )

        ApplyDraft ->
            let
                ( nextModel, request ) =
                    applyDraft (Model data)
            in
            case nextModel of
                Model nextData ->
                    ( Model nextData, fetchTransactions request, NoOutMsg )

        LoadMore ->
            case beginLoadingMore (Model data) of
                Just ( nextModel, request ) ->
                    case nextModel of
                        Model nextData ->
                            ( Model nextData, fetchTransactions request, NoOutMsg )

                Nothing ->
                    ( Model data, Cmd.none, NoOutMsg )

        Select transaction ->
            ( Model { data | state = mapWindow (selectInWindow transaction) data.state }
            , Cmd.none
            , NoOutMsg
            )

        Back ->
            ( Model { data | state = mapWindow clearWindowSelection data.state }
            , Cmd.none
            , NoOutMsg
            )

        Loaded request result ->
            ( receive request result (Model data), Cmd.none, NoOutMsg )


fetchTransactions : Request -> Cmd Msg
fetchTransactions request =
    Api.getTransactions
        { query = requestQuery request
        , cursor = requestCursor request
        }
        (Loaded request)


fetch : Request -> Cmd Msg
fetch =
    fetchTransactions


pageSize : Int
pageSize =
    50


beginLoading : Model -> ( Model, Request )
beginLoading (Model data) =
    let
        applied =
            stateAppliedQuery data.state

        ( nextData, token, request ) =
            issueFirstRequest applied data

        nextState =
            case visibleWindow data.state of
                Just window ->
                    RefreshingWindow window token

                Nothing ->
                    InitialLoading applied token
    in
    ( Model { nextData | state = nextState }, FirstPageRequest request )


beginLoadingMore : Model -> Maybe ( Model, Request )
beginLoadingMore (Model data) =
    loadMoreCandidate data.state
        |> Maybe.map
            (\( window, cursor ) ->
                let
                    ( nextData, attempt, request ) =
                        issueNextRequest (windowAppliedQuery window) cursor data
                in
                ( Model { nextData | state = LoadingMoreWindow window attempt }
                , NextPageRequest request
                )
            )


setDraft : String -> Model -> Model
setDraft draft (Model data) =
    Model { data | draft = draft }


applyDraft : Model -> ( Model, Request )
applyDraft (Model data) =
    let
        query =
            String.trim data.draft

        applied =
            AppliedQuery query

        appliedData =
            { data | draft = query }

        ( nextData, token, request ) =
            issueFirstRequest applied appliedData
    in
    ( Model { nextData | state = InitialLoading applied token }, FirstPageRequest request )


receive : Request -> Result ApiError Response -> Model -> Model
receive request result (Model data) =
    case data.state of
        InitialLoading applied active ->
            case request of
                FirstPageRequest received ->
                    if matchesFirstRequest received applied active then
                        case result of
                            Ok response ->
                                Model
                                    { data
                                        | state =
                                            Showing
                                                (windowFromResponse applied response OnePage Nothing)
                                    }

                            Err error ->
                                Model { data | state = InitialFailure applied error }

                    else
                        Model data

                NextPageRequest _ ->
                    Model data

        RefreshingWindow window active ->
            case request of
                FirstPageRequest received ->
                    if matchesFirstRequest received (windowAppliedQuery window) active then
                        case result of
                            Ok response ->
                                Model
                                    { data
                                        | state =
                                            Showing
                                                (windowFromResponse
                                                    (windowAppliedQuery window)
                                                    response
                                                    OnePage
                                                    (windowSelection window)
                                                )
                                    }

                            Err error ->
                                Model { data | state = RefreshFailedWindow window error }

                    else
                        Model data

                NextPageRequest _ ->
                    Model data

        LoadingMoreWindow window active ->
            case request of
                NextPageRequest received ->
                    if matchesNextRequest received (windowAppliedQuery window) active then
                        case result of
                            Ok page ->
                                let
                                    nextWindow =
                                        if revisionChanged window page then
                                            windowFromResponse (windowAppliedQuery window)
                                                page
                                                OnePage
                                                Nothing

                                        else
                                            appendPage window page
                                in
                                Model
                                    { data
                                        | state = Showing nextWindow
                                    }

                            Err error ->
                                Model { data | state = LoadMoreFailedWindow window active error }

                    else
                        Model data

                FirstPageRequest _ ->
                    Model data

        _ ->
            Model data


select : Transaction -> Model -> Model
select transaction (Model data) =
    Model { data | state = mapWindow (selectInWindow transaction) data.state }


clearSelection : Model -> Model
clearSelection (Model data) =
    Model { data | state = mapWindow clearWindowSelection data.state }


revisionChanged : Window -> Response -> Bool
revisionChanged window current =
    (windowResponse window).metadata.revision /= current.metadata.revision


issueFirstRequest : AppliedQuery -> ModelData -> ( ModelData, Token, FirstRequest )
issueFirstRequest applied data =
    let
        nextToken =
            data.nextToken + 1

        token =
            Token nextToken
    in
    ( { data | nextToken = nextToken }
    , token
    , FirstRequest { token = token, query = appliedQueryText applied }
    )


issueNextRequest : AppliedQuery -> String -> ModelData -> ( ModelData, NextAttempt, NextRequest )
issueNextRequest applied cursor data =
    let
        nextToken =
            data.nextToken + 1

        token =
            Token nextToken

        attempt =
            NextAttempt token cursor
    in
    ( { data | nextToken = nextToken }
    , attempt
    , NextRequest { token = token, query = appliedQueryText applied } cursor
    )


matchesFirstRequest : FirstRequest -> AppliedQuery -> Token -> Bool
matchesFirstRequest (FirstRequest request) applied token =
    request.token == token && request.query == appliedQueryText applied


matchesNextRequest : NextRequest -> AppliedQuery -> NextAttempt -> Bool
matchesNextRequest (NextRequest request cursor) applied (NextAttempt token activeCursor) =
    request.token
        == token
        && request.query
        == appliedQueryText applied
        && cursor
        == activeCursor


requestQuery : Request -> String
requestQuery request =
    case request of
        FirstPageRequest (FirstRequest first) ->
            first.query

        NextPageRequest (NextRequest next _) ->
            next.query


requestCursor : Request -> Maybe String
requestCursor request =
    case request of
        FirstPageRequest _ ->
            Nothing

        NextPageRequest (NextRequest _ cursor) ->
            Just cursor


visibleWindow : State -> Maybe Window
visibleWindow state =
    case state of
        Showing window ->
            Just window

        RefreshingWindow window _ ->
            Just window

        RefreshFailedWindow window _ ->
            Just window

        LoadingMoreWindow window _ ->
            Just window

        LoadMoreFailedWindow window _ _ ->
            Just window

        _ ->
            Nothing


loadMoreCandidate : State -> Maybe ( Window, String )
loadMoreCandidate state =
    case state of
        Showing window ->
            windowResponse window |> .nextCursor |> Maybe.map (Tuple.pair window)

        RefreshFailedWindow window _ ->
            windowResponse window |> .nextCursor |> Maybe.map (Tuple.pair window)

        LoadMoreFailedWindow window (NextAttempt _ cursor) _ ->
            Just ( window, cursor )

        _ ->
            Nothing


windowResponse : Window -> Response
windowResponse (Window window) =
    { metadata = window.metadata
    , transactions = rowsToList window.rows
    , nextCursor = window.nextCursor
    }


windowFromResponse : AppliedQuery -> Response -> RetainedPages -> Maybe Transaction -> Window
windowFromResponse applied response pages selected =
    Window
        { applied = applied
        , metadata = response.metadata
        , rows = rowsWithSelection selected response.transactions
        , nextCursor = response.nextCursor
        , retainedPages = pages
        }


appendPage : Window -> Response -> Window
appendPage (Window current) page =
    let
        combined =
            rowsToList current.rows ++ page.transactions

        retainedTransactions =
            case current.retainedPages of
                FivePages ->
                    List.drop (min pageSize (List.length (rowsToList current.rows))) combined

                _ ->
                    combined
    in
    Window
        { applied = current.applied
        , metadata = page.metadata
        , rows = rowsWithSelection (selectedRow current.rows) retainedTransactions
        , nextCursor = page.nextCursor
        , retainedPages = advancePages current.retainedPages
        }


advancePages : RetainedPages -> RetainedPages
advancePages pages =
    case pages of
        OnePage ->
            TwoPages

        TwoPages ->
            ThreePages

        ThreePages ->
            FourPages

        FourPages ->
            FivePages

        FivePages ->
            FivePages


rowsToList : Rows -> List Transaction
rowsToList rows =
    case rows of
        Unselected transactions ->
            transactions

        Selected before selected after ->
            before ++ (selected :: after)


selectedRow : Rows -> Maybe Transaction
selectedRow rows =
    case rows of
        Unselected _ ->
            Nothing

        Selected _ selected _ ->
            Just selected


rowsWithSelection : Maybe Transaction -> List Transaction -> Rows
rowsWithSelection selected transactions =
    selected
        |> Maybe.andThen (\transaction -> selectRows transaction transactions)
        |> Maybe.withDefault (Unselected transactions)


selectRows : Transaction -> List Transaction -> Maybe Rows
selectRows target transactions =
    selectRowsHelp target [] transactions


selectRowsHelp : Transaction -> List Transaction -> List Transaction -> Maybe Rows
selectRowsHelp target reversedBefore remaining =
    case remaining of
        [] ->
            Nothing

        transaction :: after ->
            if transaction == target then
                Just (Selected (List.reverse reversedBefore) transaction after)

            else
                selectRowsHelp target (transaction :: reversedBefore) after


selectInWindow : Transaction -> Window -> Window
selectInWindow transaction (Window window) =
    case selectRows transaction (rowsToList window.rows) of
        Just rows ->
            Window { window | rows = rows }

        Nothing ->
            Window window


clearWindowSelection : Window -> Window
clearWindowSelection (Window window) =
    Window { window | rows = Unselected (rowsToList window.rows) }


windowSelection : Window -> Maybe Transaction
windowSelection (Window window) =
    selectedRow window.rows


windowAppliedQuery : Window -> AppliedQuery
windowAppliedQuery (Window window) =
    window.applied


mapWindow : (Window -> Window) -> State -> State
mapWindow transform state =
    case state of
        Showing window ->
            Showing (transform window)

        RefreshingWindow window request ->
            RefreshingWindow (transform window) request

        RefreshFailedWindow window error ->
            RefreshFailedWindow (transform window) error

        LoadingMoreWindow window request ->
            LoadingMoreWindow (transform window) request

        LoadMoreFailedWindow window request error ->
            LoadMoreFailedWindow (transform window) request error

        _ ->
            state


remoteData : Model -> RemoteData Response
remoteData (Model data) =
    case data.state of
        Dormant ->
            NotAsked

        InitialLoading _ _ ->
            Loading Nothing

        InitialFailure _ error ->
            Failure error Nothing

        Showing window ->
            Success (windowResponse window)

        RefreshingWindow window _ ->
            Loading (Just (windowResponse window))

        RefreshFailedWindow window error ->
            Failure error (Just (windowResponse window))

        LoadingMoreWindow window _ ->
            Success (windowResponse window)

        LoadMoreFailedWindow window _ _ ->
            Success (windowResponse window)


draftQuery : Model -> String
draftQuery (Model data) =
    data.draft


appliedQuery : Model -> String
appliedQuery (Model data) =
    appliedQueryText (stateAppliedQuery data.state)


stateAppliedQuery : State -> AppliedQuery
stateAppliedQuery state =
    case state of
        Dormant ->
            AppliedQuery ""

        InitialLoading applied _ ->
            applied

        InitialFailure applied _ ->
            applied

        Showing window ->
            windowAppliedQuery window

        RefreshingWindow window _ ->
            windowAppliedQuery window

        RefreshFailedWindow window _ ->
            windowAppliedQuery window

        LoadingMoreWindow window _ ->
            windowAppliedQuery window

        LoadMoreFailedWindow window _ _ ->
            windowAppliedQuery window


appliedQueryText : AppliedQuery -> String
appliedQueryText (AppliedQuery query) =
    query


selectedTransaction : Model -> Maybe Transaction
selectedTransaction (Model data) =
    visibleWindow data.state |> Maybe.andThen windowSelection


visibleResponse : Model -> Maybe Response
visibleResponse model =
    RemoteData.value (remoteData model)


retainedPageCount : Model -> Int
retainedPageCount (Model data) =
    visibleWindow data.state
        |> Maybe.map (\(Window window) -> pagesToInt window.retainedPages)
        |> Maybe.withDefault 0


pagesToInt : RetainedPages -> Int
pagesToInt pages =
    case pages of
        OnePage ->
            1

        TwoPages ->
            2

        ThreePages ->
            3

        FourPages ->
            4

        FivePages ->
            5


status : Model -> Status
status (Model data) =
    case data.state of
        Dormant ->
            NotAskedStatus

        InitialLoading _ _ ->
            InitialLoadingStatus

        InitialFailure _ _ ->
            InitialFailureStatus

        Showing _ ->
            ShowingStatus

        RefreshingWindow _ _ ->
            RefreshingStatus

        RefreshFailedWindow _ _ ->
            RefreshFailureStatus

        LoadingMoreWindow _ _ ->
            LoadingMoreStatus

        LoadMoreFailedWindow _ _ _ ->
            LoadMoreFailureStatus


loadMoreError : Model -> Maybe ApiError
loadMoreError (Model data) =
    case data.state of
        LoadMoreFailedWindow _ _ error ->
            Just error

        _ ->
            Nothing


canBeginLoadingMore : Model -> Bool
canBeginLoadingMore (Model data) =
    loadMoreCandidate data.state /= Nothing



-- VIEW


view :
    Theme
    -> Viewport
    -> Model
    -> Element Msg
view theme viewport model =
    let
        device =
            Element.classifyDevice viewport

        response =
            visibleResponse model

        currentSelection =
            selectedTransaction model

        wideSelected =
            viewport.width >= 1000 && currentSelection /= Nothing
    in
    if wideSelected then
        case ( response, currentSelection ) of
            ( Just current, Just selected ) ->
                wideLayout theme device model current selected

            _ ->
                listPage theme device model

    else if currentSelection /= Nothing && Ui.isSmall device then
        case currentSelection of
            Just selected ->
                detailPage theme device (Just Back) selected

            Nothing ->
                listPage theme device model

    else
        listPage theme device model


listPage :
    Theme
    -> Device
    -> Model
    -> Element Msg
listPage theme device model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Element.scrollbarY
        , Element.htmlAttribute (Html.Attributes.class "page-scroll transactions-page")
        ]
        [ transactionsHeader theme device
        , Ui.pageContainer device
            [ queryPanel theme model
            , remoteContent theme device model
            ]
        ]


wideLayout :
    Theme
    -> Device
    -> Model
    -> Response
    -> Transaction
    -> Element Msg
wideLayout theme device model response selected =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.clip
        , Element.htmlAttribute (Html.Attributes.class "transactions-wide-layout")
        ]
        [ Element.column
            [ Element.width (Element.fillPortion 2)
            , Element.height Element.fill
            , Element.scrollbarY
            , Background.color theme.pageBg
            , Element.htmlAttribute (Html.Attributes.class "transactions-master-pane transactions-page")
            ]
            [ transactionsHeader theme device
            , Ui.pageContainer device
                [ queryPanel theme model
                , remoteNotice theme (remoteData model) response
                , transactionCount theme response.transactions
                , transactionsList theme
                    device
                    (selectedTransaction model)
                    response.transactions
                , pagingControls theme model response
                ]
            ]
        , Element.el
            [ Element.width (Element.fillPortion 3)
            , Element.height Element.fill
            , Element.scrollbarY
            , Element.htmlAttribute (Html.Attributes.class "transactions-detail-pane")
            ]
            (detailPage theme device (Just Back) selected)
        ]


transactionsHeader : Theme -> Device -> Element Msg
transactionsHeader theme device =
    Ui.pageHeaderBand theme device <|
        Ui.pageHeader theme
            { leading = Nothing
            , title = Typography.pageTitle theme "Transactions"
            , subtitle = Just "Browse your ledger activity"
            , actions = [ Ui.ghostButton theme { label = "Refresh", onPress = Refresh } ]
            }


queryPanel : Theme -> Model -> Element Msg
queryPanel theme model =
    Ui.card theme
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing 8
            ]
            [ Ui.searchInput theme
                { label = "hledger query"
                , text = draftQuery model
                , onChange = DraftChanged
                , extraAttrs =
                    [ Element.htmlAttribute (Html.Attributes.id "transaction-query")
                    , Element.htmlAttribute (Html.Attributes.attribute "autocomplete" "off")
                    , Element.htmlAttribute
                        (Html.Attributes.attribute "aria-describedby"
                            "transaction-query-help transaction-query-error"
                        )
                    ]
                }
            , Ui.primaryButton theme { label = "Apply", onPress = ApplyDraft }
            ]
        , case queryError (remoteData model) of
            Just error ->
                Element.el
                    [ Element.width Element.fill
                    , Element.htmlAttribute (Html.Attributes.id "transaction-query-error")
                    , Element.htmlAttribute (Html.Attributes.attribute "role" "alert")
                    ]
                    (Ui.errorBanner theme (ApiError.message error))

            Nothing ->
                Element.none
        , Element.el [ Element.htmlAttribute (Html.Attributes.id "transaction-query-help") ]
            (Typography.mutedParagraph theme
                ("Type an hledger query below. Terms separated by spaces combine with AND. "
                    ++ "Prefix terms with acct:, desc:, date:, amt:, cur:, payee:, note:, status:, "
                    ++ "real:, tag:, or depth: to filter by field. Use not:, any:, all:, and expr: "
                    ++ "for boolean logic."
                )
            )
        , Element.wrappedRow [ Element.width Element.fill, Element.spacing 8 ]
            (Typography.mutedCaption theme "Examples:"
                :: List.map
                    (\example ->
                        Ui.chip theme
                            { label = example
                            , selected = draftQuery model == example
                            , onClick = Just (DraftChanged example)
                            }
                    )
                    [ "acct:expenses"
                    , "desc:grocery"
                    , "date:2024-01-01..2024-01-31"
                    , "cur:EUR"
                    , "amt:'>50'"
                    , "status:*"
                    , "not:status:cleared"
                    , "expr:'acct:food AND amt:>20'"
                    ]
            )
        ]


queryError : RemoteData Response -> Maybe ApiError
queryError remote =
    case remote of
        Failure error _ ->
            if List.member error.code [ "invalid_query", "unsupported_query_option", "query_too_long" ] then
                Just error

            else
                Nothing

        _ ->
            Nothing


remoteContent :
    Theme
    -> Device
    -> Model
    -> Element Msg
remoteContent theme device model =
    case remoteData model of
        NotAsked ->
            Ui.card theme [ Ui.loading theme "Loading transactions…" ]

        Loading Nothing ->
            Ui.card theme [ Ui.skeletonLoading theme ]

        Loading (Just response) ->
            transactionContent theme device model response

        Success response ->
            transactionContent theme device model response

        Failure error stale ->
            case stale of
                Just response ->
                    transactionContent theme device model response

                Nothing ->
                    if queryError (remoteData model) /= Nothing then
                        Ui.card theme
                            [ Typography.mutedParagraph theme
                                "Correct the query above, then apply it again."
                            ]

                    else
                        Ui.card theme
                            [ Ui.errorBanner theme
                                ("Transactions failed to load: " ++ ApiError.message error)
                            ]


transactionContent :
    Theme
    -> Device
    -> Model
    -> Response
    -> Element Msg
transactionContent theme device model response =
    Element.column [ Element.width Element.fill, Element.spacing 12 ]
        [ remoteNotice theme (remoteData model) response
        , transactionCount theme response.transactions
        , if List.isEmpty response.transactions then
            noMatches theme model

          else
            transactionsList theme
                device
                (selectedTransaction model)
                response.transactions
        , pagingControls theme model response
        ]


noMatches : Theme -> Model -> Element Msg
noMatches theme model =
    Ui.card theme
        [ Typography.mutedParagraph theme
            (if String.isEmpty (appliedQuery model) then
                "No transactions are available."

             else
                "No transactions match the applied query. "
                    ++ "Edit it above and apply again to see all transactions."
            )
        ]


pagingControls : Theme -> Model -> Response -> Element Msg
pagingControls theme model response =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 8
        , Element.htmlAttribute (Html.Attributes.id "transaction-list-end")
        ]
        [ case loadMoreError model of
            Just error ->
                Ui.errorBanner theme ("Could not load more transactions: " ++ ApiError.message error)

            Nothing ->
                Element.none
        , if status model == LoadingMoreStatus then
            Ui.loading theme "Loading more transactions…"

          else
            case response.nextCursor of
                Just _ ->
                    if canBeginLoadingMore model then
                        Ui.secondaryButton theme { label = "Load more", onPress = LoadMore }

                    else
                        Typography.mutedCaption theme "Finish refreshing before loading more."

                Nothing ->
                    Typography.mutedCaption theme "All matching transactions are loaded."
        , if retainedPageCount model == 5 && response.nextCursor /= Nothing then
            Typography.mutedCaption theme
                ("Keeping a bounded window of "
                    ++ "5"
                    ++ " pages; loading more advances the visible window."
                )

          else
            Element.none
        ]


remoteNotice : Theme -> RemoteData Response -> Response -> Element msg
remoteNotice theme remote response =
    case remote of
        Loading _ ->
            Ui.statusBadge theme Ui.StatusPrimary "Refreshing transactions"

        Failure error _ ->
            Ui.errorBanner theme
                ("Refresh failed; showing stale transactions: " ++ ApiError.message error)

        _ ->
            case response.metadata.freshness of
                Fresh ->
                    Element.none

                Refreshing ->
                    Ui.statusBadge theme Ui.StatusPrimary "Refreshing transactions"

                Stale ->
                    Ui.statusBadge theme Ui.StatusWarning "Showing a stale journal snapshot"

                Partial ->
                    Ui.statusBadge theme Ui.StatusWarning "Transaction data is partial"

                Unavailable ->
                    Ui.errorBanner theme "Transaction data is unavailable."


transactionCount : Theme -> List Transaction -> Element msg
transactionCount theme transactions =
    Typography.mutedBody theme (String.fromInt (List.length transactions) ++ " transactions")


transactionsList :
    Theme
    -> Device
    -> Maybe Transaction
    -> List Transaction
    -> Element Msg
transactionsList theme device selected transactions =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 12
        , Element.htmlAttribute (Html.Attributes.id "transaction-list")
        ]
        (List.map
            (monthSection theme device selected)
            (TimeExtra.groupByKey (Date.toString << .primaryDate) transactions)
        )


monthSection :
    Theme
    -> Device
    -> Maybe Transaction
    -> ( String, List Transaction )
    -> Element Msg
monthSection theme device selected ( month, transactions ) =
    Element.column [ Element.width Element.fill, Element.spacing 4 ]
        (Element.el [ Element.paddingEach { top = 8, right = 0, bottom = 4, left = 4 } ]
            (Typography.subsectionTitle theme (TimeExtra.monthLabel month))
            :: List.map (transactionRow theme device selected) transactions
        )


transactionRow :
    Theme
    -> Device
    -> Maybe Transaction
    -> Transaction
    -> Element Msg
transactionRow theme device selected transaction =
    let
        rowContent =
            Element.column
                (Ui.rowSurfaceAttrs theme (selected == Just transaction) ++ [ Element.spacing 6 ])
                [ if Ui.isSmall device then
                    phonePrimaryRow theme transaction

                  else
                    desktopPrimaryRow theme transaction
                , Element.column
                    [ Element.width Element.fill
                    , Element.spacing 3
                    , Element.paddingEach { top = 0, right = 0, bottom = 0, left = 12 }
                    ]
                    (List.map (postingPreviewRow theme) (List.take 5 transaction.postings))
                , let
                    omitted =
                        max 0 (List.length transaction.postings - 5)
                  in
                  if omitted > 0 then
                    Element.el [ Element.paddingEach { top = 0, right = 0, bottom = 0, left = 12 } ]
                        (Typography.mutedCaption theme ("+" ++ String.fromInt omitted ++ " more"))

                  else
                    Element.none
                ]
    in
    Input.button [ Element.width Element.fill, Element.padding 0 ]
        { onPress = Just (Select transaction), label = rowContent }


desktopPrimaryRow : Theme -> Transaction -> Element msg
desktopPrimaryRow theme transaction =
    Element.row [ Element.width Element.fill, Element.spacing 8 ]
        [ Element.el [ Typography.rowDate, Font.color theme.textMuted, Element.width (Element.px 90) ]
            (Element.text (Date.toString transaction.primaryDate))
        , Element.el
            [ Typography.rowTitle
            , Font.semiBold
            , Element.width Element.fill
            , Font.color theme.textPrimary
            , Typography.minWidthZero
            , Typography.uiClass "ui-text-ellipsis"
            ]
            (Element.text transaction.description)
        , trailingSummary theme transaction
        ]


phonePrimaryRow : Theme -> Transaction -> Element msg
phonePrimaryRow theme transaction =
    Element.column [ Element.width Element.fill, Element.spacing 3 ]
        [ Element.el
            [ Typography.rowDate, Font.color theme.textMuted ]
            (Element.text (Date.toString transaction.primaryDate))
        , Element.row [ Element.width Element.fill, Element.spacing 8 ]
            [ Element.paragraph
                [ Typography.rowTitle
                , Font.semiBold
                , Element.width Element.fill
                , Font.color theme.textPrimary
                , Typography.minWidthZero
                , Typography.uiClass "ui-text-wrap"
                ]
                [ Element.text transaction.description ]
            , trailingSummary theme transaction
            ]
        ]


trailingSummary : Theme -> Transaction -> Element msg
trailingSummary theme transaction =
    case transaction.presentation.classification of
        "unclassified" ->
            Element.none

        _ ->
            Element.column [ Element.alignRight, Element.spacing 2 ]
                [ Element.el [ Element.alignRight ]
                    (Ui.financialBadge theme
                        (classificationTone transaction.presentation.classification)
                        (classificationLabel transaction)
                    )
                ]


classificationTone : String -> Ui.FinancialTone
classificationTone classification =
    case classification of
        "income" ->
            Ui.FinancialIncome

        "expense" ->
            Ui.FinancialExpense

        "opening_balance" ->
            Ui.FinancialEquity

        "borrowing" ->
            Ui.FinancialBorrowing

        "repayment" ->
            Ui.FinancialRepayment

        "asset_acquisition" ->
            Ui.FinancialAsset

        "asset_disposal" ->
            Ui.FinancialAsset

        "asset_reallocation" ->
            Ui.FinancialAsset

        _ ->
            Ui.FinancialNeutral


classificationLabel : Transaction -> String
classificationLabel transaction =
    case transaction.presentation.classification of
        "income" ->
            "Income"

        "expense" ->
            "Expense"

        "opening_balance" ->
            "Opening balance"

        "borrowing" ->
            "Borrowing"

        "repayment" ->
            "Repayment"

        "asset_acquisition" ->
            "Asset acquisition"

        "asset_disposal" ->
            "Asset disposal"

        "asset_reallocation" ->
            "Asset reallocation"

        "unclassified" ->
            "Unclassified"

        _ ->
            "Transfer"


postingPreviewRow : Theme -> Posting -> Element msg
postingPreviewRow theme posting =
    Element.row [ Element.width Element.fill, Element.spacing 4 ]
        [ Icon.view
            { icon = AccountPresentation.iconForAccountName posting.account
            , size = 14
            , color = theme.textMuted
            }
        , Element.paragraph
            [ Typography.rowMetadata
            , Font.color theme.textMuted
            , Element.width Element.fill
            , Typography.minWidthZero
            , Typography.uiClass "ui-text-wrap"
            ]
            [ Element.text posting.account ]
        , Element.el
            [ Typography.rowMetadata
            , Font.color theme.textMuted
            , Element.alignRight
            , Element.width (Element.shrink |> Element.maximum 180)
            , Typography.minWidthZero
            , Typography.uiClass "ui-text-ellipsis"
            , Typography.financeNumber
            ]
            (Element.text (amountsText posting.amounts))
        ]


detailPage : Theme -> Device -> Maybe Msg -> Transaction -> Element Msg
detailPage theme device onBack transaction =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Element.scrollbarY
        , Element.htmlAttribute (Html.Attributes.class "page-scroll transaction-detail-page")
        , Element.htmlAttribute (Html.Attributes.id "transaction-detail")
        ]
        [ Ui.pageHeaderBand theme device <|
            Element.row [ Element.width Element.fill, Element.spacing 8 ]
                [ case onBack of
                    Just message ->
                        Ui.ghostButton theme { label = "← Back", onPress = message }

                    Nothing ->
                        Element.none
                , Typography.sectionTitle theme "Transaction Detail"
                ]
        , Ui.pageContainer device [ transactionDetail theme transaction ]
        ]


transactionDetail : Theme -> Transaction -> Element msg
transactionDetail theme transaction =
    Element.column [ Element.width Element.fill, Element.spacing 16 ]
        [ Ui.card theme
            (List.concat
                [ [ detailRow theme "Date" (Date.toString transaction.primaryDate) ]
                , maybeDetailRow theme "Secondary date" (Maybe.map Date.toString transaction.secondaryDate)
                , [ detailRow theme "Status" (Status.toString transaction.status) ]
                , maybeDetailRow theme "Code" transaction.code
                , [ detailRow theme "Description" transaction.description ]
                , maybeDetailRow theme "Comment" transaction.comment
                , if List.isEmpty transaction.tags then
                    []

                  else
                    [ detailRow theme "Tags" (tagsText transaction.tags) ]
                ]
            )
        , Ui.section theme
            { header = Just (Typography.sectionTitle theme "Postings")
            , body =
                if List.isEmpty transaction.postings then
                    [ Typography.mutedParagraph theme "No postings" ]

                else
                    List.intersperse (Ui.hairline theme)
                        (List.map (postingDetailRow theme) transaction.postings)
            , footer = Nothing
            }
        ]


detailRow : Theme -> String -> String -> Element msg
detailRow theme label value =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 16
        , Element.alignTop
        , Typography.minWidthZero
        ]
        [ Element.el
            [ Element.width (Element.px 112)
            , Typography.detailLabel
            , Font.semiBold
            , Font.color theme.textMedium
            , Element.alignTop
            ]
            (Element.text (label ++ ":"))
        , Element.paragraph
            [ Element.width Element.fill
            , Typography.detailValue
            , Font.alignRight
            , Font.color theme.textPrimary
            , Typography.minWidthZero
            , Typography.uiClass "ui-text-wrap"
            ]
            [ Element.text value ]
        ]


maybeDetailRow : Theme -> String -> Maybe String -> List (Element msg)
maybeDetailRow theme label value =
    value |> Maybe.map (\current -> [ detailRow theme label current ]) |> Maybe.withDefault []


postingDetailRow : Theme -> Posting -> Element msg
postingDetailRow theme posting =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 12
        , Element.paddingXY 0 10
        , Element.alignTop
        , Typography.minWidthZero
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing 8
            , Element.alignTop
            , Typography.minWidthZero
            ]
            [ Icon.view
                { icon = AccountPresentation.iconForAccountName posting.account
                , size = 16
                , color = theme.textMedium
                }
            , Element.column [ Element.width Element.fill, Element.spacing 4 ]
                ([ Typography.naturalWrapText
                    [ Typography.detailValue, Font.color theme.textMedium ]
                    ":"
                    posting.account
                 , Typography.mutedCaption theme (Status.toString posting.status)
                 ]
                    ++ maybeCaption theme posting.comment
                    ++ (if List.isEmpty posting.tags then
                            []

                        else
                            [ Typography.mutedCaption theme (tagsText posting.tags) ]
                       )
                )
            ]
        , Element.column
            [ Element.width Element.shrink
            , Element.spacing 4
            , Element.alignRight
            , Element.alignTop
            , Font.alignRight
            ]
            (List.map (postingAmountLine theme) posting.amounts)
        ]


postingAmountLine : Theme -> Amount -> Element msg
postingAmountLine theme amount =
    Element.column [ Element.alignRight, Element.spacing 2 ]
        [ Element.paragraph
            [ Element.width (Element.fill |> Element.maximum 180)
            , Element.alignRight
            , Typography.rowAmount
            , Typography.financeNumber
            , Font.alignRight
            , Font.semiBold
            , Font.color theme.textPrimary
            , Typography.minWidthZero
            , Typography.uiClass "ui-text-wrap"
            ]
            [ Element.text (amountText amount) ]
        , case amount.cost of
            Just cost ->
                Typography.mutedCaption theme
                    ((if cost.costType == "total" then
                        "Total cost "

                      else
                        "Unit cost "
                     )
                        ++ Decimal.toString cost.quantity
                        ++ " "
                        ++ cost.commodity
                    )

            Nothing ->
                Element.none
        ]


maybeCaption : Theme -> Maybe String -> List (Element msg)
maybeCaption theme value =
    value |> Maybe.map (\current -> [ Typography.mutedCaption theme current ]) |> Maybe.withDefault []


tagsText : List Tag -> String
tagsText =
    List.map (\tag -> tag.name ++ ": " ++ tag.value) >> String.join ", "


amountText : Amount -> String
amountText amount =
    Decimal.toString amount.quantity ++ " " ++ amount.commodity


amountsText : List Amount -> String
amountsText =
    List.map amountText >> String.join ", "
