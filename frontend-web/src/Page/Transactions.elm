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
    , update
    , view
    , visibleResponse
    )

import Api.Client as Api
import Api.Error as ApiError exposing (ApiError)
import Api.Transaction exposing (Response)
import Data.Date as Date
import Data.Freshness exposing (Freshness(..))
import Data.Paginated as Paginated exposing (Paginated)
import Data.Revision as Revision
import Data.Transaction exposing (Posting, Transaction, TransactionEffect(..))
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Lib.RemoteData exposing (RemoteData(..))
import Lib.Time as TimeExtra
import Ui.Button as Button
import Ui.Chip as Chip
import Ui.Common as Common
import Ui.DesignTokens as Tokens
import Ui.Env exposing (Env)
import Ui.Feedback as Feedback
import Ui.Icon as Icon
import Ui.Notice as Notice
import Ui.Page as Page
import Ui.Skeleton as Skeleton
import Ui.Surface as Surface
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import Ui.Viewport as Viewport
import Ui.Workspace as Workspace
import View.AccountPresentation as AccountPresentation
import View.QueryPanel as QueryPanel
import View.States as States
import View.Transaction as TransactionView



-- MODEL


type Model
    = Model ModelData


type alias ModelData =
    { paginated : Paginated ApiError AppliedQuery PageData
    , draft : DraftQuery
    , selection : Maybe Transaction
    }


{-| The accumulated window plus a count of how many pages it spans, so the
bounded-window logic can drop the oldest page once the cap is reached.
-}
type alias PageData =
    { response : Response
    , retainedPages : Int
    }


type AppliedQuery
    = AppliedQuery String


type DraftQuery
    = DraftQuery String


type alias Request =
    Paginated.Request AppliedQuery


type Status
    = NotAskedStatus
    | InitialLoadingStatus
    | InitialFailureStatus
    | ShowingStatus
    | RefreshingStatus
    | RefreshFailureStatus
    | LoadingMoreStatus
    | LoadMoreFailureStatus


pageSize : Int
pageSize =
    50


maxPages : Int
maxPages =
    5


{-| Transactions keep a bounded, self-healing window: appends drop the oldest
page past the cap, and a changed journal revision resets the window to the newly
loaded page rather than stitching incompatible snapshots together.
-}
config : Paginated.Config PageData
config =
    { nextCursor = .response >> .nextCursor
    , append = appendPageData
    , shouldReset =
        \old new ->
            case
                Revision.ensureMatching
                    old.response.metadata.revision
                    new.response.metadata.revision
            of
                Ok () ->
                    False

                Err _ ->
                    True
    }


appendPageData : PageData -> PageData -> PageData
appendPageData current next =
    let
        combined =
            current.response.transactions ++ next.response.transactions

        retained =
            if current.retainedPages >= maxPages then
                List.drop
                    (min pageSize (List.length current.response.transactions))
                    combined

            else
                combined

        response =
            next.response
    in
    { response = { response | transactions = retained }
    , retainedPages = min maxPages (current.retainedPages + 1)
    }


wrap : Response -> PageData
wrap response =
    { response = response, retainedPages = 1 }


init : Model
init =
    Model
        { paginated = Paginated.init
        , draft = DraftQuery ""
        , selection = Nothing
        }



-- UPDATE


type Msg
    = Refresh
    | DraftChanged String
    | ApplyDraft
    | ClearDraft
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
            ( nextModel, fetchTransactions request, NoOutMsg )

        DraftChanged draft ->
            ( Model { data | draft = DraftQuery draft }, Cmd.none, NoOutMsg )

        ApplyDraft ->
            let
                ( nextModel, request ) =
                    applyDraft (Model data)
            in
            ( nextModel, fetchTransactions request, NoOutMsg )

        ClearDraft ->
            let
                ( nextModel, request ) =
                    applyDraft (setDraft "" (Model data))
            in
            ( nextModel, fetchTransactions request, NoOutMsg )

        LoadMore ->
            case beginLoadingMore (Model data) of
                Just ( nextModel, request ) ->
                    ( nextModel, fetchTransactions request, NoOutMsg )

                Nothing ->
                    ( Model data, Cmd.none, NoOutMsg )

        Select transaction ->
            ( select transaction (Model data), Cmd.none, NoOutMsg )

        Back ->
            ( clearSelection (Model data), Cmd.none, NoOutMsg )

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


{-| Refresh the current window in place, re-fetching the applied query the window
owns — not whatever draft the user may be editing.
-}
beginLoading : Model -> ( Model, Request )
beginLoading (Model data) =
    let
        applied =
            Paginated.appliedCriteria data.paginated
                |> Maybe.withDefault (AppliedQuery "")

        ( nextPaginated, request ) =
            Paginated.beginRefresh applied data.paginated
    in
    ( Model { data | paginated = nextPaginated }, request )


beginLoadingMore : Model -> Maybe ( Model, Request )
beginLoadingMore (Model data) =
    Paginated.beginLoadMore config data.paginated
        |> Maybe.map
            (\( nextPaginated, request ) ->
                ( Model { data | paginated = nextPaginated }, request )
            )


setDraft : String -> Model -> Model
setDraft draft (Model data) =
    Model { data | draft = DraftQuery draft }


{-| Apply the (trimmed) draft as a new query, blanking the current window.
-}
applyDraft : Model -> ( Model, Request )
applyDraft (Model data) =
    let
        query =
            String.trim (draftQueryText data.draft)

        ( nextPaginated, request ) =
            Paginated.beginFresh (AppliedQuery query) data.paginated
    in
    ( Model { data | draft = DraftQuery query, paginated = nextPaginated }, request )


receive : Request -> Result ApiError Response -> Model -> Model
receive request result (Model data) =
    Model
        { data
            | paginated =
                Paginated.receive config request (Result.map wrap result) data.paginated
        }


{-| Selection is validated against the visible window: only rows present in the
current window can be selected, and an outsider leaves the prior selection
untouched.
-}
select : Transaction -> Model -> Model
select transaction (Model data) =
    if isVisibleRow transaction (Model data) then
        Model { data | selection = Just transaction }

    else
        Model data


clearSelection : Model -> Model
clearSelection (Model data) =
    Model { data | selection = Nothing }


{-| The selection re-derived on read: a stored selection only counts while its
row remains in the visible window, so refreshes and revision resets that drop
the row clear it structurally.
-}
selectedTransaction : Model -> Maybe Transaction
selectedTransaction (Model data) =
    data.selection
        |> Maybe.andThen
            (\transaction ->
                if isVisibleRow transaction (Model data) then
                    Just transaction

                else
                    Nothing
            )


isVisibleRow : Transaction -> Model -> Bool
isVisibleRow transaction model =
    visibleResponse model
        |> Maybe.map (\response -> List.member transaction response.transactions)
        |> Maybe.withDefault False


requestQuery : Request -> String
requestQuery request =
    appliedQueryText (Paginated.requestCriteria request)


requestCursor : Request -> Maybe String
requestCursor =
    Paginated.requestCursor


visibleResponse : Model -> Maybe Response
visibleResponse (Model data) =
    Paginated.currentPage data.paginated |> Maybe.map .response


retainedPageCount : Model -> Int
retainedPageCount (Model data) =
    Paginated.currentPage data.paginated
        |> Maybe.map .retainedPages
        |> Maybe.withDefault 0


draftQuery : Model -> String
draftQuery (Model data) =
    draftQueryText data.draft


appliedQuery : Model -> String
appliedQuery (Model data) =
    Paginated.appliedCriteria data.paginated
        |> Maybe.map appliedQueryText
        |> Maybe.withDefault ""


appliedQueryText : AppliedQuery -> String
appliedQueryText (AppliedQuery query) =
    query


draftQueryText : DraftQuery -> String
draftQueryText (DraftQuery query) =
    query


status : Model -> Status
status (Model data) =
    case Paginated.status data.paginated of
        Paginated.NotAsked ->
            NotAskedStatus

        Paginated.LoadingFirst ->
            InitialLoadingStatus

        Paginated.LoadFailure ->
            InitialFailureStatus

        Paginated.Showing_ ->
            ShowingStatus

        Paginated.Refreshing_ ->
            RefreshingStatus

        Paginated.RefreshFailure ->
            RefreshFailureStatus

        Paginated.LoadingMore_ ->
            LoadingMoreStatus

        Paginated.LoadMoreFailure ->
            LoadMoreFailureStatus


{-| Project the machine onto `RemoteData` for the view: refreshes surface the
stale window as loading, and failures over a window keep it as stale data.
-}
remoteData : Model -> RemoteData ApiError Response
remoteData (Model data) =
    let
        visible =
            visibleResponse (Model data)
    in
    case Paginated.status data.paginated of
        Paginated.NotAsked ->
            NotAsked

        Paginated.LoadingFirst ->
            Loading Nothing

        Paginated.LoadFailure ->
            Failure (activeError data.paginated) Nothing

        Paginated.Showing_ ->
            successOrLoading visible

        Paginated.Refreshing_ ->
            Loading visible

        Paginated.RefreshFailure ->
            Failure (activeError data.paginated) visible

        Paginated.LoadingMore_ ->
            successOrLoading visible

        Paginated.LoadMoreFailure ->
            successOrLoading visible


successOrLoading : Maybe Response -> RemoteData ApiError Response
successOrLoading visible =
    case visible of
        Just response ->
            Success response

        Nothing ->
            Loading Nothing


activeError : Paginated ApiError AppliedQuery PageData -> ApiError
activeError paginated =
    Paginated.error paginated
        |> Maybe.withDefault
            { code = "unknown_error"
            , message = "Unknown error"
            , requestId = Nothing
            , details = Nothing
            }


loadMoreError : Model -> Maybe ApiError
loadMoreError (Model data) =
    Paginated.loadMoreError data.paginated


canBeginLoadingMore : Model -> Bool
canBeginLoadingMore (Model data) =
    Paginated.canLoadMore config data.paginated



-- VIEW


view :
    Env
    -> Model
    -> Element Msg
view env model =
    let
        theme =
            env.theme

        viewport =
            env.viewport

        response =
            visibleResponse model

        currentSelection =
            selectedTransaction model

        compact =
            Viewport.isCompact (Viewport.contentLayout viewport)

        wideSelected =
            Viewport.usesMasterDetail viewport && currentSelection /= Nothing
    in
    if wideSelected then
        case ( response, currentSelection ) of
            ( Just _, Just selected ) ->
                Workspace.view env
                    [ { key = "transactions"
                      , width = Workspace.Portion Tokens.layoutPortionDouble
                      , content = listPage theme True viewport.height model
                      }
                    , { key = "transaction-detail"
                      , width = Workspace.Portion Tokens.layoutPortionTriple
                      , content = detailPage theme (Just Back) selected
                      }
                    ]

            _ ->
                listPage theme compact viewport.height model

    else if currentSelection /= Nothing then
        case currentSelection of
            Just selected ->
                detailPage theme (Just Back) selected

            Nothing ->
                listPage theme compact viewport.height model

    else
        listPage theme compact viewport.height model


listPage :
    Theme
    -> Bool
    -> Int
    -> Model
    -> Element Msg
listPage theme compact viewportHeight model =
    Page.scaffold theme
        { header = transactionsHeaderConfig theme model
        , body =
            [ queryPanel theme compact model
            , remoteContent theme compact viewportHeight model
            ]
                ++ failureNotices theme model
                ++ queryErrorNotices theme model
        }


transactionsHeaderConfig : Theme -> Model -> Page.Header Msg
transactionsHeaderConfig theme model =
    { leading = Nothing
    , title = Typography.pageTitle theme "Transactions"
    , subtitle = Just "Browse your ledger activity"
    , actions =
        [ States.refreshStatus theme
            { freshness = responseFreshness model
            , loading = responseLoading model
            }
        , Button.refreshButton theme
            { loading = responseLoading model
            , onPress = Refresh
            }
        ]
    }


responseLoading : Model -> Bool
responseLoading model =
    case status model of
        InitialLoadingStatus ->
            True

        RefreshingStatus ->
            True

        _ ->
            False


responseFreshness : Model -> Freshness
responseFreshness model =
    case ( status model, visibleResponse model ) of
        ( _, Nothing ) ->
            Unavailable

        ( RefreshFailureStatus, Just _ ) ->
            Stale

        ( InitialFailureStatus, _ ) ->
            Unavailable

        ( _, Just response ) ->
            response.metadata.freshness


failureNotices : Theme -> Model -> List (Element Msg)
failureNotices theme model =
    case status model of
        RefreshFailureStatus ->
            [ failureNotice theme
                "transaction-refresh-error"
                ("Refresh failed; showing stale transactions: " ++ activeErrorMessage model)
            ]

        LoadMoreFailureStatus ->
            [ failureNotice theme
                "transaction-load-more-error"
                ("Could not load more transactions: " ++ loadMoreErrorMessage model)
            ]

        _ ->
            []


failureNotice : Theme -> String -> String -> Element Msg
failureNotice theme id message =
    Notice.view theme
        { placement = Notice.Bottom
        , tone = Notice.Danger
        , icon = Icon.AlertTriangle
        , message = message
        , actions = []
        , id = Just id
        }


queryErrorNotices : Theme -> Model -> List (Element Msg)
queryErrorNotices theme model =
    case queryError (remoteData model) of
        Just _ ->
            [ Notice.view theme
                { placement = Notice.Bottom
                , tone = Notice.Danger
                , icon = Icon.AlertTriangle
                , message = "Invalid query. Correct it above."
                , actions = []
                , id = Nothing
                }
            ]

        Nothing ->
            []


activeErrorMessage : Model -> String
activeErrorMessage (Model data) =
    activeError data.paginated |> ApiError.message


loadMoreErrorMessage : Model -> String
loadMoreErrorMessage model =
    loadMoreError model
        |> Maybe.map ApiError.message
        |> Maybe.withDefault "Unknown error"


queryPanel : Theme -> Bool -> Model -> Element Msg
queryPanel theme compact model =
    QueryPanel.view
        { theme = theme
        , compact = compact
        , draftQuery = draftQuery model
        , onDraftChanged = DraftChanged
        , onApply = ApplyDraft
        , onClear = ClearDraft
        , queryError = Nothing
        , helpText =
            "Type an hledger query below. Terms separated by spaces combine with AND. "
                ++ "Prefix terms with acct:, desc:, date:, amt:, cur:, payee:, note:, status:, "
                ++ "real:, tag:, or depth: to filter by field. Use not:, any:, all:, and expr: "
                ++ "for boolean logic."
        , examples =
            [ "acct:expenses"
            , "desc:grocery"
            , "date:2024-01-01..2024-01-31"
            , "cur:EUR"
            , "amt:'>50'"
            , "status:*"
            , "not:status:cleared"
            , "expr:'acct:food AND amt:>20'"
            ]
        , baseId = "transaction-query"
        }


queryError : RemoteData ApiError Response -> Maybe ApiError
queryError remote =
    case remote of
        Failure error _ ->
            if ApiError.isQueryError error then
                Just error

            else
                Nothing

        _ ->
            Nothing


remoteContent :
    Theme
    -> Bool
    -> Int
    -> Model
    -> Element Msg
remoteContent theme compact viewportHeight model =
    case remoteData model of
        NotAsked ->
            transactionsSkeleton theme compact viewportHeight

        Loading Nothing ->
            transactionsSkeleton theme compact viewportHeight

        Loading (Just response) ->
            transactionContent theme compact viewportHeight model response

        Success response ->
            transactionContent theme compact viewportHeight model response

        Failure error stale ->
            case stale of
                Just response ->
                    transactionContent theme compact viewportHeight model response

                Nothing ->
                    if queryError (remoteData model) /= Nothing then
                        Surface.card theme
                            [ Element.el
                                [ Element.width Element.fill
                                , Element.htmlAttribute
                                    (Html.Attributes.id "transaction-query-error")
                                ]
                                (Feedback.errorBanner theme (ApiError.message error))
                            , Typography.mutedParagraph theme
                                "Correct the query above, then apply it again."
                            ]

                    else
                        Surface.card theme
                            [ Feedback.errorBanner theme
                                ("Transactions failed to load: " ++ ApiError.message error)
                            ]


transactionContent :
    Theme
    -> Bool
    -> Int
    -> Model
    -> Response
    -> Element Msg
transactionContent theme compact viewportHeight model response =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        [ transactionCount theme response.transactions
        , if List.isEmpty response.transactions then
            listRegion viewportHeight (noMatches theme compact model)

          else
            transactionsList theme
                compact
                (selectedTransaction model)
                response.transactions
        , pagingControls theme model response
        ]


noMatches : Theme -> Bool -> Model -> Element Msg
noMatches theme compact model =
    Surface.cardWith
        [ Element.height
            (Element.minimum (emptyListMinHeight compact) Element.shrink)
        ]
        theme
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
        , Element.spacing Tokens.space8
        , Element.htmlAttribute (Html.Attributes.id "transaction-list-end")
        ]
        [ Element.el
            [ Element.width Element.fill
            , Element.height (Element.px Tokens.minimumTouchTarget)
            ]
            (Element.el [ Element.centerY ] (pagingControl theme model response))
        , Element.el
            [ Element.width Element.fill
            , Element.height (Element.px (Tokens.bodyTextHeight + Tokens.space2))
            ]
            (boundedWindowNote theme model response)
        ]


pagingControl : Theme -> Model -> Response -> Element Msg
pagingControl theme model response =
    if status model == LoadingMoreStatus then
        Feedback.loading theme "Loading more transactions…"

    else
        case response.nextCursor of
            Just _ ->
                if canBeginLoadingMore model then
                    Button.secondaryButton theme { label = "Load more", onPress = LoadMore }

                else
                    Typography.mutedCaption theme "Finish refreshing before loading more."

            Nothing ->
                Typography.mutedCaption theme "All matching transactions are loaded."


boundedWindowNote : Theme -> Model -> Response -> Element msg
boundedWindowNote theme model response =
    if retainedPageCount model == 5 && response.nextCursor /= Nothing then
        Typography.mutedCaption theme
            ("Keeping a bounded window of "
                ++ "5"
                ++ " pages; loading more advances the visible window."
            )

    else
        Element.none


transactionCount : Theme -> List Transaction -> Element msg
transactionCount theme transactions =
    Typography.mutedBody theme (String.fromInt (List.length transactions) ++ " transactions")


transactionsList :
    Theme
    -> Bool
    -> Maybe Transaction
    -> List Transaction
    -> Element Msg
transactionsList theme compact selected transactions =
    listContainer
        (List.map
            (monthSection theme compact selected)
            (TimeExtra.groupByKey (Date.toString << .primaryDate) transactions)
        )


listContainer : List (Element msg) -> Element msg
listContainer sections =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Tokens.space12
        , Element.htmlAttribute (Html.Attributes.id "transaction-list")
        ]
        sections


monthSection :
    Theme
    -> Bool
    -> Maybe Transaction
    -> ( String, List Transaction )
    -> Element Msg
monthSection theme compact selected ( month, transactions ) =
    monthColumn
        (monthHeading (Typography.subsectionTitle theme (TimeExtra.monthLabel month))
            :: List.map (transactionRow theme compact selected) transactions
        )


monthColumn : List (Element msg) -> Element msg
monthColumn children =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space4 ] children


monthHeading : Element msg -> Element msg
monthHeading content =
    Element.el
        [ Element.paddingEach
            { top = Tokens.space8
            , right = Tokens.space0
            , bottom = Tokens.space4
            , left = Tokens.space4
            }
        ]
        content


transactionRow :
    Theme
    -> Bool
    -> Maybe Transaction
    -> Transaction
    -> Element Msg
transactionRow theme compact selected transaction =
    let
        omitted =
            max 0 (List.length transaction.postings - previewPostingLimit)

        rowContent =
            Element.column
                (Surface.rowSurfaceAttrs theme (selected == Just transaction)
                    ++ [ Element.spacing Tokens.space8
                       , Element.height
                            (Element.minimum
                                (transactionRowHeight compact)
                                Element.shrink
                            )
                       ]
                )
                [ if compact then
                    phonePrimaryRow theme transaction

                  else
                    desktopPrimaryRow theme transaction
                , Element.column
                    [ Element.width Element.fill
                    , Element.spacing Tokens.space4
                    , Element.paddingEach
                        { top = Tokens.space0
                        , right = Tokens.space0
                        , bottom = Tokens.space0
                        , left = Tokens.space12
                        }
                    ]
                    (List.map
                        (postingPreviewRow theme)
                        (List.take previewPostingLimit transaction.postings)
                        ++ [ omittedPostings theme omitted ]
                    )
                ]
    in
    Input.button [ Element.width Element.fill, Element.padding Tokens.space0 ]
        { onPress = Just (Select transaction), label = rowContent }


omittedPostings : Theme -> Int -> Element msg
omittedPostings theme omitted =
    if omitted > 0 then
        Typography.mutedCaption theme ("+" ++ String.fromInt omitted ++ " more")

    else
        Element.el [ Element.height (Element.px Tokens.space12) ] Element.none


rowTitleStyle : List (Element.Attribute msg)
rowTitleStyle =
    Typography.body ++ [ Typography.fontWeight Typography.Bold ]


desktopPrimaryRow : Theme -> Transaction -> Element msg
desktopPrimaryRow theme transaction =
    Element.row [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        [ Element.el
            (Typography.caption
                ++ [ Font.color theme.textMuted
                   , Element.width (Element.px Tokens.registerDateWidth)
                   ]
            )
            (Element.text (Date.toString transaction.primaryDate))
        , Element.el
            (rowTitleStyle
                ++ [ Element.width Element.fill
                   , Font.color theme.textPrimary
                   , Common.minWidthZero
                   , Common.uiClass "ui-text-ellipsis"
                   ]
            )
            (Element.text transaction.description)
        , trailingSummary theme transaction
        ]


phonePrimaryRow : Theme -> Transaction -> Element msg
phonePrimaryRow theme transaction =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space4 ]
        [ Element.el
            (Typography.caption ++ [ Font.color theme.textMuted ])
            (Element.text (Date.toString transaction.primaryDate))
        , Element.row [ Element.width Element.fill, Element.spacing Tokens.space8 ]
            [ Element.el
                (rowTitleStyle
                    ++ [ Element.width Element.fill
                       , Font.color theme.textPrimary
                       , Common.minWidthZero
                       , Common.uiClass "ui-text-ellipsis"
                       ]
                )
                (Element.text transaction.description)
            , trailingSummary theme transaction
            ]
        ]


trailingSummary : Theme -> Transaction -> Element msg
trailingSummary theme transaction =
    let
        effects =
            (if List.isEmpty transaction.presentation.effects then
                [ transaction.presentation.classification ]

             else
                transaction.presentation.effects
            )
                |> List.filter ((/=) Unclassified)

        visibleEffects =
            List.take previewEffectLimit effects

        omittedEffects =
            max 0 (List.length effects - previewEffectLimit)

        badges =
            List.map
                (\effect ->
                    Chip.financialBadge theme
                        (TransactionView.classificationTone effect)
                        (TransactionView.effectLabel effect)
                )
                visibleEffects
    in
    Element.row
        [ Element.alignRight
        , Element.spacing Tokens.space2
        , Element.width (Element.shrink |> Element.maximum Tokens.narrowContentWidth)
        , Common.minWidthZero
        , Element.clip
        ]
        (badges
            ++ (if omittedEffects > 0 then
                    [ Chip.financialBadge theme
                        Chip.FinancialNeutral
                        ("+" ++ String.fromInt omittedEffects)
                    ]

                else
                    []
               )
        )


postingPreviewRow : Theme -> Posting -> Element msg
postingPreviewRow theme posting =
    Element.row [ Element.width Element.fill, Element.spacing Tokens.space4 ]
        [ Icon.view
            { icon = AccountPresentation.iconForAccount posting.accountType posting.account
            , size = Tokens.iconSizeTinyGlyph
            , color = theme.textMuted
            }
        , Element.el
            (Typography.caption
                ++ [ Font.color theme.textMuted
                   , Element.width Element.fill
                   , Common.minWidthZero
                   , Common.uiClass "ui-text-ellipsis"
                   ]
            )
            (Element.text posting.account)
        , Element.el
            (Typography.caption
                ++ Typography.numeric
                ++ [ Font.color theme.textMuted
                   , Element.alignRight
                   , Element.width (Element.shrink |> Element.maximum Tokens.narrowContentWidth)
                   , Common.minWidthZero
                   , Common.uiClass "ui-text-ellipsis"
                   ]
            )
            (Element.text (TransactionView.amountsText posting.amounts))
        ]


detailPage : Theme -> Maybe Msg -> Transaction -> Element Msg
detailPage theme onBack transaction =
    Element.el
        [ Element.width Element.fill
        , Element.htmlAttribute (Html.Attributes.id "transaction-detail")
        ]
        (Page.scaffold theme
            { header =
                { leading =
                    Maybe.map
                        (\message -> Button.backButton theme { onPress = message })
                        onBack
                , title = Typography.pageTitle theme "Transaction Detail"
                , subtitle = Nothing
                , actions = []
                }
            , body = [ TransactionView.transactionDetail theme transaction ]
            }
        )


previewPostingLimit : Int
previewPostingLimit =
    2


previewEffectLimit : Int
previewEffectLimit =
    1


emptyListRowCount : Int
emptyListRowCount =
    2


transactionRowHeight : Bool -> Int
transactionRowHeight compact =
    2
        * Tokens.space12
        + 2
        * Tokens.borderHairline
        + (if compact then
            2
                * Tokens.space12
                + 2
                * Tokens.space4

           else
            Tokens.space12 + Tokens.space4
          )
        + Tokens.space8
        + (2
            * Tokens.iconSizeTinyGlyph
            + 2
            * Tokens.space4
            + Tokens.space12
          )


monthHeadingHeight : Int
monthHeadingHeight =
    Tokens.subsectionTitleHeight + Tokens.space8 + Tokens.space4


listRegionTopOffset : Int
listRegionTopOffset =
    300


listRegionMaxHeight : Int
listRegionMaxHeight =
    760


listRegionMinHeight : Int -> Int
listRegionMinHeight viewportHeight =
    clamp 360 listRegionMaxHeight (viewportHeight - listRegionTopOffset)


{-| Reserve the first-screen list region for empty results. Skeleton rows are
sized from this region and normal loaded pages are taller, so the footer stays
below the fold without making the empty card itself tall.
-}
listRegion : Int -> Element msg -> Element msg
listRegion viewportHeight content =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px (listRegionMinHeight viewportHeight))
        ]
        content


emptyListMinHeight : Bool -> Int
emptyListMinHeight compact =
    monthHeadingHeight
        + emptyListRowCount
        * transactionRowHeight compact
        + emptyListRowCount
        * Tokens.space4


skeletonRowCount : Bool -> Int -> Int
skeletonRowCount compact viewportHeight =
    let
        rowStride =
            transactionRowHeight compact + Tokens.space4

        rowsNeeded =
            (listRegionMinHeight viewportHeight - monthHeadingHeight)
                // rowStride
    in
    clamp 1 8 rowsNeeded


transactionsSkeleton : Theme -> Bool -> Int -> Element Msg
transactionsSkeleton theme compact viewportHeight =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        [ Skeleton.block theme Tokens.skeletonLineShort Tokens.bodyTextHeight
        , listRegion viewportHeight
            (listContainer
                [ monthColumn
                    (monthHeading
                        (Skeleton.block
                            theme
                            Tokens.skeletonLineShort
                            Tokens.subsectionTitleHeight
                        )
                        :: List.repeat
                            (skeletonRowCount compact viewportHeight)
                            (transactionRowSkeleton theme compact)
                    )
                ]
            )
        , pagingSkeleton theme
        ]


transactionRowSkeleton : Theme -> Bool -> Element msg
transactionRowSkeleton theme compact =
    Element.column
        (Surface.rowSurfaceAttrs theme False
            ++ [ Element.spacing Tokens.space8
               , Element.height
                    (Element.minimum
                        (transactionRowHeight compact)
                        Element.shrink
                    )
               ]
        )
        [ if compact then
            compactPrimarySkeleton theme

          else
            desktopPrimarySkeleton theme
        , Element.column
            [ Element.width Element.fill
            , Element.spacing Tokens.space4
            , Element.paddingEach
                { top = Tokens.space0
                , right = Tokens.space0
                , bottom = Tokens.space0
                , left = Tokens.space12
                }
            ]
            (List.repeat 2 (postingSkeleton theme)
                ++ [ omittedSkeleton theme ]
            )
        ]


desktopPrimarySkeleton : Theme -> Element msg
desktopPrimarySkeleton theme =
    Element.row [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        [ Skeleton.block theme Tokens.registerDateWidth Tokens.space12
        , Element.el [ Element.width Element.fill ]
            (Skeleton.fillBlock theme Tokens.bodyTextHeight)
        , Skeleton.block
            theme
            Tokens.skeletonLineShort
            (Tokens.space12 + Tokens.space4)
        ]


compactPrimarySkeleton : Theme -> Element msg
compactPrimarySkeleton theme =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space4 ]
        [ Skeleton.block theme Tokens.skeletonLineShort Tokens.space12
        , Element.row [ Element.width Element.fill, Element.spacing Tokens.space8 ]
            [ Element.el [ Element.width Element.fill ]
                (Skeleton.fillBlock theme Tokens.bodyTextHeight)
            , Skeleton.block
                theme
                Tokens.skeletonLineShort
                (Tokens.space12 + Tokens.space4)
            ]
        ]


postingSkeleton : Theme -> Element msg
postingSkeleton theme =
    Element.row [ Element.width Element.fill, Element.spacing Tokens.space4 ]
        [ Skeleton.block
            theme
            Tokens.iconSizeTinyGlyph
            Tokens.iconSizeTinyGlyph
        , Element.el [ Element.width Element.fill ]
            (Skeleton.fillBlock theme Tokens.space12)
        , Skeleton.block theme Tokens.skeletonLineTiny Tokens.space12
        ]


omittedSkeleton : Theme -> Element msg
omittedSkeleton theme =
    Skeleton.block theme Tokens.skeletonLineShort Tokens.space12


pagingSkeleton : Theme -> Element msg
pagingSkeleton theme =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
        [ Element.el
            [ Element.width Element.fill
            , Element.height (Element.px Tokens.minimumTouchTarget)
            ]
            (Element.el [ Element.centerY ]
                (Skeleton.block
                    theme
                    Tokens.skeletonLineLong
                    Tokens.minimumTouchTarget
                )
            )
        , Element.el
            [ Element.width Element.fill
            , Element.height (Element.px (Tokens.bodyTextHeight + Tokens.space2))
            ]
            Element.none
        ]
