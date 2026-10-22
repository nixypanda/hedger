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
import Data.Transaction exposing (Posting, Transaction)
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Lib.Time as TimeExtra
import Page.Shared.QueryPanel as QueryPanel
import RemoteData exposing (RemoteData(..))
import Ui
import Ui.Common as Common
import Ui.DesignTokens as Tokens
import Ui.Icon as Icon
import Ui.Skeleton as Skeleton
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import Ui.Viewport as Viewport exposing (Viewport)
import View.AccountPresentation as AccountPresentation
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
    Theme
    -> Viewport
    -> Model
    -> Element Msg
view theme viewport model =
    let
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
            ( Just current, Just selected ) ->
                wideLayout theme model current selected

            _ ->
                listPage theme compact model

    else if currentSelection /= Nothing then
        case currentSelection of
            Just selected ->
                detailPage theme (Just Back) selected

            Nothing ->
                listPage theme compact model

    else
        listPage theme compact model


listPage :
    Theme
    -> Bool
    -> Model
    -> Element Msg
listPage theme compact model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Element.scrollbarY
        , Common.uiClass "page-scroll transactions-page"
        ]
        [ transactionsHeader theme
        , Ui.pageContainer
            [ queryPanel theme compact model
            , remoteContent theme compact model
            ]
        ]


wideLayout :
    Theme
    -> Model
    -> Response
    -> Transaction
    -> Element Msg
wideLayout theme model response selected =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.clip
        , Common.uiClass "transactions-wide-layout"
        ]
        [ Element.column
            [ Element.width (Element.fillPortion Tokens.layoutPortionDouble)
            , Element.height Element.fill
            , Element.scrollbarY
            , Background.color theme.pageBg
            , Common.uiClass "transactions-master-pane transactions-page"
            ]
            [ transactionsHeader theme
            , Ui.pageContainer
                [ queryPanel theme True model
                , remoteNotice theme (remoteData model) response
                , transactionCount theme response.transactions
                , transactionsList theme
                    True
                    (selectedTransaction model)
                    response.transactions
                , pagingControls theme model response
                ]
            ]
        , Element.el
            [ Element.width (Element.fillPortion Tokens.layoutPortionTriple)
            , Element.height Element.fill
            , Element.clipY
            , Common.uiClass "transactions-detail-pane"
            ]
            (detailPage theme (Just Back) selected)
        ]


transactionsHeader : Theme -> Element Msg
transactionsHeader theme =
    Ui.pageHeaderBand theme <|
        Ui.pageHeader theme
            { leading = Nothing
            , title = Typography.pageTitle theme "Transactions"
            , subtitle = Just "Browse your ledger activity"
            , actions =
                [ Ui.iconButton theme
                    { icon = Icon.Refresh, title = "Refresh", onPress = Refresh }
                ]
            }


queryPanel : Theme -> Bool -> Model -> Element Msg
queryPanel theme compact model =
    QueryPanel.view
        { theme = theme
        , compact = compact
        , draftQuery = draftQuery model
        , onDraftChanged = DraftChanged
        , onApply = ApplyDraft
        , onClear = ClearDraft
        , queryError = queryError (remoteData model)
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
    -> Model
    -> Element Msg
remoteContent theme compact model =
    case remoteData model of
        NotAsked ->
            Ui.card theme [ Ui.loading theme "Loading transactions…" ]

        Loading Nothing ->
            transactionsSkeleton theme

        Loading (Just response) ->
            transactionContent theme compact model response

        Success response ->
            transactionContent theme compact model response

        Failure error stale ->
            case stale of
                Just response ->
                    transactionContent theme compact model response

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
    -> Bool
    -> Model
    -> Response
    -> Element Msg
transactionContent theme compact model response =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        [ remoteNotice theme (remoteData model) response
        , transactionCount theme response.transactions
        , if List.isEmpty response.transactions then
            noMatches theme model

          else
            transactionsList theme
                compact
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
        , Element.spacing Tokens.space8
        , Element.htmlAttribute (Html.Attributes.id "transaction-list-end")
        ]
        [ case loadMoreError model of
            Just error ->
                Ui.errorBanner theme
                    ("Could not load more transactions: " ++ ApiError.message error)

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


remoteNotice : Theme -> RemoteData ApiError Response -> Response -> Element msg
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
    -> Bool
    -> Maybe Transaction
    -> List Transaction
    -> Element Msg
transactionsList theme compact selected transactions =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Tokens.space12
        , Element.htmlAttribute (Html.Attributes.id "transaction-list")
        ]
        (List.map
            (monthSection theme compact selected)
            (TimeExtra.groupByKey (Date.toString << .primaryDate) transactions)
        )


monthSection :
    Theme
    -> Bool
    -> Maybe Transaction
    -> ( String, List Transaction )
    -> Element Msg
monthSection theme compact selected ( month, transactions ) =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space4 ]
        (Element.el
            [ Element.paddingEach
                { top = Tokens.space8
                , right = Tokens.space0
                , bottom = Tokens.space4
                , left = Tokens.space4
                }
            ]
            (Typography.subsectionTitle theme (TimeExtra.monthLabel month))
            :: List.map (transactionRow theme compact selected) transactions
        )


transactionRow :
    Theme
    -> Bool
    -> Maybe Transaction
    -> Transaction
    -> Element Msg
transactionRow theme compact selected transaction =
    let
        rowContent =
            Element.column
                (Ui.rowSurfaceAttrs theme (selected == Just transaction)
                    ++ [ Element.spacing Tokens.space8
                       ]
                )
                [ if compact then
                    phonePrimaryRow theme transaction

                  else
                    desktopPrimaryRow theme transaction
                , Element.column
                    [ Element.width Element.fill
                    , Element.spacing Tokens.space3
                    , Element.paddingEach
                        { top = Tokens.space0
                        , right = Tokens.space0
                        , bottom = Tokens.space0
                        , left = Tokens.space12
                        }
                    ]
                    (List.map (postingPreviewRow theme) (List.take 5 transaction.postings))
                , let
                    omitted =
                        max 0 (List.length transaction.postings - 5)
                  in
                  if omitted > 0 then
                    Element.el
                        [ Element.paddingEach
                            { top = Tokens.space0
                            , right = Tokens.space0
                            , bottom = Tokens.space0
                            , left = Tokens.space12
                            }
                        ]
                        (Typography.mutedCaption theme ("+" ++ String.fromInt omitted ++ " more"))

                  else
                    Element.none
                ]
    in
    Input.button [ Element.width Element.fill, Element.padding Tokens.space0 ]
        { onPress = Just (Select transaction), label = rowContent }


desktopPrimaryRow : Theme -> Transaction -> Element msg
desktopPrimaryRow theme transaction =
    Element.row [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        [ Element.el
            [ Typography.rowDate
            , Font.color theme.textMuted
            , Element.width (Element.px Tokens.registerDateWidth)
            ]
            (Element.text (Date.toString transaction.primaryDate))
        , Element.el
            [ Typography.rowTitle
            , Font.semiBold
            , Element.width Element.fill
            , Font.color theme.textPrimary
            , Common.minWidthZero
            , Common.uiClass "ui-text-ellipsis"
            ]
            (Element.text transaction.description)
        , trailingSummary theme transaction
        ]


phonePrimaryRow : Theme -> Transaction -> Element msg
phonePrimaryRow theme transaction =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space3 ]
        [ Element.el
            [ Typography.rowDate, Font.color theme.textMuted ]
            (Element.text (Date.toString transaction.primaryDate))
        , Element.row [ Element.width Element.fill, Element.spacing Tokens.space8 ]
            [ Element.paragraph
                [ Typography.rowTitle
                , Font.semiBold
                , Element.width Element.fill
                , Font.color theme.textPrimary
                , Common.minWidthZero
                , Common.uiClass "ui-text-wrap"
                ]
                [ Element.text transaction.description ]
            , trailingSummary theme transaction
            ]
        ]


trailingSummary : Theme -> Transaction -> Element msg
trailingSummary theme transaction =
    let
        effects =
            if List.isEmpty transaction.presentation.effects then
                [ transaction.presentation.classification ]

            else
                transaction.presentation.effects
    in
    Element.wrappedRow [ Element.alignRight, Element.spacing Tokens.space2 ]
        (effects
            |> List.filter ((/=) "unclassified")
            |> List.map
                (\effect ->
                    Ui.financialBadge theme
                        (TransactionView.classificationTone effect)
                        (TransactionView.effectLabel effect)
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
        , Element.paragraph
            [ Typography.rowMetadata
            , Font.color theme.textMuted
            , Element.width Element.fill
            , Common.minWidthZero
            , Common.uiClass "ui-text-wrap"
            ]
            [ Element.text posting.account ]
        , Element.el
            [ Typography.rowMetadata
            , Font.color theme.textMuted
            , Element.alignRight
            , Element.width (Element.shrink |> Element.maximum Tokens.narrowContentWidth)
            , Common.minWidthZero
            , Common.uiClass "ui-text-ellipsis"
            , Typography.financeNumber
            ]
            (Element.text (TransactionView.amountsText posting.amounts))
        ]


detailPage : Theme -> Maybe Msg -> Transaction -> Element Msg
detailPage theme onBack transaction =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Element.scrollbarY
        , Common.uiClass "page-scroll transaction-detail-page"
        , Element.htmlAttribute (Html.Attributes.id "transaction-detail")
        ]
        [ Ui.pageHeaderBand theme <|
            Element.row [ Element.width Element.fill, Element.spacing Tokens.space8 ]
                [ case onBack of
                    Just message ->
                        Ui.backButton theme { onPress = message }

                    Nothing ->
                        Element.none
                , Typography.sectionTitle theme "Transaction Detail"
                ]
        , Ui.pageContainer [ TransactionView.transactionDetail theme transaction ]
        ]


transactionsSkeleton : Theme -> Element Msg
transactionsSkeleton theme =
    Ui.card theme
        [ Typography.mutedBody theme "Loading transactions…"
        , Element.column [ Element.width Element.fill, Element.spacing Tokens.space4 ]
            [ Element.el
                [ Element.paddingEach
                    { top = Tokens.space8
                    , right = Tokens.space0
                    , bottom = Tokens.space4
                    , left = Tokens.space4
                    }
                ]
                (Typography.subsectionTitle theme "January 2026")
            , Skeleton.listRow theme
            , Ui.hairline theme
            , Skeleton.listRow theme
            , Ui.hairline theme
            , Skeleton.listRow theme
            ]
        ]
