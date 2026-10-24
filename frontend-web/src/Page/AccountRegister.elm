module Page.AccountRegister exposing
    ( Model
    , Msg(..)
    , OutMsg(..)
    , Request
    , beginLoading
    , beginLoadingMore
    , clearDetail
    , fetch
    , init
    , receive
    , requestAccount
    , requestCursor
    , requestFrom
    , requestTo
    , selectDetail
    , setFrom
    , setTo
    , update
    , view
    , visibleResponse
    )

import Api.Client as Api
import Api.Error as ApiError exposing (ApiError)
import Api.Register exposing (Entry, Response, Value)
import Data.Completeness exposing (Completeness(..))
import Data.Date as Date
import Data.Decimal as Decimal
import Data.Formatting as Formatting
import Data.Freshness as Freshness exposing (Freshness)
import Data.Paginated as Paginated exposing (Paginated)
import Data.Quantity as Quantity exposing (Quantity)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Lib.Time as TimeExtra
import Route exposing (Route(..))
import Ui.Button as Button
import Ui.Chip as Chip
import Ui.Common as Common
import Ui.DesignTokens as Tokens
import Ui.Env exposing (Env)
import Ui.Feedback as Feedback
import Ui.Icon as Icon
import Ui.Input
import Ui.Notice as Notice
import Ui.Page as Page
import Ui.Skeleton as Skeleton
import Ui.Surface as Surface
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import Ui.Viewport as Viewport
import Ui.Workspace as Workspace
import View.States as States



-- MODEL


type Model
    = Model ModelData


type alias ModelData =
    { paginated : Paginated ApiError Criteria Response
    , account : String
    , from : String
    , to : String
    , selection : Selection
    }


{-| The applied scope carried through the pagination machine: which account we
are viewing and the date filters that were in effect when the request was sent.
-}
type alias Criteria =
    { account : String
    , filters : Filters
    }


type Selection
    = NoSelection
    | Selected Entry


type alias Filters =
    { from : Maybe String
    , to : Maybe String
    }


type alias Request =
    Paginated.Request Criteria


{-| Register entries append unboundedly and never reset — a register is a single
running ledger, not a re-queryable window.
-}
config : Paginated.Config Response
config =
    { nextCursor = .nextCursor
    , append = \current next -> { next | entries = current.entries ++ next.entries }
    , shouldReset = \_ _ -> False
    }


init : Model
init =
    Model
        { paginated = Paginated.init
        , account = ""
        , from = ""
        , to = ""
        , selection = NoSelection
        }



-- UPDATE


type Msg
    = BackToAccounts
    | FromChange String
    | ToChange String
    | Refresh
    | LoadMore
    | SelectDetail Entry
    | CloseDetail
    | Loaded Request (Result ApiError Response)


type OutMsg
    = NoOutMsg
    | Navigate Route


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg (Model data) =
    case msg of
        BackToAccounts ->
            ( Model data, Cmd.none, Navigate Accounts )

        FromChange value ->
            let
                changed =
                    setFrom value (Model data)

                ( nextModel, request ) =
                    beginLoading (modelAccount changed) changed
            in
            ( nextModel, fetchAccountRegister request, NoOutMsg )

        ToChange value ->
            let
                changed =
                    setTo value (Model data)

                ( nextModel, request ) =
                    beginLoading (modelAccount changed) changed
            in
            ( nextModel, fetchAccountRegister request, NoOutMsg )

        Refresh ->
            let
                ( nextModel, request ) =
                    beginLoading data.account (Model data)
            in
            ( nextModel, fetchAccountRegister request, NoOutMsg )

        LoadMore ->
            case beginLoadingMore (Model data) of
                Just ( nextModel, request ) ->
                    ( nextModel, fetchAccountRegister request, NoOutMsg )

                Nothing ->
                    ( Model data, Cmd.none, NoOutMsg )

        SelectDetail entry ->
            ( selectDetail entry (Model data), Cmd.none, NoOutMsg )

        CloseDetail ->
            ( clearDetail (Model data), Cmd.none, NoOutMsg )

        Loaded request result ->
            ( receive request result (Model data), Cmd.none, NoOutMsg )


fetchAccountRegister : Request -> Cmd Msg
fetchAccountRegister request =
    Api.getAccountRegister
        { account = requestAccount request
        , commodity = Nothing
        , from = requestFrom request
        , to = requestTo request
        , cursor = requestCursor request
        }
        (Loaded request)


fetch : Request -> Cmd Msg
fetch =
    fetchAccountRegister


beginLoading : String -> Model -> ( Model, Request )
beginLoading account (Model data) =
    let
        criteria =
            { account = account, filters = currentFilters data }

        ( nextPaginated, request ) =
            if hasWindowFor data account then
                Paginated.beginRefresh criteria data.paginated

            else
                Paginated.beginFresh criteria data.paginated
    in
    ( Model
        { data
            | paginated = nextPaginated
            , account = account
            , selection = NoSelection
        }
    , request
    )


beginLoadingMore : Model -> Maybe ( Model, Request )
beginLoadingMore (Model data) =
    Paginated.beginLoadMore config data.paginated
        |> Maybe.map
            (\( nextPaginated, request ) ->
                ( Model { data | paginated = nextPaginated, selection = NoSelection }, request )
            )


receive : Request -> Result ApiError Response -> Model -> Model
receive request result (Model data) =
    Model { data | paginated = Paginated.receive config request result data.paginated }


currentFilters : ModelData -> Filters
currentFilters data =
    { from = blankToNothing data.from
    , to = blankToNothing data.to
    }


hasWindowFor : ModelData -> String -> Bool
hasWindowFor data account =
    data.account == account && Paginated.currentPage data.paginated /= Nothing


setFrom : String -> Model -> Model
setFrom value (Model data) =
    Model { data | from = value }


setTo : String -> Model -> Model
setTo value (Model data) =
    Model { data | to = value }


selectDetail : Entry -> Model -> Model
selectDetail entry (Model data) =
    Model { data | selection = Selected entry }


clearDetail : Model -> Model
clearDetail (Model data) =
    Model { data | selection = NoSelection }


requestAccount : Request -> String
requestAccount request =
    (Paginated.requestCriteria request).account


requestFilters : Request -> Filters
requestFilters request =
    (Paginated.requestCriteria request).filters


requestFrom : Request -> Maybe String
requestFrom request =
    (requestFilters request).from


requestTo : Request -> Maybe String
requestTo request =
    (requestFilters request).to


requestCursor : Request -> Maybe String
requestCursor =
    Paginated.requestCursor


blankToNothing : String -> Maybe String
blankToNothing value =
    if String.isEmpty value then
        Nothing

    else
        Just value


modelAccount : Model -> String
modelAccount (Model data) =
    data.account


modelFilters : Model -> ( String, String )
modelFilters (Model data) =
    ( data.from, data.to )


modelPaginated : Model -> Paginated ApiError Criteria Response
modelPaginated (Model data) =
    data.paginated


visibleResponse : Model -> Maybe Response
visibleResponse (Model data) =
    Paginated.currentPage data.paginated



-- VIEW


view : Env -> Model -> Element Msg
view env (Model data) =
    let
        theme =
            env.theme

        viewport =
            env.viewport

        commodity =
            visibleResponse (Model data)
                |> Maybe.map .displayCommodity
                |> Maybe.withDefault ""

        compact =
            Viewport.isCompact (Viewport.contentLayout viewport)
    in
    if Viewport.usesMasterDetail viewport then
        case data.selection of
            NoSelection ->
                listPage theme compact viewport.height (Model data)

            Selected entry ->
                Workspace.view env
                    [ { key = "register"
                      , width = Workspace.Portion Tokens.layoutPortionDouble
                      , content = listPage theme True viewport.height (Model data)
                      }
                    , { key = "register-detail"
                      , width = Workspace.Portion Tokens.layoutPortionTriple
                      , content = detailPage theme CloseDetail commodity entry
                      }
                    ]

    else
        case data.selection of
            NoSelection ->
                listPage theme compact viewport.height (Model data)

            Selected entry ->
                detailPage theme CloseDetail commodity entry


listPage : Theme -> Bool -> Int -> Model -> Element Msg
listPage theme compact viewportHeight model =
    Element.column
        [ Element.width Element.fill
        , Background.color theme.pageBg
        ]
        [ Page.pageHeaderBand theme <|
            registerHeader theme compact model
        , Page.pageContainer
            ([ filters theme model, content theme compact viewportHeight model ]
                ++ failureNotices theme model
            )
        ]


registerHeader : Theme -> Bool -> Model -> Element Msg
registerHeader theme compact model =
    let
        account =
            modelAccount model

        actions =
            [ States.refreshStatus theme
                { freshness = registerFreshness (modelPaginated model)
                , loading = registerLoading (modelPaginated model)
                }
            , Button.refreshButton theme
                { loading = registerLoading (modelPaginated model)
                , onPress = Refresh
                }
            ]
    in
    if compact then
        Element.column
            [ Element.width Element.fill
            , Element.spacing Tokens.space16
            , Common.minWidthZero
            ]
            [ Element.row [ Element.width Element.fill, Element.spacing Tokens.space8 ]
                [ Button.backButton theme { onPress = BackToAccounts }
                , Element.el [ Element.alignRight ]
                    (Element.row [ Element.spacing Tokens.space8 ] actions)
                ]
            , Element.column
                [ Element.width Element.fill
                , Element.spacing Tokens.space8
                , Common.minWidthZero
                ]
                [ Typography.pageTitleWithDelimiter theme ":" account
                , Typography.mutedParagraph theme "Account register"
                ]
            ]

    else
        Page.pageHeader theme
            { leading = Just (Button.backButton theme { onPress = BackToAccounts })
            , title = Typography.pageTitleWithDelimiter theme ":" account
            , subtitle = Just "Account register"
            , actions = actions
            }


filters : Theme -> Model -> Element Msg
filters theme model =
    let
        ( from, to ) =
            modelFilters model
    in
    Surface.card theme
        [ Element.wrappedRow [ Element.width Element.fill, Element.spacing Tokens.space12 ]
            [ Element.el
                [ Element.width (Element.fill |> Element.maximum Tokens.splitPaneControlWidth) ]
              <|
                Ui.Input.dateInput theme
                    { label = "From"
                    , value = from
                    , onChange = FromChange
                    }
            , Element.el
                [ Element.width (Element.fill |> Element.maximum Tokens.splitPaneControlWidth)
                ]
              <|
                Ui.Input.dateInput theme { label = "To", value = to, onChange = ToChange }
            ]
        ]


content : Theme -> Bool -> Int -> Model -> Element Msg
content theme compact viewportHeight (Model data) =
    case ( Paginated.status data.paginated, visibleResponse (Model data) ) of
        ( Paginated.LoadFailure, _ ) ->
            Surface.card theme
                [ Feedback.errorBanner theme
                    ("Register failed to load: " ++ failureMessage data.paginated)
                ]

        ( _, Just response ) ->
            registerContent theme compact viewportHeight (Model data) response

        ( _, Nothing ) ->
            registerSkeleton theme compact viewportHeight


failureMessage : Paginated ApiError Criteria Response -> String
failureMessage paginated =
    Paginated.error paginated
        |> Maybe.map ApiError.message
        |> Maybe.withDefault "Unknown error"


registerFreshness : Paginated ApiError Criteria Response -> Freshness
registerFreshness paginated =
    case ( Paginated.status paginated, Paginated.currentPage paginated ) of
        ( Paginated.LoadFailure, _ ) ->
            Freshness.Unavailable

        ( Paginated.RefreshFailure, Just _ ) ->
            Freshness.Stale

        ( _, Just response ) ->
            response.metadata.freshness

        ( _, Nothing ) ->
            Freshness.Unavailable


registerLoading : Paginated ApiError Criteria Response -> Bool
registerLoading paginated =
    case Paginated.status paginated of
        Paginated.LoadingFirst ->
            True

        Paginated.Refreshing_ ->
            True

        _ ->
            False


failureNotices : Theme -> Model -> List (Element Msg)
failureNotices theme model =
    case Paginated.status (modelPaginated model) of
        Paginated.RefreshFailure ->
            [ failureNotice theme
                "register-refresh-error"
                ("Refresh failed; showing stale register: "
                    ++ failureMessage (modelPaginated model)
                )
            ]

        Paginated.LoadMoreFailure ->
            [ failureNotice theme
                "register-load-more-error"
                ("Loading more entries failed: "
                    ++ failureMessage (modelPaginated model)
                )
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


registerContent : Theme -> Bool -> Int -> Model -> Response -> Element Msg
registerContent theme compact viewportHeight model response =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        [ if List.isEmpty response.entries then
            listRegion viewportHeight (registerEmptyState theme compact)

          else
            registerRows theme compact model response.displayCommodity response.entries
        , paging theme model response
        ]


{-| Reserve the first-screen list region for empty results. Skeleton rows are
sized from this region and normal loaded pages are taller, so the footer stays
below the fold without making the empty card itself tall.
-}
listRegion : Int -> Element msg -> Element msg
listRegion viewportHeight body =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px (registerListRegionMinHeight viewportHeight))
        ]
        body


registerEmptyState : Theme -> Bool -> Element msg
registerEmptyState theme compact =
    Surface.cardWith
        [ Element.height
            (Element.minimum (registerEmptyMinHeight compact) Element.shrink)
        ]
        theme
        [ Typography.mutedParagraph theme
            "No register entries match this date range."
        ]


paging : Theme -> Model -> Response -> Element Msg
paging theme (Model data) response =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px Tokens.minimumTouchTarget)
        ]
        (Element.el [ Element.centerY ] (pagingControl theme data.paginated response))


pagingControl : Theme -> Paginated ApiError Criteria Response -> Response -> Element Msg
pagingControl theme paginated response =
    if Paginated.status paginated == Paginated.LoadingMore_ then
        Feedback.loading theme "Loading more entries…"

    else
        case response.nextCursor of
            Just _ ->
                Button.secondaryButton theme { label = "Load more", onPress = LoadMore }

            Nothing ->
                Typography.mutedCaption theme "All matching entries are loaded."


registerRowHeight : Bool -> Int
registerRowHeight compact =
    2
        * Tokens.space12
        + 2
        * Tokens.borderHairline
        + (if compact then
            compactRowContentHeight

           else
            Tokens.iconSizeSmallGlyph
          )


compactRowContentHeight : Int
compactRowContentHeight =
    Tokens.space12
        + Tokens.space8
        + (Tokens.space12 + Tokens.space4)
        + Tokens.space8
        + Tokens.space12
        + Tokens.space8
        + (2 * (Tokens.space12 + Tokens.space4) + Tokens.space4)


registerMonthHeadingHeight : Int
registerMonthHeadingHeight =
    Tokens.subsectionTitleHeight + Tokens.space8 + Tokens.space4


registerEmptyRowCount : Int
registerEmptyRowCount =
    2


registerEmptyMinHeight : Bool -> Int
registerEmptyMinHeight compact =
    registerMonthHeadingHeight
        + registerEmptyRowCount
        * registerRowHeight compact
        + registerEmptyRowCount
        * Tokens.space4


registerListTopOffset : Int
registerListTopOffset =
    160


registerListMaxHeight : Int
registerListMaxHeight =
    900


registerListRegionMinHeight : Int -> Int
registerListRegionMinHeight viewportHeight =
    clamp 400 registerListMaxHeight (viewportHeight - registerListTopOffset)


registerSkeletonRowCount : Bool -> Int -> Int
registerSkeletonRowCount compact viewportHeight =
    let
        rowStride =
            registerRowHeight compact + Tokens.space4

        rowsNeeded =
            (registerListRegionMinHeight viewportHeight
                - registerMonthHeadingHeight
            )
                // rowStride
    in
    clamp 1 20 rowsNeeded


registerSkeleton : Theme -> Bool -> Int -> Element msg
registerSkeleton theme compact viewportHeight =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        [ listRegion viewportHeight
            (Element.column
                [ Element.width Element.fill, Element.spacing Tokens.space4 ]
                (monthHeadingSkeleton theme
                    :: List.repeat
                        (registerSkeletonRowCount compact viewportHeight)
                        (registerRowSkeleton theme compact)
                )
            )
        , pagingSkeleton theme
        ]


monthHeadingSkeleton : Theme -> Element msg
monthHeadingSkeleton theme =
    Element.el
        [ Element.paddingEach
            { top = Tokens.space8
            , right = Tokens.space0
            , bottom = Tokens.space4
            , left = Tokens.space4
            }
        ]
        (Skeleton.block theme Tokens.skeletonLineShort Tokens.subsectionTitleHeight)


registerRowSkeleton : Theme -> Bool -> Element msg
registerRowSkeleton theme compact =
    if compact then
        compactRowSkeleton theme

    else
        desktopRowSkeleton theme


desktopRowSkeleton : Theme -> Element msg
desktopRowSkeleton theme =
    Element.row
        (Surface.rowSurfaceAttrs theme False
            ++ [ Element.spacing Tokens.space12
               , Common.minWidthZero
               , Element.height
                    (Element.minimum (registerRowHeight False) Element.shrink)
               ]
        )
        [ Element.el
            [ Element.width (Element.px Tokens.registerDateWidth) ]
            (Skeleton.block theme Tokens.skeletonLineShort Tokens.space12)
        , Element.el [ Element.width Element.fill, Common.minWidthZero ]
            (Skeleton.fillBlock theme Tokens.bodyTextHeight)
        , Element.el
            [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
            , Common.minWidthZero
            ]
            (Skeleton.fillBlock theme Tokens.space12)
        , Element.el
            [ Element.width (Element.px Tokens.registerAmountWidth) ]
            (Skeleton.fillBlock theme Tokens.space12)
        , Element.el
            [ Element.width (Element.px Tokens.registerAmountWidth) ]
            (Skeleton.fillBlock theme Tokens.space12)
        , Skeleton.block
            theme
            Tokens.iconSizeSmallGlyph
            Tokens.iconSizeSmallGlyph
        ]


compactRowSkeleton : Theme -> Element msg
compactRowSkeleton theme =
    Element.column
        (Surface.rowSurfaceAttrs theme False
            ++ [ Element.spacing Tokens.space8
               , Element.height
                    (Element.minimum (registerRowHeight True) Element.shrink)
               ]
        )
        [ Skeleton.block theme Tokens.skeletonLineShort Tokens.space12
        , Element.row
            [ Element.width Element.fill
            , Element.spacing Tokens.space8
            , Common.minWidthZero
            ]
            [ Element.el [ Element.width Element.fill ]
                (Skeleton.fillBlock theme Tokens.bodyTextHeight)
            , Skeleton.block
                theme
                Tokens.iconSizeSmallGlyph
                Tokens.iconSizeSmallGlyph
            ]
        , Skeleton.block theme Tokens.skeletonLineLong Tokens.space12
        , Element.column
            [ Element.width Element.fill, Element.spacing Tokens.space4 ]
            [ labeledSkeleton theme, labeledSkeleton theme ]
        ]


labeledSkeleton : Theme -> Element msg
labeledSkeleton theme =
    Element.row
        [ Element.width Element.fill
        , Element.spacing Tokens.space8
        , Common.minWidthZero
        , Element.height
            (Element.minimum
                (Tokens.space12 + Tokens.space4)
                Element.shrink
            )
        ]
        [ Skeleton.block theme Tokens.skeletonLineTiny Tokens.space12
        , Element.el [ Element.width Element.fill ]
            (Skeleton.fillBlock theme Tokens.space12)
        ]


pagingSkeleton : Theme -> Element msg
pagingSkeleton theme =
    Element.el
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


registerRows : Theme -> Bool -> Model -> String -> List Entry -> Element Msg
registerRows theme compact model commodity entries =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        (List.map
            (monthSection theme compact model commodity)
            (TimeExtra.groupByKey (Date.toString << .date) entries)
        )


monthSection : Theme -> Bool -> Model -> String -> ( String, List Entry ) -> Element Msg
monthSection theme compact model commodity ( month, entries ) =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space4 ]
        (Element.el
            [ Element.paddingEach
                { top = Tokens.space8
                , right = Tokens.space0
                , bottom = Tokens.space4
                , left = Tokens.space4
                }
            ]
            (Typography.subsectionTitle theme month)
            :: List.map (entryRow theme compact model commodity) entries
        )


entryRow : Theme -> Bool -> Model -> String -> Entry -> Element Msg
entryRow theme compact model commodity entry =
    let
        amount =
            valueDisplay
                theme
                commodity
                True
                (movementColor theme commodity entry.amount)
                entry.amount

        balance =
            valueDisplay theme commodity False theme.textPrimary entry.balance

        description =
            Common.truncatedText entry.description

        account =
            Common.truncatedText entry.account

        date =
            Element.text (Date.toString entry.date)
                |> Element.el [ Font.color theme.textMuted ]

        hasDetails =
            needsDetail entry.amount || needsDetail entry.balance

        chevron =
            if hasDetails then
                Icon.view
                    { icon = Icon.ChevronRight
                    , size = Tokens.iconSizeSmallGlyph
                    , color = theme.textMuted
                    }

            else
                Element.none

        rowContent =
            Element.column
                (Surface.rowSurfaceAttrs theme False
                    ++ [ Element.spacing Tokens.space8
                       , Element.height
                            (Element.minimum
                                (registerRowHeight compact)
                                Element.shrink
                            )
                       ]
                )
                [ if compact then
                    Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
                        [ Element.el Typography.caption date
                        , Element.row
                            [ Element.width Element.fill
                            , Element.spacing Tokens.space8
                            , Common.minWidthZero
                            ]
                            [ Element.el
                                [ Element.width Element.fill
                                , Common.minWidthZero
                                ]
                                description
                            , chevron
                            ]
                        , Element.el
                            (Typography.caption
                                ++ [ Font.color theme.textMuted
                                   , Common.minWidthZero
                                   ]
                            )
                            account
                        , Element.column
                            [ Element.width Element.fill
                            , Element.spacing Tokens.space4
                            , Common.minWidthZero
                            ]
                            [ labeledValue theme "Amount" amount
                            , labeledValue theme "Balance" balance
                            ]
                        ]

                  else
                    Element.row
                        [ Element.width Element.fill
                        , Element.spacing Tokens.space12
                        , Common.minWidthZero
                        ]
                        [ Element.el
                            (Typography.caption
                                ++ [ Element.width (Element.px Tokens.registerDateWidth) ]
                            )
                            date
                        , Element.el
                            [ Element.width (Element.fillPortion Tokens.layoutPortionDouble)
                            , Common.minWidthZero
                            ]
                            description
                        , Element.el
                            (Typography.caption
                                ++ [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
                                   , Font.color theme.textMuted
                                   , Common.minWidthZero
                                   ]
                            )
                            account
                        , Element.el
                            [ Element.width (Element.px Tokens.registerAmountWidth)
                            , Common.minWidthZero
                            ]
                            amount
                        , Element.el
                            [ Element.width (Element.px Tokens.registerAmountWidth)
                            , Common.minWidthZero
                            ]
                            balance
                        , chevron
                        ]
                ]
    in
    if hasDetails then
        Input.button [ Element.width Element.fill, Element.padding Tokens.space0 ]
            { onPress = Just (SelectDetail entry), label = rowContent }

    else
        rowContent


labeledValue : Theme -> String -> Element msg -> Element msg
labeledValue theme label valueEl =
    Element.row
        [ Element.width Element.fill
        , Element.spacing Tokens.space8
        , Common.minWidthZero
        , Element.height
            (Element.minimum
                (Tokens.space12 + Tokens.space4)
                Element.shrink
            )
        ]
        [ Element.text label
            |> Element.el (Typography.caption ++ [ Font.color theme.textMuted ])
        , Element.el [ Element.width Element.fill, Common.minWidthZero ] valueEl
        ]


valueDisplay : Theme -> String -> Bool -> Element.Color -> Value -> Element msg
valueDisplay theme commodity isMovement color value =
    Element.row
        [ Element.width Element.fill
        , Element.spacing Tokens.space4
        , Common.minWidthZero
        ]
        [ Common.truncatedText (fullDisplayValue commodity value)
            |> Element.el
                (Typography.caption
                    ++ Typography.numeric
                    ++ [ Element.width Element.fill
                       , Font.alignRight
                       , Font.color color
                       ]
                    ++ (if isMovement then
                            [ Typography.fontWeight Typography.SemiBold ]

                        else
                            []
                       )
                )
        , if value.completeness == Partial then
            Chip.statusBadge theme Chip.StatusWarning "Partial"

          else
            Element.none
        ]


movementColor : Theme -> String -> Value -> Element.Color
movementColor theme commodity value =
    case displayQuantity commodity value of
        Just amount ->
            if amount < 0 then
                theme.negative

            else
                theme.textPrimary

        Nothing ->
            theme.textPrimary


displayQuantity : String -> Value -> Maybe Float
displayQuantity commodity value =
    value.display
        |> List.filter (\amount -> amount.commodity == commodity)
        |> List.head
        |> Maybe.andThen (.quantity >> Decimal.toString >> String.toFloat)


needsDetail : Value -> Bool
needsDetail value =
    commodityNames value.raw /= commodityNames value.display


commodityNames : List Quantity -> List String
commodityNames quantities =
    quantities |> List.map .commodity |> List.sort


displayValue : String -> Value -> String
displayValue commodity value =
    value.display
        |> List.filter (\amount -> amount.commodity == commodity)
        |> List.head
        |> Maybe.map Formatting.formatBalance
        |> Maybe.withDefault "—"


fullDisplayValue : String -> Value -> String
fullDisplayValue commodity value =
    value.display
        |> List.filter (\amount -> amount.commodity == commodity)
        |> List.head
        |> Maybe.map Formatting.formatFullBalance
        |> Maybe.withDefault "—"


detailPage : Theme -> Msg -> String -> Entry -> Element Msg
detailPage theme onClose commodity entry =
    Page.scaffold theme
        { header =
            { leading = Just (Button.backButton theme { onPress = onClose })
            , title = Typography.pageTitle theme "Details"
            , subtitle = Nothing
            , actions = []
            }
        , body =
            [ Surface.card theme
                [ Typography.sectionTitle theme entry.description
                , Element.text (Date.toString entry.date ++ " · " ++ entry.account)
                    |> Element.el (Typography.caption ++ [ Font.color theme.textMuted ])
                , valueDetails theme commodity "Movement" entry.amount
                , Surface.hairline theme
                , valueDetails theme commodity "Balance" entry.balance
                ]
            ]
        }


valueDetails : Theme -> String -> String -> Value -> Element msg
valueDetails theme commodity label value =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
        [ Typography.cardTitle theme label
        , Element.text (displayValue commodity value)
            |> Element.el Typography.headline
        , States.registerCompletenessBadge theme value.completeness
        , Typography.mutedCaption theme "Raw amounts included"
        , Element.column
            [ Element.width Element.fill, Element.spacing Tokens.space8 ]
            (List.map (rawRow theme) value.raw)
        , if value.completeness == Partial then
            Typography.mutedParagraph theme
                "The displayed total excludes the listed unconvertible commodity amounts."

          else
            Element.none
        ]


rawRow : Theme -> Quantity -> Element msg
rawRow theme quantity =
    Element.row
        [ Element.width Element.fill
        , Element.paddingXY Tokens.space8 Tokens.space12
        , Background.color theme.inputFill
        , Border.rounded Tokens.radiusControl
        ]
        [ Element.text (Quantity.commodity quantity)
            |> Element.el [ Element.width Element.fill, Font.color theme.textMedium ]
        , Element.text (Formatting.formatFullValue (Quantity.quantity quantity))
            |> Element.el (Typography.body ++ [ Typography.fontWeight Typography.SemiBold ])
        ]
