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
import Data.Paginated as Paginated exposing (Paginated)
import Data.Quantity as Quantity exposing (Quantity)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Lib.Time as TimeExtra
import Route exposing (Route(..))
import Ui
import Ui.Common as Common
import Ui.DesignTokens as Tokens
import Ui.Icon as Icon
import Ui.Skeleton as Skeleton
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import Ui.Viewport as Viewport exposing (Viewport)
import View.Formatting as Formatting
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


visibleResponse : Model -> Maybe Response
visibleResponse (Model data) =
    Paginated.currentPage data.paginated



-- VIEW


view : Theme -> Viewport -> Model -> Element Msg
view theme viewport (Model data) =
    let
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
                listPage theme compact (Model data)

            Selected entry ->
                Element.row
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.clip
                    ]
                    [ Element.el
                        [ Element.width (Element.fillPortion Tokens.layoutPortionDouble)
                        , Element.height Element.fill
                        , Element.scrollbarY
                        ]
                        (listPage theme True (Model data))
                    , Element.el
                        [ Element.width (Element.fillPortion Tokens.layoutPortionTriple)
                        , Element.height Element.fill
                        , Element.scrollbarY
                        ]
                        (detailPage theme CloseDetail commodity entry)
                    ]

    else
        case data.selection of
            NoSelection ->
                listPage theme compact (Model data)

            Selected entry ->
                detailPage theme CloseDetail commodity entry


listPage : Theme -> Bool -> Model -> Element Msg
listPage theme compact model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbarY
        , Background.color theme.pageBg
        , Common.uiClass "page-scroll account-register-page"
        ]
        [ Ui.pageHeaderBand theme <|
            registerHeader theme compact model
        , Ui.pageContainer [ filters theme model, content theme compact model ]
        ]


registerHeader : Theme -> Bool -> Model -> Element Msg
registerHeader theme compact model =
    let
        account =
            modelAccount model
    in
    if compact then
        Element.column
            [ Element.width Element.fill
            , Element.spacing Tokens.space16
            , Common.minWidthZero
            ]
            [ Element.row [ Element.width Element.fill, Element.spacing Tokens.space8 ]
                [ Ui.backButton theme { onPress = BackToAccounts }
                , Element.el [ Element.alignRight ]
                    (Ui.iconButton theme
                        { icon = Icon.Refresh, title = "Refresh", onPress = Refresh }
                    )
                ]
            , Element.column
                [ Element.width Element.fill
                , Element.spacing Tokens.space8
                , Common.minWidthZero
                ]
                [ Typography.delimitedPageTitle theme ":" account
                , Typography.mutedParagraph theme "Account register"
                ]
            ]

    else
        Ui.pageHeader theme
            { leading = Just (Ui.backButton theme { onPress = BackToAccounts })
            , title = Typography.delimitedPageTitle theme ":" account
            , subtitle = Just "Account register"
            , actions =
                [ Ui.iconButton theme
                    { icon = Icon.Refresh, title = "Refresh", onPress = Refresh }
                ]
            }


filters : Theme -> Model -> Element Msg
filters theme model =
    let
        ( from, to ) =
            modelFilters model
    in
    Ui.card theme
        [ Element.wrappedRow [ Element.width Element.fill, Element.spacing Tokens.space12 ]
            [ Element.el
                [ Element.width (Element.fill |> Element.maximum Tokens.splitPaneControlWidth) ]
              <|
                Ui.dateInput theme
                    { label = "From"
                    , value = from
                    , onChange = FromChange
                    }
            , Element.el
                [ Element.width (Element.fill |> Element.maximum Tokens.splitPaneControlWidth)
                ]
              <|
                Ui.dateInput theme { label = "To", value = to, onChange = ToChange }
            ]
        ]


content : Theme -> Bool -> Model -> Element Msg
content theme compact (Model data) =
    case ( Paginated.status data.paginated, visibleResponse (Model data) ) of
        ( Paginated.NotAsked, _ ) ->
            Ui.card theme [ Ui.loading theme "Loading register…" ]

        ( Paginated.LoadingFirst, _ ) ->
            Ui.card theme
                [ Skeleton.listRow theme
                , Ui.hairline theme
                , Skeleton.listRow theme
                , Ui.hairline theme
                , Skeleton.listRow theme
                ]

        ( Paginated.LoadFailure, _ ) ->
            Ui.card theme
                [ Ui.errorBanner theme
                    ("Register failed to load: " ++ failureMessage data.paginated)
                ]

        ( Paginated.RefreshFailure, Just response ) ->
            Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
                [ Ui.errorBanner theme
                    ("Refresh failed; showing stale register: " ++ failureMessage data.paginated)
                , registerContent theme compact (Model data) response
                ]

        ( Paginated.LoadMoreFailure, Just response ) ->
            Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
                [ Ui.errorBanner theme
                    ("Loading more entries failed: " ++ failureMessage data.paginated)
                , registerContent theme compact (Model data) response
                ]

        ( _, Just response ) ->
            registerContent theme compact (Model data) response

        ( _, Nothing ) ->
            Ui.card theme [ Ui.loading theme "Loading register…" ]


failureMessage : Paginated ApiError Criteria Response -> String
failureMessage paginated =
    Paginated.error paginated
        |> Maybe.map ApiError.message
        |> Maybe.withDefault "Unknown error"


registerContent : Theme -> Bool -> Model -> Response -> Element Msg
registerContent theme compact model response =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        [ States.freshnessBanner theme response.metadata.freshness
        , if List.isEmpty response.entries then
            Ui.card theme [ Ui.emptyState theme "No register entries match this date range." ]

          else
            registerRows theme compact model response.displayCommodity response.entries
        , paging theme model response
        ]


paging : Theme -> Model -> Response -> Element Msg
paging theme (Model data) response =
    if Paginated.status data.paginated == Paginated.LoadingMore_ then
        Ui.loading theme "Loading more entries…"

    else
        case response.nextCursor of
            Just _ ->
                Ui.secondaryButton theme { label = "Load more", onPress = LoadMore }

            Nothing ->
                Typography.mutedCaption theme "All matching entries are loaded."


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
            Element.column (Ui.rowSurfaceAttrs theme False ++ [ Element.spacing Tokens.space8 ])
                [ if compact then
                    Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
                        [ Element.el [ Typography.caption ] date
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
                            [ Typography.caption
                            , Font.color theme.textMuted
                            , Common.minWidthZero
                            ]
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
                            [ Element.width (Element.px Tokens.registerDateWidth)
                            ]
                            date
                        , Element.el
                            [ Element.width (Element.fillPortion Tokens.layoutPortionDouble)
                            , Common.minWidthZero
                            ]
                            description
                        , Element.el
                            [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
                            , Typography.caption
                            , Font.color theme.textMuted
                            , Common.minWidthZero
                            ]
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
        ]
        [ Element.text label
            |> Element.el [ Typography.caption, Font.color theme.textMuted ]
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
                ([ Element.width Element.fill
                 , Font.alignRight
                 , Font.color color
                 ]
                    ++ (if isMovement then
                            [ Font.semiBold ]

                        else
                            []
                       )
                )
        , if value.completeness == Partial then
            Ui.statusBadge theme Ui.StatusWarning "Partial"

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
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbarY
        , Background.color theme.pageBg
        , Common.uiClass "page-scroll register-detail-page"
        ]
        [ Ui.pageHeaderBand theme <|
            Element.row [ Element.width Element.fill, Element.spacing Tokens.space8 ]
                [ Ui.backButton theme { onPress = onClose }
                , Typography.sectionTitle theme "Details"
                ]
        , Ui.pageContainer
            [ Ui.card theme
                [ Typography.sectionTitle theme entry.description
                , Element.text (Date.toString entry.date ++ " · " ++ entry.account)
                    |> Element.el [ Typography.caption, Font.color theme.textMuted ]
                , valueDetails theme commodity "Movement" entry.amount
                , Ui.hairline theme
                , valueDetails theme commodity "Balance" entry.balance
                ]
            ]
        ]


valueDetails : Theme -> String -> String -> Value -> Element msg
valueDetails theme commodity label value =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
        [ Typography.subsectionTitle theme label
        , Element.text (displayValue commodity value)
            |> Element.el
                [ Typography.detailHeroValue
                , Font.semiBold
                ]
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
        , Element.paddingXY Tokens.space10 Tokens.space8
        , Background.color theme.inputFill
        , Border.rounded Tokens.radiusControl
        ]
        [ Element.text (Quantity.commodity quantity)
            |> Element.el [ Element.width Element.fill, Font.color theme.textMedium ]
        , Element.text (Formatting.formatFullValue (Quantity.quantity quantity))
            |> Element.el [ Font.semiBold ]
        ]
