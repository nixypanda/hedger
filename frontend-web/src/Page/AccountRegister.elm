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
import Element exposing (Device, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Lib.Time as TimeExtra
import Route exposing (Route(..))
import State.Viewport exposing (Viewport)
import Ui
import Ui.Icon as Icon
import Ui.Skeleton as Skeleton
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import View.Formatting as Formatting
import View.States as States



-- MODEL


type Model
    = Model ModelData


type alias ModelData =
    { paginated : Paginated Criteria Response
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
        device =
            Element.classifyDevice viewport

        commodity =
            visibleResponse (Model data)
                |> Maybe.map .displayCommodity
                |> Maybe.withDefault ""
    in
    if viewport.width >= 1000 then
        case data.selection of
            NoSelection ->
                listPage theme device (Model data)

            Selected entry ->
                Element.row [ Element.width Element.fill, Element.height Element.fill, Element.clip ]
                    [ Element.el
                        [ Element.width (Element.fillPortion 2)
                        , Element.height Element.fill
                        , Element.scrollbarY
                        ]
                        (listPage theme device (Model data))
                    , Element.el
                        [ Element.width (Element.fillPortion 3)
                        , Element.height Element.fill
                        , Element.scrollbarY
                        ]
                        (detailPage theme device CloseDetail commodity entry)
                    ]

    else
        case data.selection of
            NoSelection ->
                listPage theme device (Model data)

            Selected entry ->
                detailPage theme device CloseDetail commodity entry


listPage : Theme -> Device -> Model -> Element Msg
listPage theme device model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbarY
        , Background.color theme.pageBg
        , Element.htmlAttribute (Html.Attributes.class "page-scroll account-register-page")
        ]
        [ Ui.pageHeaderBand theme device <|
            registerHeader theme device model
        , Ui.pageContainer device [ filters theme model, content theme device model ]
        ]


registerHeader : Theme -> Device -> Model -> Element Msg
registerHeader theme device model =
    let
        account =
            modelAccount model
    in
    if Ui.isSmall device then
        Element.column [ Element.width Element.fill, Element.spacing 16, Typography.minWidthZero ]
            [ Element.row [ Element.width Element.fill, Element.spacing 8 ]
                [ Ui.backButton device theme { onPress = BackToAccounts }
                , Element.el [ Element.alignRight ]
                    (Ui.iconButton device
                        theme
                        { icon = Icon.Refresh, title = "Refresh", onPress = Refresh }
                    )
                ]
            , Element.column [ Element.width Element.fill, Element.spacing 8, Typography.minWidthZero ]
                [ Typography.pageTitle theme (breakAccountPath account)
                , Typography.mutedParagraph theme "Account register"
                ]
            ]

    else
        Ui.pageHeader theme
            { leading = Just (Ui.backButton device theme { onPress = BackToAccounts })
            , title = Typography.pageTitle theme account
            , subtitle = Just "Account register"
            , actions =
                [ Ui.iconButton device
                    theme
                    { icon = Icon.Refresh, title = "Refresh", onPress = Refresh }
                ]
            }


breakAccountPath : String -> String
breakAccountPath =
    String.replace ":" ":\u{200B}"


filters : Theme -> Model -> Element Msg
filters theme model =
    let
        ( from, to ) =
            modelFilters model
    in
    Ui.card theme
        [ Element.wrappedRow [ Element.width Element.fill, Element.spacing 12 ]
            [ Element.el [ Element.width (Element.fill |> Element.maximum 220) ] <|
                Ui.dateInput theme
                    { label = "From"
                    , value = from
                    , onChange = FromChange
                    }
            , Element.el [ Element.width (Element.fill |> Element.maximum 220) ] <|
                Ui.dateInput theme { label = "To", value = to, onChange = ToChange }
            ]
        ]


content : Theme -> Device -> Model -> Element Msg
content theme device (Model data) =
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
            Element.column [ Element.width Element.fill, Element.spacing 12 ]
                [ Ui.errorBanner theme
                    ("Refresh failed; showing stale register: " ++ failureMessage data.paginated)
                , registerContent theme device (Model data) response
                ]

        ( Paginated.LoadMoreFailure, Just response ) ->
            Element.column [ Element.width Element.fill, Element.spacing 12 ]
                [ Ui.errorBanner theme
                    ("Loading more entries failed: " ++ failureMessage data.paginated)
                , registerContent theme device (Model data) response
                ]

        ( _, Just response ) ->
            registerContent theme device (Model data) response

        ( _, Nothing ) ->
            Ui.card theme [ Ui.loading theme "Loading register…" ]


failureMessage : Paginated Criteria Response -> String
failureMessage paginated =
    Paginated.error paginated
        |> Maybe.map ApiError.message
        |> Maybe.withDefault "Unknown error"


registerContent : Theme -> Device -> Model -> Response -> Element Msg
registerContent theme device model response =
    Element.column [ Element.width Element.fill, Element.spacing 12 ]
        [ States.freshnessBanner theme response.metadata.freshness
        , if List.isEmpty response.entries then
            Ui.card theme [ Ui.emptyState theme "No register entries match this date range." ]

          else
            registerRows theme device model response.displayCommodity response.entries
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


registerRows : Theme -> Device -> Model -> String -> List Entry -> Element Msg
registerRows theme device model commodity entries =
    Element.column [ Element.width Element.fill, Element.spacing 12 ]
        (List.map
            (monthSection theme device model commodity)
            (TimeExtra.groupByKey (Date.toString << .date) entries)
        )


monthSection : Theme -> Device -> Model -> String -> ( String, List Entry ) -> Element Msg
monthSection theme device model commodity ( month, entries ) =
    Element.column [ Element.width Element.fill, Element.spacing 4 ]
        (Element.el
            [ Element.paddingEach { top = 8, right = 0, bottom = 4, left = 4 } ]
            (Typography.subsectionTitle theme month)
            :: List.map (entryRow theme device model commodity) entries
        )


entryRow : Theme -> Device -> Model -> String -> Entry -> Element Msg
entryRow theme device model commodity entry =
    let
        amount =
            valueDisplay theme device commodity True entry.amount

        balance =
            valueDisplay theme device commodity False entry.balance

        description =
            Element.column
                [ Element.width (Element.fillPortion 3)
                , Element.spacing 3
                , Typography.minWidthZero
                ]
                [ Ui.truncatedText device entry.description
                , Element.text entry.account |> Element.el [ Font.size 12, Font.color theme.textMuted ]
                ]

        hasDetails =
            needsDetail entry.amount || needsDetail entry.balance

        chevron =
            if hasDetails then
                Icon.view { icon = Icon.ChevronRight, size = 16, color = theme.textMuted }

            else
                Element.none

        rowContent =
            Element.column (Ui.rowSurfaceAttrs theme False ++ [ Element.spacing 8 ])
                [ if Ui.isSmall device then
                    Element.column [ Element.width Element.fill, Element.spacing 8 ]
                        [ Element.text (Date.toString entry.date)
                            |> Element.el [ Font.size 12, Font.color theme.textMuted ]
                        , Element.row
                            [ Element.width Element.fill
                            , Element.spacing 8
                            , Typography.minWidthZero
                            ]
                            [ description
                            , chevron
                            ]
                        , Element.row
                            [ Element.width Element.fill
                            , Element.spacing 12
                            , Typography.minWidthZero
                            ]
                            [ Element.el
                                [ Element.width (Element.fillPortion 1)
                                , Typography.minWidthZero
                                ]
                                amount
                            , Element.el
                                [ Element.width (Element.fillPortion 1)
                                , Typography.minWidthZero
                                ]
                                balance
                            ]
                        ]

                  else
                    Element.row [ Element.width Element.fill, Element.spacing 12, Typography.minWidthZero ]
                        [ Element.text (Date.toString entry.date)
                            |> Element.el [ Element.width (Element.px 92), Font.color theme.textMuted ]
                        , description
                        , Element.el [ Element.width (Element.px 150), Typography.minWidthZero ] amount
                        , Element.el [ Element.width (Element.px 150), Typography.minWidthZero ] balance
                        , chevron
                        ]
                ]
    in
    if hasDetails then
        Input.button [ Element.width Element.fill, Element.padding 0 ]
            { onPress = Just (SelectDetail entry), label = rowContent }

    else
        rowContent


valueDisplay : Theme -> Device -> String -> Bool -> Value -> Element msg
valueDisplay theme device commodity isMovement value =
    Element.row [ Element.width Element.fill, Element.spacing 4, Typography.minWidthZero ]
        [ Ui.truncatedText device (displayValue commodity value)
            |> Element.el
                ([ Element.width Element.fill, Font.alignRight ]
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


detailPage : Theme -> Device -> Msg -> String -> Entry -> Element Msg
detailPage theme device onClose commodity entry =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbarY
        , Background.color theme.pageBg
        , Element.htmlAttribute (Html.Attributes.class "page-scroll register-detail-page")
        ]
        [ Ui.pageHeaderBand theme device <|
            Element.row [ Element.width Element.fill, Element.spacing 8 ]
                [ Ui.backButton device theme { onPress = onClose }
                , Typography.sectionTitle theme "Details"
                ]
        , Ui.pageContainer device
            [ Ui.card theme
                [ Typography.sectionTitle theme entry.description
                , Element.text (Date.toString entry.date ++ " · " ++ entry.account)
                    |> Element.el [ Font.size 12, Font.color theme.textMuted ]
                , valueDetails theme commodity "Movement" entry.amount
                , Ui.hairline theme
                , valueDetails theme commodity "Balance" entry.balance
                ]
            ]
        ]


valueDetails : Theme -> String -> String -> Value -> Element msg
valueDetails theme commodity label value =
    Element.column [ Element.width Element.fill, Element.spacing 8 ]
        [ Typography.subsectionTitle theme label
        , Element.text (displayValue commodity value) |> Element.el [ Font.size 22, Font.semiBold ]
        , States.registerCompletenessBadge theme value.completeness
        , Typography.mutedCaption theme "Raw amounts included"
        , Element.column [ Element.width Element.fill, Element.spacing 8 ] (List.map (rawRow theme) value.raw)
        , if value.completeness == Partial then
            Typography.mutedParagraph theme
                "The displayed total excludes the listed unconvertible commodity amounts."

          else
            Element.none
        ]


emptyValue : Value
emptyValue =
    { raw = [], display = [], completeness = Unavailable }


rawRow : Theme -> Quantity -> Element msg
rawRow theme quantity =
    Element.row
        [ Element.width Element.fill
        , Element.paddingXY 10 8
        , Background.color theme.inputFill
        , Border.rounded 6
        ]
        [ Element.text (Quantity.commodity quantity)
            |> Element.el [ Element.width Element.fill, Font.color theme.textMedium ]
        , Element.text (Decimal.toString (Quantity.quantity quantity)) |> Element.el [ Font.semiBold ]
        ]
