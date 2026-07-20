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
import Api.Register exposing (Completeness(..), Entry, Response, Value)
import Data.Date as Date
import Data.Decimal as Decimal
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
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import View.States as States



-- MODEL


type Model
    = Model ModelData


type alias ModelData =
    { state : State
    , account : String
    , from : String
    , to : String
    , nextToken : Int
    }


type State
    = NotStarted
    | Loading Filters Int
    | LoadFailed Filters ApiError
    | Showing Window
    | Refreshing Window Int
    | RefreshFailed Window ApiError
    | LoadingMore Window NextRequest
    | LoadMoreFailed Window NextRequest ApiError


type alias Window =
    { filters : Filters
    , response : Response
    , selection : Selection
    }


type Selection
    = NoSelection
    | Selected Entry


type alias Filters =
    { from : Maybe String
    , to : Maybe String
    }


type Request
    = InitialRequest RequestData
    | RefreshRequest RequestData
    | NextPageRequest NextRequest


type alias RequestData =
    { token : Int
    , account : String
    , filters : Filters
    }


type alias NextRequest =
    { token : Int
    , account : String
    , filters : Filters
    , cursor : String
    }


init : Model
init =
    Model
        { state = NotStarted
        , account = ""
        , from = ""
        , to = ""
        , nextToken = 0
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
        appliedFilters =
            currentFilters data

        requestData =
            { token = data.nextToken + 1
            , account = account
            , filters = appliedFilters
            }

        request =
            if hasWindowFor data account then
                RefreshRequest requestData

            else
                InitialRequest requestData

        state =
            case currentWindow data of
                Just window ->
                    if hasWindowFor data account then
                        Refreshing { window | selection = NoSelection } requestData.token

                    else
                        Loading appliedFilters requestData.token

                _ ->
                    Loading appliedFilters requestData.token
    in
    ( Model
        { data
            | state = state
            , account = account
            , nextToken = requestData.token
        }
    , request
    )


beginLoadingMore : Model -> Maybe ( Model, Request )
beginLoadingMore (Model data) =
    case data.state of
        Showing window ->
            nextRequest data window

        RefreshFailed window _ ->
            nextRequest data window

        LoadMoreFailed window request _ ->
            nextRequestFrom data window request.cursor

        _ ->
            Nothing


nextRequest : ModelData -> Window -> Maybe ( Model, Request )
nextRequest data window =
    case window.response.nextCursor of
        Just cursor ->
            nextRequestFrom data window cursor

        Nothing ->
            Nothing


nextRequestFrom : ModelData -> Window -> String -> Maybe ( Model, Request )
nextRequestFrom data window cursor =
    let
        requestData =
            { token = data.nextToken + 1
            , account = data.account
            , filters = window.filters
            , cursor = cursor
            }

        request =
            NextPageRequest requestData
    in
    Just
        ( Model
            { data
                | state = LoadingMore { window | selection = NoSelection } requestData
                , nextToken = requestData.token
            }
        , request
        )


receive : Request -> Result ApiError Response -> Model -> Model
receive request result (Model data) =
    if not (requestIsActive request data.state) then
        Model data

    else
        case ( request, result ) of
            ( InitialRequest requestData, Ok response ) ->
                Model { data | state = Showing (newWindow requestData.filters response) }

            ( RefreshRequest requestData, Ok response ) ->
                Model { data | state = Showing (newWindow requestData.filters response) }

            ( NextPageRequest activeNextRequest, Ok response ) ->
                case data.state of
                    LoadingMore window _ ->
                        Model
                            { data
                                | state =
                                    Showing
                                        { window
                                            | response = appendResponse window.response response
                                        }
                            }

                    _ ->
                        Model data

            ( InitialRequest requestData, Err error ) ->
                Model { data | state = LoadFailed requestData.filters error }

            ( RefreshRequest _, Err error ) ->
                case data.state of
                    Refreshing window _ ->
                        Model { data | state = RefreshFailed window error }

                    _ ->
                        Model data

            ( NextPageRequest activeNextRequest, Err error ) ->
                case data.state of
                    LoadingMore window _ ->
                        Model { data | state = LoadMoreFailed window activeNextRequest error }

                    _ ->
                        Model data


requestIsActive : Request -> State -> Bool
requestIsActive request state =
    case ( request, state ) of
        ( InitialRequest requestData, Loading _ token ) ->
            requestData.token == token

        ( RefreshRequest requestData, Refreshing _ token ) ->
            requestData.token == token

        ( NextPageRequest requestData, LoadingMore _ activeRequest ) ->
            requestData.token == activeRequest.token

        _ ->
            False


newWindow : Filters -> Response -> Window
newWindow appliedFilters response =
    { filters = appliedFilters
    , response = response
    , selection = NoSelection
    }


appendResponse : Response -> Response -> Response
appendResponse current next =
    { next | entries = current.entries ++ next.entries }


currentFilters : ModelData -> Filters
currentFilters data =
    { from = blankToNothing data.from
    , to = blankToNothing data.to
    }


currentWindow : ModelData -> Maybe Window
currentWindow data =
    case data.state of
        Showing window ->
            Just window

        Refreshing window _ ->
            Just window

        RefreshFailed window _ ->
            Just window

        LoadingMore window _ ->
            Just window

        LoadMoreFailed window _ _ ->
            Just window

        _ ->
            Nothing


hasWindowFor : ModelData -> String -> Bool
hasWindowFor data account =
    data.account == account && currentWindow data /= Nothing


setFrom : String -> Model -> Model
setFrom value (Model data) =
    Model { data | from = value }


setTo : String -> Model -> Model
setTo value (Model data) =
    Model { data | to = value }


selectDetail : Entry -> Model -> Model
selectDetail entry (Model data) =
    Model { data | state = mapWindow (selectInWindow entry) data.state }


clearDetail : Model -> Model
clearDetail (Model data) =
    Model { data | state = mapWindow (\window -> { window | selection = NoSelection }) data.state }


mapWindow : (Window -> Window) -> State -> State
mapWindow transform state =
    case state of
        Showing window ->
            Showing (transform window)

        Refreshing window token ->
            Refreshing (transform window) token

        RefreshFailed window error ->
            RefreshFailed (transform window) error

        LoadingMore window request ->
            LoadingMore (transform window) request

        LoadMoreFailed window request error ->
            LoadMoreFailed (transform window) request error

        _ ->
            state


selectInWindow : Entry -> Window -> Window
selectInWindow entry window =
    { window | selection = Selected entry }


requestAccount : Request -> String
requestAccount request =
    case request of
        InitialRequest data ->
            data.account

        RefreshRequest data ->
            data.account

        NextPageRequest data ->
            data.account


requestFilters : Request -> Filters
requestFilters request =
    case request of
        InitialRequest data ->
            data.filters

        RefreshRequest data ->
            data.filters

        NextPageRequest data ->
            data.filters


requestFrom : Request -> Maybe String
requestFrom request =
    (requestFilters request).from


requestTo : Request -> Maybe String
requestTo request =
    (requestFilters request).to


requestCursor : Request -> Maybe String
requestCursor request =
    case request of
        NextPageRequest data ->
            Just data.cursor

        _ ->
            Nothing


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
    responseFor data.state



-- VIEW


view : Theme -> Viewport -> Model -> Element Msg
view theme viewport (Model data) =
    let
        device =
            Element.classifyDevice viewport

        commodity =
            responseFor data.state
                |> Maybe.map .displayCommodity
                |> Maybe.withDefault ""

        selection =
            selectionFor data.state
    in
    if viewport.width >= 1000 then
        case selection of
            NoDetail ->
                listPage theme device (Model data)

            DetailShown entry ->
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
        case selection of
            NoDetail ->
                listPage theme device (Model data)

            DetailShown entry ->
                detailPage theme device CloseDetail commodity entry


type ViewSelection
    = NoDetail
    | DetailShown Entry


selectionFor : State -> ViewSelection
selectionFor state =
    case selectionIn state of
        Just entry ->
            DetailShown entry

        Nothing ->
            NoDetail


selectionIn : State -> Maybe Entry
selectionIn state =
    case state of
        Showing window ->
            selectedEntry window.selection

        Refreshing window _ ->
            selectedEntry window.selection

        RefreshFailed window _ ->
            selectedEntry window.selection

        LoadingMore window _ ->
            selectedEntry window.selection

        LoadMoreFailed window _ _ ->
            selectedEntry window.selection

        _ ->
            Nothing


selectedEntry : Selection -> Maybe Entry
selectedEntry selection =
    case selection of
        NoSelection ->
            Nothing

        Selected entry ->
            Just entry


responseFor : State -> Maybe Response
responseFor state =
    currentWindowFromState state |> Maybe.map .response


currentWindowFromState : State -> Maybe Window
currentWindowFromState state =
    case state of
        Showing window ->
            Just window

        Refreshing window _ ->
            Just window

        RefreshFailed window _ ->
            Just window

        LoadingMore window _ ->
            Just window

        LoadMoreFailed window _ _ ->
            Just window

        _ ->
            Nothing


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
    case data.state of
        NotStarted ->
            Ui.card theme [ Ui.loading theme "Loading register…" ]

        Loading _ _ ->
            Ui.card theme [ Ui.skeletonLoading theme ]

        LoadFailed _ error ->
            Ui.card theme
                [ Ui.errorBanner theme
                    ("Register failed to load: " ++ ApiError.message error)
                ]

        Showing window ->
            registerContent theme device (Model data) window.response

        Refreshing window _ ->
            registerContent theme device (Model data) window.response

        RefreshFailed window error ->
            Element.column [ Element.width Element.fill, Element.spacing 12 ]
                [ Ui.errorBanner theme
                    ("Refresh failed; showing stale register: " ++ ApiError.message error)
                , registerContent theme device (Model data) window.response
                ]

        LoadingMore window _ ->
            registerContent theme device (Model data) window.response

        LoadMoreFailed window _ error ->
            Element.column [ Element.width Element.fill, Element.spacing 12 ]
                [ Ui.errorBanner theme
                    ("Loading more entries failed: " ++ ApiError.message error)
                , registerContent theme device (Model data) window.response
                ]


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
    if isLoadingMore data.state then
        Ui.loading theme "Loading more entries…"

    else
        case response.nextCursor of
            Just _ ->
                Ui.secondaryButton theme { label = "Load more", onPress = LoadMore }

            Nothing ->
                Typography.mutedCaption theme "All matching entries are loaded."


isLoadingMore : State -> Bool
isLoadingMore state =
    case state of
        LoadingMore _ _ ->
            True

        _ ->
            False


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
        |> Maybe.map formatQuantity
        |> Maybe.withDefault "—"


formatQuantity : Quantity -> String
formatQuantity quantity =
    Decimal.toString (Quantity.quantity quantity) ++ " " ++ Quantity.commodity quantity


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
