module Page.AccountRegister exposing
    ( Model
    , Request
    , beginLoading
    , beginLoadingMore
    , clearDetail
    , init
    , receive
    , requestAccount
    , requestCursor
    , requestFrom
    , requestTo
    , selectDetail
    , setFrom
    , setTo
    , view
    )

import Api.Error as ApiError exposing (ApiError)
import Api.Metadata as Metadata exposing (Freshness)
import Api.Register exposing (Completeness(..), Entry, Quantity, Response, Value)
import Element exposing (Device, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import RemoteData exposing (RemoteData(..))
import State.Viewport exposing (Viewport)
import Ui
import Ui.Icon as Icon
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography


type alias Model =
    { remote : RemoteData Response
    , account : String
    , from : String
    , to : String
    , nextToken : Int
    , pending : Maybe Request
    , loadingMore : Bool
    , detail : Detail
    }


type Detail
    = NoDetail
    | Detail Entry


type alias Request =
    { token : Int
    , account : String
    , from : Maybe String
    , to : Maybe String
    , cursor : Maybe String
    }


type alias ViewConfig msg =
    { model : Model
    , onBack : msg
    , onFromChange : String -> msg
    , onToChange : String -> msg
    , onRefresh : msg
    , onLoadMore : msg
    , onDetail : Entry -> msg
    , onCloseDetail : msg
    }


init : Model
init =
    { remote = NotAsked
    , account = ""
    , from = ""
    , to = ""
    , nextToken = 0
    , pending = Nothing
    , loadingMore = False
    , detail = NoDetail
    }


beginLoading : String -> Model -> ( Model, Request )
beginLoading account model =
    let
        resetRemote =
            if account /= model.account then
                NotAsked

            else
                model.remote
    in
    issue account
        (Just (blankToNothing model.from))
        (Just (blankToNothing model.to))
        Nothing
        { model | account = account, remote = resetRemote, detail = NoDetail, loadingMore = False }


beginLoadingMore : Model -> Maybe ( Model, Request )
beginLoadingMore model =
    case ( RemoteData.value model.remote, model.pending, model.loadingMore ) of
        ( Just response, Nothing, False ) ->
            response.nextCursor
                |> Maybe.map
                    (\cursor ->
                        issue model.account
                            (Just (blankToNothing model.from))
                            (Just (blankToNothing model.to))
                            (Just cursor)
                            { model | loadingMore = True }
                    )

        _ ->
            Nothing


issue : String -> Maybe (Maybe String) -> Maybe (Maybe String) -> Maybe String -> Model -> ( Model, Request )
issue account maybeFrom maybeTo cursor model =
    let
        request =
            { token = model.nextToken + 1
            , account = account
            , from = Maybe.withDefault Nothing maybeFrom
            , to = Maybe.withDefault Nothing maybeTo
            , cursor = cursor
            }

        remote =
            if cursor == Nothing then
                RemoteData.beginLoading model.remote

            else
                model.remote
    in
    ( { model | nextToken = request.token, pending = Just request, remote = remote }, request )


receive : Request -> Result ApiError Response -> Model -> Model
receive request result model =
    if model.pending /= Just request then
        model

    else
        case result of
            Ok response ->
                let
                    merged =
                        case ( request.cursor, RemoteData.value model.remote ) of
                            ( Just _, Just current ) ->
                                { response | entries = current.entries ++ response.entries }

                            _ ->
                                response
                in
                { model | remote = Success merged, pending = Nothing, loadingMore = False }

            Err error ->
                { model
                    | remote = Failure error (RemoteData.value model.remote)
                    , pending = Nothing
                    , loadingMore = False
                }


setFrom : String -> Model -> Model
setFrom value model =
    { model | from = value }


setTo : String -> Model -> Model
setTo value model =
    { model | to = value }


selectDetail : Entry -> Model -> Model
selectDetail entry model =
    { model | detail = Detail entry }


clearDetail : Model -> Model
clearDetail model =
    { model | detail = NoDetail }


requestAccount : Request -> String
requestAccount =
    .account


requestFrom : Request -> Maybe String
requestFrom =
    .from


requestTo : Request -> Maybe String
requestTo =
    .to


requestCursor : Request -> Maybe String
requestCursor =
    .cursor


blankToNothing : String -> Maybe String
blankToNothing value =
    if String.isEmpty value then
        Nothing

    else
        Just value


view : Theme -> Viewport -> ViewConfig msg -> Element msg
view theme viewport config =
    let
        device =
            Element.classifyDevice viewport

        commodity =
            config.model.remote
                |> RemoteData.value
                |> Maybe.map .displayCommodity
                |> Maybe.withDefault ""
    in
    if viewport.width >= 1000 then
        case config.model.detail of
            NoDetail ->
                listPage theme device config

            detail ->
                Element.row [ Element.width Element.fill, Element.height Element.fill, Element.clip ]
                    [ Element.el
                        [ Element.width (Element.fillPortion 2)
                        , Element.height Element.fill
                        , Element.scrollbarY
                        ]
                        (listPage theme device config)
                    , Element.el
                        [ Element.width (Element.fillPortion 3)
                        , Element.height Element.fill
                        , Element.scrollbarY
                        ]
                        (detailPage theme device config.onCloseDetail commodity detail)
                    ]

    else
        case config.model.detail of
            NoDetail ->
                listPage theme device config

            detail ->
                detailPage theme device config.onCloseDetail commodity detail


listPage : Theme -> Device -> ViewConfig msg -> Element msg
listPage theme device config =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbarY
        , Background.color theme.pageBg
        , Element.htmlAttribute (Html.Attributes.class "page-scroll account-register-page")
        ]
        [ Ui.pageHeaderBand theme device <|
            registerHeader theme device config
        , Ui.pageContainer device [ filters theme config, content theme device config ]
        ]


registerHeader : Theme -> Device -> ViewConfig msg -> Element msg
registerHeader theme device config =
    if Ui.isSmall device then
        Element.column [ Element.width Element.fill, Element.spacing 16, Typography.minWidthZero ]
            [ Element.row [ Element.width Element.fill, Element.spacing 8 ]
                [ Ui.ghostButton theme { label = "← Accounts", onPress = config.onBack }
                , Element.el [ Element.alignRight ]
                    (Ui.ghostButton theme { label = "Refresh", onPress = config.onRefresh })
                ]
            , Element.column [ Element.width Element.fill, Element.spacing 8, Typography.minWidthZero ]
                [ Typography.pageTitle theme (breakAccountPath config.model.account)
                , Typography.mutedParagraph theme "Account register"
                ]
            ]

    else
        Ui.pageHeader theme
            { leading = Just (Ui.ghostButton theme { label = "← Accounts", onPress = config.onBack })
            , title = Typography.pageTitle theme config.model.account
            , subtitle = Just "Account register"
            , actions = [ Ui.ghostButton theme { label = "Refresh", onPress = config.onRefresh } ]
            }


breakAccountPath : String -> String
breakAccountPath =
    String.replace ":" ":\u{200B}"


filters : Theme -> ViewConfig msg -> Element msg
filters theme config =
    Ui.card theme
        [ Element.wrappedRow [ Element.width Element.fill, Element.spacing 12 ]
            [ Element.el [ Element.width (Element.fill |> Element.maximum 220) ] <|
                Ui.dateInput theme
                    { label = "From"
                    , value = config.model.from
                    , onChange = config.onFromChange
                    }
            , Element.el [ Element.width (Element.fill |> Element.maximum 220) ] <|
                Ui.dateInput theme { label = "To", value = config.model.to, onChange = config.onToChange }
            ]
        ]


content : Theme -> Device -> ViewConfig msg -> Element msg
content theme device config =
    case config.model.remote of
        NotAsked ->
            Ui.card theme [ Ui.loading theme "Loading register…" ]

        Loading Nothing ->
            Ui.card theme [ Ui.skeletonLoading theme ]

        Loading (Just response) ->
            registerContent theme device config response

        Success response ->
            registerContent theme device config response

        Failure error stale ->
            case stale of
                Just response ->
                    Element.column [ Element.width Element.fill, Element.spacing 12 ]
                        [ Ui.errorBanner theme
                            ("Refresh failed; showing stale register: " ++ ApiError.message error)
                        , registerContent theme device config response
                        ]

                Nothing ->
                    Ui.card theme
                        [ Ui.errorBanner theme
                            ("Register failed to load: " ++ ApiError.message error)
                        ]


registerContent : Theme -> Device -> ViewConfig msg -> Response -> Element msg
registerContent theme device config response =
    Element.column [ Element.width Element.fill, Element.spacing 12 ]
        [ freshnessNotice theme response
        , if List.isEmpty response.entries then
            Ui.card theme [ Ui.emptyState theme "No register entries match this date range." ]

          else
            registerRows theme device config response.displayCommodity response.entries
        , paging theme config response
        ]


freshnessNotice : Theme -> Response -> Element msg
freshnessNotice theme response =
    case response.metadata.freshness of
        Metadata.Fresh ->
            Element.none

        Metadata.Refreshing ->
            Ui.statusBadge theme Ui.StatusPrimary "Refreshing register"

        Metadata.Stale ->
            Ui.statusBadge theme Ui.StatusWarning "Showing a stale journal snapshot"

        Metadata.Partial ->
            Ui.statusBadge theme Ui.StatusWarning "Register data is partial"

        Metadata.Unavailable ->
            Ui.errorBanner theme "Register data is unavailable."


paging : Theme -> ViewConfig msg -> Response -> Element msg
paging theme config response =
    if config.model.loadingMore then
        Ui.loading theme "Loading more entries…"

    else
        case response.nextCursor of
            Just _ ->
                Ui.secondaryButton theme { label = "Load more", onPress = config.onLoadMore }

            Nothing ->
                Typography.mutedCaption theme "All matching entries are loaded."


registerRows : Theme -> Device -> ViewConfig msg -> String -> List Entry -> Element msg
registerRows theme device config commodity entries =
    Element.column [ Element.width Element.fill, Element.spacing 12 ]
        (List.map (monthSection theme device config commodity) (groupMonths entries))


monthSection : Theme -> Device -> ViewConfig msg -> String -> ( String, List Entry ) -> Element msg
monthSection theme device config commodity ( month, entries ) =
    Element.column [ Element.width Element.fill, Element.spacing 4 ]
        (Element.el
            [ Element.paddingEach { top = 8, right = 0, bottom = 4, left = 4 } ]
            (Typography.subsectionTitle theme month)
            :: List.map (entryRow theme device config commodity) entries
        )


groupMonths : List Entry -> List ( String, List Entry )
groupMonths entries =
    List.foldr
        (\entry groups ->
            let
                month =
                    String.left 7 entry.date
            in
            case groups of
                ( current, grouped ) :: rest ->
                    if current == month then
                        ( current, entry :: grouped ) :: rest

                    else
                        ( month, [ entry ] ) :: groups

                [] ->
                    [ ( month, [ entry ] ) ]
        )
        []
        entries


entryRow : Theme -> Device -> ViewConfig msg -> String -> Entry -> Element msg
entryRow theme device config commodity entry =
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
                        [ Element.text entry.date |> Element.el [ Font.size 12, Font.color theme.textMuted ]
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
                        [ Element.text entry.date
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
            { onPress = Just (config.onDetail entry), label = rowContent }

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
    quantity.quantity ++ " " ++ quantity.commodity


detailPage : Theme -> Device -> msg -> String -> Detail -> Element msg
detailPage theme device onClose commodity detail =
    let
        entry =
            case detail of
                Detail selected ->
                    selected

                NoDetail ->
                    { date = "", description = "", account = "", amount = emptyValue, balance = emptyValue }
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbarY
        , Background.color theme.pageBg
        , Element.htmlAttribute (Html.Attributes.class "page-scroll register-detail-page")
        ]
        [ Ui.pageHeaderBand theme device <|
            Element.row [ Element.width Element.fill, Element.spacing 8 ]
                [ Ui.ghostButton theme { label = "← Back", onPress = onClose }
                , Typography.sectionTitle theme "Details"
                ]
        , Ui.pageContainer device
            [ Ui.card theme
                [ Typography.sectionTitle theme entry.description
                , Element.text (entry.date ++ " · " ++ entry.account)
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
        , completenessBadge theme value.completeness
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


completenessBadge : Theme -> Completeness -> Element msg
completenessBadge theme completeness =
    case completeness of
        Complete ->
            Ui.statusBadge theme Ui.StatusSuccess "Complete valuation"

        Partial ->
            Ui.statusBadge theme Ui.StatusWarning "Partial valuation"

        Unavailable ->
            Ui.statusBadge theme Ui.StatusDanger "Valuation unavailable"


rawRow : Theme -> Quantity -> Element msg
rawRow theme quantity =
    Element.row
        [ Element.width Element.fill
        , Element.paddingXY 10 8
        , Background.color theme.inputFill
        , Border.rounded 6
        ]
        [ Element.text quantity.commodity
            |> Element.el [ Element.width Element.fill, Font.color theme.textMedium ]
        , Element.text quantity.quantity |> Element.el [ Font.semiBold ]
        ]
