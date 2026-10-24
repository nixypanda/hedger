module View.Analytics.Activity exposing
    ( activityMonth
    , activityYear
    )

import Data.ActivityCalendar as ActivityCalendar
import Data.Analytics
    exposing
        ( ActivityBucket
        , ActivityResponse
        , BreakdownItem
        )
import Data.Decimal as Decimal
import Data.Formatting as Formatting
import Data.Transaction exposing (Transaction)
import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes as Attr
import Lib.ListExtra as ListExtra
import List
import Ui.Calendar.Month as MonthCalendar
import Ui.Chip as Chip
import Ui.Common as Common
import Ui.DesignTokens as Tokens
import Ui.Skeleton as Skeleton
import Ui.Surface as Surface
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import View.Analytics.Activity.Ring as Ring
import View.Analytics.Legend as Legend
import View.Analytics.Types exposing (ActivityTone, CalendarDisplay, LegendState(..))


activityMonth :
    Theme
    ->
        { title : String
        , period : String
        , commodity : String
        , tone : ActivityTone
        , calendarDisplay : CalendarDisplay
        , response : Maybe ActivityResponse
        , selectedBucket : Maybe ActivityBucket
        , onSelect : ActivityBucket -> msg
        , onTransaction : ActivityBucket -> Transaction -> msg
        , legendState : LegendState
        , onLegendToggle : msg
        , highlightedCategory : Maybe Int
        , onCategoryHighlight : Int -> msg
        , maxVisible : Int
        }
    -> Element msg
activityMonth theme config =
    let
        bucketFor day =
            let
                key =
                    dayKey config.period day
            in
            config.response
                |> Maybe.andThen
                    (\response ->
                        List.filter (.key >> (==) key) response.buckets
                            |> List.head
                    )

        categories =
            config.response
                |> Maybe.map activityCategories
                |> Maybe.withDefault []

        maxTotal =
            config.response
                |> Maybe.map (.buckets >> maximumActivityTotal)
                |> Maybe.withDefault 0

        calendar =
            MonthCalendar.view
                { period = config.period
                , weekdayLabels = [ "S", "M", "T", "W", "T", "F", "S" ]
                , weekdayColor = theme.textMuted
                , cellHeight = Tokens.calendarCellHeight
                , viewDay =
                    \day attributes ->
                        dayCell
                            { attributes = attributes
                            , theme = theme
                            , tone = config.tone
                            , calendarDisplay = config.calendarDisplay
                            , commodity = config.commodity
                            , categories = categories
                            , highlightedCategory = config.highlightedCategory
                            , selected =
                                Maybe.map .key config.selectedBucket
                                    == Just (dayKey config.period day)
                            , day = day
                            , maybeBucket = bucketFor day
                            , maxTotal = maxTotal
                            , onSelect = config.onSelect
                            }
                }
    in
    Surface.cardWith
        [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
        , Element.alignTop
        ]
        theme
        [ activityTitleRow theme config.title (activityWarnings config.response)
        , calendar
        , case config.response of
            Nothing ->
                Skeleton.activityLegend theme

            Just _ ->
                activityLegendSlot
                    config.legendState
                    (Legend.activityLegend theme
                        config.calendarDisplay
                        config.maxVisible
                        config.tone
                        categories
                        config.legendState
                        config.onLegendToggle
                        config.highlightedCategory
                        config.onCategoryHighlight
                    )
        , case config.selectedBucket of
            Nothing ->
                Element.el
                    [ Element.width Element.fill
                    , Element.paddingXY Tokens.space0 Tokens.space12
                    ]
                    (Typography.mutedCaption theme "Select a day to see details")

            Just bucket ->
                activityTransactionList theme
                    ("Transactions · " ++ bucket.key)
                    bucket.transactions
                    (config.onTransaction bucket)
        ]


dayKey : String -> Int -> String
dayKey period day =
    period ++ "-" ++ String.padLeft 2 '0' (String.fromInt day)


type alias DayCellConfig msg =
    { attributes : List (Element.Attribute msg)
    , theme : Theme
    , tone : ActivityTone
    , calendarDisplay : CalendarDisplay
    , commodity : String
    , categories : List BreakdownItem
    , highlightedCategory : Maybe Int
    , selected : Bool
    , day : Int
    , maybeBucket : Maybe ActivityBucket
    , maxTotal : Float
    , onSelect : ActivityBucket -> msg
    }


dayCell : DayCellConfig msg -> Element msg
dayCell config =
    let
        attributes =
            config.attributes

        theme =
            config.theme

        tone =
            config.tone

        calendarDisplay =
            config.calendarDisplay

        commodity =
            config.commodity

        categories =
            config.categories

        highlightedCategory =
            config.highlightedCategory

        selected =
            config.selected

        day =
            config.day

        maybeBucket =
            config.maybeBucket

        maxTotal =
            config.maxTotal

        onSelect =
            config.onSelect

        label =
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing Tokens.space8
                , Element.paddingXY Tokens.space2 Tokens.space4
                ]
                [ Element.el [ Element.centerX ]
                    (Ring.activityDayRing
                        theme
                        tone
                        calendarDisplay
                        categories
                        highlightedCategory
                        selected
                        day
                        maybeBucket
                    )
                , maybeBucket
                    |> Maybe.map
                        (\bucket ->
                            Element.el
                                (Typography.caption
                                    ++ [ Element.centerX
                                       , Typography.fontWeight Typography.SemiBold
                                       , Font.color
                                            (Ring.amountTextColor theme
                                                (Ring.ringAccent theme tone bucket)
                                                maxTotal
                                                bucket.total
                                            )
                                       ]
                                    ++ Typography.numeric
                                )
                                (Element.text
                                    (Formatting.formatDecimalDisplay commodity bucket.total)
                                )
                        )
                    |> Maybe.withDefault Element.none
                ]
    in
    case maybeBucket of
        Nothing ->
            Element.el
                attributes
                label

        Just bucket ->
            Input.button
                (Border.rounded Tokens.radiusSmall
                    :: Element.htmlAttribute
                        (Attr.attribute
                            "aria-label"
                            (Ring.activityAccessibleLabel commodity bucket)
                        )
                    :: attributes
                )
                { onPress = Just (onSelect bucket), label = label }


activityCategories : ActivityResponse -> List BreakdownItem
activityCategories response =
    response.buckets
        |> List.concatMap .categories
        |> List.foldl addCategoryTotal []
        |> List.sortBy (Tuple.first >> negate)
        |> List.map Tuple.second


maximumActivityTotal : List ActivityBucket -> Float
maximumActivityTotal buckets =
    buckets
        |> List.map (.total >> Decimal.toFloat >> abs)
        |> List.maximum
        |> Maybe.withDefault 0


addCategoryTotal : BreakdownItem -> List ( Float, BreakdownItem ) -> List ( Float, BreakdownItem )
addCategoryTotal category totals =
    let
        amount =
            abs (Decimal.toFloat category.quantity)

        isSameAccount ( _, item ) =
            item.account == category.account
    in
    if List.any isSameAccount totals then
        List.map
            (\( total, item ) ->
                if item.account == category.account then
                    ( total + amount, item )

                else
                    ( total, item )
            )
            totals

    else
        ( amount, category ) :: totals


activityTransactionList : Theme -> String -> List Transaction -> (Transaction -> msg) -> Element msg
activityTransactionList theme title transactions onSelect =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
        [ Element.el
            (Typography.body
                ++ [ Typography.fontWeight Typography.SemiBold
                   , Font.color theme.textPrimary
                   , Region.heading 4
                   ]
            )
            (Element.text title)
        , if List.isEmpty transactions then
            Typography.mutedCaption theme "No transactions for this period"

          else
            Element.column
                [ Element.width Element.fill
                , Element.spacing Tokens.space4
                ]
                (List.map
                    (\transaction ->
                        Input.button
                            (Surface.rowSurfaceAttrs theme False ++ [ Element.width Element.fill ])
                            { onPress = Just (onSelect transaction)
                            , label =
                                Element.row
                                    [ Element.width Element.fill
                                    , Element.spacing Tokens.space12
                                    ]
                                    [ Element.paragraph
                                        (Typography.body
                                            ++ [ Element.width Element.fill
                                               , Common.minWidthZero
                                               ]
                                        )
                                        [ Element.text transaction.description ]
                                    , Element.text "›"
                                    ]
                            }
                    )
                    transactions
                )
        ]


activityYear :
    Theme
    ->
        { title : String
        , commodity : String
        , tone : ActivityTone
        , calendarDisplay : CalendarDisplay
        , response : Maybe ActivityResponse
        , selectedBucket : Maybe ActivityBucket
        , onSelect : ActivityBucket -> msg
        , onTransaction : ActivityBucket -> Transaction -> msg
        , columns : Int
        , loading : Bool
        , loadingMonths : Int
        , legendState : LegendState
        , onLegendToggle : msg
        , highlightedCategory : Maybe Int
        , onCategoryHighlight : Int -> msg
        , maxVisible : Int
        }
    -> Element msg
activityYear theme config =
    Surface.cardWith
        [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
        , Element.alignTop
        ]
        theme
        [ activityTitleRow
            theme
            config.title
            (activityWarnings config.response)
        , case ( config.loading, config.response ) of
            ( True, _ ) ->
                yearLoadingSkeleton theme config.columns config.loadingMonths

            ( False, Nothing ) ->
                yearLoadingSkeleton theme config.columns config.loadingMonths

            ( False, Just response ) ->
                let
                    categories =
                        activityCategories response

                    selected =
                        config.selectedBucket

                    displayBuckets =
                        ActivityCalendar.yearBuckets response.kind response.period response.buckets

                    maxTotal =
                        maximumActivityTotal displayBuckets

                    slots =
                        displayBuckets
                            |> List.map Just
                            |> ListExtra.padToMultiple config.columns Nothing
                in
                Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
                    [ Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
                        (ListExtra.chunksOf config.columns slots
                            |> List.map
                                (\row ->
                                    Element.row
                                        [ Element.width Element.fill
                                        , Element.spacing Tokens.space16
                                        ]
                                        (List.map
                                            (\maybeBucket ->
                                                case maybeBucket of
                                                    Just bucket ->
                                                        Element.el
                                                            [ Element.width
                                                                (Element.fillPortion
                                                                    Tokens.layoutPortionSingle
                                                                )
                                                            , Element.paddingXY
                                                                Tokens.space0
                                                                Tokens.space8
                                                            ]
                                                            (Ring.activityMonthRing theme
                                                                config.commodity
                                                                config.tone
                                                                config.calendarDisplay
                                                                categories
                                                                config.highlightedCategory
                                                                (Maybe.map .key selected
                                                                    == Just bucket.key
                                                                )
                                                                maxTotal
                                                                bucket
                                                                config.onSelect
                                                            )

                                                    Nothing ->
                                                        Element.el
                                                            [ Element.width
                                                                (Element.fillPortion
                                                                    Tokens.layoutPortionSingle
                                                                )
                                                            , Element.paddingXY
                                                                Tokens.space0
                                                                Tokens.space8
                                                            ]
                                                            Element.none
                                            )
                                            row
                                        )
                                )
                        )
                    , activityLegendSlot
                        config.legendState
                        (Legend.activityLegend theme
                            config.calendarDisplay
                            config.maxVisible
                            config.tone
                            categories
                            config.legendState
                            config.onLegendToggle
                            config.highlightedCategory
                            config.onCategoryHighlight
                        )
                    , case selected of
                        Nothing ->
                            Element.el
                                [ Element.width Element.fill
                                , Element.paddingXY Tokens.space0 Tokens.space12
                                ]
                                (Typography.mutedCaption theme "Select a month to see details")

                        Just bucket ->
                            activityTransactionList theme
                                ("Transactions · " ++ bucket.key)
                                bucket.transactions
                                (config.onTransaction bucket)
                    ]
        ]


activityTitleRow : Theme -> String -> List String -> Element msg
activityTitleRow theme title warnings =
    Element.row
        [ Element.width Element.fill
        , Element.spacing Tokens.space12
        , Element.alignTop
        ]
        [ Element.el [ Element.width Element.fill ]
            (Typography.cardTitle theme title)
        , warningIndicator theme warnings
        ]


warningIndicator : Theme -> List String -> Element msg
warningIndicator theme warnings =
    if List.isEmpty warnings then
        Element.el
            [ Element.height (Element.px Tokens.chartTitleHeight) ]
            Element.none

    else
        let
            count =
                List.length warnings

            description =
                String.join " · " warnings

            label =
                String.fromInt count
                    ++ (if count == 1 then
                            " warning"

                        else
                            " warnings"
                       )
        in
        Element.el
            [ Element.height (Element.px Tokens.chartTitleHeight)
            , Element.alignRight
            , Element.htmlAttribute (Attr.tabindex 0)
            , Element.htmlAttribute (Attr.attribute "role" "status")
            , Element.htmlAttribute (Attr.attribute "aria-live" "polite")
            , Element.htmlAttribute (Attr.title description)
            , Element.htmlAttribute (Attr.attribute "aria-label" description)
            ]
            (Element.el [ Element.centerY ]
                (Chip.statusBadge theme Chip.StatusWarning label)
            )


activityLegendSlot : LegendState -> Element msg -> Element msg
activityLegendSlot state legend =
    case state of
        LegendExpanded ->
            legend

        LegendCollapsed ->
            Element.el
                [ Element.width Element.fill
                , Element.height (Element.shrink |> Element.minimum Tokens.minimumTouchTarget)
                ]
                legend


activityWarnings : Maybe ActivityResponse -> List String
activityWarnings response =
    response
        |> Maybe.map .warnings
        |> Maybe.withDefault []


yearLoadingSkeleton : Theme -> Int -> Int -> Element msg
yearLoadingSkeleton theme columns loadingMonths =
    let
        rows =
            (loadingMonths + columns - 1) // columns
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing Tokens.space12
        ]
        [ Element.column
            [ Element.width Element.fill
            , Element.spacing Tokens.space16
            ]
            (List.map
                (\_ -> yearLoadingRow theme columns)
                (List.range 1 rows)
            )
        , Skeleton.activityLegend theme
        , activityDetailsSkeleton theme
        ]


yearLoadingRow : Theme -> Int -> Element msg
yearLoadingRow theme columns =
    Element.row
        [ Element.width Element.fill
        , Element.spacing Tokens.space16
        ]
        (List.map
            (\_ ->
                Element.el
                    [ Element.width
                        (Element.fillPortion Tokens.layoutPortionSingle)
                    , Element.paddingXY Tokens.space0 Tokens.space8
                    ]
                    (Skeleton.activityRing theme)
            )
            (List.range 1 columns)
        )


activityDetailsSkeleton : Theme -> Element msg
activityDetailsSkeleton theme =
    Element.el
        [ Element.width Element.fill
        , Element.height
            (Element.px (Tokens.iconSizeTiny + 2 * Tokens.space12))
        , Element.paddingXY Tokens.space0 Tokens.space12
        ]
        (Skeleton.block theme Tokens.skeletonLineShort Tokens.iconSizeTiny)
