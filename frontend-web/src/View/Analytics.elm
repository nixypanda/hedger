module View.Analytics exposing
    ( activityMonth
    , activityTransactions
    , activityYear
    , warningList
    )

import Data.ActivityCalendar as ActivityCalendar
import Data.Analytics
    exposing
        ( ActivityBucket
        , ActivityDirection(..)
        , ActivityMovement(..)
        , ActivityResponse
        , BreakdownItem
        )
import Data.Date as Date
import Data.Decimal as Decimal exposing (Decimal)
import Data.Formatting as Formatting
import Data.Transaction exposing (Transaction)
import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attr
import List
import Ui.Calendar.Month as MonthCalendar
import Ui.Chart as Chart
import Ui.Chart.Ring as Ring
import Ui.Chip as Chip
import Ui.Color as Color
import Ui.Common as Common
import Ui.DesignTokens as Tokens
import Ui.Feedback as Feedback
import Ui.Surface as Surface
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import View.Analytics.Category exposing (categoryLabel)
import View.Analytics.Palette
    exposing
        ( activityCategoryColor
        , activityColor
        , directionSpecsFor
        , flowMovementColors
        , otherColor
        , scaleColor
        )
import View.Analytics.Types
    exposing
        ( ActivityTone(..)
        , CalendarDisplay(..)
        , DirectionSpec
        , LegendState(..)
        )


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
                , cellHeight = 72
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
        , Element.height Element.fill
        ]
        theme
        [ Typography.subsectionTitle theme config.title
        , calendar
        , activityLegend theme
            config.calendarDisplay
            config.maxVisible
            config.tone
            categories
            config.legendState
            config.onLegendToggle
            config.highlightedCategory
            config.onCategoryHighlight
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
        , config.response
            |> Maybe.map (.warnings >> warningList theme)
            |> Maybe.withDefault Element.none
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
                    (activityDayRing
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
                                [ Element.centerX
                                , Typography.caption
                                , Font.semiBold
                                , Font.color
                                    (amountTextColor theme
                                        (ringAccent theme tone bucket)
                                        maxTotal
                                        bucket.total
                                    )
                                , Typography.financeNumber
                                ]
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
                        (Attr.attribute "aria-label" (activityAccessibleLabel commodity bucket))
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


amountTextColor : Theme -> Element.Color -> Float -> Decimal -> Element.Color
amountTextColor theme strongColor maximum amount =
    if maximum <= 0 then
        theme.textMuted

    else
        let
            ratio =
                clamp 0 1 (abs (Decimal.toFloat amount) / maximum)
        in
        Color.mix theme.textMuted strongColor (sqrt ratio)


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


type alias ActivityCategoryLegendConfig msg =
    { theme : Theme
    , tone : ActivityTone
    , categories : List BreakdownItem
    , maxVisible : Int
    , legendState : LegendState
    , onToggle : msg
    , highlightedCategory : Maybe Int
    , onCategoryHighlight : Int -> msg
    }


activityCategoryLegend : ActivityCategoryLegendConfig msg -> Element msg
activityCategoryLegend config =
    if List.isEmpty config.categories then
        Element.none

    else
        let
            theme =
                config.theme

            tone =
                config.tone

            isExpanded =
                config.legendState == LegendExpanded

            indexedCategories =
                List.indexedMap Tuple.pair config.categories

            hiddenCount =
                max 0 (List.length config.categories - config.maxVisible)

            chip ( index, category ) =
                let
                    dotColor =
                        if category.account == "Other" then
                            otherColor theme

                        else
                            activityCategoryColor theme tone index
                in
                Chip.legendButton theme
                    { accent = dotColor
                    , selected = config.highlightedCategory == Just index
                    , onPress = config.onCategoryHighlight index
                    , label =
                        Element.row [ Element.spacing Tokens.space5 ]
                            [ Surface.accentDot dotColor
                            , Typography.mutedCaption theme (categoryLabel category.account)
                            ]
                    }

            toggle text =
                Chip.legendToggleButton theme
                    { onPress = config.onToggle
                    , label = Typography.mutedCaption theme text
                    }
        in
        if isExpanded then
            Element.wrappedRow
                [ Element.width Element.fill, Element.spacing Tokens.space10 ]
                (List.map chip indexedCategories
                    ++ (if hiddenCount > 0 then
                            [ toggle "show less" ]

                        else
                            []
                       )
                )

        else
            Element.row
                [ Element.width Element.fill, Element.spacing Tokens.space10 ]
                [ Element.el
                    [ Element.width Element.fill
                    , Element.clip
                    , Common.minWidthZero
                    ]
                    (Element.row
                        [ Element.spacing Tokens.space10 ]
                        (List.map chip (List.take config.maxVisible indexedCategories))
                    )
                , if hiddenCount > 0 then
                    toggle ("+" ++ String.fromInt hiddenCount ++ " more")

                  else
                    Element.none
                ]


activityDayRing :
    Theme
    -> ActivityTone
    -> CalendarDisplay
    -> List BreakdownItem
    -> Maybe Int
    -> Bool
    -> Int
    -> Maybe ActivityBucket
    -> Element msg
activityDayRing theme tone display categories highlightedCategory selected day maybeBucket =
    let
        size =
            34

        segments =
            maybeBucket
                |> Maybe.map
                    (activityRingSegments theme tone display categories highlightedCategory)
                |> Maybe.withDefault []

        emptyColor =
            case maybeBucket of
                Nothing ->
                    theme.transparent

                Just bucket ->
                    ringEmptyColor theme tone bucket segments
    in
    Ring.view
        { size = Ring.Pixels size
        , radius = toFloat size / 2
        , thickness = 4
        , gap = 2
        , segments = segments
        , emptyColor = emptyColor
        , centerColor =
            if selected then
                theme.selectedBg

            else
                theme.cardBg
        , selected = False
        , onPress = Nothing
        , label = Nothing
        , center =
            Chart.ringDayLabel theme (String.fromInt day)
        }


activityMonthRing :
    Theme
    -> String
    -> ActivityTone
    -> CalendarDisplay
    -> List BreakdownItem
    -> Maybe Int
    -> Bool
    -> Float
    -> ActivityBucket
    -> (ActivityBucket -> msg)
    -> Element msg
activityMonthRing theme commodity tone display groups highlight selected maxTotal bucket onSelect =
    let
        isEmpty =
            isEmptyActivityBucket bucket

        segments =
            activityRingSegments theme tone display groups highlight bucket

        monthLabel =
            String.left 3 (Formatting.periodDisplayName bucket.key)
    in
    Ring.view
        { size = Ring.Fill { minimum = 70, maximum = 110 }
        , radius = 45
        , thickness = 12
        , gap = 2
        , segments = segments
        , emptyColor = ringEmptyColor theme tone bucket segments
        , centerColor =
            if selected then
                theme.selectedBg

            else
                theme.cardBg
        , selected = selected
        , onPress = Just (onSelect bucket)
        , label = Just (activityAccessibleLabel commodity bucket)
        , center =
            Chart.ringMonthCenter theme
                monthLabel
                (monthRingCenter theme tone commodity isEmpty maxTotal bucket)
        }


activityRingSegments :
    Theme
    -> ActivityTone
    -> CalendarDisplay
    -> List BreakdownItem
    -> Maybe Int
    -> ActivityBucket
    -> List Ring.Segment
activityRingSegments theme tone display categories highlightedCategory bucket =
    case bucket.movement of
        AssetMovements _ ->
            directionSegments
                (Maybe.withDefault [] (directionSpecsFor theme tone))
                highlightedCategory
                bucket

        LiabilityMovements figures ->
            [ { color = theme.borrowingFg, value = Decimal.toFloat figures.borrowed }
            , { color = theme.repaymentFg, value = Decimal.toFloat figures.repaid }
            , { color = theme.warningFg, value = Decimal.toFloat figures.forgiven }
            , { color = theme.textMuted, value = Decimal.toFloat figures.borrowingReversed }
            ]

        IncomeMovements figures ->
            case display of
                ByMovement ->
                    flowRingSegments theme tone figures

                ByCategory ->
                    categoryRingSegments theme tone categories highlightedCategory bucket

        ExpenseMovements figures ->
            case display of
                ByMovement ->
                    flowRingSegments theme tone figures

                ByCategory ->
                    categoryRingSegments theme tone categories highlightedCategory bucket


flowRingSegments : Theme -> ActivityTone -> Data.Analytics.FlowFigures -> List Ring.Segment
flowRingSegments theme tone figures =
    let
        ( postedColor, reversedColor ) =
            flowMovementColors theme tone
    in
    [ { color = postedColor, value = Decimal.toFloat figures.posted }
    , { color = reversedColor, value = Decimal.toFloat figures.reversed }
    ]


categoryRingSegments :
    Theme
    -> ActivityTone
    -> List BreakdownItem
    -> Maybe Int
    -> ActivityBucket
    -> List Ring.Segment
categoryRingSegments theme tone categories highlighted bucket =
    bucket.categories
        |> List.map
            (\category ->
                let
                    index =
                        categoryIndex category.account categories

                    baseColor =
                        if category.account == "Other" then
                            otherColor theme

                        else
                            activityCategoryColor theme tone index
                in
                { color =
                    if highlighted == Nothing || highlighted == Just index then
                        baseColor

                    else
                        Color.withAlpha 0.14 baseColor
                , value = abs (Decimal.toFloat category.quantity)
                }
            )


ringEmptyColor : Theme -> ActivityTone -> ActivityBucket -> List Ring.Segment -> Element.Color
ringEmptyColor theme tone bucket segments =
    if isEmptyActivityBucket bucket then
        theme.stale

    else if List.all (\segment -> segment.value <= 0) segments then
        theme.primary

    else
        ringAccent theme tone bucket


categoryIndex : String -> List BreakdownItem -> Int
categoryIndex account categories =
    categories
        |> List.indexedMap Tuple.pair
        |> List.filter (Tuple.second >> .account >> (==) account)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 0


isEmptyActivityBucket : ActivityBucket -> Bool
isEmptyActivityBucket bucket =
    case bucket.movement of
        AssetMovements figures ->
            List.isEmpty figures.events

        LiabilityMovements figures ->
            List.isEmpty figures.events

        IncomeMovements figures ->
            List.isEmpty figures.events

        ExpenseMovements figures ->
            List.isEmpty figures.events


activityTransactionList : Theme -> String -> List Transaction -> (Transaction -> msg) -> Element msg
activityTransactionList theme title transactions onSelect =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
        [ Typography.subsectionTitle theme title
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
                                    , Element.spacing Tokens.space14
                                    , Element.paddingXY Tokens.space4 Tokens.space10
                                    ]
                                    [ Element.paragraph
                                        [ Element.width Element.fill
                                        , Common.minWidthZero
                                        , Typography.chartLabel
                                        ]
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
        , Element.height Element.fill
        ]
        theme
        [ Typography.subsectionTitle theme config.title
        , case config.response of
            Nothing ->
                Feedback.loading theme "Loading activity"

            Just response ->
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
                            |> padToMultiple config.columns Nothing
                in
                Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
                    [ Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
                        (chunks config.columns slots
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
                                                            (activityMonthRing theme
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
                    , activityLegend theme
                        config.calendarDisplay
                        config.maxVisible
                        config.tone
                        categories
                        config.legendState
                        config.onLegendToggle
                        config.highlightedCategory
                        config.onCategoryHighlight
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
                    , warningList theme response.warnings
                    ]
        ]


activityTransactions : Theme -> String -> List Transaction -> (Transaction -> msg) -> Element msg
activityTransactions theme title transactions onSelect =
    Surface.card theme
        [ Typography.subsectionTitle theme title
        , if List.isEmpty transactions then
            Typography.mutedParagraph theme "No contributing transactions"

          else
            Element.column [ Element.width Element.fill ]
                (List.intersperse (Surface.hairline theme)
                    (List.map
                        (\transaction ->
                            Input.button
                                [ Element.width Element.fill
                                , Common.touchTargetHeight
                                , Element.paddingXY Tokens.space0 Tokens.space10
                                ]
                                { onPress = Just (onSelect transaction)
                                , label =
                                    Element.row
                                        [ Element.width Element.fill
                                        , Element.spacing Tokens.space12
                                        ]
                                        [ Element.el [ Font.color theme.textMuted ]
                                            (Element.text (Date.toString transaction.primaryDate))
                                        , Element.el [ Element.width Element.fill ]
                                            (Element.text transaction.description)
                                        , Element.text "›"
                                        ]
                                }
                        )
                        transactions
                    )
                )
        ]


warningList : Theme -> List String -> Element msg
warningList theme warnings =
    if List.isEmpty warnings then
        Element.none

    else
        Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
            (List.map (Feedback.errorBanner theme) warnings)


directionSegments : List DirectionSpec -> Maybe Int -> ActivityBucket -> List Ring.Segment
directionSegments specs highlighted bucket =
    specs
        |> List.indexedMap
            (\specIndex spec ->
                let
                    baseColor =
                        scaleColor spec.scale 0
                in
                { color =
                    if highlighted == Nothing || highlighted == Just specIndex then
                        baseColor

                    else
                        Color.withAlpha 0.14 baseColor
                , value = Decimal.toFloat (directionAmount spec.direction bucket)
                }
            )


ringAccent : Theme -> ActivityTone -> ActivityBucket -> Element.Color
ringAccent theme tone bucket =
    case directionSpecsFor theme tone of
        Nothing ->
            activityColor theme tone

        Just specs ->
            specs
                |> List.map
                    (\spec ->
                        ( Decimal.toFloat (directionAmount spec.direction bucket)
                        , scaleColor spec.scale 0
                        )
                    )
                |> List.sortBy (Tuple.first >> negate)
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault (activityColor theme tone)


directionAmount : ActivityDirection -> ActivityBucket -> Decimal
directionAmount direction bucket =
    case bucket.movement of
        AssetMovements figures ->
            case direction of
                Increase ->
                    figures.acquired

                Reallocate ->
                    figures.transferred

                Decrease ->
                    figures.disposed

        LiabilityMovements figures ->
            case direction of
                Increase ->
                    figures.borrowed

                Decrease ->
                    figures.repaid

                Reallocate ->
                    Decimal.zero

        _ ->
            Decimal.zero


presentDirections : List DirectionSpec -> ActivityBucket -> List ( DirectionSpec, Decimal )
presentDirections specs bucket =
    specs
        |> List.filterMap
            (\spec ->
                let
                    amount =
                        directionAmount spec.direction bucket
                in
                if Decimal.toFloat amount > 0 then
                    Just ( spec, amount )

                else
                    Nothing
            )


monthRingCenter :
    Theme
    -> ActivityTone
    -> String
    -> Bool
    -> Float
    -> ActivityBucket
    -> List (Html msg)
monthRingCenter theme tone commodity isEmpty maxTotal bucket =
    case directionSpecsFor theme tone of
        Just specs ->
            case presentDirections specs bucket of
                [] ->
                    [ Chart.ringValueText theme.textMuted
                        (Formatting.formatDecimalDisplay commodity bucket.total)
                    ]

                present ->
                    List.map (directionValueText theme maxTotal commodity) present

        Nothing ->
            [ Chart.ringValueText
                (if isEmpty then
                    theme.textMuted

                 else
                    amountTextColor theme
                        (ringAccent theme tone bucket)
                        maxTotal
                        bucket.total
                )
                (Formatting.formatDecimalDisplay commodity bucket.total)
            ]


directionValueText : Theme -> Float -> String -> ( DirectionSpec, Decimal ) -> Html msg
directionValueText theme maxTotal commodity ( spec, amount ) =
    Chart.ringValueText
        (amountTextColor theme (scaleColor spec.scale 0) maxTotal amount)
        (Formatting.formatDecimalDisplay commodity amount)


activityLegend :
    Theme
    -> CalendarDisplay
    -> Int
    -> ActivityTone
    -> List BreakdownItem
    -> LegendState
    -> msg
    -> Maybe Int
    -> (Int -> msg)
    -> Element msg
activityLegend theme display limit tone categories state onToggle highlighted onHighlight =
    case directionSpecsFor theme tone of
        Just specs ->
            activityDirectionLegend theme specs highlighted onHighlight

        Nothing ->
            if display == ByCategory && (tone == IncomeTone || tone == ExpenseTone) then
                activityCategoryLegend
                    { theme = theme
                    , tone = tone
                    , categories = categories
                    , maxVisible = limit
                    , legendState = state
                    , onToggle = onToggle
                    , highlightedCategory = highlighted
                    , onCategoryHighlight = onHighlight
                    }

            else
                activityMeaningLegend theme tone


activityMeaningLegend : Theme -> ActivityTone -> Element msg
activityMeaningLegend theme tone =
    let
        meanings =
            case tone of
                LiabilityTone ->
                    [ ( "Borrowed", theme.borrowingFg )
                    , ( "Repaid", theme.repaymentFg )
                    , ( "Forgiven", theme.warningFg )
                    , ( "Borrowing reversed", theme.textMuted )
                    ]

                _ ->
                    let
                        ( postedColor, reversedColor ) =
                            flowMovementColors theme tone
                    in
                    [ ( "Posted", postedColor ), ( "Reversed", reversedColor ) ]
    in
    Element.wrappedRow [ Element.width Element.fill, Element.spacing Tokens.space10 ]
        (List.map
            (\( label, color ) ->
                Element.row [ Element.spacing Tokens.space5 ]
                    [ Surface.accentDot color, Typography.mutedCaption theme label ]
            )
            meanings
        )


movementSummary : ActivityBucket -> List ( String, Decimal )
movementSummary bucket =
    case bucket.movement of
        AssetMovements figures ->
            [ ( "Acquired", figures.acquired )
            , ( "Disposed", figures.disposed )
            , ( "Transferred", figures.transferred )
            ]

        LiabilityMovements figures ->
            [ ( "Borrowed", figures.borrowed )
            , ( "Repaid", figures.repaid )
            , ( "Forgiven", figures.forgiven )
            , ( "Borrowing reversed", figures.borrowingReversed )
            , ( "Net debt movement", figures.netDebtMovement )
            ]

        IncomeMovements figures ->
            [ ( "Posted", figures.posted )
            , ( "Reversed", figures.reversed )
            , ( "Net income", bucket.total )
            ]

        ExpenseMovements figures ->
            [ ( "Posted", figures.posted )
            , ( "Reversed", figures.reversed )
            , ( "Net expense", bucket.total )
            ]


activityAccessibleLabel : String -> ActivityBucket -> String
activityAccessibleLabel commodity bucket =
    bucket.key
        ++ " · "
        ++ (movementSummary bucket
                |> List.map
                    (\( label, amount ) ->
                        label
                            ++ " "
                            ++ Formatting.formatDecimalDisplay commodity amount
                    )
                |> String.join ", "
           )


activityDirectionLegend : Theme -> List DirectionSpec -> Maybe Int -> (Int -> msg) -> Element msg
activityDirectionLegend theme specs highlighted onHighlight =
    Element.wrappedRow [ Element.width Element.fill, Element.spacing Tokens.space10 ]
        (List.indexedMap
            (\index spec ->
                Chip.legendButton theme
                    { accent = scaleColor spec.scale 0
                    , selected = highlighted == Just index
                    , onPress = onHighlight index
                    , label =
                        Element.row [ Element.spacing Tokens.space5 ]
                            [ Surface.accentDot (scaleColor spec.scale 0)
                            , Typography.mutedCaption theme spec.label
                            ]
                    }
            )
            specs
        )


chunks : Int -> List value -> List (List value)
chunks size values =
    if List.isEmpty values then
        []

    else
        List.take size values :: chunks size (List.drop size values)


padToMultiple : Int -> value -> List value -> List value
padToMultiple size padding values =
    let
        missing =
            modBy size (size - modBy size (List.length values))
    in
    values ++ List.repeat missing padding
