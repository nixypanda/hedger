module View.Analytics.Activity.Ring exposing
    ( activityAccessibleLabel
    , activityDayRing
    , activityMonthRing
    , amountTextColor
    , directionSegments
    , monthRingCenter
    , presentDirections
    , ringAccent
    , ringEmptyColor
    )

import Data.Analytics
    exposing
        ( ActivityBucket
        , ActivityDirection(..)
        , ActivityMovement(..)
        , BreakdownItem
        )
import Data.Decimal as Decimal exposing (Decimal)
import Data.Formatting as Formatting
import Element exposing (Element)
import Html exposing (Html)
import Ui.Chart as Chart
import Ui.Chart.Ring as Ring
import Ui.Color as Color
import Ui.Theme exposing (Theme)
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
        ( ActivityTone
        , CalendarDisplay(..)
        , DirectionSpec
        )


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
