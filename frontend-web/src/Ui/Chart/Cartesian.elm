module Ui.Chart.Cartesian exposing
    ( Config
    , Domain(..)
    , MarkerMode(..)
    , Series(..)
    , view
    )

import Element exposing (Element)
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Ui.Chart as Chart
import Ui.Css as Css
import Ui.Theme exposing (Theme)


type Domain
    = SymmetricAroundZero
    | IncludingBaseline
        { baseline : Float
        , minimumSpan : Float
        }
    | PaddedExtent
        { ratio : Float
        , minimum : Float
        }


type MarkerMode
    = SelectedMarkers
    | AllMarkers


type Series item
    = Bars
        { value : item -> Float
        , baseline : Float
        , color : Element.Color
        }
    | Line
        { value : item -> Maybe Float
        , color : Element.Color
        , width : Float
        , markers : MarkerMode
        }
    | AreaBetween
        { first : item -> Maybe Float
        , second : item -> Maybe Float
        , positiveColor : Element.Color
        , negativeColor : Element.Color
        , opacity : Float
        }


type alias Config item msg =
    { availableWidth : Int
    , height : Int
    , targetSlotWidth : Int
    , label : String
    , items : List item
    , key : item -> String
    , xLabel : item -> Maybe String
    , describe : item -> String
    , selected : Maybe String
    , onSelect : String -> msg
    , formatTick : Float -> String
    , domain : Domain
    , series : List (Series item)
    }


view : Theme -> Config item msg -> Element msg
view theme config =
    let
        chartLayout =
            Chart.layout
                config.availableWidth
                (List.length config.items)
                config.height
                config.targetSlotWidth

        ( low, high ) =
            domainRange config.domain config.series config.items

        y value =
            chartLayout.plotTop
                + ((high - value)
                    / max 0.000001 (high - low)
                    * chartLayout.plotHeight
                  )

        ticks =
            List.range 0 4
                |> List.map (\index -> low + toFloat index * (high - low) / 4)

        x index =
            (toFloat index + 0.5) * chartLayout.slotWidth

        selectedBand index item =
            Chart.activeBand
                theme
                chartLayout
                (config.selected == Just (config.key item))
                (x index)

        bars =
            config.series
                |> List.concatMap
                    (\series ->
                        case series of
                            Bars bar ->
                                config.items
                                    |> List.indexedMap
                                        (\index item ->
                                            let
                                                value =
                                                    bar.value item
                                            in
                                            Chart.roundedBar
                                                bar.color
                                                chartLayout.barWidth
                                                (x index)
                                                (y bar.baseline)
                                                (y value)
                                                (value - bar.baseline)
                                        )

                            _ ->
                                []
                    )

        areas =
            config.series
                |> List.concatMap
                    (\series ->
                        case series of
                            AreaBetween area ->
                                adjacentPairs config.items
                                    |> List.indexedMap
                                        (areaSegments x y area)
                                    |> List.concat

                            _ ->
                                []
                    )

        lines =
            config.series
                |> List.concatMap
                    (\series ->
                        case series of
                            Line line ->
                                lineSegments x y line.value config.items
                                    |> List.map
                                        (\points ->
                                            Svg.polyline
                                                [ SvgA.points points
                                                , SvgA.fill "none"
                                                , SvgA.stroke (Css.toCssColor line.color)
                                                , SvgA.strokeWidth (float line.width)
                                                , SvgA.strokeLinecap "round"
                                                , SvgA.strokeLinejoin "round"
                                                , SvgA.pointerEvents "none"
                                                ]
                                                []
                                        )

                            _ ->
                                []
                    )

        markers index item =
            config.series
                |> List.filterMap
                    (\series ->
                        case series of
                            Line line ->
                                if
                                    line.markers
                                        == AllMarkers
                                        || (line.markers
                                                == SelectedMarkers
                                                && config.selected
                                                == Just (config.key item)
                                           )
                                then
                                    line.value item
                                        |> Maybe.map (y >> Chart.marker theme line.color (x index))

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )

        annotations index item =
            let
                itemKey =
                    config.key item
            in
            (config.xLabel item
                |> Maybe.map (Chart.xAxisLabel theme chartLayout (x index))
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
            )
                ++ markers index item
                ++ [ Chart.hitTarget
                        chartLayout
                        (x index)
                        itemKey
                        (config.describe item)
                        (config.onSelect itemKey)
                   ]

        plot =
            Svg.svg
                (Chart.plotAttributes chartLayout config.label)
                (Chart.gridLines theme chartLayout y ticks
                    ++ List.indexedMap selectedBand config.items
                    ++ areas
                    ++ bars
                    ++ lines
                    ++ List.concat (List.indexedMap annotations config.items)
                )
    in
    Chart.fixedAxis theme config.formatTick chartLayout ticks y plot


domainRange : Domain -> List (Series item) -> List item -> ( Float, Float )
domainRange domain series items =
    let
        values =
            List.concatMap
                (\item -> List.concatMap (seriesValues item) series)
                items
    in
    case domain of
        SymmetricAroundZero ->
            let
                high =
                    max 1
                        (values
                            |> List.map abs
                            |> List.maximum
                            |> Maybe.withDefault 1
                        )
            in
            ( -high, high )

        IncludingBaseline baselineConfig ->
            let
                minimum =
                    min baselineConfig.baseline
                        (List.minimum values |> Maybe.withDefault baselineConfig.baseline)

                maximum =
                    max baselineConfig.baseline
                        (List.maximum values |> Maybe.withDefault baselineConfig.baseline)
            in
            ( minimum, max maximum (minimum + baselineConfig.minimumSpan) )

        PaddedExtent paddingConfig ->
            let
                minimum =
                    List.minimum values |> Maybe.withDefault 0

                maximum =
                    List.maximum values |> Maybe.withDefault 1

                padding =
                    max paddingConfig.minimum
                        ((maximum - minimum) * paddingConfig.ratio)
            in
            ( minimum - padding, maximum + padding )


seriesValues : item -> Series item -> List Float
seriesValues item series =
    case series of
        Bars bar ->
            [ bar.baseline, bar.value item ]

        Line line ->
            line.value item |> Maybe.map List.singleton |> Maybe.withDefault []

        AreaBetween area ->
            List.filterMap identity [ area.first item, area.second item ]


lineSegments :
    (Int -> Float)
    -> (Float -> Float)
    -> (item -> Maybe Float)
    -> List item
    -> List String
lineSegments x y value items =
    items
        |> List.indexedMap
            (\index item ->
                value item
                    |> Maybe.map
                        (\pointValue ->
                            float (x index) ++ "," ++ float (y pointValue)
                        )
            )
        |> contiguousValues
        |> List.map (String.join " ")


contiguousValues : List (Maybe value) -> List (List value)
contiguousValues values =
    let
        finish current completed =
            if List.isEmpty current then
                completed

            else
                List.reverse current :: completed

        collect remaining current completed =
            case remaining of
                [] ->
                    finish current completed |> List.reverse

                (Just value) :: rest ->
                    collect rest (value :: current) completed

                Nothing :: rest ->
                    collect rest [] (finish current completed)
    in
    collect values [] []


areaSegments :
    (Int -> Float)
    -> (Float -> Float)
    ->
        { first : item -> Maybe Float
        , second : item -> Maybe Float
        , positiveColor : Element.Color
        , negativeColor : Element.Color
        , opacity : Float
        }
    -> Int
    -> ( item, item )
    -> List (Svg msg)
areaSegments x y area index ( firstItem, secondItem ) =
    let
        firstX =
            x index

        secondX =
            x (index + 1)

        polygon color points =
            Svg.polygon
                [ SvgA.points
                    (points
                        |> List.map
                            (\( pointX, value ) ->
                                float pointX ++ "," ++ float (y value)
                            )
                        |> String.join " "
                    )
                , SvgA.fill (Css.toCssColor color)
                , SvgA.opacity (float area.opacity)
                ]
                []

        colorFor difference =
            if difference >= 0 then
                area.positiveColor

            else
                area.negativeColor
    in
    case
        ( ( area.first firstItem, area.second firstItem )
        , ( area.first secondItem, area.second secondItem )
        )
    of
        ( ( Just firstValue, Just firstBaseline ), ( Just secondValue, Just secondBaseline ) ) ->
            let
                firstDifference =
                    firstValue - firstBaseline

                secondDifference =
                    secondValue - secondBaseline
            in
            if firstDifference * secondDifference >= 0 then
                [ polygon (colorFor firstDifference)
                    [ ( firstX, firstValue )
                    , ( secondX, secondValue )
                    , ( secondX, secondBaseline )
                    , ( firstX, firstBaseline )
                    ]
                ]

            else
                let
                    ratio =
                        abs firstDifference / (abs firstDifference + abs secondDifference)

                    crossX =
                        firstX + (secondX - firstX) * ratio

                    crossValue =
                        firstValue + (secondValue - firstValue) * ratio
                in
                [ polygon (colorFor firstDifference)
                    [ ( firstX, firstValue ), ( crossX, crossValue ), ( firstX, firstBaseline ) ]
                , polygon (colorFor secondDifference)
                    [ ( crossX, crossValue )
                    , ( secondX, secondValue )
                    , ( secondX, secondBaseline )
                    ]
                ]

        _ ->
            []


float : Float -> String
float =
    String.fromFloat


adjacentPairs : List value -> List ( value, value )
adjacentPairs values =
    case values of
        first :: second :: rest ->
            ( first, second ) :: adjacentPairs (second :: rest)

        _ ->
            []
