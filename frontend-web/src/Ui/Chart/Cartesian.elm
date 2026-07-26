module Ui.Chart.Cartesian exposing
    ( Config
    , Domain(..)
    , Series(..)
    , view
    )

import Element exposing (Element)
import Svg
import Svg.Attributes as SvgA
import Ui.Chart as Chart
import Ui.Css as Css
import Ui.Theme exposing (Theme)


type Domain
    = SymmetricAroundZero
    | PaddedExtent
        { ratio : Float
        , minimum : Float
        }


type Series item
    = Bars
        { value : item -> Float
        , baseline : Float
        , color : Element.Color
        }
    | Line
        { value : item -> Float
        , color : Element.Color
        , width : Float
        , selectedMarker : Bool
        }
    | AreaBetween
        { first : item -> Float
        , second : item -> Float
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
    , xLabel : item -> String
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
                |> List.filterMap
                    (\series ->
                        case series of
                            Line line ->
                                Just
                                    (Svg.polyline
                                        [ SvgA.points (pointsFor x y line.value config.items)
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
                                Nothing
                    )

        markers index item =
            if config.selected == Just (config.key item) then
                config.series
                    |> List.filterMap
                        (\series ->
                            case series of
                                Line line ->
                                    if line.selectedMarker then
                                        Just (Chart.marker theme line.color (x index) (y (line.value item)))

                                    else
                                        Nothing

                                _ ->
                                    Nothing
                        )

            else
                []

        annotations index item =
            let
                itemKey =
                    config.key item
            in
            [ Chart.xAxisLabel theme chartLayout (x index) (config.xLabel item)
            ]
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
            [ line.value item ]

        AreaBetween area ->
            [ area.first item, area.second item ]


pointsFor :
    (Int -> Float)
    -> (Float -> Float)
    -> (item -> Float)
    -> List item
    -> String
pointsFor x y value items =
    items
        |> List.indexedMap
            (\index item -> float (x index) ++ "," ++ float (y (value item)))
        |> String.join " "


areaSegments :
    (Int -> Float)
    -> (Float -> Float)
    ->
        { first : item -> Float
        , second : item -> Float
        , positiveColor : Element.Color
        , negativeColor : Element.Color
        , opacity : Float
        }
    -> Int
    -> ( item, item )
    -> List (Svg.Svg msg)
areaSegments x y area index ( firstItem, secondItem ) =
    let
        firstValue =
            area.first firstItem

        firstBaseline =
            area.second firstItem

        secondValue =
            area.first secondItem

        secondBaseline =
            area.second secondItem

        firstDifference =
            firstValue - firstBaseline

        secondDifference =
            secondValue - secondBaseline

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
            [ ( crossX, crossValue ), ( secondX, secondValue ), ( secondX, secondBaseline ) ]
        ]


adjacentPairs : List value -> List ( value, value )
adjacentPairs values =
    case values of
        first :: second :: rest ->
            ( first, second ) :: adjacentPairs (second :: rest)

        _ ->
            []


float : Float -> String
float =
    String.fromFloat
