module View.Analytics.Timeline exposing
    ( netWorthPerformanceChart
    , positiveNegativeTimelineChart
    )

import Data.Analytics exposing (PerformancePoint, TimelinePoint)
import Data.Decimal as Decimal
import Data.Formatting as Formatting
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Ui.Chart.Cartesian as Cartesian
import Ui.DesignTokens as Tokens
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography


positiveNegativeTimelineChart :
    Theme
    ->
        { commodity : String
        , points : List TimelinePoint
        , selected : Maybe String
        , onSelect : String -> msg
        , availableWidth : Int
        , positiveLabel : String
        , negativeLabel : String
        , netLabel : String
        }
    -> Element msg
positiveNegativeTimelineChart theme config =
    let
        selectedPoint =
            config.selected
                |> Maybe.andThen
                    (\period -> List.filter (.period >> (==) period) config.points |> List.head)

        chart =
            Cartesian.view theme
                { availableWidth = config.availableWidth
                , height = Tokens.chartHeightTimeline
                , targetSlotWidth = 40
                , label = "Assets and liabilities timeline"
                , items = config.points
                , key = .period
                , xLabel = .period >> Formatting.shortPeriod >> Just
                , describe =
                    \point ->
                        point.period
                            ++ ": "
                            ++ String.toLower config.positiveLabel
                            ++ " "
                            ++ Formatting.formatDecimalDisplay config.commodity point.positive
                            ++ ", "
                            ++ String.toLower config.negativeLabel
                            ++ " "
                            ++ Formatting.formatDecimalDisplay config.commodity point.negative
                , selected = config.selected
                , onSelect = config.onSelect
                , formatTick = Formatting.formatAxisValue config.commodity
                , domain = Cartesian.SymmetricAroundZero
                , series =
                    [ Cartesian.Bars
                        { value = .positive >> Decimal.toFloat
                        , baseline = 0
                        , color = theme.positive
                        }
                    , Cartesian.Bars
                        { value = .negative >> Decimal.toFloat >> abs >> negate
                        , baseline = 0
                        , color = theme.negative
                        }
                    , Cartesian.Line
                        { value = .net >> Decimal.toFloat >> Just
                        , color = theme.primary
                        , width = 2.6
                        , markers = Cartesian.SelectedMarkers
                        }
                    ]
                }
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        [ chart
        , selectedPoint
            |> Maybe.map
                (\point ->
                    selectedPeriodDetails theme
                        (Formatting.periodDisplayName point.period)
                        [ { label = config.positiveLabel
                          , value = Formatting.formatDecimalDisplay config.commodity point.positive
                          , color = theme.positive
                          }
                        , { label = config.negativeLabel
                          , value = Formatting.formatDecimalDisplay config.commodity point.negative
                          , color = theme.negative
                          }
                        , { label = config.netLabel
                          , value = Formatting.formatDecimalDisplay config.commodity point.net
                          , color = theme.primary
                          }
                        ]
                )
            |> Maybe.withDefault Element.none
        ]


netWorthPerformanceChart :
    Theme
    ->
        { commodity : String
        , points : List PerformancePoint
        , selected : Maybe String
        , onSelect : String -> msg
        , availableWidth : Int
        }
    -> Element msg
netWorthPerformanceChart theme config =
    let
        selectedPoint =
            config.selected
                |> Maybe.andThen
                    (\period -> List.filter (.period >> (==) period) config.points |> List.head)

        chart =
            Cartesian.view theme
                { availableWidth = config.availableWidth
                , height = Tokens.chartHeightStandard
                , targetSlotWidth = 52
                , label = "Net investment and net worth timeline"
                , items = config.points
                , key = .period
                , xLabel = .period >> Formatting.shortPeriod >> Just
                , describe =
                    \point ->
                        point.period
                            ++ ": net investment "
                            ++ Formatting.formatDecimalDisplay config.commodity point.netInvestment
                            ++ ", net worth "
                            ++ Formatting.formatDecimalDisplay config.commodity point.netWorth
                , selected = config.selected
                , onSelect = config.onSelect
                , formatTick = Formatting.formatAxisValue config.commodity
                , domain = Cartesian.PaddedExtent { ratio = 0.12, minimum = 1 }
                , series =
                    [ Cartesian.AreaBetween
                        { first = .netWorth >> Decimal.toFloat >> Just
                        , second = .netInvestment >> Decimal.toFloat >> Just
                        , positiveColor = theme.positive
                        , negativeColor = theme.negative
                        , opacity = 0.18
                        }
                    , Cartesian.Line
                        { value = .netWorth >> Decimal.toFloat >> Just
                        , color = theme.primary
                        , width = 2.6
                        , markers = Cartesian.SelectedMarkers
                        }
                    , Cartesian.Line
                        { value = .netInvestment >> Decimal.toFloat >> Just
                        , color = theme.positive
                        , width = 2.4
                        , markers = Cartesian.SelectedMarkers
                        }
                    ]
                }
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        [ chart
        , selectedPoint
            |> Maybe.map
                (\point ->
                    let
                        gainLoss =
                            Decimal.toFloat point.gainLoss

                        gainLossLabel =
                            if gainLoss >= 0 then
                                "Gain"

                            else
                                "Loss"

                        gainLossColor =
                            if gainLoss >= 0 then
                                theme.positive

                            else
                                theme.negative
                    in
                    selectedPeriodDetails theme
                        (Formatting.periodDisplayName point.period)
                        [ { label = "Net investment"
                          , value =
                                Formatting.formatDecimalDisplay
                                    config.commodity
                                    point.netInvestment
                          , color = theme.positive
                          }
                        , { label = "Net worth"
                          , value =
                                Formatting.formatDecimalDisplay
                                    config.commodity
                                    point.netWorth
                          , color = theme.primary
                          }
                        , { label = gainLossLabel
                          , value = Formatting.formatFloatDisplay config.commodity (abs gainLoss)
                          , color = gainLossColor
                          }
                        ]
                )
            |> Maybe.withDefault Element.none
        ]


selectedPeriodDetails :
    Theme
    -> String
    -> List { label : String, value : String, color : Element.Color }
    -> Element msg
selectedPeriodDetails theme period metrics =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Tokens.space8
        , Element.padding Tokens.space12
        , Background.color theme.primarySoft
        , Border.rounded Tokens.radiusControl
        ]
        [ Element.el
            (Typography.body ++ [ Typography.fontWeight Typography.SemiBold ])
            (Element.text period)
        , Element.wrappedRow
            [ Element.width Element.fill
            , Element.spacing Tokens.space16
            ]
            (List.map (selectedPeriodMetric theme) metrics)
        ]


selectedPeriodMetric :
    Theme
    -> { label : String, value : String, color : Element.Color }
    -> Element msg
selectedPeriodMetric theme metric =
    Element.column
        [ Element.spacing Tokens.space4
        , Element.width (Element.fillPortion Tokens.layoutPortionSingle)
        ]
        [ Typography.mutedCaption theme metric.label
        , Element.el
            (Typography.body
                ++ Typography.numeric
                ++ [ Typography.fontWeight Typography.SemiBold
                   , Font.color metric.color
                   ]
            )
            (Element.text metric.value)
        ]
