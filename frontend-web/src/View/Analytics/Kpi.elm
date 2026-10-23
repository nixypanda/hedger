module View.Analytics.Kpi exposing
    ( kpiCard
    , metricCard
    )

import Data.Analytics exposing (Kpi)
import Data.Decimal as Decimal
import Data.Formatting as Formatting
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Lib.Number as Number
import Ui.Chart.Sparkline as Sparkline
import Ui.DesignTokens as Tokens
import Ui.Icon as Icon exposing (Icon)
import Ui.Surface as Surface
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography


kpiCard :
    Theme
    ->
        { title : String
        , commodity : String
        , kpi : Kpi
        , color : Element.Color
        , icon : Icon
        , decreaseIsPositive : Bool
        }
    -> Element msg
kpiCard theme config =
    Surface.cardWith
        [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
        , Element.height (Element.px Tokens.analyticsChartHeight)
        ]
        theme
        [ Element.row [ Element.width Element.fill, Element.spacing Tokens.space12 ]
            [ iconTile
                { size = Tokens.analyticsRingSize
                , background = theme.primarySoft
                , icon = config.icon
                , iconSize = Tokens.iconSizeDetail
                , color = config.color
                }
            , kpiTrendSparkline config.color (List.map Decimal.toFloat config.kpi.trend)
            ]
        , Typography.mutedBody theme config.title
        , Element.el
            [ Typography.kpiValue
            , Typography.financeNumber
            , Font.bold
            ]
            (Element.text (Formatting.formatDecimalDisplay config.commodity config.kpi.value))
        , comparison theme config.commodity config.decreaseIsPositive config.kpi
        ]


metricCard :
    Theme
    -> String
    -> String
    -> Icon
    -> Element.Color
    -> Element msg
metricCard theme title value icon color =
    Surface.cardWith [ Element.width (Element.fillPortion Tokens.layoutPortionSingle) ]
        theme
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing Tokens.space12
            , Element.alignTop
            ]
            [ iconTile
                { size = Tokens.compactControlVisualHeight
                , background = theme.primarySoft
                , icon = icon
                , iconSize = Tokens.iconSizeNavigation
                , color = color
                }
            , Element.column [ Element.width Element.fill, Element.spacing Tokens.space4 ]
                [ Typography.mutedCaption theme title
                , Element.el [ Typography.kpiValue, Typography.financeNumber, Font.bold ]
                    (Element.text value)
                ]
            ]
        ]


iconTile :
    { size : Int
    , background : Element.Color
    , icon : Icon
    , iconSize : Int
    , color : Element.Color
    }
    -> Element msg
iconTile config =
    Element.el
        [ Element.width (Element.px config.size)
        , Element.height (Element.px config.size)
        , Background.color config.background
        , Border.rounded Tokens.radiusMedium
        ]
        (Element.el [ Element.centerX, Element.centerY ]
            (Icon.view
                { icon = config.icon
                , size = config.iconSize
                , color = config.color
                }
            )
        )


comparison : Theme -> String -> Bool -> Kpi -> Element msg
comparison theme commodity decreaseIsPositive kpi =
    case kpi.absoluteChange of
        Nothing ->
            Typography.mutedCaption theme "No previous period"

        Just change ->
            let
                amount =
                    Decimal.toFloat change

                color =
                    if (amount >= 0) /= decreaseIsPositive then
                        theme.positive

                    else
                        theme.negative

                percentage =
                    kpi.percentageChange
                        |> Maybe.map
                            (\value ->
                                " · " ++ Number.roundedString (Decimal.toFloat value) 1 ++ "%"
                            )
                        |> Maybe.withDefault ""
            in
            Element.el
                [ Font.color color
                , Font.semiBold
                , Typography.caption
                , Typography.financeNumber
                ]
                (Element.text
                    ("vs previous  "
                        ++ Formatting.formatDecimalDisplay commodity change
                        ++ percentage
                    )
                )


kpiTrendSparkline : Element.Color -> List Float -> Element msg
kpiTrendSparkline color values =
    Sparkline.view
        { width = 126
        , height = 48
        , color = color
        , label = "Recent trend"
        , values = values
        }
