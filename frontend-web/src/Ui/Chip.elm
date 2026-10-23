module Ui.Chip exposing
    ( FinancialTone(..)
    , StatusTone(..)
    , chip
    , financialBadge
    , legendButton
    , legendToggleButton
    , statusBadge
    )

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Ui.Common as Common
import Ui.DesignTokens
    exposing
        ( compactControlVisualHeight
        , minimumTouchTarget
        , radiusControl
        , radiusPill
        , space8
        )
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography


chip :
    Theme
    ->
        { label : String
        , selected : Bool
        , onClick : Maybe msg
        }
    -> Element msg
chip theme config =
    let
        surface =
            Element.el
                (Element.width (Element.shrink |> Element.minimum minimumTouchTarget)
                    :: chipAttrs theme config.selected
                )
                (Element.el [ Element.centerX ] (Element.text config.label))

        attrs =
            chipAttrs theme config.selected
    in
    case config.onClick of
        Just msg ->
            Input.button
                [ Element.height (Element.px minimumTouchTarget)
                , Element.width Element.shrink
                ]
                { onPress = Just msg
                , label = surface
                }

        Nothing ->
            Element.el
                (Element.width (Element.shrink |> Element.minimum minimumTouchTarget) :: attrs)
                (Element.el [ Element.centerX ] (Element.text config.label))


coloredPill :
    { label : String
    , bg : Element.Color
    , fg : Element.Color
    }
    -> Element msg
coloredPill config =
    Element.el
        (Typography.labelSmall
            ++ [ Background.color config.bg
               , Font.color config.fg
               , Border.rounded radiusPill
               , Element.paddingEach { top = 4, right = 10, bottom = 4, left = 10 }
               ]
        )
        (Element.text config.label)


type StatusTone
    = StatusSuccess
    | StatusPrimary
    | StatusWarning
    | StatusDanger
    | StatusNeutral


type FinancialTone
    = FinancialIncome
    | FinancialExpense
    | FinancialNeutral
    | FinancialEquity
    | FinancialBorrowing
    | FinancialRepayment
    | FinancialAsset


financialBadge : Theme -> FinancialTone -> String -> Element msg
financialBadge theme tone label =
    let
        colors =
            case tone of
                FinancialIncome ->
                    { bg = theme.successBg, fg = theme.successFg }

                FinancialExpense ->
                    { bg = theme.warningBg, fg = theme.warningFg }

                FinancialNeutral ->
                    { bg = theme.chipBg, fg = theme.neutral }

                FinancialEquity ->
                    { bg = theme.primarySoft, fg = theme.equity }

                FinancialBorrowing ->
                    { bg = theme.borrowingBg, fg = theme.borrowingFg }

                FinancialRepayment ->
                    { bg = theme.repaymentBg, fg = theme.repaymentFg }

                FinancialAsset ->
                    { bg = theme.assetBg, fg = theme.assetFg }
    in
    Element.el
        (Typography.labelSmall
            ++ [ Background.color colors.bg
               , Font.color colors.fg
               , Border.rounded radiusPill
               , Element.paddingEach { top = 2, right = 8, bottom = 2, left = 8 }
               ]
        )
        (Element.text label)


statusBadge : Theme -> StatusTone -> String -> Element msg
statusBadge theme tone status =
    let
        colors =
            statusToneColors theme tone
    in
    coloredPill { label = status, bg = colors.bg, fg = colors.fg }


statusToneColors : Theme -> StatusTone -> { bg : Element.Color, fg : Element.Color }
statusToneColors theme tone =
    case tone of
        StatusSuccess ->
            { bg = theme.successBg, fg = theme.successFg }

        StatusPrimary ->
            { bg = theme.primarySoft, fg = theme.primary }

        StatusWarning ->
            { bg = theme.warningBg, fg = theme.warningFg }

        StatusDanger ->
            { bg = theme.dangerBg, fg = theme.dangerFg }

        StatusNeutral ->
            { bg = theme.chipBg, fg = theme.chipFg }


chipAttrs : Theme -> Bool -> List (Attribute msg)
chipAttrs theme selected =
    Typography.labelSmall
        ++ [ Background.color
                (if selected then
                    theme.chipSelectedBg

                 else
                    theme.chipBg
                )
           , Font.color
                (if selected then
                    theme.chipSelectedFg

                 else
                    theme.chipFg
                )
           , Border.rounded radiusPill
           , Border.width 1
           , Border.color
                (if selected then
                    theme.chipSelectedBg

                 else
                    theme.chipBorder
                )
           , Element.paddingEach { top = 5, right = 10, bottom = 5, left = 10 }
           , Common.uiClass "ui-motion"
           ]


legendButton :
    Theme
    ->
        { label : Element msg
        , accent : Element.Color
        , selected : Bool
        , onPress : msg
        }
    -> Element msg
legendButton _ config =
    Input.button
        [ Element.height (Element.shrink |> Element.minimum minimumTouchTarget) ]
        { onPress = Just config.onPress
        , label = legendSurface config
        }


legendToggleButton : Theme -> { label : Element msg, onPress : msg } -> Element msg
legendToggleButton theme config =
    Input.button
        [ Element.height (Element.shrink |> Element.minimum minimumTouchTarget)
        , Font.color theme.textMuted
        ]
        { onPress = Just config.onPress
        , label = legendToggleSurface config.label
        }


legendSurface :
    { config | label : Element msg, accent : Element.Color, selected : Bool }
    -> Element msg
legendSurface config =
    Element.el
        [ Element.height (Element.px compactControlVisualHeight)
        , Element.paddingXY space8 0
        , Border.rounded radiusControl
        , Border.color
            (if config.selected then
                config.accent

             else
                Element.rgba 0 0 0 0
            )
        , Border.width 1
        ]
        (Element.el [ Element.centerY ] config.label)


legendToggleSurface : Element msg -> Element msg
legendToggleSurface label =
    Element.el
        [ Element.height (Element.px compactControlVisualHeight)
        , Element.paddingXY space8 0
        ]
        (Element.el [ Element.centerY ] label)
