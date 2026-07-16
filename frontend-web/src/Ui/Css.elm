module Ui.Css exposing
    ( themeCssVariables
    , toCssColor
    )

import Element
import Ui.Theme exposing (Theme)


toCssColor : Element.Color -> String
toCssColor color =
    let
        { red, green, blue, alpha } =
            Element.toRgb color
    in
    "rgba("
        ++ String.fromInt (round (red * 255))
        ++ ", "
        ++ String.fromInt (round (green * 255))
        ++ ", "
        ++ String.fromInt (round (blue * 255))
        ++ ", "
        ++ String.fromFloat alpha
        ++ ")"


themeCssVariables : Theme -> String
themeCssVariables theme =
    String.join "; "
        [ "--ui-page-bg: " ++ toCssColor theme.pageBg
        , "--ui-surface: " ++ toCssColor theme.surface
        , "--ui-card-bg: " ++ toCssColor theme.cardBg
        , "--ui-elevated-bg: " ++ toCssColor theme.elevatedBg
        , "--ui-text-primary: " ++ toCssColor theme.textPrimary
        , "--ui-text-medium: " ++ toCssColor theme.textMedium
        , "--ui-text-muted: " ++ toCssColor theme.textMuted
        , "--ui-divider: " ++ toCssColor theme.divider
        , "--ui-input-fill: " ++ toCssColor theme.inputFill
        , "--ui-primary: " ++ toCssColor theme.primary
        , "--ui-primary-pressed: " ++ toCssColor theme.primaryPressed
        , "--ui-primary-soft: " ++ toCssColor theme.primarySoft
        , "--ui-primary-disabled: " ++ toCssColor theme.primaryDisabled
        , "--ui-danger-bg: " ++ toCssColor theme.dangerBg
        , "--ui-danger-fg: " ++ toCssColor theme.dangerFg
        , "--ui-danger-solid: " ++ toCssColor theme.dangerSolid
        , "--ui-success-bg: " ++ toCssColor theme.successBg
        , "--ui-success-fg: " ++ toCssColor theme.successFg
        , "--ui-success-solid: " ++ toCssColor theme.successSolid
        , "--ui-warning-bg: " ++ toCssColor theme.warningBg
        , "--ui-warning-fg: " ++ toCssColor theme.warningFg
        , "--ui-selected-bg: " ++ toCssColor theme.selectedBg
        , "--ui-hover-bg: " ++ toCssColor theme.hoverBg
        , "--ui-focus-ring: " ++ toCssColor theme.focusRing
        , "--ui-positive: " ++ toCssColor theme.positive
        , "--ui-negative: " ++ toCssColor theme.negative
        , "--ui-neutral: " ++ toCssColor theme.neutral
        , "--ui-asset: " ++ toCssColor theme.asset
        , "--ui-liability: " ++ toCssColor theme.liability
        , "--ui-income: " ++ toCssColor theme.income
        , "--ui-expense: " ++ toCssColor theme.expense
        , "--ui-equity: " ++ toCssColor theme.equity
        , "--ui-stale: " ++ toCssColor theme.stale
        , "--ui-card-shadow: " ++ theme.cardShadow
        , "--ui-chip-bg: " ++ toCssColor theme.chipBg
        , "--ui-chip-fg: " ++ toCssColor theme.chipFg
        , "--ui-chip-border: " ++ toCssColor theme.chipBorder
        , "--ui-chip-selected-bg: " ++ toCssColor theme.chipSelectedBg
        , "--ui-chip-selected-fg: " ++ toCssColor theme.chipSelectedFg
        ]
