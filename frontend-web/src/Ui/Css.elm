module Ui.Css exposing (themeCssVariables)

import Ui.Common as Common
import Ui.Theme exposing (Theme)


themeCssVariables : Theme -> String
themeCssVariables theme =
    String.join "; "
        [ "--ui-page-bg: " ++ Common.toCssColor theme.pageBg
        , "--ui-surface: " ++ Common.toCssColor theme.surface
        , "--ui-card-bg: " ++ Common.toCssColor theme.cardBg
        , "--ui-elevated-bg: " ++ Common.toCssColor theme.elevatedBg
        , "--ui-text-primary: " ++ Common.toCssColor theme.textPrimary
        , "--ui-text-medium: " ++ Common.toCssColor theme.textMedium
        , "--ui-text-muted: " ++ Common.toCssColor theme.textMuted
        , "--ui-divider: " ++ Common.toCssColor theme.divider
        , "--ui-input-fill: " ++ Common.toCssColor theme.inputFill
        , "--ui-primary: " ++ Common.toCssColor theme.primary
        , "--ui-primary-pressed: " ++ Common.toCssColor theme.primaryPressed
        , "--ui-primary-soft: " ++ Common.toCssColor theme.primarySoft
        , "--ui-primary-disabled: " ++ Common.toCssColor theme.primaryDisabled
        , "--ui-danger-bg: " ++ Common.toCssColor theme.dangerBg
        , "--ui-danger-fg: " ++ Common.toCssColor theme.dangerFg
        , "--ui-danger-solid: " ++ Common.toCssColor theme.dangerSolid
        , "--ui-success-bg: " ++ Common.toCssColor theme.successBg
        , "--ui-success-fg: " ++ Common.toCssColor theme.successFg
        , "--ui-success-solid: " ++ Common.toCssColor theme.successSolid
        , "--ui-warning-bg: " ++ Common.toCssColor theme.warningBg
        , "--ui-warning-fg: " ++ Common.toCssColor theme.warningFg
        , "--ui-selected-bg: " ++ Common.toCssColor theme.selectedBg
        , "--ui-hover-bg: " ++ Common.toCssColor theme.hoverBg
        , "--ui-focus-ring: " ++ Common.toCssColor theme.focusRing
        , "--ui-positive: " ++ Common.toCssColor theme.positive
        , "--ui-negative: " ++ Common.toCssColor theme.negative
        , "--ui-neutral: " ++ Common.toCssColor theme.neutral
        , "--ui-asset: " ++ Common.toCssColor theme.asset
        , "--ui-liability: " ++ Common.toCssColor theme.liability
        , "--ui-income: " ++ Common.toCssColor theme.income
        , "--ui-expense: " ++ Common.toCssColor theme.expense
        , "--ui-equity: " ++ Common.toCssColor theme.equity
        , "--ui-stale: " ++ Common.toCssColor theme.stale
        , "--ui-card-shadow: " ++ theme.cardShadow
        , "--ui-chip-bg: " ++ Common.toCssColor theme.chipBg
        , "--ui-chip-fg: " ++ Common.toCssColor theme.chipFg
        , "--ui-chip-border: " ++ Common.toCssColor theme.chipBorder
        , "--ui-chip-selected-bg: " ++ Common.toCssColor theme.chipSelectedBg
        , "--ui-chip-selected-fg: " ++ Common.toCssColor theme.chipSelectedFg
        ]
