module View.States exposing
    ( refreshStatus
    , registerCompletenessBadge
    )

import Data.Completeness as Completeness exposing (Completeness)
import Data.Freshness exposing (Freshness(..))
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes as Attr
import Ui.Chip as Chip
import Ui.Common as Common
import Ui.DesignTokens as Tokens
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography


freshnessIndicator : Theme -> Freshness -> Element msg
freshnessIndicator theme freshness =
    statusIndicator theme (freshnessStatus theme freshness)


refreshStatus : Theme -> { freshness : Freshness, loading : Bool } -> Element msg
refreshStatus theme config =
    freshnessIndicator theme
        (if config.loading then
            Refreshing

         else
            config.freshness
        )


type alias FreshnessStatus =
    { color : Element.Color
    , label : String
    , shortLabel : String
    }


freshnessStatus : Theme -> Freshness -> FreshnessStatus
freshnessStatus theme freshness =
    case freshness of
        Fresh ->
            { color = theme.positive
            , label = "Up to date"
            , shortLabel = "Live"
            }

        Refreshing ->
            { color = theme.primary
            , label = "Refreshing"
            , shortLabel = "Refreshing"
            }

        Stale ->
            { color = theme.warningSolid
            , label = "Stale snapshot"
            , shortLabel = "Stale"
            }

        Partial ->
            { color = theme.warningSolid
            , label = "Partial data"
            , shortLabel = "Partial"
            }

        Unavailable ->
            { color = theme.dangerSolid
            , label = "Unavailable"
            , shortLabel = "Unavailable"
            }


statusIndicator :
    Theme
    -> { color : Element.Color, label : String, shortLabel : String }
    -> Element msg
statusIndicator theme status =
    Element.row
        [ Element.width (Element.px Tokens.statusIndicatorWidth)
        , Element.height (Element.px Tokens.statusIndicatorHeight)
        , Element.paddingXY Tokens.space12 Tokens.space0
        , Element.spacing Tokens.space8
        , Background.color theme.chipBg
        , Border.rounded Tokens.radiusPill
        , Border.width Tokens.borderHairline
        , Border.color theme.divider
        , Element.htmlAttribute (Attr.title status.label)
        , Element.htmlAttribute (Attr.attribute "role" "status")
        , Element.htmlAttribute (Attr.attribute "aria-live" "polite")
        , Element.htmlAttribute (Attr.attribute "aria-label" status.label)
        ]
        [ Element.el
            [ Element.width (Element.px Tokens.statusDotSize)
            , Element.height (Element.px Tokens.statusDotSize)
            , Element.centerY
            , Background.color status.color
            , Border.rounded Tokens.radiusPill
            ]
            Element.none
        , Element.el
            (Typography.labelSmall
                ++ [ Font.color theme.textMedium
                   , Element.width Element.fill
                   , Element.centerY
                   , Element.clip
                   , Common.minWidthZero
                   ]
            )
            (Element.text status.shortLabel)
        ]


registerCompletenessBadge : Theme -> Completeness -> Element msg
registerCompletenessBadge theme completeness =
    case completeness of
        Completeness.Complete ->
            Chip.statusBadge theme Chip.StatusSuccess "Complete valuation"

        Completeness.Partial ->
            Chip.statusBadge theme Chip.StatusWarning "Partial valuation"

        Completeness.Unavailable ->
            Chip.statusBadge theme Chip.StatusDanger "Valuation unavailable"
