module View.States exposing
    ( freshnessBanner
    , registerCompletenessBadge
    , remoteNotice
    )

import Data.Completeness as Completeness exposing (Completeness)
import Data.Freshness exposing (Freshness(..))
import Element exposing (Element)
import RemoteData exposing (RemoteData(..))
import Ui
import Ui.Chip as Chip
import Ui.DesignTokens as Tokens
import Ui.Theme exposing (Theme)


freshnessBanner : Theme -> Freshness -> Element msg
freshnessBanner theme freshness =
    case freshness of
        Fresh ->
            Element.none

        Refreshing ->
            Chip.statusBadge theme Chip.StatusPrimary "Refreshing"

        Stale ->
            Chip.statusBadge theme Chip.StatusWarning "Stale snapshot"

        Partial ->
            Chip.statusBadge theme Chip.StatusWarning "Partial data"

        Unavailable ->
            Chip.statusBadge theme Chip.StatusDanger "Unavailable"


registerCompletenessBadge : Theme -> Completeness -> Element msg
registerCompletenessBadge theme completeness =
    case completeness of
        Completeness.Complete ->
            Chip.statusBadge theme Chip.StatusSuccess "Complete valuation"

        Completeness.Partial ->
            Chip.statusBadge theme Chip.StatusWarning "Partial valuation"

        Completeness.Unavailable ->
            Chip.statusBadge theme Chip.StatusDanger "Valuation unavailable"


remoteNotice :
    (error -> String)
    -> Theme
    -> Freshness
    -> RemoteData error value
    -> Element msg
remoteNotice errorMessage theme freshness remote =
    case remote of
        Loading _ ->
            noticeSlot (Chip.statusBadge theme Chip.StatusPrimary "Refreshing")

        Failure error _ ->
            Ui.errorBanner theme (errorMessage error)

        _ ->
            case freshness of
                Fresh ->
                    noticeSlot Element.none

                _ ->
                    noticeSlot (freshnessBanner theme freshness)


noticeSlot : Element msg -> Element msg
noticeSlot notice =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px Tokens.iconSizeSmall)
        ]
        notice
