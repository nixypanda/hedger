module View.States exposing
    ( freshnessBanner
    , registerCompletenessBadge
    )

import Api.Metadata as Metadata exposing (Freshness(..))
import Api.Register as Register
import Element exposing (Element)
import Ui
import Ui.Theme exposing (Theme)


freshnessBanner : Theme -> Freshness -> Element msg
freshnessBanner theme freshness =
    case freshness of
        Fresh ->
            Element.none

        Refreshing ->
            Ui.statusBadge theme Ui.StatusPrimary "Refreshing"

        Stale ->
            Ui.statusBadge theme Ui.StatusWarning "Stale snapshot"

        Partial ->
            Ui.statusBadge theme Ui.StatusWarning "Partial data"

        Unavailable ->
            Ui.statusBadge theme Ui.StatusDanger "Unavailable"


registerCompletenessBadge : Theme -> Register.Completeness -> Element msg
registerCompletenessBadge theme completeness =
    case completeness of
        Register.Complete ->
            Ui.statusBadge theme Ui.StatusSuccess "Complete valuation"

        Register.Partial ->
            Ui.statusBadge theme Ui.StatusWarning "Partial valuation"

        Register.Unavailable ->
            Ui.statusBadge theme Ui.StatusDanger "Valuation unavailable"
