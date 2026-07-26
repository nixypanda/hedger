module View.States exposing
    ( freshnessBanner
    , registerCompletenessBadge
    , remoteNotice
    )

import Api.Error as ApiError
import Api.Metadata as Metadata exposing (Freshness(..))
import Api.Register as Register
import Element exposing (Element)
import RemoteData exposing (RemoteData(..))
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


remoteNotice : Theme -> Freshness -> RemoteData a -> Element msg
remoteNotice theme freshness remote =
    Element.column [ Element.width Element.fill, Element.spacing 8 ]
        [ freshnessBanner theme freshness
        , case remote of
            Loading _ ->
                Ui.statusBadge theme Ui.StatusPrimary "Refreshing"

            Failure error _ ->
                Ui.errorBanner theme (ApiError.message error)

            _ ->
                Element.none
        ]
