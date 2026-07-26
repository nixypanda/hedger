module View.States exposing
    ( freshnessBanner
    , registerCompletenessBadge
    , remoteNotice
    )

import Api.Error as ApiError
import Data.Completeness as Completeness exposing (Completeness)
import Data.Freshness exposing (Freshness(..))
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


registerCompletenessBadge : Theme -> Completeness -> Element msg
registerCompletenessBadge theme completeness =
    case completeness of
        Completeness.Complete ->
            Ui.statusBadge theme Ui.StatusSuccess "Complete valuation"

        Completeness.Partial ->
            Ui.statusBadge theme Ui.StatusWarning "Partial valuation"

        Completeness.Unavailable ->
            Ui.statusBadge theme Ui.StatusDanger "Valuation unavailable"


remoteNotice : Theme -> Freshness -> RemoteData a -> Element msg
remoteNotice theme freshness remote =
    case remote of
        Loading _ ->
            noticeSlot (Ui.statusBadge theme Ui.StatusPrimary "Refreshing")

        Failure error _ ->
            Ui.errorBanner theme (ApiError.message error)

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
        , Element.height (Element.px 20)
        ]
        notice
