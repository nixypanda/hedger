module LedgerRouting exposing (entryRedirect, scopedLocation, scopedPath, selectionRoute)

import Route exposing (Route(..))
import String
import Url exposing (Url)


scopedPath : String -> Route -> String
scopedPath ledgerId route =
    "/ledgers/" ++ ledgerId ++ Route.toPath route


scopedLocation : Url -> Maybe ( String, Route )
scopedLocation url =
    case String.split "/" url.path of
        "" :: "ledgers" :: ledgerId :: pageParts ->
            if String.isEmpty ledgerId then
                Nothing

            else
                let
                    pageUrl =
                        { url | path = "/" ++ String.join "/" pageParts }
                in
                Just ( ledgerId, Route.fromUrl pageUrl )

        _ ->
            Nothing


entryRedirect : Maybe String -> Url -> Maybe String
entryRedirect maybeDefault url =
    case scopedLocation url of
        Just _ ->
            Nothing

        Nothing ->
            Maybe.map (\ledgerId -> scopedPath ledgerId (Route.fromUrl url)) maybeDefault


selectionRoute : Maybe String -> Route -> Url -> Route
selectionRoute selected current url =
    let
        requested =
            case selected of
                Nothing ->
                    Route.fromUrl url

                Just _ ->
                    current
    in
    case requested of
        Account _ ->
            NetWorth

        AccountValue _ ->
            NetWorth

        route ->
            route
