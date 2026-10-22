module Lib.RemoteData exposing (RemoteData(..), beginLoading, map, value)


type RemoteData error value
    = NotAsked
    | Loading (Maybe value)
    | Success value
    | Failure error (Maybe value)


beginLoading : RemoteData error value -> RemoteData error value
beginLoading remote =
    Loading (value remote)


value : RemoteData error value -> Maybe value
value remote =
    case remote of
        NotAsked ->
            Nothing

        Loading stale ->
            stale

        Success current ->
            Just current

        Failure _ stale ->
            stale


map : (a -> b) -> RemoteData error a -> RemoteData error b
map transform remote =
    case remote of
        NotAsked ->
            NotAsked

        Loading stale ->
            Loading (Maybe.map transform stale)

        Success current ->
            Success (transform current)

        Failure error stale ->
            Failure error (Maybe.map transform stale)
