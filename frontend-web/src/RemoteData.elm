module RemoteData exposing (RemoteData(..), beginLoading, map, value)

import Api.Error exposing (ApiError)


type RemoteData a
    = NotAsked
    | Loading (Maybe a)
    | Success a
    | Failure ApiError (Maybe a)


beginLoading : RemoteData a -> RemoteData a
beginLoading remote =
    Loading (value remote)


value : RemoteData a -> Maybe a
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


map : (a -> b) -> RemoteData a -> RemoteData b
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
