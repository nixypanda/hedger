module Data.Paginated exposing
    ( Config
    , Paginated
    , Request
    , Status(..)
    , appliedCriteria
    , beginFresh
    , beginLoadMore
    , beginRefresh
    , canLoadMore
    , currentPage
    , error
    , init
    , loadMoreError
    , receive
    , requestCriteria
    , requestCursor
    , status
    )

{-| A generic windowed-pagination state machine shared by list pages that load
a first page, refresh it in place, and append further pages behind a cursor.

The machine owns the parts that are easy to get subtly wrong: minting a
monotonic request token, rejecting stale or cross-kind responses, and appending
or resetting the accumulated window. Everything page-specific — the applied
query/filters (`criteria`), the accumulated payload (`page`), how pages combine,
whether a fresh revision forces a reset, and any row selection — stays in the
caller. Selection deliberately lives outside this module: callers derive it on
read against `currentPage`, so it never needs threading through transitions.

-}

import Api.Error exposing (ApiError)


{-| Opaque pagination value parameterised by the applied `criteria` and the
accumulated window `page`.
-}
type Paginated criteria page
    = Paginated (Internals criteria page)


type alias Internals criteria page =
    { state : State criteria page
    , nextToken : Int
    }


type State criteria page
    = Dormant
    | Loading criteria Int
    | LoadFailed criteria ApiError
    | Showing criteria page
    | Refreshing criteria page Int
    | RefreshFailed criteria page ApiError
    | LoadingMore criteria page Int String
    | LoadMoreFailed criteria page ApiError Int String


{-| An in-flight request handed back to the caller so it can issue the HTTP call
and tag the eventual `Loaded` message. Its token and kind let `receive` reject
stale or cross-kind completions.
-}
type Request criteria
    = Request
        { token : Int
        , kind : Kind
        , criteria : criteria
        , cursor : Maybe String
        }


type Kind
    = FirstPage
    | NextPage


{-| Page-specific policy injected into `receive`/`beginLoadMore`.

  - `nextCursor` reads the continuation cursor out of an accumulated page.
  - `append` merges the existing window with a freshly loaded next page (this is
    where a bounded-window page drops its oldest rows).
  - `shouldReset` decides whether a loaded next page is discontinuous with the
    window (e.g. the backend revision changed), in which case it replaces the
    window instead of appending.

-}
type alias Config page =
    { nextCursor : page -> Maybe String
    , append : page -> page -> page
    , shouldReset : page -> page -> Bool
    }


{-| Coarse lifecycle projection for view code and status assertions.
-}
type Status
    = NotAsked
    | LoadingFirst
    | LoadFailure
    | Showing_
    | Refreshing_
    | RefreshFailure
    | LoadingMore_
    | LoadMoreFailure


init : Paginated criteria page
init =
    Paginated { state = Dormant, nextToken = 0 }



-- BEGIN TRANSITIONS


{-| Start a first-page load that blanks any visible window (a new query/scope).
-}
beginFresh : criteria -> Paginated criteria page -> ( Paginated criteria page, Request criteria )
beginFresh newCriteria (Paginated internals) =
    let
        ( token, request ) =
            mint internals newCriteria FirstPage Nothing
    in
    ( Paginated { internals | state = Loading newCriteria token, nextToken = token }
    , request
    )


{-| Re-fetch the first page while keeping the current window visible if one
exists (a refresh). Falls back to a blanking load when nothing is shown yet.
-}
beginRefresh : criteria -> Paginated criteria page -> ( Paginated criteria page, Request criteria )
beginRefresh newCriteria (Paginated internals) =
    let
        ( token, request ) =
            mint internals newCriteria FirstPage Nothing

        state =
            case visiblePage internals.state of
                Just page ->
                    Refreshing newCriteria page token

                Nothing ->
                    Loading newCriteria token
    in
    ( Paginated { internals | state = state, nextToken = token }, request )


{-| Request the next page behind the window's cursor. Yields `Nothing` when
there is no retryable window and cursor to advance from.
-}
beginLoadMore :
    Config page
    -> Paginated criteria page
    -> Maybe ( Paginated criteria page, Request criteria )
beginLoadMore config (Paginated internals) =
    loadMoreTarget config internals.state
        |> Maybe.map
            (\( crit, page, cursor ) ->
                let
                    ( token, request ) =
                        mint internals crit NextPage (Just cursor)
                in
                ( Paginated
                    { internals
                        | state = LoadingMore crit page token cursor
                        , nextToken = token
                    }
                , request
                )
            )


mint : Internals criteria page -> criteria -> Kind -> Maybe String -> ( Int, Request criteria )
mint internals crit kind cursor =
    let
        token =
            internals.nextToken + 1
    in
    ( token
    , Request { token = token, kind = kind, criteria = crit, cursor = cursor }
    )


loadMoreTarget : Config page -> State criteria page -> Maybe ( criteria, page, String )
loadMoreTarget config state =
    case state of
        Showing crit page ->
            config.nextCursor page |> Maybe.map (\cursor -> ( crit, page, cursor ))

        RefreshFailed crit page _ ->
            config.nextCursor page |> Maybe.map (\cursor -> ( crit, page, cursor ))

        LoadMoreFailed crit page _ _ cursor ->
            Just ( crit, page, cursor )

        _ ->
            Nothing



-- RECEIVE


{-| Fold a completed request into the machine, rejecting it when its token or
kind no longer matches the active state so out-of-order responses cannot
clobber newer work.
-}
receive :
    Config page
    -> Request criteria
    -> Result ApiError page
    -> Paginated criteria page
    -> Paginated criteria page
receive config (Request req) result (Paginated internals) =
    let
        keep =
            Paginated internals

        set nextState =
            Paginated { internals | state = nextState }
    in
    case internals.state of
        Loading crit token ->
            if req.kind == FirstPage && req.token == token then
                case result of
                    Ok page ->
                        set (Showing crit page)

                    Err err ->
                        set (LoadFailed crit err)

            else
                keep

        Refreshing crit stale token ->
            if req.kind == FirstPage && req.token == token then
                case result of
                    Ok page ->
                        set (Showing crit page)

                    Err err ->
                        set (RefreshFailed crit stale err)

            else
                keep

        LoadingMore crit window token cursor ->
            if req.kind == NextPage && req.token == token then
                case result of
                    Ok page ->
                        let
                            merged =
                                if config.shouldReset window page then
                                    page

                                else
                                    config.append window page
                        in
                        set (Showing crit merged)

                    Err err ->
                        set (LoadMoreFailed crit window err token cursor)

            else
                keep

        _ ->
            keep



-- REQUEST ACCESSORS


requestCriteria : Request criteria -> criteria
requestCriteria (Request req) =
    req.criteria


requestCursor : Request criteria -> Maybe String
requestCursor (Request req) =
    req.cursor



-- QUERIES


{-| The window currently visible to the user, retained through refreshes,
load-more, and their failures. `Nothing` before the first page arrives.
-}
currentPage : Paginated criteria page -> Maybe page
currentPage (Paginated internals) =
    visiblePage internals.state


visiblePage : State criteria page -> Maybe page
visiblePage state =
    case state of
        Showing _ page ->
            Just page

        Refreshing _ page _ ->
            Just page

        RefreshFailed _ page _ ->
            Just page

        LoadingMore _ page _ _ ->
            Just page

        LoadMoreFailed _ page _ _ _ ->
            Just page

        _ ->
            Nothing


{-| The applied criteria owned by the current state — the loaded window's own
criteria, independent of any draft the caller is editing. `Nothing` only when
dormant.
-}
appliedCriteria : Paginated criteria page -> Maybe criteria
appliedCriteria (Paginated internals) =
    case internals.state of
        Dormant ->
            Nothing

        Loading crit _ ->
            Just crit

        LoadFailed crit _ ->
            Just crit

        Showing crit _ ->
            Just crit

        Refreshing crit _ _ ->
            Just crit

        RefreshFailed crit _ _ ->
            Just crit

        LoadingMore crit _ _ _ ->
            Just crit

        LoadMoreFailed crit _ _ _ _ ->
            Just crit


status : Paginated criteria page -> Status
status (Paginated internals) =
    case internals.state of
        Dormant ->
            NotAsked

        Loading _ _ ->
            LoadingFirst

        LoadFailed _ _ ->
            LoadFailure

        Showing _ _ ->
            Showing_

        Refreshing _ _ _ ->
            Refreshing_

        RefreshFailed _ _ _ ->
            RefreshFailure

        LoadingMore _ _ _ _ ->
            LoadingMore_

        LoadMoreFailed _ _ _ _ _ ->
            LoadMoreFailure


{-| The error from whichever failed state is currently active — first load,
refresh, or load-more. At most one is active at a time.
-}
error : Paginated criteria page -> Maybe ApiError
error (Paginated internals) =
    case internals.state of
        LoadFailed _ err ->
            Just err

        RefreshFailed _ _ err ->
            Just err

        LoadMoreFailed _ _ err _ _ ->
            Just err

        _ ->
            Nothing


{-| The error from the most recent failed load-more, retained for retry.
-}
loadMoreError : Paginated criteria page -> Maybe ApiError
loadMoreError (Paginated internals) =
    case internals.state of
        LoadMoreFailed _ _ err _ _ ->
            Just err

        _ ->
            Nothing


{-| Whether a load-more can be started right now (a retryable window with a
cursor, and nothing already in flight).
-}
canLoadMore : Config page -> Paginated criteria page -> Bool
canLoadMore config (Paginated internals) =
    loadMoreTarget config internals.state /= Nothing
