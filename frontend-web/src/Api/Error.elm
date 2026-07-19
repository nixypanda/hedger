module Api.Error exposing (ApiError, ErrorDetails, decoder, fromHttpError, isQueryError, message)

import Http
import Json.Decode as Decode exposing (Decoder)


type alias ApiError =
    { code : String
    , message : String
    , requestId : Maybe String
    , details : Maybe ErrorDetails
    }


type alias ErrorDetails =
    { reason : String
    , position : Maybe Int
    }


decoder : Decoder ApiError
decoder =
    Decode.field "errorPayload" <|
        Decode.map4 ApiError
            (Decode.field "code" Decode.string)
            (Decode.field "message" Decode.string)
            (Decode.maybe (Decode.field "requestId" Decode.string))
            (Decode.maybe (Decode.field "details" detailsDecoder))


detailsDecoder : Decoder ErrorDetails
detailsDecoder =
    Decode.map2 ErrorDetails
        (Decode.field "reason" Decode.string)
        (Decode.field "position" (Decode.nullable Decode.int))


fromHttpError : Http.Error -> ApiError
fromHttpError error =
    case error of
        Http.BadUrl _ ->
            local "bad_url" "The API URL is invalid."

        Http.Timeout ->
            local "timeout" "The API request timed out."

        Http.NetworkError ->
            local "network_error" "The API could not be reached."

        Http.BadStatus status ->
            local "bad_status" ("The API returned status " ++ String.fromInt status ++ ".")

        Http.BadBody detail ->
            local "invalid_response" detail


message : ApiError -> String
message error =
    let
        detail =
            case error.details of
                Just details ->
                    " "
                        ++ details.reason
                        ++ (details.position
                                |> Maybe.map
                                    (\position ->
                                        " (at character " ++ String.fromInt position ++ ")"
                                    )
                                |> Maybe.withDefault ""
                           )

                Nothing ->
                    ""

        request =
            error.requestId
                |> Maybe.map (\requestId -> " (request " ++ requestId ++ ")")
                |> Maybe.withDefault ""
    in
    error.message ++ detail ++ request


local : String -> String -> ApiError
local code detail =
    { code = code, message = detail, requestId = Nothing, details = Nothing }


isQueryError : ApiError -> Bool
isQueryError error =
    List.member error.code [ "invalid_query", "unsupported_query_option", "query_too_long" ]
