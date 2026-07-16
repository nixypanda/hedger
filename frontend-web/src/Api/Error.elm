module Api.Error exposing (ApiError, decoder, fromHttpError, message)

import Http
import Json.Decode as Decode exposing (Decoder)


type alias ApiError =
    { code : String
    , message : String
    , requestId : Maybe String
    }


decoder : Decoder ApiError
decoder =
    Decode.field "error" <|
        Decode.map3 ApiError
            (Decode.field "code" Decode.string)
            (Decode.field "message" Decode.string)
            (Decode.maybe (Decode.field "requestId" Decode.string))


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
    case error.requestId of
        Just requestId ->
            error.message ++ " (request " ++ requestId ++ ")"

        Nothing ->
            error.message


local : String -> String -> ApiError
local code detail =
    { code = code, message = detail, requestId = Nothing }
