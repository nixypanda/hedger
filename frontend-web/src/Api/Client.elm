module Api.Client exposing (getBootstrap, getTransactions)

import Api.Bootstrap as Bootstrap exposing (Bootstrap)
import Api.Error as ApiError exposing (ApiError)
import Api.Transaction as Transaction exposing (Response)
import Http
import Json.Decode as Decode


getBootstrap : (Result ApiError Bootstrap -> msg) -> Cmd msg
getBootstrap toMsg =
    Http.get
        { url = "/api/v1/bootstrap"
        , expect = expectApiJson toMsg Bootstrap.decoder
        }


getTransactions : (Result ApiError Response -> msg) -> Cmd msg
getTransactions toMsg =
    Http.get
        { url = "/api/v1/transactions"
        , expect = expectApiJson toMsg Transaction.decoder
        }


expectApiJson : (Result ApiError value -> msg) -> Decode.Decoder value -> Http.Expect msg
expectApiJson toMsg valueDecoder =
    Http.expectStringResponse
        (\result ->
            case result of
                Ok apiResult ->
                    toMsg apiResult

                Err httpError ->
                    toMsg (Err (ApiError.fromHttpError httpError))
        )
        (responseToResult valueDecoder)


responseToResult : Decode.Decoder value -> Http.Response String -> Result Http.Error (Result ApiError value)
responseToResult valueDecoder response =
    case response of
        Http.GoodStatus_ _ body ->
            Decode.decodeString valueDecoder body
                |> Result.mapError (Decode.errorToString >> Http.BadBody)
                |> Result.map Ok

        Http.BadStatus_ _ body ->
            case Decode.decodeString ApiError.decoder body of
                Ok apiError ->
                    Ok (Err apiError)

                Err decodeError ->
                    Err (Http.BadBody (Decode.errorToString decodeError))

        Http.BadUrl_ url ->
            Ok (Err (ApiError.fromHttpError (Http.BadUrl url)))

        Http.Timeout_ ->
            Ok (Err (ApiError.fromHttpError Http.Timeout))

        Http.NetworkError_ ->
            Ok (Err (ApiError.fromHttpError Http.NetworkError))
