module Api.Client exposing (getAccounts, getBootstrap, getTransactions, transactionsUrl)

import Api.Account as Account exposing (Response)
import Api.Bootstrap as Bootstrap exposing (Bootstrap)
import Api.Error as ApiError exposing (ApiError)
import Api.Transaction as Transaction exposing (Response)
import Http
import Json.Decode as Decode
import Url.Builder as UrlBuilder


getBootstrap : (Result ApiError Bootstrap -> msg) -> Cmd msg
getBootstrap toMsg =
    Http.get
        { url = "/api/v1/bootstrap"
        , expect = expectApiJson toMsg Bootstrap.decoder
        }


getAccounts : Maybe String -> (Result ApiError Account.Response -> msg) -> Cmd msg
getAccounts commodity toMsg =
    Http.get
        { url = accountsUrl commodity
        , expect = expectApiJson toMsg Account.decoder
        }


accountsUrl : Maybe String -> String
accountsUrl commodity =
    UrlBuilder.absolute [ "api", "v1", "accounts", "tree" ]
        (commodity
            |> Maybe.map (UrlBuilder.string "commodity" >> List.singleton)
            |> Maybe.withDefault []
        )


getTransactions : { query : String, cursor : Maybe String } -> (Result ApiError Response -> msg) -> Cmd msg
getTransactions request toMsg =
    Http.get
        { url = transactionsUrl request
        , expect = expectApiJson toMsg Transaction.decoder
        }


transactionsUrl : { query : String, cursor : Maybe String } -> String
transactionsUrl request =
    UrlBuilder.absolute [ "api", "v1", "transactions" ]
        (List.concat
            [ if String.isEmpty request.query then
                []

              else
                [ UrlBuilder.string "query" request.query ]
            , [ UrlBuilder.int "limit" 50 ]
            , request.cursor
                |> Maybe.map (UrlBuilder.string "cursor" >> List.singleton)
                |> Maybe.withDefault []
            ]
        )


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
