module Api.Client exposing
    ( getAccountRegister
    , getAccounts
    , getActivity
    , getBootstrap
    , getCashflow
    , getNetWorth
    , getTransactions
    , transactionsUrl
    )

import Api.Account as Account exposing (Response)
import Api.Analytics as Analytics
import Api.Bootstrap as Bootstrap exposing (Bootstrap)
import Api.Error as ApiError exposing (ApiError)
import Api.Register as Register
import Api.Transaction as Transaction exposing (Response)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
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


getNetWorth : Analytics.AnalyticsRequest -> (Result ApiError Analytics.NetWorthResponse -> msg) -> Cmd msg
getNetWorth request toMsg =
    analyticsPost "net-worth" (Analytics.requestEncoder request) Analytics.netWorthDecoder toMsg


getCashflow : Analytics.AnalyticsRequest -> (Result ApiError Analytics.CashflowResponse -> msg) -> Cmd msg
getCashflow request toMsg =
    analyticsPost "cashflow" (Analytics.requestEncoder request) Analytics.cashflowDecoder toMsg


getActivity : Analytics.ActivityRequest -> (Result ApiError Analytics.ActivityResponse -> msg) -> Cmd msg
getActivity request toMsg =
    analyticsPost "activity" (Analytics.activityRequestEncoder request) Analytics.activityDecoder toMsg


analyticsPost : String -> Encode.Value -> Decode.Decoder value -> (Result ApiError value -> msg) -> Cmd msg
analyticsPost screen body decoder toMsg =
    Http.post
        { url = UrlBuilder.absolute [ "api", "v1", screen, "query" ] []
        , body = Http.jsonBody body
        , expect = expectApiJson toMsg decoder
        }


getAccountRegister :
    { account : String
    , commodity : Maybe String
    , from : Maybe String
    , to : Maybe String
    , cursor : Maybe String
    }
    -> (Result ApiError Register.Response -> msg)
    -> Cmd msg
getAccountRegister request toMsg =
    Http.get
        { url = accountRegisterUrl request
        , expect = expectApiJson toMsg Register.decoder
        }


accountRegisterUrl :
    { account : String
    , commodity : Maybe String
    , from : Maybe String
    , to : Maybe String
    , cursor : Maybe String
    }
    -> String
accountRegisterUrl request =
    UrlBuilder.absolute [ "api", "v1", "accounts", request.account, "register" ]
        (List.concat
            [ [ UrlBuilder.int "limit" 50 ]
            , optionalQuery "commodity" request.commodity
            , optionalQuery "from" request.from
            , optionalQuery "to" request.to
            , optionalQuery "cursor" request.cursor
            ]
        )


optionalQuery : String -> Maybe String -> List UrlBuilder.QueryParameter
optionalQuery name value =
    value
        |> Maybe.map (UrlBuilder.string name >> List.singleton)
        |> Maybe.withDefault []


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
