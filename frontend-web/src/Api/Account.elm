module Api.Account exposing (AccountNode(..), Completeness(..), Date(..), Response, Valuation, decoder)

import Api.Metadata as Metadata exposing (Metadata)
import Json.Decode as Decode exposing (Decoder)


type alias Quantity =
    { commodity : String
    , quantity : String
    }


type Date
    = Date
        { year : Int
        , month : Int
        , day : Int
        }


type Completeness
    = Complete
    | Partial
    | Unavailable


type AccountNode
    = AccountNode
        { path : String
        , name : String
        , accountType : Maybe String
        , hasChildren : Bool
        , directBalances : List Quantity
        , subtreeBalances : List Quantity
        , valuation : Valuation
        , children : List AccountNode
        }


type alias Valuation =
    { asOf : Date
    , commodity : String
    , directBalances : List Quantity
    , subtreeBalances : List Quantity
    , completeness : Completeness
    }


type alias Response =
    { metadata : Metadata
    , displayCommodity : String
    , accounts : List AccountNode
    }


decoder : Decoder Response
decoder =
    Decode.map3 Response
        (Decode.field "metadata" Metadata.decoder)
        (Decode.field "displayCommodity" Decode.string)
        (Decode.field "accounts" (Decode.list accountDecoder))


accountDecoder : Decoder AccountNode
accountDecoder =
    Decode.map8
        (\path name accountType hasChildren directBalances subtreeBalances valuation children ->
            AccountNode
                { path = path
                , name = name
                , accountType = accountType
                , hasChildren = hasChildren
                , directBalances = directBalances
                , subtreeBalances = subtreeBalances
                , valuation = valuation
                , children = children
                }
        )
        (Decode.field "path" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "accountType" (Decode.nullable Decode.string))
        (Decode.field "hasChildren" Decode.bool)
        (Decode.field "directBalances" (Decode.list quantityDecoder))
        (Decode.field "subtreeBalances" (Decode.list quantityDecoder))
        (Decode.field "valuation" valuationDecoder)
        (Decode.field "children" (Decode.list (Decode.lazy (\_ -> accountDecoder))))


quantityDecoder : Decoder Quantity
quantityDecoder =
    Decode.map2 Quantity
        (Decode.field "commodity" Decode.string)
        (Decode.field "quantity" Decode.string)


valuationDecoder : Decoder Valuation
valuationDecoder =
    Decode.map5 Valuation
        (Decode.field "asOf" dateDecoder)
        (Decode.field "commodity" Decode.string)
        (Decode.field "directBalances" (Decode.list quantityDecoder))
        (Decode.field "subtreeBalances" (Decode.list quantityDecoder))
        (Decode.field "completeness" completenessDecoder)


dateDecoder : Decoder Date
dateDecoder =
    Decode.string
        |> Decode.andThen parseDate


parseDate : String -> Decoder Date
parseDate value =
    case String.split "-" value of
        [ yearText, monthText, dayText ] ->
            case ( String.toInt yearText, String.toInt monthText, String.toInt dayText ) of
                ( Just year, Just month, Just day ) ->
                    if String.length yearText == 4 && String.length monthText == 2 && String.length dayText == 2 && validDate year month day then
                        Decode.succeed (Date { year = year, month = month, day = day })

                    else
                        Decode.fail "Expected a valid ISO date."

                _ ->
                    Decode.fail "Expected a valid ISO date."

        _ ->
            Decode.fail "Expected a valid ISO date."


validDate : Int -> Int -> Int -> Bool
validDate year month day =
    month >= 1 && month <= 12 && day >= 1 && day <= daysInMonth year month


daysInMonth : Int -> Int -> Int
daysInMonth year month =
    case month of
        2 ->
            if modBy 400 year == 0 || (modBy 4 year == 0 && modBy 100 year /= 0) then
                29

            else
                28

        4 ->
            30

        6 ->
            30

        9 ->
            30

        11 ->
            30

        _ ->
            31


completenessDecoder : Decoder Completeness
completenessDecoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "complete" ->
                        Decode.succeed Complete

                    "partial" ->
                        Decode.succeed Partial

                    "unavailable" ->
                        Decode.succeed Unavailable

                    _ ->
                        Decode.fail ("Unknown completeness: " ++ value)
            )
