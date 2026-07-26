module Api.Investments exposing
    ( Breadcrumb
    , Child
    , Request
    , Response
    , Summary
    , TimelinePoint
    , Warning
    , decoder
    , requestEncoder
    )

import Api.Json exposing (maybeString, required)
import Api.Metadata as Metadata exposing (Metadata)
import Data.Completeness as Completeness exposing (Completeness)
import Data.Date as Date exposing (Date)
import Data.Decimal as Decimal exposing (Decimal)
import Data.Granularity as Granularity exposing (Granularity)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Request =
    { granularity : Granularity
    , displayCommodity : Maybe String
    , accountPrefix : Maybe String
    }


type alias Summary =
    { marketValue : Maybe Decimal
    , purchaseCost : Maybe Decimal
    , gainLoss : Maybe Decimal
    , returnPercentage : Maybe Decimal
    , attributedIncome : Maybe Decimal
    , realisedGain : Maybe Decimal
    , totalProfit : Maybe Decimal
    , completeness : Completeness
    }


type alias TimelinePoint =
    { period : String
    , startDate : Date
    , summary : Summary
    }


type alias Breadcrumb =
    { account : String
    , label : String
    }


type alias Child =
    { account : String
    , label : String
    , summary : Summary
    , hasChildren : Bool
    }


type alias Warning =
    { kind : String
    , account : Maybe String
    , message : String
    }


type alias Response =
    { metadata : Metadata
    , displayCommodity : String
    , granularity : Granularity
    , accountPrefix : Maybe String
    , summary : Summary
    , timeline : List TimelinePoint
    , breadcrumbs : List Breadcrumb
    , children : List Child
    , warnings : List Warning
    }


requestEncoder : Request -> Encode.Value
requestEncoder request =
    Encode.object
        [ ( "granularity", Encode.string (Granularity.toString request.granularity) )
        , ( "displayCommodity", maybeString request.displayCommodity )
        , ( "accountPrefix", maybeString request.accountPrefix )
        ]


decoder : Decoder Response
decoder =
    Decode.succeed Response
        |> required "metadata" Metadata.decoder
        |> required "displayCommodity" Decode.string
        |> required "granularity" Granularity.decoder
        |> required "accountPrefix" (Decode.nullable Decode.string)
        |> required "summary" summaryDecoder
        |> required "timeline" (Decode.list timelineDecoder)
        |> required "breadcrumbs" (Decode.list breadcrumbDecoder)
        |> required "children" (Decode.list childDecoder)
        |> required "warnings" (Decode.list warningDecoder)


summaryDecoder : Decoder Summary
summaryDecoder =
    Decode.succeed Summary
        |> required "marketValue" (Decode.nullable Decimal.decoder)
        |> required "purchaseCost" (Decode.nullable Decimal.decoder)
        |> required "gainLoss" (Decode.nullable Decimal.decoder)
        |> required "returnPercentage" (Decode.nullable Decimal.decoder)
        |> required "attributedIncome" (Decode.nullable Decimal.decoder)
        |> required "realisedGain" (Decode.nullable Decimal.decoder)
        |> required "totalProfit" (Decode.nullable Decimal.decoder)
        |> required "completeness" Completeness.decoder


timelineDecoder : Decoder TimelinePoint
timelineDecoder =
    Decode.map3 TimelinePoint
        (Decode.field "period" Decode.string)
        (Decode.field "startDate" Date.decoder)
        summaryDecoder


breadcrumbDecoder : Decoder Breadcrumb
breadcrumbDecoder =
    Decode.map2 Breadcrumb
        (Decode.field "account" Decode.string)
        (Decode.field "label" Decode.string)


childDecoder : Decoder Child
childDecoder =
    Decode.map4 Child
        (Decode.field "account" Decode.string)
        (Decode.field "label" Decode.string)
        (Decode.field "summary" summaryDecoder)
        (Decode.field "hasChildren" Decode.bool)


warningDecoder : Decoder Warning
warningDecoder =
    Decode.map3 Warning
        (Decode.field "kind" Decode.string)
        (Decode.field "account" (Decode.nullable Decode.string))
        (Decode.field "message" Decode.string)
