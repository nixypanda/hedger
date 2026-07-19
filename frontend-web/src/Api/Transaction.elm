module Api.Transaction exposing
    ( Amount
    , Cost
    , Posting
    , Presentation
    , Response
    , Tag
    , Transaction
    , decoder
    , transactionDecoder
    )

import Api.Metadata as Metadata exposing (Metadata)
import Data.Date as Date exposing (Date)
import Data.Decimal as Decimal exposing (Decimal)
import Data.Status as Status exposing (Status)
import Json.Decode as Decode exposing (Decoder)


type alias Response =
    { metadata : Metadata
    , transactions : List Transaction
    , nextCursor : Maybe String
    }


type alias Transaction =
    { primaryDate : Date
    , secondaryDate : Maybe Date
    , status : Status
    , code : Maybe String
    , description : String
    , comment : Maybe String
    , tags : List Tag
    , postings : List Posting
    , presentation : Presentation
    }


type alias Posting =
    { account : String
    , status : Status
    , comment : Maybe String
    , tags : List Tag
    , amounts : List Amount
    }


type alias Presentation =
    { classification : String
    }


type alias Amount =
    { commodity : String
    , quantity : Decimal
    , cost : Maybe Cost
    }


type alias Cost =
    { costType : String
    , commodity : String
    , quantity : Decimal
    }


type alias Tag =
    { name : String
    , value : String
    }


decoder : Decoder Response
decoder =
    Decode.map3 Response
        (Decode.field "metadata" Metadata.decoder)
        (Decode.field "transactions" (Decode.list transactionDecoder))
        (Decode.field "nextCursor" (Decode.nullable Decode.string))


transactionDecoder : Decoder Transaction
transactionDecoder =
    Decode.map2
        (\partial presentation -> partial presentation)
        (Decode.map8
            (\primaryDate secondaryDate status code description comment tags postings presentation ->
                Transaction primaryDate
                    secondaryDate
                    status
                    code
                    description
                    comment
                    tags
                    postings
                    presentation
            )
            (Decode.field "primaryDate" Date.decoder)
            (Decode.field "secondaryDate" (Decode.nullable Date.decoder))
            (Decode.field "status" Status.decoder)
            (Decode.field "code" (Decode.nullable Decode.string))
            (Decode.field "description" Decode.string)
            (Decode.field "comment" (Decode.nullable Decode.string))
            (Decode.field "tags" (Decode.list tagDecoder))
            (Decode.field "postings" (Decode.list postingDecoder))
        )
        (Decode.field "presentation" presentationDecoder)


presentationDecoder : Decoder Presentation
presentationDecoder =
    Decode.map Presentation
        (Decode.field "classification" Decode.string)


postingDecoder : Decoder Posting
postingDecoder =
    Decode.map5 Posting
        (Decode.field "account" Decode.string)
        (Decode.field "status" Status.decoder)
        (Decode.field "comment" (Decode.nullable Decode.string))
        (Decode.field "tags" (Decode.list tagDecoder))
        (Decode.field "amounts" (Decode.list amountDecoder))


amountDecoder : Decoder Amount
amountDecoder =
    Decode.map3 Amount
        (Decode.field "commodity" Decode.string)
        (Decode.field "quantity" Decimal.decoder)
        (Decode.field "cost" (Decode.nullable costDecoder))


costDecoder : Decoder Cost
costDecoder =
    Decode.map3 Cost
        (Decode.field "costType" Decode.string)
        (Decode.field "commodity" Decode.string)
        (Decode.field "quantity" Decimal.decoder)


tagDecoder : Decoder Tag
tagDecoder =
    Decode.map2 Tag
        (Decode.field "name" Decode.string)
        (Decode.field "value" Decode.string)
