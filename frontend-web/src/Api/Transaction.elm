module Api.Transaction exposing
    ( Response
    , decoder
    , transactionDecoder
    )

import Api.Metadata as Metadata exposing (Metadata)
import Data.AccountType as AccountType
import Data.Date as Date
import Data.Decimal as Decimal
import Data.Status as Status
import Data.Transaction exposing (Amount, Cost, Posting, Presentation, Tag, Transaction)
import Json.Decode as Decode exposing (Decoder)


type alias Response =
    { metadata : Metadata
    , transactions : List Transaction
    , nextCursor : Maybe String
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
            (\primaryDate ->
                \secondaryDate ->
                    \status ->
                        \code ->
                            \description ->
                                \comment ->
                                    \tags ->
                                        \postings ->
                                            \presentation ->
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
    Decode.map6 Posting
        (Decode.field "account" Decode.string)
        (Decode.field "accountType" (Decode.nullable AccountType.decoder))
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
