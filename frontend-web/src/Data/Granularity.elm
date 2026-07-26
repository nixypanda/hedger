module Data.Granularity exposing (Granularity(..), decoder, toString)

import Json.Decode as Decode exposing (Decoder)


type Granularity
    = Monthly
    | Yearly


toString : Granularity -> String
toString granularity =
    case granularity of
        Monthly ->
            "monthly"

        Yearly ->
            "yearly"


decoder : Decoder Granularity
decoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "monthly" ->
                        Decode.succeed Monthly

                    "yearly" ->
                        Decode.succeed Yearly

                    _ ->
                        Decode.fail ("Unknown granularity: " ++ value)
            )
