module Data.Status exposing (Status(..), decoder, toString)

import Json.Decode as Decode exposing (Decoder)


type Status
    = Unmarked
    | Pending
    | Cleared


toString : Status -> String
toString status =
    case status of
        Unmarked ->
            "Unmarked"

        Pending ->
            "Pending"

        Cleared ->
            "Cleared"


decoder : Decoder Status
decoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "unmarked" ->
                        Decode.succeed Unmarked

                    "pending" ->
                        Decode.succeed Pending

                    "cleared" ->
                        Decode.succeed Cleared

                    _ ->
                        Decode.fail ("Unknown status: " ++ value)
            )
