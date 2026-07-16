module Data.Decimal exposing (Decimal, decoder, fromString, toString)

import Json.Decode as Decode exposing (Decoder)


type Decimal
    = Decimal String


fromString : String -> Result String Decimal
fromString raw =
    let
        unsigned =
            if String.startsWith "-" raw then
                String.dropLeft 1 raw

            else
                raw

        parts =
            String.split "." unsigned

        allDigits value =
            not (String.isEmpty value) && String.all Char.isDigit value

        valid =
            not (String.isEmpty raw)
                && raw
                /= "-"
                && (case parts of
                        [ whole ] ->
                            allDigits whole

                        [ whole, fraction ] ->
                            allDigits whole && allDigits fraction

                        _ ->
                            False
                   )
    in
    if valid then
        Ok (Decimal raw)

    else
        Err "Expected a decimal string."


toString : Decimal -> String
toString (Decimal value) =
    value


decoder : Decoder Decimal
decoder =
    Decode.string
        |> Decode.andThen
            (fromString
                >> Result.map Decode.succeed
                >> Result.withDefault (Decode.fail "Expected a decimal string.")
            )
