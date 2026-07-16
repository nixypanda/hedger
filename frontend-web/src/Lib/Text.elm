module Lib.Text exposing (dotSeparated, formatOptionalFloat)


formatOptionalFloat : Maybe Float -> String
formatOptionalFloat amount =
    amount
        |> Maybe.map String.fromFloat
        |> Maybe.withDefault ""


dotSeparated : List (Maybe String) -> String
dotSeparated values =
    values |> List.filterMap identity |> String.join " · "
