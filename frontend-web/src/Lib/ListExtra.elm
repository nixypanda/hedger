module Lib.ListExtra exposing (chunksOf, padToMultiple)


chunksOf : Int -> List value -> List (List value)
chunksOf size values =
    if List.isEmpty values then
        []

    else
        List.take size values :: chunksOf size (List.drop size values)


padToMultiple : Int -> value -> List value -> List value
padToMultiple size padding values =
    values
        ++ List.repeat
            (modBy size (size - modBy size (List.length values)))
            padding
