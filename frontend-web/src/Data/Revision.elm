module Data.Revision exposing (Mismatch, ensureMatching)


type alias Mismatch =
    { expected : String
    , received : String
    }


ensureMatching : Maybe String -> Maybe String -> Result Mismatch ()
ensureMatching expected received =
    case ( expected, received ) of
        ( Just expectedRevision, Just receivedRevision ) ->
            if expectedRevision == receivedRevision then
                Ok ()

            else
                Err { expected = expectedRevision, received = receivedRevision }

        _ ->
            Ok ()
