module Api.Revision exposing (Mismatch, ensureMatching)

import Api.Metadata exposing (Metadata)


type alias Mismatch =
    { expected : String
    , received : String
    }


ensureMatching : Maybe String -> Metadata -> Result Mismatch ()
ensureMatching expected metadata =
    case ( expected, metadata.revision ) of
        ( Just expectedRevision, Just receivedRevision ) ->
            if expectedRevision == receivedRevision then
                Ok ()

            else
                Err { expected = expectedRevision, received = receivedRevision }

        _ ->
            Ok ()
