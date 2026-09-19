module Data.Metadata exposing (Metadata)

import Data.Freshness exposing (Freshness)


type alias Metadata =
    { revision : Maybe String
    , freshness : Freshness
    , warnings : List String
    }
