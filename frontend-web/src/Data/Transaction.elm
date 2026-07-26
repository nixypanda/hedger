module Data.Transaction exposing
    ( Amount
    , Cost
    , Posting
    , Presentation
    , Tag
    , Transaction
    )

import Data.Date exposing (Date)
import Data.Decimal exposing (Decimal)
import Data.Status exposing (Status)


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
