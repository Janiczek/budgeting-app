module Data.Category exposing (Category, CategoryId, CategoryIdTag)

import Data.Id exposing (Id)


type alias CategoryId =
    Id CategoryIdTag


type CategoryIdTag
    = CategoryIdTag Never


type alias Category =
    { id : CategoryId
    , name : String
    }
