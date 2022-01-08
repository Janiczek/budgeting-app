module Data.Bucket exposing (Bucket, BucketId, BucketIdTag)

import Data.Category exposing (CategoryId)
import Data.Id exposing (Id)
import Data.Money as Money exposing (Money)


type alias BucketId =
    Id BucketIdTag


type BucketIdTag
    = BucketIdTag Never


type alias Bucket =
    { id : BucketId
    , name : String
    , categoryId : CategoryId
    , value : Money
    }
