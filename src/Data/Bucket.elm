module Data.Bucket exposing
    ( Bucket
    , BucketId
    , BucketIdTag
    , decoder
    , encode
    )

import Data.Category exposing (CategoryId)
import Data.Id exposing (Id)
import Data.Money as Money exposing (Money)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Json.Encode.Extra as Encode


type alias BucketId =
    Id BucketIdTag


type BucketIdTag
    = BucketIdTag Never


type alias Bucket =
    { id : BucketId
    , name : String
    , categoryId : CategoryId
    , value : Money
    , goal : Maybe Money
    }


encode : Bucket -> Encode.Value
encode bucket =
    Encode.object
        [ ( "id", Data.Id.encode bucket.id )
        , ( "name", Encode.string bucket.name )
        , ( "categoryId", Data.Id.encode bucket.categoryId )
        , ( "value", Money.encode bucket.value )
        , ( "goal", Encode.maybe Money.encode bucket.goal )
        ]


decoder : Decoder Bucket
decoder =
    Decode.map5 Bucket
        (Decode.field "id" Data.Id.decoder)
        (Decode.field "name" Decode.string)
        (Decode.field "categoryId" Data.Id.decoder)
        (Decode.field "value" Money.decoder)
        (Decode.field "goal" (Decode.nullable Money.decoder))
