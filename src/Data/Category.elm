module Data.Category exposing
    ( Category
    , CategoryId
    , CategoryIdTag
    , decoder
    , encode
    )

import Data.Id exposing (Id)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias CategoryId =
    Id CategoryIdTag


type CategoryIdTag
    = CategoryIdTag Never


type alias Category =
    { id : CategoryId
    , name : String
    }


encode : Category -> Encode.Value
encode category =
    Encode.object
        [ ( "id", Data.Id.encode category.id )
        , ( "name", Encode.string category.name )
        ]


decoder : Decoder Category
decoder =
    Decode.map2 Category
        (Decode.field "id" Data.Id.decoder)
        (Decode.field "name" Decode.string)
