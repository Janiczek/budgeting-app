module Data.Id exposing
    ( Id
    , decoder
    , encode
    , fromString
    , generator
    , unwrap
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random exposing (Generator)
import UUID


type Id tag
    = Id String


generator : Generator (Id tag)
generator =
    UUID.generator
        |> Random.map (UUID.toString >> Id)


fromString : String -> Id tag
fromString string =
    Id string


unwrap : Id tag -> String
unwrap (Id string) =
    string


encode : Id tag -> Encode.Value
encode (Id string) =
    Encode.string string


decoder : Decoder (Id tag)
decoder =
    Decode.map Id Decode.string
