module Data.Id exposing (Id, generator, unwrap)

import Random exposing (Generator)
import UUID


type Id tag
    = Id String


generator : Generator (Id tag)
generator =
    UUID.generator
        |> Random.map (UUID.toString >> Id)


unwrap : Id tag -> String
unwrap (Id string) =
    string
