module Data.IdDict exposing
    ( IdDict
    , empty
    , filter
    , fromList
    , get
    , insert
    , keys
    , remove
    , update
    , values
    )

import Data.Id exposing (Id)
import Dict.Any exposing (AnyDict)


type IdDict tag value
    = IdDict (AnyDict String (Id tag) value)


empty : IdDict tag value
empty =
    IdDict <| Dict.Any.empty Data.Id.unwrap


fromList : List ( Id tag, value ) -> IdDict tag value
fromList list =
    IdDict <| Dict.Any.fromList Data.Id.unwrap list


insert : Id tag -> value -> IdDict tag value -> IdDict tag value
insert id value (IdDict dict) =
    IdDict <| Dict.Any.insert id value dict


remove : Id tag -> IdDict tag value -> IdDict tag value
remove id (IdDict dict) =
    IdDict <| Dict.Any.remove id dict


values : IdDict tag value -> List value
values (IdDict dict) =
    Dict.Any.values dict


keys : IdDict tag value -> List (Id tag)
keys (IdDict dict) =
    Dict.Any.keys dict


get : Id tag -> IdDict tag value -> Maybe value
get id (IdDict dict) =
    Dict.Any.get id dict


filter : (Id tag -> value -> Bool) -> IdDict tag value -> IdDict tag value
filter pred (IdDict dict) =
    IdDict <| Dict.Any.filter pred dict


update : Id tag -> (Maybe value -> Maybe value) -> IdDict tag value -> IdDict tag value
update id fn (IdDict dict) =
    IdDict <| Dict.Any.update id fn dict
