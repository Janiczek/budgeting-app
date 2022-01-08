module Data.IdSet exposing (IdSet, fromList, member)

import Data.Id exposing (Id)
import Set.Any exposing (AnySet)


type IdSet tag
    = IdSet (AnySet String (Id tag))


fromList : List (Id tag) -> IdSet tag
fromList list =
    IdSet <| Set.Any.fromList Data.Id.unwrap list


member : Id tag -> IdSet tag -> Bool
member id (IdSet set) =
    Set.Any.member id set
