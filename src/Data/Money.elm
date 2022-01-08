module Data.Money exposing
    ( Money
    , add
    , decoder
    , encode
    , fromString
    , isNegative
    , subtract
    , toString
    , zero
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| Cents are included, so Money 12355 is really 123.55
-}
type Money
    = Money Int


fromParts : Int -> Int -> Money
fromParts whole cents =
    Money <| whole * 100 + cents


toParts : Money -> ( Int, Int )
toParts (Money n) =
    ( n // 100
    , n |> modBy 100
    )


{-|

    fromString "10" -> 1000
    fromString ".2" -> 20
    fromString "10.2" -> 1020
    fromString "10.02" -> 1002
    fromString "10.029" -> 1002

    fromString "10,2" -> 1020

-}
fromString : String -> Maybe Money
fromString string =
    let
        fixedString =
            String.replace "," "." string

        fixWholeString str =
            if String.isEmpty str then
                "0"

            else
                str
    in
    case String.split "." fixedString of
        [ wholeStr ] ->
            wholeStr
                |> fixWholeString
                |> String.toInt
                |> Maybe.map (\whole -> fromParts whole 0)

        [ wholeStr, centsStr ] ->
            let
                fixedWholeStr =
                    fixWholeString wholeStr

                fixedCentsStr =
                    centsStr
                        |> String.padRight 2 '0'
                        |> String.left 2
            in
            Maybe.map2 fromParts
                (String.toInt fixedWholeStr)
                (String.toInt fixedCentsStr)

        _ ->
            Nothing


toString : Money -> String
toString money =
    let
        ( whole, cents ) =
            toParts money
    in
    String.fromInt whole
        ++ "."
        ++ String.pad 2 '0' (String.fromInt cents)
        ++ " Kč"


add : Money -> Money -> Money
add (Money int1) (Money int2) =
    Money <| int1 + int2


{-| subtract 100 300 -> 200
-}
subtract : Money -> Money -> Money
subtract (Money int1) (Money int2) =
    Money <| int2 - int1


zero : Money
zero =
    Money 0


encode : Money -> Encode.Value
encode (Money int) =
    Encode.int int


decoder : Decoder Money
decoder =
    Decode.map Money Decode.int


isNegative : Money -> Bool
isNegative money =
    money
        |> toParts
        |> Tuple.first
        |> (\n -> n < 0)
