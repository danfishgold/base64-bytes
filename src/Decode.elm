module Decode exposing (decoder, fromBytes)

import Basic exposing (intToChar, listFromMaybeList, tripleMap)
import Bytes exposing (Bytes)
import Bytes.Decode as Decode


fromBytes : Bytes -> Maybe String
fromBytes bytes =
    Decode.decode (decoder (Bytes.width bytes)) bytes


decoder : Int -> Decode.Decoder String
decoder width =
    intsDecoder width
        |> Decode.andThen
            (\ints ->
                case intsToString ints of
                    Nothing ->
                        Decode.fail

                    Just string ->
                        Decode.succeed string
            )



--


intsDecoder : Int -> Decode.Decoder (List Int)
intsDecoder width =
    Decode.loop ( width, [] )
        (\( len, ints ) ->
            if len <= 0 then
                Decode.succeed <| Decode.Done ( len, ints )

            else
                Decode.unsignedInt8
                    |> Decode.map (\int -> Decode.Loop ( len - 1, int :: ints ))
        )
        |> Decode.map (Tuple.second >> List.reverse)


intsToString : List Int -> Maybe String
intsToString ints =
    ints
        |> tripleMap threeBytesToFourChars
        |> listFromMaybeList
        |> Maybe.map List.concat
        |> Maybe.map String.fromList


{-| Turn three int8s to four characters in b64
(- - - - - - - -)(- - - - - - - -)(- - - - - - - -)
(- - - - - -|- - - - - -|- - - - - -|- - - - - -)
-}
threeBytesToFourChars : Int -> Maybe Int -> Maybe Int -> Maybe (List Char)
threeBytesToFourChars a b c =
    let
        n =
            a * 2 ^ 16 + Maybe.withDefault 0 b * 2 ^ 8 + Maybe.withDefault 0 c

        c1 =
            n // 2 ^ 18 |> intToChar

        c2 =
            n // 2 ^ 12 |> modBy (2 ^ 6) |> intToChar

        c3 =
            if c == Nothing && b == Nothing then
                Just '='

            else
                n // 2 ^ 6 |> modBy (2 ^ 6) |> intToChar

        c4 =
            if c == Nothing then
                Just '='

            else
                modBy (2 ^ 6) n |> intToChar
    in
    listFromMaybeList [ c1, c2, c3, c4 ]
