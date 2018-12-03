module Encode exposing (encode, toBytes)

import Basic exposing (Int6(..), charToInt6, listFromMaybeList, quadMap)
import Bytes exposing (Bytes)
import Bytes.Encode as Encode


toBytes : List Char -> Maybe Bytes
toBytes chars =
    Maybe.map Encode.encode (encode chars)


encode : List Char -> Maybe Encode.Encoder
encode chars =
    chars
        |> charsToInts
        |> Maybe.map (List.map Encode.unsignedInt8)
        |> Maybe.map Encode.sequence


charsToInts : List Char -> Maybe (List Int)
charsToInts chars =
    chars
        |> quadMap fourCharsToThreeInts
        |> listFromMaybeList
        |> Maybe.map List.concat


fourCharsToThreeInts : Char -> Maybe Char -> Maybe Char -> Maybe Char -> Maybe (List Int)
fourCharsToThreeInts a b c d =
    case fourCharsToInt24 a b c d of
        Nothing ->
            Nothing

        Just ( n, padding ) ->
            let
                b1 =
                    n // (2 ^ 8) // (2 ^ 8) |> modBy (2 ^ 8)

                b2 =
                    n // (2 ^ 8) |> modBy (2 ^ 8)

                b3 =
                    modBy (2 ^ 8) n
            in
            case padding of
                None ->
                    Just [ b1, b2, b3 ]

                One ->
                    Just [ b1, b2 ]

                Two ->
                    Just [ b1 ]


type Padding
    = None
    | One
    | Two


fourCharsToInt24 : Char -> Maybe Char -> Maybe Char -> Maybe Char -> Maybe ( Int, Padding )
fourCharsToInt24 a b c d =
    case ( b, c, d ) of
        ( Just b_, Just c_, Just d_ ) ->
            case ( charToInt6 a, charToInt6 b_ ) of
                ( Number n1, Number n2 ) ->
                    case ( charToInt6 c_, charToInt6 d_ ) of
                        ( Number n3, Number n4 ) ->
                            Just ( n1 * 2 ^ 18 + n2 * 2 ^ 12 + n3 * 2 ^ 6 + n4, None )

                        ( Number n3, Padding ) ->
                            Just ( n1 * 2 ^ 18 + n2 * 2 ^ 12 + n3 * 2 ^ 6, One )

                        ( Padding, Padding ) ->
                            Just ( n1 * 2 ^ 18 + n2 * 2 ^ 12, Two )

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing
