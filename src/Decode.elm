module Decode exposing (decoder, fromBytes)

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode


fromBytes : Bytes -> Maybe String
fromBytes bytes =
    Decode.decode (decoder (Bytes.width bytes)) bytes


decoder : Int -> Decode.Decoder String
decoder width =
    Decode.loop ( width, "" ) loopHelp



-- INTERNALS


{-| Base64 uses 6 bits per digit (because 2^6 == 64)
and can nicely store 4 digits in 24 bits, which are 3 bytes.

The decoding process is thus roughly

  - read a 3-byte chunk
  - extract the 4 6-bit segments
  - convert those segments into characters

But the input does not need to have a multiple of 4 characters,
so at the end of the string some characters can be omitted.
This means there may be 2 or 1 byte remaining at the end. We have to cover those cases!

-}
loopHelp : ( Int, String ) -> Decode.Decoder (Decode.Step ( Int, String ) String)
loopHelp ( remaining, string ) =
    {- Performance Notes

       the elm/bytes package uses a DataView under the hood.
       These only allow reading/writing uint8, so there is no gain in decoding a uint16 here
    -}
    if remaining >= 3 then
        let
            helper a b c =
                let
                    combined =
                        Bitwise.or (Bitwise.or (Bitwise.shiftLeftBy 16 a) (Bitwise.shiftLeftBy 8 b)) c
                in
                Decode.Loop
                    ( remaining - 3
                    , string ++ bitsToChars combined 0
                    )
        in
        Decode.map3 helper
            Decode.unsignedInt8
            Decode.unsignedInt8
            Decode.unsignedInt8

    else if remaining == 0 then
        Decode.succeed (Decode.Done string)

    else if remaining == 2 then
        let
            helper a b =
                let
                    combined =
                        Bitwise.or (Bitwise.shiftLeftBy 16 a) (Bitwise.shiftLeftBy 8 b)
                in
                Decode.Done (string ++ bitsToChars combined 1)
        in
        Decode.map2 helper
            Decode.unsignedInt8
            Decode.unsignedInt8

    else
        -- remaining == 1
        Decode.map (\a -> Decode.Done (string ++ bitsToChars (Bitwise.shiftLeftBy 16 a) 2))
            Decode.unsignedInt8


{-| Mask that can be used to get the lowest 6 bits of a binary number
-}
lowest6BitsMask : Int
lowest6BitsMask =
    63


{-| Turn the decoded bits (at most 24, can be fewer because of padding) into 4 base64 characters.

(- - - - - - - -)(- - - - - - - -)(- - - - - - - -)
(- - - - - -|- - - - - -|- - - - - -|- - - - - -)

-}
bitsToChars : Int -> Int -> String
bitsToChars bits missing =
    {- Performance Notes

       `String.cons` proved to be the fastest way of combining characters into a string

       The input is 24 bits, which we have to partition into 4 6-bit segments. We achieve this by
       shifting to the right by (a multiple of) 6 to remove unwanted bits on the right, then `Bitwise.and`
       with `0b111111` (which is 2^6 - 1 or 63) (so, 6 1s) to remove unwanted bits on the left.

    -}
    let
        x =
            Bitwise.shiftRightBy 18 bits

        y =
            Bitwise.and (Bitwise.shiftRightBy 12 bits) lowest6BitsMask

        z =
            Bitwise.and (Bitwise.shiftRightBy 6 bits) lowest6BitsMask

        w =
            Bitwise.and bits lowest6BitsMask

        -- any 6-bit number is a valid base64 digit, so this is actually safe
        p =
            unsafeToChar x

        q =
            unsafeToChar y

        r =
            unsafeToChar z

        s =
            unsafeToChar w
    in
    case missing of
        2 ->
            String.cons p (String.cons q "==")

        1 ->
            String.cons p (String.cons q (String.cons r "="))

        _ ->
            String.cons p (String.cons q (String.cons r (String.fromChar s)))


{-| Base64 index to character/digit
-}
unsafeToChar : Int -> Char
unsafeToChar n =
    if n <= 25 then
        Char.fromCode (65 + n)

    else if n <= 51 then
        Char.fromCode (97 + (n - 26))

    else if n <= 61 then
        Char.fromCode (48 + (n - 52))

    else
        case n of
            62 ->
                '+'

            63 ->
                '/'

            _ ->
                '\u{0000}'
