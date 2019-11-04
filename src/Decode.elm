module Decode exposing (decoder, fromBytes)

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode


fromBytes : Bytes -> Maybe String
fromBytes bytes =
    Decode.decode (decoder (Bytes.width bytes)) bytes


decoder : Int -> Decode.Decoder String
decoder width =
    Decode.loop { remaining = width, string = "" } loopHelp



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
loopHelp : { remaining : Int, string : String } -> Decode.Decoder (Decode.Step { remaining : Int, string : String } String)
loopHelp { remaining, string } =
    if remaining >= 18 then
        -- Note: this case is heavily optimized. To understand what happens, look at the case `remaining >= 3` below.
        decode18Bytes
            |> Decode.map
                (\result ->
                    Decode.Loop
                        { remaining = remaining - 18
                        , string = string ++ result
                        }
                )

    else if remaining >= 3 then
        let
            helper a b c =
                let
                    combined =
                        Bitwise.or (Bitwise.or (Bitwise.shiftLeftBy 16 a) (Bitwise.shiftLeftBy 8 b)) c
                in
                Decode.Loop
                    { remaining = remaining - 3
                    , string = string ++ bitsToChars combined 0
                    }
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
        -- any 6-bit number is a valid base64 digit, so this is actually safe
        p =
            unsafeToChar (Bitwise.shiftRightZfBy 18 bits)

        q =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 12 bits) lowest6BitsMask)

        r =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 6 bits) lowest6BitsMask)

        s =
            unsafeToChar (Bitwise.and bits lowest6BitsMask)
    in
    case missing of
        0 ->
            String.cons p (String.cons q (String.cons r (String.fromChar s)))

        2 ->
            String.cons p (String.cons q "==")

        1 ->
            String.cons p (String.cons q (String.cons r "="))

        _ ->
            ""


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



-- OPTIMIZED VERSION


u32BE : Decode.Decoder Int
u32BE =
    Decode.unsignedInt32 Bytes.BE


u16BE : Decode.Decoder Int
u16BE =
    Decode.unsignedInt16 Bytes.BE


{-| A specialized version reading 18 bytes at once

This tries to take the biggest step possible within a `Decode.loop` iteration.
There is also some manual inlining to limit the number of function calls.

-}
decode18Bytes : Decode.Decoder String
decode18Bytes =
    Decode.map5 decode18Help
        u32BE
        u32BE
        u32BE
        u32BE
        u16BE


decode18Help : Int -> Int -> Int -> Int -> Int -> String
decode18Help a b c d e =
    let
        combined1 =
            Bitwise.shiftRightZfBy 8 a

        combined2 =
            Bitwise.or
                (Bitwise.and 0xFF a |> Bitwise.shiftLeftBy 16)
                (Bitwise.shiftRightZfBy 16 b)

        combined3 =
            Bitwise.or
                (Bitwise.and 0xFFFF b |> Bitwise.shiftLeftBy 8)
                (Bitwise.shiftRightZfBy 24 c)

        combined4 =
            Bitwise.and 0x00FFFFFF c

        combined5 =
            Bitwise.shiftRightZfBy 8 d

        combined6 =
            Bitwise.or
                (Bitwise.and 0xFF d |> Bitwise.shiftLeftBy 16)
                e
    in
    ""
        |> bitsToCharsSpec combined6 combined5 combined4
        |> bitsToCharsSpec combined3 combined2 combined1


bitsToCharsSpec : Int -> Int -> Int -> String -> String
bitsToCharsSpec bits1 bits2 bits3 accum =
    -- Performance: prevent calls to `bitsToChar`. The overhead of function calls
    -- became significant. This also allows more efficient string creation using String.cons
    let
        -- any 6-bit number is a valid base64 digit, so this is actually safe
        p =
            unsafeToChar (Bitwise.shiftRightZfBy 18 bits1)

        q =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 12 bits1) lowest6BitsMask)

        r =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 6 bits1) lowest6BitsMask)

        s =
            unsafeToChar (Bitwise.and bits1 lowest6BitsMask)
    in
    let
        a =
            unsafeToChar (Bitwise.shiftRightZfBy 18 bits2)

        b =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 12 bits2) lowest6BitsMask)

        c =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 6 bits2) lowest6BitsMask)

        d =
            unsafeToChar (Bitwise.and bits2 lowest6BitsMask)
    in
    let
        x =
            unsafeToChar (Bitwise.shiftRightZfBy 18 bits3)

        y =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 12 bits3) lowest6BitsMask)

        z =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 6 bits3) lowest6BitsMask)

        w =
            unsafeToChar (Bitwise.and bits3 lowest6BitsMask)
    in
    accum
        |> String.cons s
        |> String.cons r
        |> String.cons q
        |> String.cons p
        |> String.cons d
        |> String.cons c
        |> String.cons b
        |> String.cons a
        |> String.cons w
        |> String.cons z
        |> String.cons y
        |> String.cons x
