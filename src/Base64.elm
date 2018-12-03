module Base64 exposing
    ( fromBytes, toBytes
    , encoder, decoder
    )

{-| This package can convert
[bytes](https://package.elm-lang.org/packages/elm/bytes/latest/)
to Base64 strings and vice versa.


# Conversion

@docs fromBytes, toBytes


# Bytes Encoder and Decoder

Slightly lower level functions.

[`fromBytes`](#fromBytes) and [`toBytes`](#toBytes) functions
are pretty much wrappers around these functions.

@docs encoder, decoder

-}

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Decode
import Encode


{-| Convert bytes to a Base64 string.
If you want more control over the process, you should use [`decoder`](#decoder).

This function should never return `Nothing`, but it uses
[`Bytes.Decode.decode`](https://package.elm-lang.org/packages/elm/bytes/latest/Bytes-Decode#decode),
which returns a `Maybe String`.

-}
fromBytes : Bytes -> Maybe String
fromBytes =
    Decode.fromBytes


{-| Convert a Base64 string to bytes.
If you want more control over the process, you should use [`encoder`](#encoder).

This function fails (returns `Nothing`) if you give it an invalid Base64 sequence.

-}
toBytes : String -> Maybe Bytes
toBytes =
    Encode.toBytes


{-| `decoder width` is a bytes decoder that will convert `width` bytes into a
Base64 string.

It's used in [`fromBytes`](#fromBytes):

    fromBytes : Bytes -> Maybe String
    fromBytes bytes =
        Bytes.Decode.decode (decoder (Bytes.width bytes)) bytes

-}
decoder : Int -> Bytes.Decode.Decoder String
decoder =
    Decode.decoder


{-| `encoder` returns a bytes encoder. It fails if the string that is passed
to it is not a valid Base64 sequence.

It's used in [`toBytes`](#toBytes):

    toBytes : String -> Maybe Bytes
    toBytes string =
        Maybe.map Bytes.Encode.encode (encoder string)

-}
encoder : String -> Maybe Bytes.Encode.Encoder
encoder =
    Encode.encoder
