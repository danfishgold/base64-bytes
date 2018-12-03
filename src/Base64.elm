module Base64 exposing (decoder, encode, fromBytes, toBytes)

import Bytes exposing (Bytes)
import Decode
import Encode


encode =
    Encode.encode


toBytes =
    Encode.toBytes


decoder =
    Decode.decoder


fromBytes =
    Decode.fromBytes
