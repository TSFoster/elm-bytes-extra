module Bytes.Extra exposing (fromByteValues, toByteValues)

import Bytes exposing (Bytes)
import Bytes.Decode exposing (Decoder, decode)
import Bytes.Decode.Extra as Decode
import Bytes.Encode exposing (Encoder, encode)
import Bytes.Encode.Extra as Encode


fromByteValues : List Int -> Bytes
fromByteValues =
    encode << Encode.byteValues


toByteValues : Bytes -> List Int
toByteValues bytes =
    bytes
        |> decode (Decode.byteValues (Bytes.width bytes))
        |> Maybe.withDefault []
