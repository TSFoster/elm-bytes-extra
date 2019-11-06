module Bytes.Encode.Extra exposing (byteValues, list)

import Bytes.Encode exposing (Encoder, sequence, unsignedInt8)


list : (a -> Encoder) -> List a -> Encoder
list encode =
    sequence << List.map encode


byteValues : List Int -> Encoder
byteValues =
    list unsignedInt8
