module Bytes.Decode.Extra exposing (byteValues, list)

import Bytes.Decode exposing (Decoder, Step(..), loop, map, succeed, unsignedInt8)


list : Int -> Decoder a -> Decoder (List a)
list length aDecoder =
    loop ( length, [] ) (listStep aDecoder)


listStep : Decoder a -> ( Int, List a ) -> Decoder (Step ( Int, List a ) (List a))
listStep elementDecoder ( n, elements ) =
    if n <= 0 then
        succeed (Done (List.reverse elements))

    else
        map (\element -> Loop ( n - 1, element :: elements )) elementDecoder


byteValues : Int -> Decoder (List Int)
byteValues length =
    list length unsignedInt8
