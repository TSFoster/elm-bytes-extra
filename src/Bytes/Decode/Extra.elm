module Bytes.Decode.Extra exposing (andMap, byteValues, hardcoded, list, map6, map7, map8)

import Bytes.Decode exposing (..)


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


required : Decoder a -> Decoder (a -> b) -> Decoder b
required aDecoder fnDecoder =
    map2 (<|) fnDecoder aDecoder


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap aDecoder fnDecoder =
    map2 (<|) fnDecoder aDecoder


map6 : (a -> b -> c -> d -> e -> f -> result) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder result
map6 f decoderA decoderB decoderC decoderD decoderE decoderF =
    map5 f decoderA decoderB decoderC decoderD decoderE
        |> andMap decoderF


map7 : (a -> b -> c -> d -> e -> f -> g -> result) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder result
map7 f decoderA decoderB decoderC decoderD decoderE decoderF decoderG =
    map4 f decoderA decoderB decoderC decoderD
        |> andThen (\g -> map3 g decoderE decoderF decoderG)


map8 : (a -> b -> c -> d -> e -> f -> g -> h -> result) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder result
map8 f decoderA decoderB decoderC decoderD decoderE decoderF decoderG decoderH =
    map4 f decoderA decoderB decoderC decoderD
        |> andThen (\g -> map4 g decoderE decoderF decoderG decoderH)


hardcoded : a -> Decoder (a -> b) -> Decoder b
hardcoded =
    map << (|>)
