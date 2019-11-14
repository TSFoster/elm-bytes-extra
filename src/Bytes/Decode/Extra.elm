module Bytes.Decode.Extra exposing
    ( list, byteValues
    , unsignedInt24, signedInt24
    , andMap, hardcoded
    , map6, map7, map8, map9, map16
    , onlyOks, onlyJusts
    )

{-| Helpers for working with `Bytes.Decoder`s.

@docs list, byteValues


## 24-bit Integers

One notable use of 24-bit integers is in 24-bit color, a.k.a. "True color".

@docs unsignedInt24, signedInt24


## Pipeline-style

[`Json.Decode.Pipeline`][json-pipeline] provides a neat, scalable way for
decoding JSON. This module provides a few helpers for adopting a similar style
when decoding `Bytes`.

It is important to note that decoding bytes poses more restrictions than
decoding JSON, as bytes are consumed in order, and a maximum of one time. This
makes an equivalent to `Json.Decode.Pipeline.optional` difficult to impossible.

    import Bytes exposing (Endianness(..))
    import Bytes.Decode exposing (Decoder, succeed, andThen, unsignedInt32, string, float64, decode)
    import Bytes.Extra exposing (fromByteValues)

    type Status = Downloaded | LocallyGenerated

    type alias MyData =
      { id: Int
      , status : Status
      , name: String
      , value: Float
      }

    myDataDecoder : Decoder MyData
    myDataDecoder =
        succeed MyData
            |> andMap (unsignedInt32 BE)
            |> hardcoded Downloaded
            |> andMap (unsignedInt32 BE |> andThen string)
            |> andMap (float64 BE)


    decode myDataDecoder (fromByteValues [0,0,0,252,0,0,0,8,116,104,101,95,110,97,109,101,63,224,0,0,0,0,0,0])
    --> Just
    -->     { id = 252
    -->     , status = Downloaded
    -->     , name = "the_name"
    -->     , value = 0.5
    -->     }

[json-pipeline]: https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/

@docs andMap, hardcoded


## Extra maps

@docs map6, map7, map8, map9, map16


## Working with Results and Maybes

@docs onlyOks, onlyJusts

-}

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode exposing (Decoder, Step(..), andThen, fail, loop, map, map2, map3, map4, map5, signedInt8, succeed, unsignedInt8)


{-| Run a decoder a set amount of times, and collect the result in a list.

    import Bytes.Decode
    import Bytes.Extra exposing (fromByteValues)


    fromByteValues (List.range 0 20)
        |> Bytes.Decode.decode (Bytes.Decode.Extra.list 21 Bytes.Decode.unsignedInt8)
    --> Just (List.range 0 20)

-}
list : Int -> Decoder a -> Decoder (List a)
list length aDecoder =
    loop ( length, [] ) (listStep aDecoder)


listStep : Decoder a -> ( Int, List a ) -> Decoder (Step ( Int, List a ) (List a))
listStep elementDecoder ( n, elements ) =
    if n <= 0 then
        succeed (Done (List.reverse elements))

    else
        map (\element -> Loop ( n - 1, element :: elements )) elementDecoder


{-| Before the release of [elm/bytes], many packages would use `List Int`
to represent bytes. `byteValues` aids interaciton between those packages
and `Bytes`.

    import Bytes exposing (Bytes)
    import Bytes.Decode
    import Bytes.Extra exposing (fromByteValues)

    input : Bytes
    input =
        fromByteValues [ 0x32, 0xFF, 0x53, 0x54, 0x55 ]

    Bytes.Decode.decode (byteValues 3) input
    --> Just [ 0x32, 0xFF, 0x53 ]

-}
byteValues : Int -> Decoder (List Int)
byteValues length =
    list length unsignedInt8



-- 24-BIT INTEGERS


pack24 : Int -> Int -> Int -> Int
pack24 a b c =
    Bitwise.or (Bitwise.shiftLeftBy 16 a) (Bitwise.or (Bitwise.shiftLeftBy 8 b) c)


{-| Decode three bytes into an integer from `-8388608` to `8388607`.
-}
signedInt24 : Endianness -> Decoder Int
signedInt24 endianness =
    case endianness of
        BE ->
            map3 pack24 signedInt8 unsignedInt8 unsignedInt8

        LE ->
            map3 (\b1 b2 b3 -> pack24 b3 b2 b1) unsignedInt8 unsignedInt8 signedInt8


{-| Decode three bytes into an integer from `0` to `16777215`.
-}
unsignedInt24 : Endianness -> Decoder Int
unsignedInt24 endianness =
    case endianness of
        BE ->
            map3 pack24 unsignedInt8 unsignedInt8 unsignedInt8

        LE ->
            map3 (\b1 b2 b3 -> pack24 b3 b2 b1) unsignedInt8 unsignedInt8 unsignedInt8



--> PIPELINE


{-| `andMap` behaves in a similar way to [`Json.Decode.Pipeline.required`][pipeline-required]. It is named `andMap` to match the naming of similar functions in other packages, and because this package does not provide an equivalent to `Json.Decode.Pipeline.optional`.

    modelDecoder : Decoder Model
    modelDecoder =
        succeed Model
            |> andMap (unsignedInt32 BE)
            |> andMap (unsignedInt32 BE |> andThen string)
            |> andMap myCustomTypeDecoder
            |> andMap myCustomBooleanDecoder

[pipeline-required]: https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/Json-Decode-Pipeline#required

-}
andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap aDecoder fnDecoder =
    map2 (<|) fnDecoder aDecoder


{-| A neat way to fill in predetermined information that's not part of the data
being decoded, similar to [`Json.Decode.Pipeline.hardcoded`][json-hardcoded].

    modelDecoder : Decoder Model
    modelDecoder =
        succeed Model
            |> andMap (unsignedInt32 BE)
            |> andMap (unsignedInt32 BE |> andThen string)
            |> hardcoded []
            |> andMap myCustomBooleanDecoder
            |> hardcoded SubModel.default

[json-hardcoded]: https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/Json-Decode-Pipeline#hardcoded

-}
hardcoded : a -> Decoder (a -> b) -> Decoder b
hardcoded =
    map << (|>)



-- EXTRA MAPS


{-| -}
map6 :
    (a -> b -> c -> d -> e -> f -> result)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder result
map6 func a b c d e f =
    map3 identity (map4 func a b c d) e f


{-| -}
map7 :
    (a -> b -> c -> d -> e -> f -> g -> result)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder result
map7 func a b c d e f g =
    map4 identity (map4 func a b c d) e f g


{-| -}
map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> result)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder h
    -> Decoder result
map8 func a b c d e f g h =
    map5 identity (map4 func a b c d) e f g h


{-| -}
map9 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> result)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder h
    -> Decoder i
    -> Decoder result
map9 func a b c d e f g h i =
    map5 identity (map5 func a b c d e) f g h i


{-| -}
map16 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p -> result)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder h
    -> Decoder i
    -> Decoder j
    -> Decoder k
    -> Decoder l
    -> Decoder m
    -> Decoder n
    -> Decoder o
    -> Decoder p
    -> Decoder result
map16 func a b c d e f g h i j k l m n o p =
    map9 identity (map8 func a b c d e f g h) i j k l m n o p



-- RESULTS AND MAYBES


{-| Take a `Decoder (Maybe a)` and make it fail if it decodes to `Nothing`.

    import Bytes.Extra exposing (fromByteValues)
    import Bytes.Decode exposing (decode, map, string)

    fromByteValues [ 0x30, 0x30, 0x30, 0x31, 0x30 ] -- "00010"
        |> decode (onlyJusts (map String.toInt (string 5)))
    --> Just 10

-}
onlyJusts : Decoder (Maybe a) -> Decoder a
onlyJusts =
    andThen (Maybe.map succeed >> Maybe.withDefault fail)


{-| Take a `Decoder (Result err a)` and make it fail if it decodes an `Err`.
-}
onlyOks : Decoder (Result err a) -> Decoder a
onlyOks =
    andThen (Result.map succeed >> Result.withDefault fail)
