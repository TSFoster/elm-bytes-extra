module Bytes.Decode.Extra exposing
    ( list, byteValues
    , unsignedInt24, signedInt24
    , andMap, hardcoded
    , map6, map7, map8, map16
    , onlyOks, onlyJusts
    )

{-| Helpers for working with `Bytes.Decoder`s.

@docs list, byteValues

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

@docs map6, map7, map8, map16


## Working with Results and Maybes

@docs onlyOks, onlyJusts

-}

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode exposing (..)


{-| Run a decoder a set amount of times, and collect the result in a list.

    import Bytes.Decode exposing (Decoder)
    import Bytes.Encode
    import Bytes.Encode.Extra

    myDecoder : Decoder (List String)
    myDecoder =
        list 3 (Bytes.Decode.string 5)

    [ "HELLO", "WORLD", "third" ]
        |> Bytes.Encode.Extra.list Bytes.Encode.string
        |> Bytes.Encode.encode
        |> Bytes.Decode.decode myDecoder
    --> Just [ "HELLO", "WORLD", "third" ]

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

    import Hex
    import Bytes exposing (Bytes)
    import Bytes.Decode exposing (Decoder)
    import Bytes.Extra exposing (fromByteValues)

    input : Bytes
    input = fromByteValues [ 0x32, 0xff, 0x53 ]

    decoder : Decoder (List String)
    decoder =
        byteValues (Bytes.width input)
          |> Bytes.Decode.map (List.map Hex.toString)

    Bytes.Decode.decode decoder input
    --> Just [ "32", "ff", "53" ]

-}
byteValues : Int -> Decoder (List Int)
byteValues length =
    list length unsignedInt8


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


{-| -}
map6 : (a -> b -> c -> d -> e -> f -> result) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder result
map6 f b1 b2 b3 b4 b5 b6 =
    let
        d1 =
            Bytes.Decode.map4 (\a b c d -> f a b c d) b1 b2 b3 b4

        d2 =
            Bytes.Decode.map3 (\h a b -> h a b) d1 b5 b6
    in
    d2


{-| -}
map7 : (a -> b -> c -> d -> e -> f -> g -> result) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder result
map7 f b1 b2 b3 b4 b5 b6 b7 =
    let
        d1 =
            Bytes.Decode.map4 (\a b c d -> f a b c d) b1 b2 b3 b4

        d2 =
            Bytes.Decode.map4 (\h a b c -> h a b c) d1 b5 b6 b7
    in
    d2


{-| -}
map8 : (a -> b -> c -> d -> e -> f -> g -> h -> result) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder result
map8 f b1 b2 b3 b4 b5 b6 b7 b8 =
    let
        d1 =
            Bytes.Decode.map4 (\a b c d -> f a b c d) b1 b2 b3 b4

        d2 =
            Bytes.Decode.map5 (\h a b c d -> h a b c d) d1 b5 b6 b7 b8
    in
    d2


{-| -}
map16 :
    (b1 -> b2 -> b3 -> b4 -> b5 -> b6 -> b7 -> b8 -> b9 -> b10 -> b11 -> b12 -> b13 -> b14 -> b15 -> b16 -> result)
    -> Decoder b1
    -> Decoder b2
    -> Decoder b3
    -> Decoder b4
    -> Decoder b5
    -> Decoder b6
    -> Decoder b7
    -> Decoder b8
    -> Decoder b9
    -> Decoder b10
    -> Decoder b11
    -> Decoder b12
    -> Decoder b13
    -> Decoder b14
    -> Decoder b15
    -> Decoder b16
    -> Decoder result
map16 f b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 =
    let
        d1 =
            Bytes.Decode.map4 (\a b c d -> f a b c d) b1 b2 b3 b4

        d2 =
            Bytes.Decode.map5 (\h a b c d -> h a b c d) d1 b5 b6 b7 b8

        d3 =
            Bytes.Decode.map5 (\h a b c d -> h a b c d) d2 b9 b10 b11 b12

        d4 =
            Bytes.Decode.map5 (\h a b c d -> h a b c d) d3 b13 b14 b15 b16
    in
    d4


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


{-| Take a `Decoder (Maybe a)` and make it fail if it decodes to `Nothing`.

    import Bytes.Extra exposing (fromByteValues)
    import Bytes.Decode exposing (..)

    maybeBoolDecoder : Decoder (Maybe Bool)
    maybeBoolDecoder =
      Bytes.Decode.map
          (\int ->
              case int of
                 0 -> Just False
                 1 -> Just True
                 _ -> Nothing
          )
          Bytes.Decode.unsignedInt8

    boolDecoder : Decoder Bool
    boolDecoder =
      onlyJusts maybeBoolDecoder

    decode maybeBoolDecoder (fromByteValues [0x01])
    --> Just (Just True)

    decode boolDecoder (fromByteValues [0x01])
    --> Just True

-}
onlyJusts : Decoder (Maybe a) -> Decoder a
onlyJusts =
    andThen (Maybe.map succeed >> Maybe.withDefault fail)


{-| Take a `Decoder (Result err a)` and make it fail if it decodes an `Err`.
-}
onlyOks : Decoder (Result err a) -> Decoder a
onlyOks =
    andThen (Result.map succeed >> Result.withDefault fail)



-- 24 bit


pack24 : Int -> Int -> Int -> Int
pack24 a b c =
    Bitwise.or (Bitwise.shiftLeftBy 16 a) (Bitwise.or (Bitwise.shiftLeftBy 8 b) c)


{-| Decode a signed 24-bit integer
-}
signedInt24 : Endianness -> Decoder Int
signedInt24 endianness =
    case endianness of
        BE ->
            Bytes.Decode.map3 (\byte1 byte2 byte3 -> pack24 byte1 byte2 byte3)
                Bytes.Decode.signedInt8
                Bytes.Decode.unsignedInt8
                Bytes.Decode.unsignedInt8

        LE ->
            Bytes.Decode.map3 (\byte1 byte2 byte3 -> pack24 byte3 byte2 byte1)
                Bytes.Decode.unsignedInt8
                Bytes.Decode.unsignedInt8
                Bytes.Decode.signedInt8


{-| Decode an unsigned 24-bit integer
-}
unsignedInt24 : Endianness -> Decoder Int
unsignedInt24 endianness =
    let
        recombine byte1 byte2 byte3 =
            case endianness of
                BE ->
                    pack24 byte1 byte2 byte3

                LE ->
                    pack24 byte3 byte2 byte1
    in
    Bytes.Decode.map3 recombine Bytes.Decode.unsignedInt8 Bytes.Decode.unsignedInt8 Bytes.Decode.unsignedInt8
