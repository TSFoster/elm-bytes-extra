module Bytes.Extra exposing
    ( empty
    , fromByteValues, toByteValues
    , slice, take, drop
    )

{-| Before the release of [elm/bytes], many packages would use `List Int`
to represent bytes. To enable interaction with these packages, you can use
`fromByteValues` and `toByteValues`.

@docs empty
@docs fromByteValues, toByteValues
@docs slice, take, drop

[elm/bytes]: https://package.elm-lang.org/packages/elm/bytes/latest/

-}

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Decode.Extra
import Bytes.Encode
import Bytes.Encode.Extra


{-| An empty `Bytes`. Useful for default cases or unreachable branches.

    import Bytes

    Bytes.width empty
        --> 0

-}
empty : Bytes
empty =
    Bytes.Encode.encode (Bytes.Encode.sequence [])


{-| Slice a segment from a `Bytes`

    fromByteValues (List.range 0 20)
        |> slice 5 10
        |> toByteValues
        --> [ 5, 6, 7, 8, 9 ]

-}
slice : Int -> Int -> Bytes -> Bytes
slice from to buffer =
    let
        toKeep =
            min (to - from) (Bytes.width buffer - to)

        decoder =
            Bytes.Decode.map2 (\_ v -> v)
                (Bytes.Decode.bytes from)
                (Bytes.Decode.bytes toKeep)
    in
    case Bytes.Decode.decode decoder buffer of
        Just v ->
            v

        Nothing ->
            empty


{-| A prefix of a `Bytes`

    fromByteValues (List.range 0 20)
        |> take 5
        |> toByteValues
        --> [ 0, 1, 2, 3, 4 ]

Returns the input when more than `Bytes.width input` values are taken

    fromByteValues (List.range 0 20)
        |> take 100
        |> toByteValues
        --> List.range 0 20

-}
take : Int -> Bytes -> Bytes
take size buffer =
    case Bytes.Decode.decode (Bytes.Decode.bytes size) buffer of
        Just v ->
            v

        Nothing ->
            buffer


{-| A suffix of a `Bytes`

    fromByteValues (List.range 0 20)
        |> drop 15
        |> toByteValues
        --> [ 15, 16, 17, 18, 19, 20 ]

Returns `empty` when all elements are dropped:

    fromByteValues (List.range 0 20)
        |> drop 100
        |> toByteValues
        --> []

-}
drop : Int -> Bytes -> Bytes
drop size buffer =
    let
        toKeep =
            Bytes.width buffer - size
    in
    if toKeep <= 0 then
        empty

    else
        let
            decoder =
                Bytes.Decode.map2 (\_ v -> v)
                    (Bytes.Decode.bytes size)
                    (Bytes.Decode.bytes toKeep)
        in
        case Bytes.Decode.decode decoder buffer of
            Just v ->
                v

            Nothing ->
                empty


{-| Convert a `List Int` to `Bytes`. Each `Int` represents a single byte,
so values are assumed to be between 0 and 255 inclusive.

    import MD5
    import Bytes.Decode
    import Bytes.Decode.Extra

    MD5.bytes "hello world"
        |> fromByteValues
        |> Bytes.Decode.decode (Bytes.Decode.Extra.list 16 Bytes.Decode.unsignedInt8)
    --> Just
    -->     [ 0x5e , 0xb6 , 0x3b , 0xbb
    -->     , 0xe0 , 0x1e , 0xee , 0xd0
    -->     , 0x93 , 0xcb , 0x22 , 0xbb
    -->     , 0x8f , 0x5a , 0xcd , 0xc3
    -->     ]

-}
fromByteValues : List Int -> Bytes
fromByteValues =
    Bytes.Encode.encode << Bytes.Encode.Extra.byteValues


{-| Convert `Bytes` to `List Int`. Each `Int` represents a single byte,
so values will be between 0 and 255 inclusive.

    import SHA1

    SHA1.fromString "And the band begins to play"
        |> SHA1.toBytes
        |> toByteValues
    --> [ 0xF3, 0x08, 0x73, 0x13
    --> , 0xD6, 0xBC, 0xE5, 0x5B
    --> , 0x60, 0x0C, 0x69, 0x2F
    --> , 0xE0, 0x92, 0xF4, 0x53
    --> , 0x87, 0x3F, 0xAE, 0x91
    --> ]

-}
toByteValues : Bytes -> List Int
toByteValues bytes =
    bytes
        |> Bytes.Decode.decode (Bytes.Decode.Extra.byteValues (Bytes.width bytes))
        |> Maybe.withDefault []
