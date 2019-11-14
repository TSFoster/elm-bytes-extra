module Bytes.Extra exposing
    ( empty
    , fromByteValues, toByteValues
    , slice, take, drop, splitAt, last
    )

{-| Before the release of [elm/bytes], many packages would use `List Int`
to represent bytes. To enable interaction with these packages, you can use
`fromByteValues` and `toByteValues`.

@docs empty
@docs fromByteValues, toByteValues
@docs slice, take, drop, splitAt, last

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


{-| Slice a segment from `Bytes`. Negative indexes are taken starting from the
end of the byte sequence.

    fromByteValues (List.range 0 20)
        |> slice 5 10
        |> toByteValues
    --> [ 5, 6, 7, 8, 9 ]

    fromByteValues (List.range 0 20)
        |> slice 30 40
        |> toByteValues
    --> []

    fromByteValues (List.range 0 20)
        |> slice 16 30
        |> toByteValues
    --> [ 16, 17, 18, 19 , 20 ]

    fromByteValues (List.range 0 20)
        |> slice 15 -1
        |> toByteValues
    --> [ 15, 16, 17, 18, 19 ]

    fromByteValues (List.range 0 20)
        |> slice 16 -7
        |> toByteValues
    --> []

    fromByteValues (List.range 0 20)
        |> slice -1 -5
        |> toByteValues
    --> []

    fromByteValues (List.range 0 20)
        |> slice -5 -1
        |> toByteValues
    --> [ 16, 17, 18, 19 ]

-}
slice : Int -> Int -> Bytes -> Bytes
slice from to bytes =
    let
        width =
            Bytes.width bytes

        dropAmount =
            max 0
                (if from < 0 then
                    width + from

                 else
                    min width from
                )

        keepAmount =
            max 0
                (if to < 0 then
                    width + to - dropAmount

                 else
                    min width to - dropAmount
                )

        decoder =
            Bytes.Decode.map2 (\_ v -> v)
                (Bytes.Decode.bytes dropAmount)
                (Bytes.Decode.bytes keepAmount)
    in
    Bytes.Decode.decode decoder bytes
        |> Maybe.withDefault empty


{-| Take the first `n` bytes of the given bytes sequence.

    fromByteValues (List.range 0 20)
        |> take 5
        |> toByteValues
    --> [ 0, 1, 2, 3, 4 ]

    fromByteValues (List.range 0 20)
        |> take 100
        |> toByteValues
    --> List.range 0 20

-}
take : Int -> Bytes -> Bytes
take amount bytes =
    Bytes.Decode.decode (Bytes.Decode.bytes amount) bytes
        |> Maybe.withDefault bytes


{-| Drop the first `n` bytes of the given byte sequence.

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
drop amount bytes =
    slice amount (Bytes.width bytes) bytes


{-| Split some `Bytes` into two at the given index. Negative indexes are counted
from the end of the byte sequence.

    fromByteValues (List.range 0 5)
        |> splitAt 0
        |> Tuple.mapBoth toByteValues toByteValues
    --> ([], [ 0, 1, 2, 3, 4, 5 ])

    fromByteValues (List.range 0 5)
        |> splitAt 3
        |> Tuple.mapBoth toByteValues toByteValues
    --> ([ 0, 1, 2 ], [ 3, 4, 5 ])

    fromByteValues (List.range 0 5)
        |> splitAt 10
        |> Tuple.mapBoth toByteValues toByteValues
    --> ([ 0, 1, 2, 3, 4, 5 ], [])

    fromByteValues (List.range 0 5)
        |> splitAt -2
        |> Tuple.mapBoth toByteValues toByteValues
    --> ([ 0, 1, 2, 3 ], [ 4, 5 ])

-}
splitAt : Int -> Bytes -> ( Bytes, Bytes )
splitAt index bytes =
    let
        width =
            Bytes.width bytes

        leftAmount =
            clamp 0
                width
                (if index < 0 then
                    width + index

                 else
                    index
                )

        decoder =
            Bytes.Decode.map2 Tuple.pair (Bytes.Decode.bytes leftAmount) (Bytes.Decode.bytes (width - leftAmount))
    in
    Bytes.Decode.decode decoder bytes
        |> Maybe.withDefault ( empty, empty )


{-| Take the last `n` bytes from a sequence of bytes.

    fromByteValues (List.range 0 20)
        |> last -1
        |> toByteValues
    --> []

    fromByteValues (List.range 0 20)
        |> last 3
        |> toByteValues
    --> [ 18, 19, 20 ]

    fromByteValues (List.range 0 20)
        |> last 100
        |> toByteValues
    --> List.range 0 20

-}
last : Int -> Bytes -> Bytes
last amount bytes =
    drop (Bytes.width bytes - amount) bytes


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
