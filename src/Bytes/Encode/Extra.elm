module Bytes.Encode.Extra exposing
    ( list, byteValues
    , signedInt24, unsignedInt24
    )

{-| This module provides helpers for working with `Bytes.Encode.Encoder`s.

@docs list, byteValues


## 24-bit Integers

One notable use of 24-bit integers is in 24-bit color, a.k.a. "True color".

@docs signedInt24, unsignedInt24

-}

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Encode exposing (Encoder, sequence, unsignedInt8)


{-| Create an encoder for a list. This does not encode the length of the list.

    import Bytes exposing (Endianness(..))
    import Bytes.Encode
    import Bytes.Decode
    import Bytes.Decode.Extra

    data : List Int
    data =
        [ 6, 3, 200, 236, 123 ]

    list (Bytes.Encode.unsignedInt32 BE) data
        |> Bytes.Encode.encode
        |> Bytes.Decode.decode (Bytes.Decode.Extra.list 5 (Bytes.Decode.unsignedInt32 BE))
    --> Just data

-}
list : (a -> Encoder) -> List a -> Encoder
list encode =
    sequence << List.map encode


{-| Before the release of [elm/bytes], many packages would use `List Int`
to represent bytes. `byteValues` aids interaciton between those packages
and `Bytes`.

    import Bytes.Encode exposing (Encoder)
    import Bytes.Extra
    import MD5

    myEncoder : String -> Encoder
    myEncoder string =
        Bytes.Encode.sequence
            [ Bytes.Encode.unsignedInt8 (String.length string)
            , Bytes.Encode.string string
            , Bytes.Encode.Extra.byteValues (MD5.bytes string)
            ]


    Bytes.Extra.toByteValues (Bytes.Encode.encode (myEncoder "hello world"))
    --> [ 11                                                               -- Length of "hello world"
    --> , 0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64 -- "hello world" in bytes
    --> , 0x5e , 0xb6 , 0x3b , 0xbb, 0xe0 , 0x1e , 0xee , 0xd0             -- |
    --> , 0x93 , 0xcb , 0x22 , 0xbb, 0x8f , 0x5a , 0xcd , 0xc3             -- |- MD5 of "hello world"
    --> ]

-}
byteValues : List Int -> Encoder
byteValues =
    list unsignedInt8



-- BASIC TYPES


{-| Encode a 24-bit signed integer
-}
signedInt24 : Endianness -> Int -> Encoder
signedInt24 endianness n =
    let
        b12 =
            n
                |> Bitwise.shiftRightZfBy 8
                |> Bitwise.and 0xFFFF

        b3 =
            n
                |> Bitwise.and 0xFF
    in
    case endianness of
        BE ->
            Bytes.Encode.sequence
                [ Bytes.Encode.signedInt16 BE b12
                , Bytes.Encode.signedInt8 b3
                ]

        LE ->
            Bytes.Encode.sequence
                [ Bytes.Encode.signedInt8 b3
                , Bytes.Encode.signedInt16 LE b12
                ]


{-| Encode a 24-bit unsigned integer
-}
unsignedInt24 : Endianness -> Int -> Encoder
unsignedInt24 endianness n =
    let
        b12 =
            n
                |> Bitwise.shiftRightZfBy 8
                |> Bitwise.and 0xFFFF

        b3 =
            n
                |> Bitwise.and 0xFF
    in
    case endianness of
        BE ->
            Bytes.Encode.sequence
                [ Bytes.Encode.unsignedInt16 BE b12
                , Bytes.Encode.unsignedInt8 b3
                ]

        LE ->
            Bytes.Encode.sequence
                [ Bytes.Encode.unsignedInt8 b3
                , Bytes.Encode.unsignedInt16 LE b12
                ]
