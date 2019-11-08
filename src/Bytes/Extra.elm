module Bytes.Extra exposing (fromByteValues, toByteValues)

{-| Before the release of [elm/bytes], many packages would use `List Int`
to represent bytes. To enable interaction with these packages, you can use
`fromByteValues` and `toByteValues`.

@docs fromByteValues, toByteValues

[elm/bytes]: https://package.elm-lang.org/packages/elm/bytes/latest/

-}

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Decode.Extra
import Bytes.Encode
import Bytes.Encode.Extra


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
