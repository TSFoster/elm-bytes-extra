module TestInt24 exposing (suite)

import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode
import Bytes.Decode.Extra as Decode
import Bytes.Encode as Encode
import Bytes.Encode.Extra as Encode
import Expect
import Fuzz
import Test exposing (..)


suite : Test
suite =
    describe "24-bit integer encoding and decoding"
        [ describe "Encode"
            [ fuzz (Fuzz.intRange 0 ((2 ^ 24) - 1)) "unsignedInt24 BE" <|
                \value ->
                    Encode.sequence
                        [ Encode.unsignedInt8 0
                        , Encode.unsignedInt24 BE value
                        ]
                        |> Encode.encode
                        |> Decode.decode (Decode.unsignedInt32 BE)
                        |> Expect.equal (Just value)
            , fuzz (Fuzz.intRange 0 ((2 ^ 24) - 1)) "unsignedInt24 LE" <|
                \value ->
                    Encode.sequence
                        [ Encode.unsignedInt24 LE value
                        , Encode.unsignedInt8 0
                        ]
                        |> Encode.encode
                        |> Decode.decode (Decode.unsignedInt32 LE)
                        |> Expect.equal (Just value)
            , fuzz (Fuzz.intRange -(2 ^ 23) ((2 ^ 23) - 1)) "signedInt24 BE" <|
                \value ->
                    Encode.sequence
                        [ Encode.signedInt24 BE value
                        , Encode.unsignedInt8 0
                        ]
                        |> Encode.encode
                        |> Decode.decode (Decode.signedInt32 BE)
                        |> Expect.equal (Just (value * 256))
            , fuzz (Fuzz.intRange -(2 ^ 23) ((2 ^ 23) - 1)) "signedInt24 LE" <|
                \value ->
                    Encode.sequence
                        [ Encode.unsignedInt8 0
                        , Encode.signedInt24 LE value
                        ]
                        |> Encode.encode
                        |> Decode.decode (Decode.signedInt32 LE)
                        |> Expect.equal (Just (value * 256))
            ]
        , describe "decode"
            [ fuzz (Fuzz.intRange 0 ((2 ^ 24) - 1)) "unsignedInt24 BE" <|
                \value ->
                    value
                        |> Encode.unsignedInt24 BE
                        |> Encode.encode
                        |> Decode.decode (Decode.unsignedInt24 BE)
                        |> Expect.equal (Just value)
            , fuzz (Fuzz.intRange 0 ((2 ^ 24) - 1)) "unsignedInt24 LE" <|
                \value ->
                    value
                        |> Encode.unsignedInt24 LE
                        |> Encode.encode
                        |> Decode.decode (Decode.unsignedInt24 LE)
                        |> Expect.equal (Just value)
            , fuzz (Fuzz.intRange -(2 ^ 23) ((2 ^ 23) - 1)) "signedInt24 BE" <|
                \value ->
                    value
                        |> Encode.signedInt24 BE
                        |> Encode.encode
                        |> Decode.decode (Decode.signedInt24 BE)
                        |> Expect.equal (Just value)
            , fuzz (Fuzz.intRange -(2 ^ 23) ((2 ^ 23) - 1)) "signedInt24 LE" <|
                \value ->
                    value
                        |> Encode.signedInt24 LE
                        |> Encode.encode
                        |> Decode.decode (Decode.signedInt24 LE)
                        |> Expect.equal (Just value)
            ]
        ]
