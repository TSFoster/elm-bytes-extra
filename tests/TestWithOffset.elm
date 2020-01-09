module TestWithOffset exposing (suite)

import Bytes.Decode exposing (bytes, decode)
import Bytes.Decode.Extra exposing (withOffset)
import Bytes.Extra exposing (fromByteValues, toByteValues)
import Expect
import Fuzz exposing (Fuzzer, int)
import Test exposing (Test, describe, fuzz, fuzz2)


suite : Test
suite =
    describe "Bytes.Decode.Extra.withOffset"
        [ fuzz byteValueFuzzer "withOffset 0 does nothing" <|
            \byteValues ->
                fromByteValues byteValues
                    |> decode (withOffset 0 (bytes (List.length byteValues)))
                    |> Maybe.map toByteValues
                    |> Expect.equal (Just byteValues)
        , fuzz byteValueFuzzer "withOffset larger than input fails" <|
            \byteValues ->
                fromByteValues byteValues
                    |> decode (withOffset (1 + List.length byteValues) (bytes 0))
                    |> Expect.equal Nothing
        , fuzz2 byteValueFuzzer int "withOffset x drops first x bytes" <|
            \byteValues integer ->
                let
                    index =
                        case List.length byteValues of
                            0 ->
                                0

                            x ->
                                integer |> modBy x

                    toDecode =
                        List.length byteValues - index
                in
                fromByteValues byteValues
                    |> decode (withOffset index (bytes toDecode))
                    |> Maybe.map toByteValues
                    |> Expect.equal (Just (List.drop index byteValues))
        ]


byteValueFuzzer : Fuzzer (List Int)
byteValueFuzzer =
    Fuzz.list (Fuzz.intRange 0 255)
