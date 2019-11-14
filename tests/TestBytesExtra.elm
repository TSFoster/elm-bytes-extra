module TestBytesExtra exposing (suite)

import Bytes
import Bytes.Decode
import Bytes.Encode
import Bytes.Extra exposing (empty, fromByteValues, toByteValues)
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Bytes.Extra"
        [ describe "empty"
            [ test "is 0 width" <|
                \_ -> Expect.equal 0 (Bytes.width empty)
            ]
        , describe "{to,from}ByteValues" <|
            [ test "[] is empty" <|
                \_ -> Expect.equal 0 (Bytes.width (fromByteValues []))
            , test "empty is []" <|
                \_ -> Expect.equal [] (toByteValues (fromByteValues []))
            , fuzz byteValueFuzzer "can convert between without issue" <|
                \byteValues ->
                    Expect.equal byteValues
                        (toByteValues (fromByteValues byteValues))
            ]
        ]


byteValueFuzzer : Fuzzer (List Int)
byteValueFuzzer =
    Fuzz.list (Fuzz.intRange 0 254)
