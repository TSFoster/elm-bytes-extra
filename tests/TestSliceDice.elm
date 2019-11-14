module TestSliceDice exposing (suite)

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Extra exposing (drop, fromByteValues, last, slice, splitAt, take, toByteValues)
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)


suite : Test
suite =
    describe "Slicing and dicing"
        [ describe "slice"
            [ test "range more than length" <|
                \_ ->
                    fromByteValues (List.range 0 20)
                        |> slice 30 40
                        |> toByteValues
                        |> Expect.equal []
            , test "larger slice than possible" <|
                \_ ->
                    fromByteValues (List.range 0 20)
                        |> slice 16 30
                        |> toByteValues
                        |> Expect.equal [ 16, 17, 18, 19, 20 ]
            , test "negative to value" <|
                \_ ->
                    fromByteValues (List.range 0 20)
                        |> slice 15 -1
                        |> toByteValues
                        |> Expect.equal [ 15, 16, 17, 18, 19 ]
            , test "from after negative to" <|
                \_ ->
                    fromByteValues (List.range 0 20)
                        |> slice 16 -7
                        |> toByteValues
                        |> Expect.equal []
            , test "negative from, negative to" <|
                \_ ->
                    fromByteValues (List.range 0 20)
                        |> slice -1 -5
                        |> toByteValues
                        |> Expect.equal []
            , fuzz3 asciiByteValuesFuzzer (Fuzz.intRange -100 100) (Fuzz.intRange -100 100) "works like String.slice" <|
                \byteValues from to ->
                    let
                        bytes =
                            fromByteValues byteValues
                    in
                    Expect.equal
                        (String.slice from to (toString bytes))
                        (toString (slice from to bytes))
            ]
        , describe "take"
            [ fuzz2 asciiByteValuesFuzzer (Fuzz.intRange -3 200) "works like String.left" <|
                \byteValues n ->
                    let
                        bytes =
                            fromByteValues byteValues
                    in
                    Expect.equal
                        (String.left n (toString bytes))
                        (toString (take n bytes))
            , fuzz2 asciiByteValuesFuzzer (Fuzz.intRange -3 200) "works like List.take" <|
                \byteValues n ->
                    Expect.equal
                        (List.take n byteValues)
                        (toByteValues (take n (fromByteValues byteValues)))
            ]
        , describe "drop"
            [ test "drop more than all bytes" <|
                \_ ->
                    fromByteValues (List.range 0 20)
                        |> drop 100
                        |> toByteValues
                        |> Expect.equal []
            , fuzz2 asciiByteValuesFuzzer (Fuzz.intRange -3 200) "works like String.dropLeft" <|
                \byteValues n ->
                    let
                        bytes =
                            fromByteValues byteValues
                    in
                    Expect.equal
                        (String.dropLeft n (toString bytes))
                        (toString (drop n bytes))
            , fuzz2 asciiByteValuesFuzzer (Fuzz.intRange -3 200) "works like List.drop" <|
                \byteValues n ->
                    Expect.equal
                        (List.drop n byteValues)
                        (toByteValues (drop n (fromByteValues byteValues)))
            ]
        , describe "splitAt"
            [ test "splitAt 0" <|
                \_ ->
                    fromByteValues (List.range 0 5)
                        |> splitAt 0
                        |> Tuple.mapBoth toByteValues toByteValues
                        |> Expect.equal ( [], [ 0, 1, 2, 3, 4, 5 ] )
            , test "splitAt large number" <|
                \_ ->
                    fromByteValues (List.range 0 5)
                        |> splitAt 10
                        |> Tuple.mapBoth toByteValues toByteValues
                        |> Expect.equal ( [ 0, 1, 2, 3, 4, 5 ], [] )
            ]
        , describe "last"
            [ test "basic usage" <|
                \_ ->
                    fromByteValues (List.range 0 20)
                        |> last 4
                        |> toByteValues
                        |> Expect.equal [ 17, 18, 19, 20 ]
            , test "last with negative int" <|
                \_ ->
                    fromByteValues (List.range 0 20)
                        |> last -1
                        |> toByteValues
                        |> Expect.equal []
            , test "requesting more than all bytes returns bytes" <|
                \_ ->
                    fromByteValues (List.range 0 20)
                        |> last 100
                        |> toByteValues
                        |> Expect.equal (List.range 0 20)
            , fuzz2 asciiByteValuesFuzzer (Fuzz.intRange -3 200) "works like String.right" <|
                \byteValues len ->
                    let
                        bytes =
                            fromByteValues byteValues
                    in
                    Expect.equal
                        (String.right len (toString bytes))
                        (toString (last len bytes))
            ]
        ]


asciiByteValuesFuzzer : Fuzzer (List Int)
asciiByteValuesFuzzer =
    -- Kept as `List Int` instead of converting to `Bytes` to improve test
    -- failure information
    Fuzz.list (Fuzz.intRange 32 126)


toString : Bytes -> String
toString bytes =
    Bytes.Decode.decode (Bytes.Decode.string (Bytes.width bytes)) bytes
        |> Maybe.withDefault ""
