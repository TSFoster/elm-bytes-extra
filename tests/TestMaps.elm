module TestMaps exposing (suite)

import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode
import Bytes.Decode.Extra exposing (map16, map6, map7, map8, map9)
import Bytes.Extra as Bytes
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Decode.Extra.mapX"
        [ test "map6" <|
            \_ ->
                let
                    input =
                        List.range 0 5

                    decoder =
                        map6 (\a b c d e f -> [ a, b, c, d, e, f ])
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                in
                input
                    |> Bytes.fromByteValues
                    |> Decode.decode decoder
                    |> Expect.equal (Just input)
        , test "map7" <|
            \_ ->
                let
                    input =
                        List.range 0 6

                    decoder =
                        map7 (\a b c d e f g -> [ a, b, c, d, e, f, g ])
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                in
                input
                    |> Bytes.fromByteValues
                    |> Decode.decode decoder
                    |> Expect.equal (Just input)
        , test "map8" <|
            \_ ->
                let
                    input =
                        List.range 0 7

                    decoder =
                        map8 (\a b c d e f g h -> [ a, b, c, d, e, f, g, h ])
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                in
                input
                    |> Bytes.fromByteValues
                    |> Decode.decode decoder
                    |> Expect.equal (Just input)
        , test "map9" <|
            \_ ->
                let
                    input =
                        List.range 0 8

                    decoder =
                        map9 (\a b c d e f g h i -> [ a, b, c, d, e, f, g, h, i ])
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                in
                input
                    |> Bytes.fromByteValues
                    |> Decode.decode decoder
                    |> Expect.equal (Just input)
        , test "map16" <|
            \_ ->
                let
                    input =
                        List.range 0 15

                    decoder =
                        map16 (\a b c d e f g h i j k l m n o p -> [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p ])
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                            Decode.unsignedInt8
                in
                input
                    |> Bytes.fromByteValues
                    |> Decode.decode decoder
                    |> Expect.equal (Just input)
        ]
