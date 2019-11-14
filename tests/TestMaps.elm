module TestMaps exposing (suite)

import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode
import Bytes.Decode.Extra as Decode
import Bytes.Extra as Bytes
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Decode.Extra.mapX"
        [ test "map6" <|
            \_ ->
                let
                    input =
                        List.range 0 5

                    decoder =
                        Decode.map6 (\a b c d e f -> [ a, b, c, d, e, f ])
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
                        Decode.map7 (\a b c d e f g -> [ a, b, c, d, e, f, g ])
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
                        Decode.map8 (\a b c d e f g h -> [ a, b, c, d, e, f, g, h ])
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
                        Decode.map16 (\a b c d e f g h i j k l m n o p -> [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p ])
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
