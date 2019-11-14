module TestOkJust exposing (suite)

import Bytes.Decode
import Bytes.Decode.Extra exposing (onlyJusts, onlyOks)
import Bytes.Extra exposing (empty, fromByteValues)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "working with Maybes and Results"
        [ describe "onlyOks"
            [ test "failure fails" <|
                \_ ->
                    Bytes.Decode.fail
                        |> Bytes.Decode.map Just
                        |> onlyJusts
                        |> (\decoder -> Bytes.Decode.decode decoder empty)
                        |> Expect.equal Nothing
            , test "Nothing fails" <|
                \_ ->
                    Bytes.Decode.succeed Nothing
                        |> onlyJusts
                        |> (\decoder -> Bytes.Decode.decode decoder empty)
                        |> Expect.equal Nothing
            , test "Just passes" <|
                \_ ->
                    Bytes.Decode.succeed (Just "Success")
                        |> onlyJusts
                        |> (\decoder -> Bytes.Decode.decode decoder empty)
                        |> Expect.equal (Just "Success")
            ]
        , describe "onlyJusts"
            [ test "failure fails" <|
                \_ ->
                    Bytes.Decode.fail
                        |> Bytes.Decode.map Ok
                        |> onlyOks
                        |> (\decoder -> Bytes.Decode.decode decoder empty)
                        |> Expect.equal Nothing
            , test "Err fails" <|
                \_ ->
                    Bytes.Decode.succeed (Err "Failed")
                        |> onlyOks
                        |> (\decoder -> Bytes.Decode.decode decoder empty)
                        |> Expect.equal Nothing
            , test "Ok passes" <|
                \_ ->
                    Bytes.Decode.succeed (Ok "Success")
                        |> onlyOks
                        |> (\decoder -> Bytes.Decode.decode decoder empty)
                        |> Expect.equal (Just "Success")
            ]
        ]
