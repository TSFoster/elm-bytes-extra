module TestPipeline exposing (suite)

import Bytes exposing (Endianness(..))
import Bytes.Decode
import Bytes.Decode.Extra exposing (andMap, hardcoded)
import Bytes.Encode
import Bytes.Extra exposing (fromByteValues)
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz2, test)


type MyModel
    = MyModel Int Int String


suite : Test
suite =
    fuzz2 (Fuzz.intRange 0 65535) Fuzz.string "pipeline-style" <|
        \int16 string ->
            let
                bytes =
                    Bytes.Encode.encode <|
                        Bytes.Encode.sequence
                            [ Bytes.Encode.unsignedInt16 BE int16
                            , Bytes.Encode.unsignedInt32 BE (String.length string)
                            , Bytes.Encode.string string
                            ]

                decoder =
                    Bytes.Decode.succeed MyModel
                        |> andMap (Bytes.Decode.unsignedInt16 BE)
                        |> hardcoded 12345
                        |> andMap (Bytes.Decode.unsignedInt32 BE |> Bytes.Decode.andThen Bytes.Decode.string)
            in
            Expect.equal
                (Just <| MyModel int16 12345 string)
                (Bytes.Decode.decode decoder bytes)
