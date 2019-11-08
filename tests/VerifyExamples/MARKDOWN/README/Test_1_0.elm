module VerifyExamples.MARKDOWN.README.Test_1_0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Bytes.Encode.Extra
import Bytes.Encode as Encode
import Bytes.Decode.Extra exposing (andMap, hardcoded)
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Extra
import Bytes exposing (Bytes, Endianness(..))

type MyData2 = MyData2 Int Int Int Int Int Int Int Int
type alias MyData =
  { count : Int
  , title : String
  , status : Status
  , weighting : Float
  }
type Status = Downloaded | Downloading | ToFetch | NotUploaded

myData2Decoder : Decoder MyData2
myData2Decoder =
    Bytes.Decode.Extra.map8 MyData2
        (Decode.unsignedInt16 LE) (Decode.unsignedInt16 LE)
        (Decode.unsignedInt16 LE) (Decode.unsignedInt16 LE)
        (Decode.unsignedInt16 LE) (Decode.unsignedInt16 LE)
        (Decode.unsignedInt16 LE) (Decode.unsignedInt16 LE)
myDataDecoder : Decoder MyData
myDataDecoder =
    Decode.succeed MyData
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt16 BE |> Decode.andThen Decode.string)
        |> hardcoded Downloaded
        |> andMap (Decode.float64 BE)
stringToBytes : String -> List Int
stringToBytes =
    Encode.string >> Encode.encode >> Bytes.Extra.toByteValues



spec1 : Test.Test
spec1 =
    Test.test "Documentation VerifyExamples: \n\n    Decode.decode myDataDecoder <|\n        Encode.encode <| Encode.sequence\n            [ Encode.unsignedInt16 BE 12\n            , Encode.unsignedInt16 BE (String.length \"Metric A\")\n            , Encode.string \"Metric A\"\n            , Encode.float64 BE 0.234\n            ]\n    --> Just\n    -->     { count = 12\n    -->     , title = \"Metric A\"\n    -->     , status = Downloaded\n    -->     , weighting = 0.234\n    -->     }" <|
        \() ->
            Expect.equal
                (
                Decode.decode myDataDecoder <|
                    Encode.encode <| Encode.sequence
                        [ Encode.unsignedInt16 BE 12
                        , Encode.unsignedInt16 BE (String.length "Metric A")
                        , Encode.string "Metric A"
                        , Encode.float64 BE 0.234
                        ]
                )
                (
                Just
                    { count = 12
                    , title = "Metric A"
                    , status = Downloaded
                    , weighting = 0.234
                    }
                )