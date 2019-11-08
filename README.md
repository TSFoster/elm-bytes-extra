# elm-bytes-extra

This package provides a few helper functions to augment the functionality of
[elm/bytes].

## Examples

```elm
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Extra
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Decode.Extra exposing (andMap, hardcoded)
import Bytes.Encode as Encode
import Bytes.Encode.Extra


stringToBytes : String -> List Int
stringToBytes =
    Encode.string >> Encode.encode >> Bytes.Extra.toByteValues


stringToBytes "Hello world!"
--> [ 0x48, 0x65, 0x6c, 0x6c
--> , 0x6f, 0x20, 0x77, 0x6f
--> , 0x72, 0x6c, 0x64, 0x21
--> ]


--


type Status = FromRemote | FromLocal

type alias MyData =
  { count : Int
  , title : String
  , status : Status
  , weighting : Float
  }

myDataDecoder : Decoder MyData
myDataDecoder =
    Decode.succeed MyData
        |> andMap (Decode.unsignedInt16 BE)
        |> andMap (Decode.unsignedInt16 BE |> Decode.andThen Decode.string)
        |> hardcoded FromRemote
        |> andMap (Decode.float64 BE)


Decode.decode myDataDecoder <|
    Encode.encode <| Encode.sequence
        [ Encode.unsignedInt16 BE 12
        , Encode.unsignedInt16 BE (String.length "Metric A")
        , Encode.string "Metric A"
        , Encode.float64 BE 0.234
        ]
--> Just
-->     { count = 12
-->     , title = "Metric A"
-->     , status = FromRemote
-->     , weighting = 0.234
-->     }


--

type MyData2 = MyData2 Int Int Int Int Int Int Int Int

myData2Decoder : Decoder MyData2
myData2Decoder =
    Bytes.Decode.Extra.map8 MyData2
        (Decode.unsignedInt16 LE) (Decode.unsignedInt16 LE)
        (Decode.unsignedInt16 LE) (Decode.unsignedInt16 LE)
        (Decode.unsignedInt16 LE) (Decode.unsignedInt16 LE)
        (Decode.unsignedInt16 LE) (Decode.unsignedInt16 LE)

Decode.decode myData2Decoder <|
    Encode.encode <| Bytes.Encode.Extra.list (Encode.unsignedInt16 LE) [1, 2, 3, 4, 5, 6, 7, 8]
--> Just (MyData2 1 2 3 4 5 6 7 8)
```

## Feedback and contributing

Feedback, suggestions and contributions are welcome. Please raise an [issue on
Github][new-issue], or [create a pull request][pull-requests].

[elm/bytes]: https://package.elm-lang.org/packages/elm/bytes/latest/
[new-issue]: https://github.com/TSFoster/elm-bytes-extra/issues/new
[pull-requests]: https://github.com/TSFoster/elm-bytes-extra/pulls
