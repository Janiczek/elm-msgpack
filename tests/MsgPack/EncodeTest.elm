module MsgPack.EncodeTest exposing (..)

import Bytes
import Bytes.Decode as BytesDecode
import Bytes.Decode.Extra as BytesDecode
import Bytes.Encode as BytesEncode exposing (Encoder)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import MsgPack.Encode as Encode
import Test exposing (..)


encodesToBytes : (a -> Encoder) -> a -> List Int -> Expectation
encodesToBytes encoder value expectedBytes =
    let
        encodedBytes =
            BytesEncode.encode (encoder value)

        length =
            Bytes.width encodedBytes
    in
    encodedBytes
        |> BytesDecode.decode (BytesDecode.byteValues length)
        |> Maybe.withDefault []
        |> Expect.equalLists expectedBytes


suite : Test
suite =
    describe "MsgPack.Encode"
        [ test "null" <|
            \() ->
                encodesToBytes (always Encode.null) () [ 0xC0 ]
        , todo "int etc."
        , todo "roundtrip fuzz"
        ]
