module MsgPack.EncodeTest exposing (..)

import Bytes
import Bytes.Decode as BytesDecode
import Bytes.Decode.Extra as BytesDecode
import Bytes.Encode as BytesEncode exposing (Encoder)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import MsgPack.Encode as Encode
import Test exposing (..)


encodesToBytes : (a -> Encoder) -> a -> List Int -> () -> Expectation
encodesToBytes encoder value expectedBytes () =
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
        [ test "null" <| encodesToBytes (always Encode.null) () [ 0xC0 ]
        , describe "bool"
            [ test "false" <| encodesToBytes Encode.bool False [ 0xC2 ]
            , test "true" <| encodesToBytes Encode.bool True [ 0xC3 ]
            ]
        , describe "int"
            [ test "fixnum 0" <| encodesToBytes Encode.int 0 [ 0x00 ]
            , test "fixnum 1" <| encodesToBytes Encode.int 1 [ 0x01 ]
            , test "fixnum 127 = 2^7 - 1" <| encodesToBytes Encode.int 127 [ 0x7F ]
            , test "128 = 2^7" <| encodesToBytes Encode.int 128 [ 0xCC, 0x80 ]
            , test "255 = 2^8 - 1" <| encodesToBytes Encode.int 255 [ 0xCC, 0xFF ]
            , test "256 = 2^8" <| encodesToBytes Encode.int 256 [ 0xCD, 0x01, 0x00 ]
            , test "512 = 2^9" <| encodesToBytes Encode.int 512 [ 0xCD, 0x02, 0x00 ]
            , test "65535 = 2^16 - 1" <| encodesToBytes Encode.int 65535 [ 0xCD, 0xFF, 0xFF ]
            , test "65536 = 2^16" <| encodesToBytes Encode.int 65536 [ 0xCE, 0x00, 0x01, 0x00, 0x00 ]
            , test "4294967295 = 2^32 - 1" <| encodesToBytes Encode.int 4294967295 [ 0xCE, 0xFF, 0xFF, 0xFF, 0xFF ]
            , test "4294967296 = 2^32" <| encodesToBytes Encode.int 4294967296 [ 0xCF, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00 ]
            , todo "JS max int"
            , todo "what happens above that JS max int?"
            , todo "some int fuzz tests"
            ]
        , todo "float etc."
        , todo "roundtrip fuzz"
        ]
