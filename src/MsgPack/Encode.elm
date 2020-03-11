module MsgPack.Encode exposing (bool, bytes, dict, extension, float, int, keyValuePairs, list, null, string, timestamp)

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Encode exposing (Encoder)
import Dict exposing (Dict)
import Time exposing (Posix)


null : Encoder
null =
    Encode.unsignedInt8 0xC0


bool : Bool -> Encoder
bool boolValue =
    if boolValue then
        Encode.unsignedInt8 0xC3

    else
        Encode.unsignedInt8 0xC2


int : Int -> Encoder
int intValue =
    if intValue >= 0 && intValue <= 0x7F then
        -- positive fixnum
        Encode.unsignedInt8 intValue

    else if intValue < 0 && intValue >= -31 then
        -- negative fixnum
        Encode.unsignedInt8 (Bitwise.or 0xE0 (abs intValue))

    else if intValue >= 0 then
        if intValue <= 0xFF then
            Encode.sequence
                [ Encode.unsignedInt8 0xCC
                , Encode.unsignedInt8 intValue
                ]

        else if intValue <= 0xFFFF then
            Encode.sequence
                [ Encode.unsignedInt8 0xCD
                , Encode.unsignedInt16 BE intValue
                ]

        else if intValue <= 0xFFFFFFFF then
            Encode.sequence
                [ Encode.unsignedInt8 0xCE
                , Encode.unsignedInt32 BE intValue
                ]

        else
            -- u64
            let
                msb =
                    -- TODO ??
                    Bitwise.shiftRightZfBy 32 intValue

                lsb =
                    Bitwise.and 0xFFFFFFFF intValue
            in
            Encode.sequence
                -- TODO can I just put these two halves next to each other like that?
                [ Encode.unsignedInt8 0xCF
                , Encode.unsignedInt32 BE msb
                , Encode.unsignedInt32 BE lsb
                ]
        -- intValue < 0, we need signed integers now:

    else if intValue >= -128 then
        Encode.sequence
            [ Encode.unsignedInt8 0xD0
            , Encode.signedInt8 intValue
            ]

    else if intValue >= -32768 then
        Encode.sequence
            [ Encode.unsignedInt8 0xD1
            , Encode.signedInt16 BE intValue
            ]

    else
        -- TODO we don't support the 0xD3 i64 format
        Encode.sequence
            [ Encode.unsignedInt8 0xD2
            , Encode.signedInt32 BE intValue
            ]


float : Float -> Encoder
float floatValue =
    -- TODO how to figure out that this number will fit into float32?
    -- TODO start sending the shorter 0xCA variant sometimes too
    Encode.sequence
        [ Encode.unsignedInt8 0xCB
        , Encode.float64 BE floatValue
        ]


string : String -> Encoder
string stringValue =
    let
        length =
            Encode.getStringWidth stringValue
    in
    if length < 2 ^ 5 then
        -- fixstr
        Encode.sequence
            [ Encode.unsignedInt8 <| Bitwise.or 0xA0 length
            , Encode.string stringValue
            ]

    else if length < 2 ^ 8 then
        Encode.sequence
            [ Encode.unsignedInt8 0xD9
            , Encode.unsignedInt8 length
            , Encode.string stringValue
            ]

    else if length < 2 ^ 16 then
        Encode.sequence
            [ Encode.unsignedInt8 0xDA
            , Encode.unsignedInt16 BE length
            , Encode.string stringValue
            ]

    else
        Encode.sequence
            [ Encode.unsignedInt8 0xDB
            , Encode.unsignedInt32 BE length
            , Encode.string stringValue
            ]


bytes : Bytes -> Encoder
bytes bytesValue =
    let
        length =
            Bytes.width bytesValue
    in
    if length < 2 ^ 8 then
        Encode.sequence
            [ Encode.unsignedInt8 0xC4
            , Encode.unsignedInt8 length
            , Encode.bytes bytesValue
            ]

    else if length < 2 ^ 16 then
        Encode.sequence
            [ Encode.unsignedInt8 0xC5
            , Encode.unsignedInt16 BE length
            , Encode.bytes bytesValue
            ]

    else
        Encode.sequence
            [ Encode.unsignedInt8 0xC6
            , Encode.unsignedInt32 BE length
            , Encode.bytes bytesValue
            ]


list : (a -> Encoder) -> List a -> Encoder
list innerEncoder listValue =
    let
        length =
            List.length listValue
    in
    if length < 2 ^ 4 then
        Encode.sequence
            [ Encode.unsignedInt8 <| Bitwise.or 0x90 length
            , List.map innerEncoder listValue
                |> Encode.sequence
            ]

    else if length < 2 ^ 16 then
        Encode.sequence
            [ Encode.unsignedInt8 0xDC
            , Encode.unsignedInt16 BE length
            , List.map innerEncoder listValue
                |> Encode.sequence
            ]

    else
        Encode.sequence
            [ Encode.unsignedInt8 0xDD
            , Encode.unsignedInt32 BE length
            , List.map innerEncoder listValue
                |> Encode.sequence
            ]


keyValuePairs : (key -> Encoder) -> (value -> Encoder) -> List ( key, value ) -> Encoder
keyValuePairs keyEncoder valueEncoder pairs =
    let
        length =
            List.length pairs

        pairEncoder ( key, value ) =
            Encode.sequence
                [ keyEncoder key
                , valueEncoder value
                ]
    in
    if length < 2 ^ 4 then
        Encode.sequence
            [ Encode.unsignedInt8 <| Bitwise.or 0x80 length
            , List.map pairEncoder pairs
                |> Encode.sequence
            ]

    else if length < 2 ^ 16 then
        Encode.sequence
            [ Encode.unsignedInt8 0xDE
            , Encode.unsignedInt16 BE length
            , List.map pairEncoder pairs
                |> Encode.sequence
            ]

    else
        Encode.sequence
            [ Encode.unsignedInt8 0xDF
            , Encode.unsignedInt32 BE length
            , List.map pairEncoder pairs
                |> Encode.sequence
            ]


dict : (comparable -> Encoder) -> (value -> Encoder) -> Dict comparable value -> Encoder
dict keyEncoder valueEncoder dictValue =
    keyValuePairs keyEncoder valueEncoder (Dict.toList dictValue)


extension : ( Int, Bytes ) -> Encoder
extension ( type_, bytesValue ) =
    let
        length =
            Bytes.width bytesValue
    in
    if length == 1 then
        Encode.sequence
            [ Encode.unsignedInt8 0xD4
            , Encode.signedInt8 type_
            , Encode.bytes bytesValue
            ]

    else if length == 2 then
        Encode.sequence
            [ Encode.unsignedInt8 0xD5
            , Encode.signedInt8 type_
            , Encode.bytes bytesValue
            ]

    else if length == 4 then
        Encode.sequence
            [ Encode.unsignedInt8 0xD6
            , Encode.signedInt8 type_
            , Encode.bytes bytesValue
            ]

    else if length == 8 then
        Encode.sequence
            [ Encode.unsignedInt8 0xD7
            , Encode.signedInt8 type_
            , Encode.bytes bytesValue
            ]

    else if length == 16 then
        Encode.sequence
            [ Encode.unsignedInt8 0xD8
            , Encode.signedInt8 type_
            , Encode.bytes bytesValue
            ]

    else if length < 2 ^ 8 then
        Encode.sequence
            [ Encode.unsignedInt8 0xC7
            , Encode.unsignedInt8 length
            , Encode.signedInt8 type_
            , Encode.bytes bytesValue
            ]

    else if length < 2 ^ 16 then
        Encode.sequence
            [ Encode.unsignedInt8 0xC8
            , Encode.unsignedInt16 BE length
            , Encode.signedInt8 type_
            , Encode.bytes bytesValue
            ]

    else
        Encode.sequence
            [ Encode.unsignedInt8 0xC9
            , Encode.unsignedInt32 BE length
            , Encode.signedInt8 type_
            , Encode.bytes bytesValue
            ]


unsignedInt64 : Endianness -> Int -> Encoder
unsignedInt64 endianness intValue =
    -- TODO probably buggy?
    Encode.sequence
        [ Encode.unsignedInt32 endianness <| Bitwise.shiftRightZfBy 32 intValue
        , Encode.unsignedInt32 endianness <| Bitwise.and 0xFFFFFFFF intValue
        ]


timestamp : Posix -> Encoder
timestamp posix =
    let
        millis =
            Time.posixToMillis posix
    in
    if modBy 1000 millis == 0 then
        Encode.sequence
            [ Encode.unsignedInt8 0xD6
            , Encode.signedInt8 -1
            , Encode.unsignedInt32 BE (millis // 1000)
            ]

    else
        -- TODO we don't do the 0xD7 format, only 0xC7
        let
            nanoseconds =
                modBy 1000 millis * 1000

            seconds =
                millis // 1000
        in
        Encode.sequence
            [ Encode.unsignedInt8 0xC7
            , Encode.unsignedInt8 12
            , Encode.signedInt8 -1
            , Encode.unsignedInt32 BE nanoseconds
            , unsignedInt64 BE seconds
            ]
