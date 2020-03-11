module MsgPack.Encode exposing (bool, bytes, float, int, null, string)

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Encode exposing (Encoder)


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
    if length <= 31 then
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
