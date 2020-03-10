module MsgPack.Decode exposing (bool, bytes, dict, extension, float, int, keyValuePairs, list, null, string)

{-| TODO docs

<https://github.com/msgpack/msgpack/blob/master/spec.md>

-}

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Time exposing (Posix)


null : Decoder ()
null =
    Decode.unsignedInt8
        |> Decode.andThen
            (\n ->
                if n == 0xC0 then
                    Decode.succeed ()

                else
                    Decode.fail
            )


bool : Decoder Bool
bool =
    Decode.unsignedInt8
        |> Decode.andThen
            (\n ->
                case n of
                    0xC2 ->
                        Decode.succeed False

                    0xC3 ->
                        Decode.succeed True

                    _ ->
                        Decode.fail
            )


int : Decoder Int
int =
    Decode.unsignedInt8
        |> Decode.andThen
            (\n ->
                if Bitwise.and 0x80 n == 0x00 then
                    -- 0x80 == 0b 1000 0000
                    -- 0x00 == 0b 0000 0000
                    -- positive fixnum: 0b 0XXX XXXX
                    -- we can just use this number
                    Decode.succeed n

                else if Bitwise.and 0xE0 n == 0xE0 then
                    -- 0xE0 == 0b 1110 0000
                    -- 0x1F == 0b 0001 1111
                    -- negative fixnum: 0b 111X XXXX
                    -- zero the three 1s, negate the number, and you're good to go
                    let
                        negativeFixnum =
                            Bitwise.and 0x1F n
                                |> negate
                    in
                    Decode.succeed negativeFixnum

                else
                    case n of
                        0xCC ->
                            Decode.unsignedInt8

                        0xCD ->
                            Decode.unsignedInt16 BE

                        0xCE ->
                            Decode.unsignedInt32 BE

                        0xCF ->
                            -- TODO in JS we're unable to represent all 64bit values, right?
                            Decode.map2
                                (\msb lsb ->
                                    Bitwise.or
                                        (Bitwise.shiftLeftBy 32 msb)
                                        lsb
                                )
                                (Decode.unsignedInt32 BE)
                                (Decode.unsignedInt32 BE)

                        0xD0 ->
                            Decode.signedInt8

                        0xD1 ->
                            Decode.signedInt16 BE

                        0xD2 ->
                            Decode.signedInt32 BE

                        0xD3 ->
                            -- TODO in JS we're unable to represent all 64bit values, right?
                            -- TODO this is most likely wrong
                            Decode.map2
                                (\msb lsb ->
                                    Bitwise.or
                                        (Bitwise.shiftLeftBy 32 msb)
                                        lsb
                                )
                                (Decode.signedInt32 BE)
                                (Decode.signedInt32 BE)

                        _ ->
                            Decode.fail
            )


float : Decoder Float
float =
    Decode.unsignedInt8
        |> Decode.andThen
            (\n ->
                -- hopefully elm/bytes solves all of this for us
                case n of
                    0xCA ->
                        Decode.float32 BE

                    0xCB ->
                        Decode.float64 BE

                    _ ->
                        Decode.fail
            )


string : Decoder String
string =
    Decode.unsignedInt8
        |> Decode.andThen
            (\n ->
                if Bitwise.and 0xE0 n == 0xA0 then
                    -- 0xE0 ==  0b 1110 0000
                    -- 0xA0 ==  0b 1010 0000
                    -- 0x1F ==  0b 0001 1111
                    -- fixstr = 0b 101X XXXX
                    let
                        length =
                            Bitwise.and 0x1F n
                    in
                    Decode.string length

                else
                    case n of
                        0xD9 ->
                            Decode.unsignedInt8
                                |> Decode.andThen Decode.string

                        0xDA ->
                            Decode.unsignedInt16 BE
                                |> Decode.andThen Decode.string

                        0xDB ->
                            Decode.unsignedInt32 BE
                                |> Decode.andThen Decode.string

                        _ ->
                            Decode.fail
            )


bytes : Decoder Bytes
bytes =
    Decode.unsignedInt8
        |> Decode.andThen
            (\n ->
                case n of
                    0xC4 ->
                        Decode.unsignedInt8
                            |> Decode.andThen Decode.bytes

                    0xC5 ->
                        Decode.unsignedInt16 BE
                            |> Decode.andThen Decode.bytes

                    0xC6 ->
                        Decode.unsignedInt32 BE
                            |> Decode.andThen Decode.bytes

                    _ ->
                        Decode.fail
            )


listLoop : Decoder a -> Int -> Decoder (List a)
listLoop innerDecoder length =
    Decode.loop
        ( length, [] )
        (\( n, xs ) ->
            if n <= 0 then
                Decode.succeed (Done xs)

            else
                innerDecoder
                    |> Decode.map (\x -> Loop ( n - 1, x :: xs ))
        )


list : Decoder a -> Decoder (List a)
list innerDecoder =
    Decode.unsignedInt8
        |> Decode.andThen
            (\n ->
                if Bitwise.and 0xF0 n == 0x90 then
                    -- 0xF0 ==    0b 1111 0000
                    -- 0x90 ==    0b 1001 0000
                    -- 0x0F ==    0b 0000 1111
                    -- fixarray = 0b 1001 XXXX
                    let
                        length =
                            Bitwise.and 0x0F n
                    in
                    listLoop innerDecoder length

                else
                    case n of
                        0xDC ->
                            Decode.unsignedInt16 BE
                                |> Decode.andThen (listLoop innerDecoder)

                        0xDD ->
                            Decode.unsignedInt32 BE
                                |> Decode.andThen (listLoop innerDecoder)

                        _ ->
                            Decode.fail
            )


keyValuePairs : Decoder key -> Decoder value -> Decoder (List ( key, value ))
keyValuePairs keyDecoder valueDecoder =
    let
        tupleDecoder : Decoder ( key, value )
        tupleDecoder =
            Decode.map2 Tuple.pair
                keyDecoder
                valueDecoder
    in
    Decode.unsignedInt8
        |> Decode.andThen
            (\n ->
                if Bitwise.and 0xF0 n == 0x80 then
                    -- 0xF0 ==  0b 1111 0000
                    -- 0x80 ==  0b 1000 0000
                    -- 0x0F ==  0b 0000 1111
                    -- fixmap = 0b 1000 XXXX
                    let
                        length =
                            Bitwise.and 0x0F n
                    in
                    listLoop tupleDecoder length

                else
                    case n of
                        0xDE ->
                            Decode.unsignedInt16 BE
                                |> Decode.andThen (listLoop tupleDecoder)

                        0xDF ->
                            Decode.unsignedInt32 BE
                                |> Decode.andThen (listLoop tupleDecoder)

                        _ ->
                            Decode.fail
            )


dict : Decoder comparable -> Decoder value -> Decoder (Dict comparable value)
dict keyDecoder valueDecoder =
    keyValuePairs keyDecoder valueDecoder
        |> Decode.map Dict.fromList


extension : Decoder ( Int, Bytes )
extension =
    Decode.unsignedInt8
        |> Decode.andThen
            (\n ->
                case n of
                    0xD4 ->
                        Decode.map2 Tuple.pair
                            Decode.signedInt8
                            (Decode.bytes 1)

                    0xD5 ->
                        Decode.map2 Tuple.pair
                            Decode.signedInt8
                            (Decode.bytes 2)

                    0xD6 ->
                        Decode.map2 Tuple.pair
                            Decode.signedInt8
                            (Decode.bytes 4)

                    0xD7 ->
                        Decode.map2 Tuple.pair
                            Decode.signedInt8
                            (Decode.bytes 8)

                    0xD8 ->
                        Decode.map2 Tuple.pair
                            Decode.signedInt8
                            (Decode.bytes 16)

                    0xC7 ->
                        Decode.unsignedInt8
                            |> Decode.andThen
                                (\length ->
                                    Decode.map2 Tuple.pair
                                        Decode.signedInt8
                                        (Decode.bytes length)
                                )

                    0xC8 ->
                        Decode.unsignedInt16 BE
                            |> Decode.andThen
                                (\length ->
                                    Decode.map2 Tuple.pair
                                        Decode.signedInt8
                                        (Decode.bytes length)
                                )

                    0xC9 ->
                        Decode.unsignedInt32 BE
                            |> Decode.andThen
                                (\length ->
                                    Decode.map2 Tuple.pair
                                        Decode.signedInt8
                                        (Decode.bytes length)
                                )

                    _ ->
                        Decode.fail
            )



--timestamp : Decoder Posix
