module Tests exposing (suite)

import Base64
import Bytes exposing (Bytes)
import Bytes.Decode as D
import Bytes.Encode as E
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Regex
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "base64-bytes package"
        [ describe "The Decode module"
            [ test "[] -> []" <|
                \_ ->
                    expectJust "" (decodeIntsToString [])
            , test "0 -> AA==" <|
                \_ ->
                    expectJust "AA==" (decodeIntsToString [ 0 ])
            , test "1 -> AQ==" <|
                \_ ->
                    expectJust "AQ==" (decodeIntsToString [ 1 ])
            , decodeFuzz "Always succeeds" <|
                \_ ->
                    Expect.pass
            , decodeFuzz "Length is 0 mod 4" <|
                \( _, str ) ->
                    str
                        |> String.length
                        |> modBy 4
                        |> Expect.equal 0
            , decodeFuzz "Length is 3/4 original length" <|
                \( ints, str ) ->
                    str
                        |> String.length
                        |> Expect.equal (4 * ceiling (toFloat (List.length ints) / 3))
            , decodeFuzz "Made up of valid characters" <|
                \( _, str ) ->
                    str
                        |> String.all isValidChar
                        |> Expect.equal True
            , decodeFuzz "Only the last characters can be '='" <|
                \( ints, str ) ->
                    isValidB64String str |> Expect.equal True
            , test "lots and lots of bytes" <|
                \_ ->
                    let
                        n =
                            1000000 * 3

                        ints =
                            List.repeat n 0

                        str =
                            String.repeat (4 * ceiling (toFloat (List.length ints) / 3)) "A"
                    in
                    decodeIntsToString ints
                        |> expectJust str
            ]
        , describe "The Encode module"
            [ test "[] -> []" <|
                \_ ->
                    expectJust [] (encodeStringToInts "")
            , test "AA== -> 0" <|
                \_ ->
                    expectJust [ 0 ] (encodeStringToInts "AA==")
            , test "AA= -> 0" <|
                \_ ->
                    expectJust [ 0 ] (encodeStringToInts "AA=")
            , test "AA -> 0" <|
                \_ ->
                    expectJust [ 0 ] (encodeStringToInts "AA")
            , fuzz (Fuzz.tuple ( b64Char, b64Char )) "omitted characters are treaded as padding (2 padding)" <|
                \( c1, c2 ) ->
                    let
                        with2Padding =
                            encodeStringToInts (String.fromList [ c1, c2, '=', '=' ])

                        with1Padding =
                            encodeStringToInts (String.fromList [ c1, c2, '=' ])

                        withoutPadding =
                            encodeStringToInts (String.fromList [ c1, c2 ])

                        tests =
                            [ \expected ->
                                withoutPadding
                                    |> Expect.equal expected
                            , \expected ->
                                with1Padding
                                    |> Expect.equal expected
                            ]
                    in
                    Expect.all tests with2Padding
            , fuzz (Fuzz.tuple3 ( b64Char, b64Char, b64Char )) "omitted characters are treaded as padding (1 padding)" <|
                \( c1, c2, c3 ) ->
                    let
                        withPadding =
                            encodeStringToInts (String.fromList [ c1, c2, c3, '=' ])

                        withoutPadding =
                            encodeStringToInts (String.fromList [ c1, c2, c3 ])
                    in
                    withoutPadding
                        |> Expect.equal withPadding
            , test "AQ== -> 1" <|
                \_ ->
                    expectJust [ 1 ] (encodeStringToInts "AQ==")
            , encodeFuzz "Always succeeds" <|
                \_ ->
                    Expect.pass
            , encodeFuzz "Length is 4/3 original length" <|
                \( ints, str ) ->
                    str
                        |> String.length
                        |> Expect.equal (4 * ceiling (toFloat (List.length ints) / 3))
            , test "lots and lots of bytes" <|
                \_ ->
                    let
                        n =
                            1000000 * 4

                        str =
                            String.repeat n "A"

                        ints =
                            List.repeat (n // 4 * 3) 0
                    in
                    encodeStringToInts str
                        |> expectJust ints
            ]
        , describe "identities"
            [ encodeFuzz "encode >> decode == identity" <|
                \( ints, str ) ->
                    str
                        |> encodeStringToInts
                        |> Maybe.andThen decodeIntsToString
                        |> expectJust str
            , decodeFuzz "decode >> encode == identity" <|
                \( ints, _ ) ->
                    ints
                        |> decodeIntsToString
                        |> Maybe.andThen encodeStringToInts
                        |> expectJust ints
            ]
        ]



-- CHARACTERS


b64Chars : List Char
b64Chars =
    String.toList
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"


validLastB64CharBeforeOneEquals : List Char
validLastB64CharBeforeOneEquals =
    String.toList
        "AEIMQUYcgkosw048"


validLastB64CharBeforeTwoEquals : List Char
validLastB64CharBeforeTwoEquals =
    String.toList
        "AQgw"


validChars : List Char
validChars =
    '=' :: b64Chars


isB64Char char =
    List.member char b64Chars


isValidChar char =
    List.member char validChars


isValidB64String : String -> Bool
isValidB64String string =
    "^([chars]{4})*([chars]{2}==|[chars]{3}=|)?$"
        |> String.replace "chars" (String.fromList b64Chars)
        |> Regex.fromString
        |> Maybe.map (\regex -> Regex.contains regex string)
        |> Maybe.withDefault False



-- FUZZERS


uint8s : Fuzzer (List Int)
uint8s =
    Fuzz.list (Fuzz.intRange 0 255)


fuzzerFromList : List a -> Fuzzer a
fuzzerFromList list =
    List.map Fuzz.constant list |> Fuzz.oneOf


b64Char : Fuzzer Char
b64Char =
    Fuzz.frequency
        [ ( 10, Fuzz.map Char.fromCode (Fuzz.intRange 65 90) )
        , ( 10, Fuzz.map Char.fromCode (Fuzz.intRange 97 122) )
        , ( 5, Fuzz.map Char.fromCode (Fuzz.intRange 48 57) )
        , ( 1, Fuzz.constant '+' )
        , ( 1, Fuzz.constant '/' )
        ]


b64String : Fuzzer String
b64String =
    let
        inputString =
            Fuzz.map String.fromList (Fuzz.list b64Char)
    in
    Fuzz.map3
        (\end1 end2 str ->
            case modBy 4 (String.length str) of
                1 ->
                    String.dropRight 1 str

                2 ->
                    String.dropRight 1 str ++ end2 ++ "=="

                3 ->
                    String.dropRight 1 str ++ end1 ++ "="

                _ ->
                    str
        )
        (fuzzerFromList validLastB64CharBeforeOneEquals
            |> Fuzz.map String.fromChar
        )
        (fuzzerFromList validLastB64CharBeforeTwoEquals
            |> Fuzz.map String.fromChar
        )
        inputString



-- CONVERTERS


bytesFromInt8s : List Int -> Bytes
bytesFromInt8s =
    E.encode << E.sequence << List.map E.unsignedInt8


decodeIntsToString : List Int -> Maybe String
decodeIntsToString =
    bytesFromInt8s >> Base64.fromBytes


encodeStringToInts : String -> Maybe (List Int)
encodeStringToInts str =
    let
        bytesToInts bytes =
            D.decode (exactly (Bytes.width bytes) D.unsignedInt8) bytes
    in
    Base64.toBytes str |> Maybe.andThen bytesToInts


exactly : Int -> D.Decoder a -> D.Decoder (List a)
exactly width elementDecoder =
    D.loop ( width, [] )
        (\( remaining, accum ) ->
            if remaining <= 0 then
                D.succeed <| D.Done (List.reverse accum)

            else
                elementDecoder
                    |> D.map (\v -> D.Loop ( remaining - 1, v :: accum ))
        )



-- TEST UTILS


decodeFuzz : String -> (( List Int, String ) -> Expectation) -> Test
decodeFuzz title expectation =
    fuzz uint8s title <|
        \ints ->
            case decodeIntsToString ints of
                Nothing ->
                    Expect.fail "Couldn't decode bytes"

                Just str ->
                    expectation ( ints, str )


encodeFuzz : String -> (( List Int, String ) -> Expectation) -> Test
encodeFuzz title expectation =
    fuzz b64String title <|
        \str ->
            case encodeStringToInts str of
                Nothing ->
                    Expect.fail "Couldn't encode string"

                Just ints ->
                    expectation ( ints, str )


expectJust : a -> Maybe a -> Expectation
expectJust b =
    Expect.equal (Just b)
