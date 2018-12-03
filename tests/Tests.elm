module Tests exposing (suite)

import Bytes exposing (Bytes)
import Bytes.Decode as D
import Bytes.Encode as E
import Decode
import Encode
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
            ]
        , describe "The Encode module"
            [ test "[] -> []" <|
                \_ ->
                    expectJust [] (encodeStringToInts "")
            , test "AA== -> 0" <|
                \_ ->
                    expectJust [ 0 ] (encodeStringToInts "AA==")
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
    Fuzz.list <| Fuzz.map (modBy 256) <| Fuzz.int


fuzzerFromList : List a -> Fuzzer a
fuzzerFromList list =
    List.map Fuzz.constant list |> Fuzz.oneOf


b64String : Fuzzer String
b64String =
    b64Chars
        |> List.map Fuzz.constant
        |> Fuzz.oneOf
        |> Fuzz.list
        |> Fuzz.map String.fromList
        |> Fuzz.map3
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



-- CONVERTERS


bytesFromInt8s : List Int -> Bytes
bytesFromInt8s =
    E.encode << E.sequence << List.map E.unsignedInt8


decodeIntsToString : List Int -> Maybe String
decodeIntsToString =
    bytesFromInt8s
        >> Decode.fromBytes
        >> Maybe.map String.fromList


encodeStringToInts : String -> Maybe (List Int)
encodeStringToInts str =
    let
        bytesToInts bytes =
            D.decode (intsDecoder (Bytes.width bytes)) bytes
    in
    str
        |> String.toList
        |> Encode.toBytes
        |> Maybe.andThen bytesToInts


intsDecoder : Int -> D.Decoder (List Int)
intsDecoder width =
    D.loop ( width, [] )
        (\( len, ints ) ->
            if len <= 0 then
                D.succeed <| D.Done ( len, ints )

            else
                D.unsignedInt8
                    |> D.map (\int -> D.Loop ( len - 1, int :: ints ))
        )
        |> D.map (Tuple.second >> List.reverse)



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
