module DecoderTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Json.Decode exposing (decodeString, list, Decoder)
import Json.Decode.Extra exposing (eString, eBool, eInt, eFloat)


eStringTests: Test 
eStringTests = 
    describe "Testing decoding values into strings"
            [ test "decode a list of mixed values into strings" <|
                \_ -> 
                testDecoder (list eString) """ [ "hello  ", 3, 10.2, true  ] """ (Ok [ "hello  ", "3", "10.2", "true"])
            ]


eBoolTests: Test 
eBoolTests = 
    describe "Testing decoding values into bools"
            [ test "decode a list of mixed values into True " <|
                \_ -> 
                let input = """ [ true, "true", " TrUe ", 1, 1.0, "1", "1.0", "01", "01.00" ] """ in 
                let expected = Ok [ True, True, True, True, True, True, True, True, True ] in 
                testDecoder (list eBool) input expected

            , test "decode a list of mixed values into False" <|
                \_ -> 
                let input = """ [ false, "false", " FALse ", 0, 0.0, "0", "0.0", "00", "00.00" ] """ in 
                let expected = Ok [ False, False, False, False, False, False, False, False, False ] in 
                testDecoder (list eBool) input expected
            ]

eIntTests: Test 
eIntTests = 
    describe "Testing decoding values into ints"
            [ test "decode a list of mixed positive values into ints" <|
                \_ -> 

                let input =  """ [ " 1 ", 2, "3.9", 4.4, "05.000" ] """  in 
                let expected = Ok [1, 2, 3, 4, 5] in 
                testDecoder (list eInt) input expected

            , test "decode a list of mixed negative values into ints" <|
                \_ -> 

                let input = """ [ " -1 ", -2, "-3.9", -4.4, "-05.000" ] """  in 
                let expected = Ok [ -1, -2, -3, -4, -5] in 
                testDecoder (list eInt) input expected
            ]


eFloatTests: Test 
eFloatTests = 
    describe "Testing decoding values into floats"
            [ test "decode a list of mixed positive values into floats" <|
                \_ -> 

                let input =  """ [ " 1.1 ", 2.9, "3.9", 4.4, "05.000" ] """  in 
                let expected = Ok [1.1, 2.9, 3.9, 4.4, 5.0] in 
                testDecoder (list eFloat) input expected

            , test "decode a list of mixed negative values into floats" <|
                \_ -> 

                let input = """ [ " -1.1 ", -2.9, "-3.9", -4.4, "-05.000" ] """  in 
                let expected = Ok [ -1.1, -2.9, -3.9, -4.4, -5.0] in 
                testDecoder (list eFloat) input expected
            ]
            

-- A helper function used to test a decoder and compare the outputs to the expected ouputs
testDecoder : Decoder a -> String -> Result String a -> Expectation
testDecoder decoder input expected =
    Expect.equal (decodeString decoder input) expected