module Doc.Json.Decode.ExtraSpec exposing (spec)

import Test
import Expect
import Json.Decode.Extra exposing(..)
import Json.Decode exposing (..)
import Date
import Set
import Dict

spec : Test.Test
spec =
    Test.describe "Json.Decode.Extra"
        [
        Test.test ">>> \"\"\" \"2012-04-23T18:25:43.511Z\" \"\"\" |> decodeString date" <|
            \() ->
                (""" "2012-04-23T18:25:43.511Z" """     |> decodeString date)
                |> Expect.equal (Date.fromString "2012-04-23T18:25:43.511Z"),
        Test.test ">>> \"\"\" \"foo\" \"\"\" |> decodeString date" <|
            \() ->
                (""" "foo" """     |> decodeString date)
                |> Expect.equal (Err "I ran into a `fail` decoder: Unable to parse 'foo' as a date. Dates must be in the ISO 8601 format."),
        Test.test ">>> \"[ 1, 1, 5, 2 ]\" |> decodeString (set int)" <|
            \() ->
                ("[ 1, 1, 5, 2 ]"     |> decodeString (set int))
                |> Expect.equal (Ok <| Set.fromList [ 1, 2, 5 ]),
        Test.test ">>> let input = \"\"\" { \"1\": \"foo\", \"2\": \"bar\" } \"\"\" in decodeString (dict2 int string) input" <|
            \() ->
                (let     input = """     { "1": "foo", "2": "bar" }     """ in decodeString (dict2 int string) input)
                |> Expect.equal (Ok <| Dict.fromList [ ( 1, "foo" ), ( 2, "bar" ) ]),
        Test.test ">>> \"\"\" { \"children\": \"oops\" } \"\"\" |> decodeString (field \"children\" (list string) |> withDefault [])" <|
            \() ->
                (""" { "children": "oops" } """     |> decodeString (field "children" (list string) |> withDefault []))
                |> Expect.equal (Ok []),
        Test.test ">>> \"\"\" null \"\"\" |> decodeString (field \"children\" (list string) |> withDefault [])" <|
            \() ->
                (""" null """     |> decodeString (field "children" (list string) |> withDefault []))
                |> Expect.equal (Ok []),
        Test.test ">>> \"\"\" 30 \"\"\" |> decodeString (int |> withDefault 42)" <|
            \() ->
                (""" 30 """     |> decodeString (int |> withDefault 42))
                |> Expect.equal (Ok 30),
        Test.test ">>> \"\"\" { } \"\"\" |> decodeString (optionalField \"stuff\" string)" <|
            \() ->
                (""" { } """ |> decodeString (optionalField "stuff" string))
                |> Expect.equal (Ok Nothing),
        Test.test ">>> \"\"\" { \"stuff\": [] } \"\"\" |> decodeString (optionalField \"stuff\" string)" <|
            \() ->
                (""" { "stuff": [] } """ |> decodeString (optionalField "stuff" string))
                |> Expect.equal (Err "Expecting a String at _.stuff but instead got: []"),
        Test.test ">>> \"\"\" { \"stuff\": \"yay!\" } \"\"\" |> decodeString (optionalField \"stuff\" string)" <|
            \() ->
                (""" { "stuff": "yay!" } """ |> decodeString (optionalField "stuff" string))
                |> Expect.equal (Ok <| Just "yay!"),
        Test.test ">>> sequence [ map Just string , succeed Nothing , map Just string ] |> flip decodeString \"\"\" [ \"pick me\", \"ignore me\", \"and pick me\" ] \"\"\"" <|
            \() ->
                (sequence     [ map Just string     , succeed Nothing     , map Just string     ]     |> flip decodeString """ [ "pick me", "ignore me", "and pick me" ] """)
                |> Expect.equal (Ok [ Just "pick me", Nothing, Just "and pick me" ])
        ]