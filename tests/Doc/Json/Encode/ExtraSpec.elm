module Doc.Json.Encode.ExtraSpec exposing (spec)

import Test
import Expect
import Json.Encode.Extra exposing(..)
import Json.Encode exposing (int, null)

spec : Test.Test
spec =
    Test.describe "Json.Encode.Extra"
        [
        Test.test ">>> maybe int (Just 50)" <|
            \() ->
                (maybe int (Just 50))
                |> Expect.equal (int 50),
        Test.test ">>> maybe int Nothing" <|
            \() ->
                (maybe int Nothing)
                |> Expect.equal (null)
        ]