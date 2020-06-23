module Json.Encode.Extra exposing
    ( maybe
    , at
    )

{-| Convenience functions for turning Elm values into Json values.

@docs maybe

-}

import Json.Encode exposing (Value, encode, int, null, object)


{-| Encode a Maybe value. If the value is `Nothing` it will be encoded as `null`

    import Json.Encode exposing (..)


    maybe int (Just 50)
    --> int 50


    maybe int Nothing
    --> null

-}
maybe : (a -> Value) -> Maybe a -> Value
maybe encoder =
    Maybe.map encoder >> Maybe.withDefault null


{-| Decode a nested JSON object, requiring certain fields.

    import Json.Encode exposing (..)

    encodedValue : Json.Encode.Value
    encodedValue =
        (Json.Encode.string "Elm Street")

    at [ "Nightmare", "At" ] encodedValue
        |> Json.Encode.encode 0
    --> "{\"Nightmare\":{\"At\":\"Elm Street\"}}"

This is really just a shorthand for:

    Json.Encode.object [ ( "Nightmare", Json.Encode.object [ ( "At", encodedValue ) ] ) ]
        |> Json.Encode.encode 0

-}
at : List String -> Json.Encode.Value -> Json.Encode.Value
at keys initial =
    List.foldr (\k current -> Json.Encode.object [ ( k, current ) ]) initial keys
