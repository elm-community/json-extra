module Json.Encode.Extra exposing (maybe)

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
