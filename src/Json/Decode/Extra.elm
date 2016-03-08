module Json.Decode.Extra (date, apply, (|:), set, dict2, withDefault, maybeNull, lazy) where
{-| Convenience functions for working with Json

# Date
@docs date

# Incremental Decoding
@docs apply, (|:)

# Set
@docs set

# Dict
@docs dict2

# Maybe
@docs withDefault, maybeNull

# Recursively Defined Decoders
@docs lazy

-}

import Json.Decode exposing (..)
import Date
import Time
import Dict exposing (Dict)
import Set exposing (Set)

{-| Can be helpful when decoding large objects incrementally.

    import Date (Date)

    type alias User =
      { id                : Int
      , createdAt         : Date
      , updatedAt         : Date
      , deletedAt         : Maybe Date
      , username          : Maybe String
      , email             : Maybe String
      , fullname          : Maybe String
      , avatar            : Maybe String
      , isModerator       : Bool
      , isOrganization    : Bool
      , isAdmin           : Bool
      }

    metaDecoder : (Int -> Date -> Date -> Maybe Date -> b) -> Decoder b
    metaDecoder f = f
      `map`      ("id"        := int)
      `apply` ("createdAt" := date)
      `apply` ("updatedAt" := date)
      `apply` ("deletedAt" := maybe date)

    userDecoder : Decoder User
    userDecoder = metaDecoder User
      `apply` ("username"          := maybe string)
      `apply` ("email"             := maybe string)
      `apply` ("fullname"          := maybe string)
      `apply` ("avatar"            := maybe string)
      `apply` ("isModerator"       := bool)
      `apply` ("isOrganization"    := bool)
      `apply` ("isAdmin"           := bool)

This is a shortened form of

    metaDecoder : (Int -> Date -> Date -> Maybe Date -> b) -> Decoder b
    metaDecoder f = f
      `map`      ("id"        := int)
      `andThen` \f -> f `map` ("createdAt" := date)
      `andThen` \f -> f `map` ("updatedAt" := date)
      `andThen` \f -> f `map` ("deletedAt" := maybe date)

    userDecoder : Decoder User
    userDecoder = metaDecoder User
      `andThen` \f -> f `map` ("username"          := maybe string)
      `andThen` \f -> f `map` ("email"             := maybe string)
      `andThen` \f -> f `map` ("fullname"          := maybe string)
      `andThen` \f -> f `map` ("avatar"            := maybe string)
      `andThen` \f -> f `map` ("isModerator"       := bool)
      `andThen` \f -> f `map` ("isOrganization"    := bool)
      `andThen` \f -> f `map` ("isAdmin"           := bool)

-}
apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply f aDecoder =
  f `andThen` (\f' -> f' `map` aDecoder)


{-| Infix version of `apply` that makes for a nice DSL when decoding objects:

    locationDecoder : Decoder Location
    locationDecoder =
        succeed Location
            |: ("id" := int)
            |: ("name" := string)
            |: ("address" := string)


    type alias Location =
        { id : Int
        , name : String
        , address : String
        }

If you're curious, here's how this works behind the scenes.

`Location` is a type alias, and type aliases give you a convenience function
that returns an instance of the record in question. Try this out in `elm repl`:

    > type alias Location = { id : Int, name: String, address: String }

    > Location
    <function> : Int -> String -> String -> Repl.Location

    > Location 1 "The White House" "1600 Pennsylvania Ave"
    { id = 1, name = "The White House", address = "1600 Pennsylvania Ave" }

In other words, if you call the `Location` function, passing three arguments,
it will return a new `Location` record by filling in each of its fields. (The
argument order is based on the order in which we listed the fields in the
type alias; the first argument sets `id`, the second argument sets `name`, etc.)

Now try running this through `elm repl`:

    > import Json.Decode exposing (succeed, int, string, (:=))

    > succeed Location
    <function>
        : Json.Decode.Decoder
            (Int -> String -> String -> Repl.Location)

So `succeed Location` gives us a `Decoder (Int -> String -> String -> Location)`.
That's not what we want! What we want is a `Decoder Location`. All we have so
far is a `Decoder` that wraps not a `Location`, but rather a function that
returns a `Location`.

What `|: ("id" := int)` does is to take that wrapped function and pass an
argument to it.

    > import Json.Decode exposing (succeed, int, string, (:=))

    > ("id" := int)
    <function> : Json.Decode.Decoder Int

    > succeed Location |: ("id" := int)
    <function>
        : Json.Decode.Decoder
            (String -> String -> Repl.Location)

Notice how the wrapped function no longer takes an `Int` as its first argument.
That's because `|:` went ahead and supplied one: the `Int` wrapped by the decoder
`("id" := int)` (which returns a `Decoder Int`).

Compare:

    -- succeed Location
    Decoder (Int -> String -> String -> Location)

    -- succeed Location |: ("id" := int)
    Decoder (String -> String -> Location)

We still want a `Decoder Location` and we still don't have it yet. Our decoder
still wraps a function instead of a plain `Location`. However, that function is
now smaller by one argument!

Let's repeat this pattern to provide the first `String` argument next.

    -- succeed Location
    Decoder (Int -> String -> String -> Location)

    -- succeed Location |: ("id" := int)
    Decoder (String -> String -> Location)

    -- succeed Location |: ("id" := int) |: ("name" := string)
    Decoder (String -> Location)

Smaller and smaller! Now we're down from `(Int -> String -> String -> Location)`
to `(String -> Location)`. What happens if we repeat the pattern one more time?

    -- succeed Location
    Decoder (Int -> String -> String -> Location)

    -- succeed Location |: ("id" := int)
    Decoder (String -> String -> Location)

    -- succeed Location |: ("id" := int) |: ("name" := string)
    Decoder (String -> Location)

    -- succeed Location |: ("id" := int) |: ("name" := string) |: ("address" := string)
    Decoder Location

Having now supplied all three arguments to the wrapped function, it has ceased
to be a function. It's now just a plain old `Location`, like we wanted all along.

We win!
-}
(|:) : Decoder (a -> b) -> Decoder a -> Decoder b
(|:) =
  apply

{-| Extract a date.

Note that this function is not total, it will throw an exception given an incorrectly formatted date.
See `Date.fromString` and `Json.customDecoder`.

-}
date : Decoder Date.Date
date = customDecoder string Date.fromString

-- {-| Extract a time value.
--
-- Note that this function is not total, it will throw an exception given an incorrectly formatted time value.
-- See `Time.fromString` and `Json.customDecoder`.
--
-- -}
-- time : Decoder Time.Time
-- time = customDecoder string (Date.fromString >> Date.toTime)


{-| Extract a set. -}
set : Decoder comparable -> Decoder (Set comparable)
set decoder =
    (list decoder)
        `andThen`
            (Set.fromList >> succeed)


{-| Extract a dict using separate decoders for keys and values. -}
dict2 : Decoder comparable -> Decoder v -> Decoder (Dict comparable v)
dict2 keyDecoder valueDecoder =
    (dict valueDecoder) `andThen`
        (Dict.toList >> (decodeDictFromTuples keyDecoder))


{- Helper function for dict -}
decodeDictFromTuples :
    Decoder comparable ->
    List (String, v) ->
    Decoder (Dict comparable v)
decodeDictFromTuples keyDecoder tuples =
    case tuples of
        [] ->
            succeed Dict.empty

        (strKey, value)::rest ->
            case decodeString keyDecoder strKey of
                Ok key ->
                    (decodeDictFromTuples keyDecoder rest) `andThen`
                        ((Dict.insert key value) >> succeed)

                Err error ->
                    fail error


{-| Try running the given decoder; if that fails, then succeed with the given
fallback value.

    -- If this field is missing or malformed, it will decode to [].
    ("optionalNames" := list string)
      |> (withDefault [])

-}
withDefault : a -> Decoder a -> Decoder a
withDefault fallback decoder =
    maybe decoder
        `andThen`
            ((Maybe.withDefault fallback) >> succeed)


{-| Extract a value that might be null. If the value is null,
succeed with Nothing. If the value is present but not null, succeed with
Just that value. If the value is missing, fail.

    -- Yields Nothing if middleName is null, and Just middleName if it's a string.
    "middleName" := maybeNull string

-}
maybeNull : Decoder a -> Decoder (Maybe a)
maybeNull decoder =
    oneOf [null Nothing, map Just decoder]


{-| Enable decoders defined in terms of themselves by lazily creating them.

    treeNode =
      object2
        instantiateTreeNode
        ("name" := string)
        ("children" := list (lazy (\_ -> treeNode)))

-}
lazy : (() -> Decoder a) -> Decoder a
lazy getDecoder =
    customDecoder value <|
       \rawValue ->
            decodeValue (getDecoder ()) rawValue
