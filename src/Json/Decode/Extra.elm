module Json.Decode.Extra exposing
    ( datetime
    , url
    , andMap
    , when
    , collection, sequence, combine, indexedList, keys
    , set
    , dict2
    , withDefault, optionalField
    , fromResult
    , parseInt, parseFloat, doubleEncoded
    )

{-| Convenience functions for working with Json


# Dates

@docs datetime


# URLs

@docs url


# Incremental Decoding

@docs andMap


# Conditional Decoding

@docs when


# List

@docs collection, sequence, combine, indexedList, keys


# Set

@docs set


# Dict

@docs dict2


# Maybe

@docs withDefault, optionalField


# Result

@docs fromResult


# Encoded strings

@docs parseInt, parseFloat, doubleEncoded

-}

import Dict exposing (Dict)
import Iso8601
import Json.Decode exposing (..)
import Set exposing (Set)
import Time
import Url


{-| Decode an ISO-8601 formatted date-time string.

This always returns a `Time.Posix` value, which is naturally always expressed in
UTC.

    import Json.Decode exposing (..)
    import Json.Encode
    import Time

    """ "2018-08-26T09:46:00+02:00" """
        |> decodeString datetime
    --> Ok (Time.millisToPosix 1535269560000)

    """ "" """
      |> decodeString datetime
    --> Err
    -->    (Failure
    -->        "Expecting an ISO-8601 formatted date+time string"
    -->        (Json.Encode.string "")
    -->    )

-}
datetime : Decoder Time.Posix
datetime =
    andThen
        (\dateString ->
            case Iso8601.toTime dateString of
                Ok v ->
                    succeed v

                Err _ ->
                    fail "Expecting an ISO-8601 formatted date+time string"
        )
        string


{-| Decode a URL

This always returns a `Url.Url` value.

    import Json.Decode exposing (..)
    import Url

    """ "http://foo.bar/quux" """
        |> decodeString url
    --> Ok <| Url.Url Url.Http "foo.bar" Nothing "/quux" Nothing Nothing

-}
url : Decoder Url.Url
url =
    andThen
        (\urlString ->
            case Url.fromString urlString of
                Just actualUrl ->
                    succeed actualUrl

                Nothing ->
                    fail "Expecting a  URL"
        )
        string


{-| Can be helpful when decoding large objects incrementally.

See [the `andMap` docs](https://github.com/elm-community/json-extra/blob/2.0.0/docs/andMap.md)
for an explanation of how `andMap` works and how to use it.

-}
andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    map2 (|>)


{-| Extract a set.

    import Json.Decode exposing (..)
    import Set


    "[ 1, 1, 5, 2 ]"
        |> decodeString (set int)
    --> Ok <| Set.fromList [ 1, 2, 5 ]

-}
set : Decoder comparable -> Decoder (Set comparable)
set decoder =
    list decoder
        |> map Set.fromList


{-| Extract a dict using separate decoders for keys and values.

    import Json.Decode exposing (..)
    import Dict


    """ { "1": "foo", "2": "bar" } """
        |> decodeString (dict2 int string)
    --> Ok <| Dict.fromList [ ( 1, "foo" ), ( 2, "bar" ) ]

-}
dict2 : Decoder comparable -> Decoder v -> Decoder (Dict comparable v)
dict2 keyDecoder valueDecoder =
    keyValuePairs valueDecoder
        |> andThen (decodeDictFromTuples keyDecoder)


{-| Helper function for dict
-}
decodeDictFromTuples : Decoder comparable -> List ( String, v ) -> Decoder (Dict comparable v)
decodeDictFromTuples keyDecoder tuples =
    case tuples of
        [] ->
            succeed Dict.empty

        ( strKey, value ) :: rest ->
            case decodeString keyDecoder strKey of
                Ok key ->
                    decodeDictFromTuples keyDecoder rest
                        |> andThen (Dict.insert key value >> succeed)

                Err error ->
                    fail (errorToString error)


{-| Try running the given decoder; if that fails, then succeed with the given
fallback value.

    import Json.Decode exposing (..)


    """ { "children": "oops" } """
        |> decodeString (field "children" (list string) |> withDefault [])
    --> Ok []


    """ null """
        |> decodeString (field "children" (list string) |> withDefault [])
    --> Ok []


    """ 30 """
        |> decodeString (int |> withDefault 42)
    --> Ok 30

-}
withDefault : a -> Decoder a -> Decoder a
withDefault fallback decoder =
    maybe decoder
        |> map (Maybe.withDefault fallback)


{-| If a field is missing, succeed with `Nothing`. If it is present, decode it
as normal and wrap successes in a `Just`.

When decoding with
[`maybe`](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode#maybe),
if a field is present but malformed, you get a success and Nothing.
`optionalField` gives you a failed decoding in that case, so you know
you received malformed data.

Examples:

    import Json.Decode exposing (..)
    import Json.Encode

Let's define a `stuffDecoder` that extracts the `"stuff"` field, if it exists.

    stuffDecoder : Decoder (Maybe String)
    stuffDecoder =
        optionalField "stuff" string

If the "stuff" field is missing, decode to Nothing.

    """ { } """
        |> decodeString stuffDecoder
    --> Ok Nothing

If the "stuff" field is present but not a String, fail decoding.

    expectedError : Error
    expectedError =
        Failure "Expecting a STRING" (Json.Encode.list identity [])
          |> Field "stuff"

    """ { "stuff": [] } """
        |> decodeString stuffDecoder
    --> Err expectedError

If the "stuff" field is present and valid, decode to Just String.

    """ { "stuff": "yay!" } """
        |> decodeString stuffDecoder
    --> Ok <| Just "yay!"

-}
optionalField : String -> Decoder a -> Decoder (Maybe a)
optionalField fieldName decoder =
    let
        finishDecoding json =
            case decodeValue (field fieldName value) json of
                Ok val ->
                    -- The field is present, so run the decoder on it.
                    map Just (field fieldName decoder)

                Err _ ->
                    -- The field was missing, which is fine!
                    succeed Nothing
    in
    value
        |> andThen finishDecoding


{-| This function turns a list of decoders into a decoder that returns a list.

The returned decoder will zip the list of decoders with a list of values,
matching each decoder with exactly one value at the same position. This is most
often useful in cases when you find yourself needing to dynamically generate a
list of decoders based on some data, and decode some other data with this list
of decoders.

Note that this function, unlike `List.map2`'s behaviour, expects the list of
decoders to have the same length as the list of values in the JSON.

    import Json.Decode exposing (..)


    decoder : Decoder (List (Maybe String))
    decoder =
        sequence
            [ map Just string
            , succeed Nothing
            , map Just string
            ]


    decodeString decoder """ [ "pick me", "ignore me", "and pick me" ] """
    --> Ok [ Just "pick me", Nothing, Just "and pick me" ]

-}
sequence : List (Decoder a) -> Decoder (List a)
sequence decoders =
    decoders
        |> List.indexedMap (\idx dec -> index idx dec)
        |> combine


{-| Get access to the current index while decoding a list element.

    import Json.Decode exposing (..)


    repeatedStringDecoder : Int -> Decoder String
    repeatedStringDecoder times =
        string |> map (String.repeat times)


    """ [ "a", "b", "c", "d" ] """
        |> decodeString (indexedList repeatedStringDecoder)
    --> Ok [ "", "b", "cc", "ddd" ]

-}
indexedList : (Int -> Decoder a) -> Decoder (List a)
indexedList indexedDecoder =
    list value
        |> andThen
            (\values ->
                List.range 0 (List.length values - 1)
                    |> List.map indexedDecoder
                    |> sequence
            )


{-| Get a list of the keys of a JSON object

    import Json.Decode exposing (..)


    """ { "alice": 42, "bob": 99 } """
        |> decodeString keys
    --> Ok [ "alice", "bob" ]

-}
keys : Decoder (List String)
keys =
    keyValuePairs (succeed ())
        |> map (List.map Tuple.first)


{-| Transform a result into a decoder

Sometimes it can be useful to use functions that primarily operate on
`Result` in decoders.

    import Json.Decode exposing (..)
    import Json.Encode


    validateString : String -> Result String String
    validateString input =
        case input of
            "" ->
                Err "Empty string is not allowed"
            _ ->
                Ok input


    """ "something" """
        |> decodeString (string |> andThen (fromResult << validateString))
    --> Ok "something"


    """ "" """
        |> decodeString (string |> andThen (fromResult << validateString))
    --> Err (Failure "Empty string is not allowed" (Json.Encode.string ""))

-}
fromResult : Result String a -> Decoder a
fromResult result =
    case result of
        Ok successValue ->
            succeed successValue

        Err errorMessage ->
            fail errorMessage


fromMaybe : String -> Maybe a -> Decoder a
fromMaybe error val =
    case val of
        Just v ->
            succeed v

        Nothing ->
            fail error


{-| Extract an int using [`String.toInt`](http://package.elm-lang.org/packages/elm-lang/core/latest/String#toInt)

    import Json.Decode exposing (..)


    """ { "field": "123" } """
        |> decodeString (field "field" parseInt)
    --> Ok 123

-}
parseInt : Decoder Int
parseInt =
    string |> andThen (String.toInt >> fromMaybe "Failed to parse as int")


{-| Extract a float using [`String.toFloat`](http://package.elm-lang.org/packages/elm-lang/core/latest/String#toFloat)

    import Json.Decode exposing (..)


    """ { "field": "50.5" } """
        |> decodeString (field "field" parseFloat)
    --> Ok 50.5

-}
parseFloat : Decoder Float
parseFloat =
    string |> andThen (String.toFloat >> fromMaybe "failed to parse as float")


{-| Extract a JSON-encoded string field

"Yo dawg, I heard you like JSON..."

If someone has put JSON in your JSON (perhaps a JSON log entry, encoded
as a string) this is the function you're looking for. Give it a decoder
and it will return a new decoder that applies your decoder to a string
field and yields the result (or fails if your decoder fails).

    import Json.Decode exposing (..)


    logEntriesDecoder : Decoder (List String)
    logEntriesDecoder =
        doubleEncoded (list string)


    logsDecoder : Decoder (List String)
    logsDecoder =
        field "logs" logEntriesDecoder


    """ { "logs": "[\\"log1\\", \\"log2\\"]"} """
        |> decodeString logsDecoder
    --> Ok [ "log1", "log2" ]

-}
doubleEncoded : Decoder a -> Decoder a
doubleEncoded decoder =
    string
        |> andThen
            (fromResult
                << Result.mapError errorToString
                << decodeString decoder
            )


{-| Helps converting a list of decoders into a decoder for a list of that type.

    import Json.Decode exposing (..)


    decoders : List (Decoder String)
    decoders =
        [ field "foo" string
        , field "bar" string
        , field "another" string
        ]


    """ { "foo": "hello", "another": "!", "bar": "world" } """
        |> decodeString (combine decoders)
    --> Ok [ "hello", "world", "!" ]

-}
combine : List (Decoder a) -> Decoder (List a)
combine =
    List.foldr (map2 (::)) (succeed [])


{-| Some JavaScript structures look like arrays, but aren't really. Examples
include `HTMLCollection`, `NodeList` and everything else that has a `length`
property, has values indexed by an integer key between 0 and `length`, but yet
_is not_ a JavaScript Array.

This decoder can come to the rescue.

    import Json.Decode exposing (..)


    """ { "length": 3, "0": "foo", "1": "bar", "2": "baz" } """
        |> decodeString (collection string)
    --> Ok [ "foo", "bar", "baz" ]

-}
collection : Decoder a -> Decoder (List a)
collection decoder =
    field "length" int
        |> andThen
            (\length ->
                List.range 0 (length - 1)
                    |> List.map (\index -> field (String.fromInt index) decoder)
                    |> combine
            )


{-| Helper for conditionally decoding values based on some discriminator
that needs to pass a certain check.

    import Json.Decode exposing (..)
    import Json.Encode


    is : a -> a -> Bool
    is a b =
        a == b


    enabledValue : Decoder Int
    enabledValue =
      (field "value" int)
        |> when (field "enabled" bool) (is True)


    """ { "enabled": true, "value": 123 } """
        |> decodeString enabledValue
    --> Ok 123


    input : Json.Decode.Value
    input =
        Json.Encode.object
            [ ( "enabled", Json.Encode.bool False )
            , ( "value", Json.Encode.int 321 )
            ]

    expectedError : Error
    expectedError =
       Failure "Check failed with input" input

    """ { "enabled": false, "value": 321 } """
        |> decodeString enabledValue
    --> Err expectedError

This can also be used to decode union types that are encoded with a discriminator field:

    type Animal = Cat String | Dog String


    dog : Decoder Animal
    dog =
        map Dog (field "name" string)


    cat : Decoder Animal
    cat =
        map Cat (field "name" string)


    animalType : Decoder String
    animalType =
        field "type" string


    animal : Decoder Animal
    animal =
        oneOf
            [ when animalType (is "dog") dog
            , when animalType (is "cat") cat
            ]


    """
    [
      { "type": "dog", "name": "Dawg" },
      { "type": "cat", "name": "Roxy" }
    ]
    """
        |> decodeString (list animal)
    --> Ok [ Dog "Dawg", Cat "Roxy" ]

-}
when : Decoder a -> (a -> Bool) -> Decoder b -> Decoder b
when checkDecoder check passDecoder =
    andThen
        (\checkVal ->
            if check checkVal then
                passDecoder

            else
                fail "Check failed with input"
        )
        checkDecoder
