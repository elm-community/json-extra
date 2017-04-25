module Json.Decode.Extra exposing (date, andMap, (|:), sequence, set, dict2, withDefault, optionalField, fromResult)

{-| Convenience functions for working with Json

# Date
@docs date

# Incremental Decoding
@docs andMap, (|:)

# List
@docs sequence

# Set
@docs set

# Dict
@docs dict2

# Maybe
@docs withDefault, optionalField

# Result
@docs fromResult

# Json (double-encoded strings)
@docs doubleEncoded

-}

import Json.Decode exposing (..)
import Date
import Dict exposing (Dict)
import Set exposing (Set)


{-| Can be helpful when decoding large objects incrementally.

See [the `andMap` docs](https://github.com/elm-community/json-extra/blob/2.0.0/docs/andMap.md)
for an explanation of how `andMap` works and how to use it.
-}
andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    map2 (|>)


{-| Infix version of `andMap` that makes for a nice DSL when decoding objects.

See [the `(|:)` docs](https://github.com/elm-community/json-extra/blob/2.0.0/docs/infixAndMap.md)
for an explanation of how `(|:)` works and how to use it.
-}
(|:) : Decoder (a -> b) -> Decoder a -> Decoder b
(|:) =
    flip andMap


{-| Extract a date using [`Date.fromString`](http://package.elm-lang.org/packages/elm-lang/core/latest/Date#fromString)
-}
date : Decoder Date.Date
date =
    string
        |> andThen (Date.fromString >> fromResult)


{-| Extract a set.
-}
set : Decoder comparable -> Decoder (Set comparable)
set decoder =
    (list decoder)
        |> andThen (Set.fromList >> succeed)


{-| Extract a dict using separate decoders for keys and values.
-}
dict2 : Decoder comparable -> Decoder v -> Decoder (Dict comparable v)
dict2 keyDecoder valueDecoder =
    (dict valueDecoder)
        |> andThen (Dict.toList >> (decodeDictFromTuples keyDecoder))


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
                    (decodeDictFromTuples keyDecoder rest)
                        |> andThen ((Dict.insert key value) >> succeed)

                Err error ->
                    fail error


{-| Try running the given decoder; if that fails, then succeed with the given
fallback value.

    -- If this field is missing or malformed, it will decode to [].
    field "optionalNames" (list string)
      |> (withDefault [])

-}
withDefault : a -> Decoder a -> Decoder a
withDefault fallback decoder =
    maybe decoder
        |> andThen ((Maybe.withDefault fallback) >> succeed)


{-| If a field is missing, succeed with `Nothing`. If it is present, decode it
as normal and wrap successes in a `Just`.

When decoding with
[`maybe`](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode#maybe),
if a field is present but malformed, you get a success and Nothing.
`optionalField` gives you a failed decoding in that case, so you know
you received malformed data.

    -- If the "stuff" field is missing, decode to Nothing.
    -- If the "stuff" field is present and valid, decode to Just (List String).
    -- If the "stuff" field is present but not a List String, fail decoding.
    optionalField "stuff" (list string)

-}
optionalField : String -> Decoder a -> Decoder (Maybe a)
optionalField fieldName decoder =
    let
        finishDecoding json =
            case decodeValue (field fieldName value) json of
                Ok val ->
                    -- The field is present, so run the decoder on it.
                    map Just decoder

                Err _ ->
                    -- The field was missing, which is fine!
                    succeed Nothing
    in
        value
            |> andThen finishDecoding


{-| This function turns a list of decoders into a decoder that returns a list.

The returned decoder will zip the list of decoders with a list of values, matching each decoder with exactly one value at the same position. This is most often useful in cases when you find yourself needing to dynamically generate a list of decoders based on some data, and decode some other data with this list of decoders. There are other functions that seem similar:

- `Json.Decode.oneOf`, which will try every decoder for every value in the list, might be too lenient (e.g. a `4.0` will be interpreted as an `Int` just fine).
- `Json.Decode.tuple1-8`, which do something similar, but have a hard-coded length. As opposed to these functions, where you can decode several different types and combine them, you'll need to manually unify all those types in one sum type to use `sequence`.

Note that this function, unlike `List.map2`'s behaviour, expects the list of decoders to have the same length as the list of values in the JSON.

    type FloatOrInt
        = I Int
        | F Float

    -- we'd like a list like [I, F, I] from this
    -- fairly contrived example, but data like this does exist!
    json = "[1, 2.0, 3]"

    intDecoder = Decode.map I Decode.int
    floatDecoder = Decode.map F Decode.float

    decoder : Decoder (List FloatOrInt)
    decoder =
        sequence [ intDecoder, floatDecoder, intDecoder ]

    decoded = Decode.decodeString decoder json
    -- Ok ([I 1,F 2,I 3]) : Result String (List FloatOrInt)

-}
sequence : List (Decoder a) -> Decoder (List a)
sequence decoders =
    list value |> andThen (sequenceHelp decoders)


{-| Helper function for sequence
-}
sequenceHelp : List (Decoder a) -> List Value -> Decoder (List a)
sequenceHelp decoders jsonValues =
    if List.length jsonValues /= List.length decoders then
        fail "Number of decoders does not match number of values"
    else
        List.map2 decodeValue decoders jsonValues
            |> List.foldr (Result.map2 (::)) (Ok [])
            |> fromResult


{-| Transform a result into a decoder

Sometimes it can be useful to use functions that primarily operate on
`Result` in decoders. An example of this is `Json.Decode.Extra.date`. It
uses the built-in `Date.fromString` to parse a `String` as a `Date`, and
then converts the `Result` from that conversion into a decoder which has
either already succeeded or failed based on the outcome.

    date : Decoder Date
    date =
        string |> andThen (Date.fromString >> fromResult)
-}
fromResult : Result String a -> Decoder a
fromResult result =
    case result of
        Ok successValue ->
            succeed successValue

        Err errorMessage ->
            fail errorMessage

{-| Extract a JSON-encoded string field

"Yo dawg, I heard you like JSON..."

If someone has put JSON in your JSON (perhaps a JSON log entry, encoded
as a string) this is the function you're looking for. Give it a decoder
and it will return a new decoder that applies your decoder to a string
field and yields the result (or fails if your decoder fails).

    log: Decoder (List LogEntry)
    log = list (doubleEncoded LogEntry)

-}
doubleEncoded : Decoder a -> Decoder a
doubleEncoded decoder = string |> andThen (fromResult << decodeString decoder)
