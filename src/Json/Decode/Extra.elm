module Json.Decode.Extra where
{-| Convenience functions for working with Json

# Date
@docs date

# Incremental Decoding
@docs apply

# Set
@docs set

# Dict
@docs dict

# Maybe
@docs withDefault

-}

import Json.Decode exposing (..)
import Date
import Time

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
    (Decode.list decoder)
        `Decode.andThen`
            (Set.fromList >> Decode.succeed)


{-| Extract a dict. -}
dict : Decoder comparable -> Decoder v -> Decoder (Dict comparable v)
dict keyDecoder valueDecoder =
    (Decode.dict valueDecoder) `Decode.andThen`
        (Dict.toList >> (decodeDictFromTuples keyDecoder))


{- Helper function for dict -}
decodeDictFromTuples :
    Decoder comparable ->
    List (String, v) ->
    Decoder (Dict comparable v)
decodeDictFromTuples keyDecoder tuples =
    case tuples of
        [] ->
            Decode.succeed Dict.empty

        (strKey, value)::rest ->
            case Decode.decodeString keyDecoder strKey of
                Ok key ->
                    (decodeDictFromTuples keyDecoder rest) `Decode.andThen`
                        ((Dict.insert key value) >> Decode.succeed)

                Err error ->
                    Decode.fail error


{-| Try running the given decoder; if that fails, then succeed with the given
fallback value.

  -- If this field is missing or malformed, it will decode to [].
  ("optionalNames" := list string)
      |> (withDefault [])
-}
withDefault : a -> Decoder a -> Decoder a
withDefault fallback decoder =
    Decode.maybe decoder
        `Decode.andThen`
            ((Maybe.withDefault fallback) >> Decode.succeed)
