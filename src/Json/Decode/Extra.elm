module Json.Decode.Extra where
{-| Convenience functions for working with Json

# Date
@docs date

# Incremental Decoding
@docs apply

# Set
@docs set

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
