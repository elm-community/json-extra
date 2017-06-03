### 2.2.0

**Additions:**
- `parseInt` and `parseFloat` for weird api's that return numbers as strings
- `doubleEncoded` for a more generic _json as a string in json_ issues

**Fixes:**
- `optionalField` decodes the field, rather than the surrounding object now.

**Other Stuff:**
- Code Style conforms to elm-format@exp
- Doc tests!
- Travis integration

### 2.1.0

**Additions:**
- `optionalField : String -> Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe.Maybe a)` - Decode an optional field, succeeding with `Nothing` if it is missing, but still giving an error if it is malformed.

### 2.0.0

**Breaking Changes:**
- Upgrade for Elm 0.18
- Removed `maybeNull` in favor of `Json.Decode.nullable`
- Removed `lazy` in favor of `Json.Decode.lazy`
- Renamed `apply` to `andMap` and reversed arguments to `Decoder a -> Decoder (a -> b) -> Decoder b` to make it work nicely with `(|>)`

**Additions:**
- `fromResult : Result String a -> Decoder a` - convert a `Result` to a `Decoder`, helpful in `andThen` callbacks following the removal of `Json.Decode.customDecoder`
- `Json.Encode.Extra.maybe : (a -> Value) -> Maybe a -> Value` - encode a `Maybe a` given an encoder for `a`. Thanks to @hendore for this addition.

**Other Stuff:**
- Code style conforms to elm-format

#### 1.1.0

**Additions:**
- `Json.Decode.Extra.sequence` - lets you generate a list of `Decoder a` and attempt to apply them to a JSON list. _Authored by @cobalamin_


#### 1.0.0

**Breaking Changes:**
- Upgrade for Elm 0.17
