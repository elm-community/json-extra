### 2.0.0

**Breaking Changes:**
- Upgrade for Elm 0.18
- Removed `maybeNull` in favor of `Json.Decode.nullable`
- Removed `lazy` in favor of `Json.Decode.lazy`
- Renamed `apply` to `andMap` and reversed arguments to `Decoder a -> Decoder (a -> b) -> Decoder b` to make it work nicely with `(|>)`

**Additions:**
- `fromResult : Result String a -> Decoder a` - convert a `Result` to a `Decoder`, helpful in `andThen` callbacks following the removal of `Json.Decode.customDecoder`

#### 1.1.0

**Additions:**
- `Json.Decode.Extra.sequence` - lets you generate a list of `Decoder a` and attempt to apply them to a JSON list. _Authored by @cobalamin_


#### 1.0.0

**Breaking Changes:**
- Upgrade for Elm 0.17
