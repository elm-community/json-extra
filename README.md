[![Build Status](https://travis-ci.org/elm-community/json-extra.svg?branch=master)](https://travis-ci.org/elm-community/json-extra)

# json-extra

```
elm install elm-community/json-extra
```

Convenience functions for working with JSON. Decoders in this package cover
reasonably common, and slightly less common use-cases you may encounter when
working with JSON in Elm.

## Contributing

Contributions are always welcome. Before opening a PR, it's generally a good
idea to open an issue first, so we can discuss the use-cases before diving into
the code.

## Upgrading to Elm 0.19

`date` is now `datetime` and only sucessfully decodes strings in Iso 8601 format.
