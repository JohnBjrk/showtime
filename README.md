# showtime

[![Package Version](https://img.shields.io/hexpm/v/showtime)](https://hex.pm/packages/showtime)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/showtime/)

A Gleam project

## Quick start

In your main test-file, replace:
```gleam
gleeunit.main()
```
with:
```gleam
showtime.main()
```

Its also possible to replace the import:
```gleam
import gleeunit/should
```
with:
```gleam
import showtime/tests/should
```

This should improve the test-report since showtime can make more assumption on the data passed when
failing an assertion.

## Installation

```sh
gleam add showtime
```

and its documentation can be found at <https://hexdocs.pm/showtime>.
