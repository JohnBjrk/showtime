![](https://raw.githubusercontent.com/JohnBjrk/showtime/main/assets/images/showtime2.png)

Test framework for Gleam

[![Package Version](https://img.shields.io/hexpm/v/showtime)](https://hex.pm/packages/showtime)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/showtime/)

## Introduction

Showtime is a test framework for Gleam. It can be used in the same way as Gleeunit and intends to support the same API so that migration can be done by simply changing some imports.

In addition to supporting the same features as Gleeunit showtime add some more features:

- Unified report of test-failures on erlang and javascript target
- Diff of equality asserts on strings and lists
- Parallell execution of test-modules on erlang target
- Possible to run specific test-modules

Showtime also adds a new way to specify tests with related meta-data which gives the following capabilities:

- Possible to ignore tests based on tags
- Test can have descriptions which will be shown in the test-report

## How to use

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
