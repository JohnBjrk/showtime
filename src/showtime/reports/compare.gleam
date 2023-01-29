import gleam/dynamic.{Dynamic}
import gleam/list
import gleam/string
import gleam/result
import gap.{compare_lists, compare_strings, to_styled}

pub type Annotated {
  InBoth(value: Dynamic)
  Unique(value: Dynamic)
}

pub type Diff {
  Diff(expected: List(Annotated), got: List(Annotated))
  Literal(expected: Dynamic, got: Dynamic)
  LiteralString(expected: String, got: String)
}

pub fn do_compare(expected: Dynamic, got: Dynamic) -> Diff {
  let expected_as_list =
    expected
    |> dynamic.list(dynamic.dynamic)
  let got_as_list =
    got
    |> dynamic.list(dynamic.dynamic)
  let expected_as_string =
    expected
    |> dynamic.string()
  let got_as_string =
    got
    |> dynamic.string()
  case expected_as_list, got_as_list, expected_as_string, got_as_string {
    Ok(expected_list), Ok(got_list), _, _ -> {
      let comparison =
        compare_lists(expected_list, got_list)
        |> to_styled()
      LiteralString(comparison.first, comparison.second)
    }
    _, _, Ok(expected_string), Ok(got_string) -> {
      let comparison =
        compare_strings(expected_string, got_string)
        |> to_styled()
      LiteralString(comparison.first, comparison.second)
    }
    _, _, _, _ -> Literal(expected, got)
  }
}

fn string_or_list_to_list(string_or_list: Dynamic) {
  string_or_list
  |> dynamic.any([
    dynamic.list(dynamic.dynamic),
    fn(maybe_string) {
      maybe_string
      |> dynamic.string()
      |> result.map(fn(as_string) {
        as_string
        |> string.to_graphemes()
        |> list.map(dynamic.from)
      })
    },
  ])
}

pub external fn diff(expected: Dynamic, got: Dynamic) -> Diff =
  "showtime_ffi" "diff"
