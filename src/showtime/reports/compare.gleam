import gleam/dynamic.{Dynamic}
import gleam/string
import showtime/reports/styles.{expected_highlight, got_highlight}
import gap.{compare_lists, compare_strings}
import gap/styling.{from_comparison, highlight, to_styled_comparison}

pub fn compare(expected: Dynamic, got: Dynamic) -> #(String, String) {
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
        |> from_comparison()
        |> highlight(expected_highlight, got_highlight, fn(item) { item })
        |> to_styled_comparison()
      #(comparison.first, comparison.second)
    }
    _, _, Ok(expected_string), Ok(got_string) -> {
      let comparison =
        compare_strings(expected_string, got_string)
        |> from_comparison()
        |> highlight(expected_highlight, got_highlight, fn(item) { item })
        |> to_styled_comparison()
      #(comparison.first, comparison.second)
    }
    _, _, _, _ -> #(
      expected_highlight(string.inspect(expected)),
      got_highlight(string.inspect(got)),
    )
  }
}
