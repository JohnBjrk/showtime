import gleam/io
import gleam/int
import gleam/list
import gleam/string
import gleam/option.{None, Option, Some}
import gleam/map.{Map}
import gleam/dynamic.{Dynamic}
import showtime/common/test_result.{
  Assert, AssertEqual, Expected, Expression, GleamError, GleamErrorDetail,
  Ignored, ReasonDetail, TestFunctionReturn, Value,
}
import showtime/common/test_suite.{CompletedTestRun, TestRun}
import showtime/tests/should.{Assertion, Eq, NotEq}
import showtime/reports/styles.{
  bold_green, bold_red, bold_yellow, error_style, expected_style, got_style,
  message_style, module_style,
}
import showtime/reports/compare.{
  Annotated, Diff, InBoth, Literal, LiteralString, Unique, do_compare,
}
import showtime/tests/meta.{Meta}

type ModuleAndTest {
  ModuleAndTestRun(module_name: String, test_run: TestRun)
}

type UnifiedError {
  UnifiedError(
    meta: Option(Meta),
    reason: String,
    message: String,
    expected: String,
    got: String,
  )
}

pub fn create_test_report(test_results: Map(String, Map(String, TestRun))) {
  let all_test_runs =
    test_results
    |> map.values()
    |> list.flat_map(map.values)
  let failed_test_runs =
    test_results
    |> map.to_list()
    |> list.flat_map(fn(entry) {
      let #(module_name, test_module_results) = entry
      test_module_results
      |> map.values()
      |> list.filter_map(fn(test_run) {
        case test_run {
          CompletedTestRun(test_function, _, result) ->
            case result {
              Error(_) -> Ok(ModuleAndTestRun(module_name, test_run))
              Ok(_) -> Error(Nil)
              Ok(Ignored(_)) -> Error(Nil)
            }
          _ -> Error(Nil)
        }
      })
    })

  let ignored_test_runs =
    test_results
    |> map.to_list()
    |> list.flat_map(fn(entry) {
      let #(module_name, test_module_results) = entry
      test_module_results
      |> map.values()
      |> list.filter_map(fn(test_run) {
        case test_run {
          CompletedTestRun(test_function, _, result) ->
            case result {
              Ok(Ignored(reason)) ->
                Ok(#(module_name <> "." <> test_function.name, reason))
              _ -> Error(Nil)
            }
          _ -> Error(Nil)
        }
      })
    })

  let failed_tests_report =
    failed_test_runs
    |> list.filter_map(fn(module_and_test_run) {
      case module_and_test_run.test_run {
        CompletedTestRun(test_function, total_time, result) ->
          case result {
            Error(exception) ->
              case exception.reason {
                AssertEqual(reason_details) ->
                  Ok(format_reason(
                    erlang_error_to_unified(reason_details, "Assert equal"),
                    module_and_test_run.module_name,
                    test_function.name,
                  ))
                GleamError(reason) ->
                  Ok(format_reason(
                    gleam_error_to_unified(reason),
                    module_and_test_run.module_name,
                    test_function.name,
                  ))
                other -> {
                  io.println("Other: " <> string.inspect(other))
                  assert True = False
                  Error(Nil)
                }
              }
            _ -> Error(Nil)
          }
        _ -> Error(Nil)
      }
    })
    |> list.fold([], fn(rows, test_rows) { list.append(rows, test_rows) })
  let all_test_execution_time_reports =
    all_test_runs
    |> list.filter_map(fn(test_run) {
      case test_run {
        CompletedTestRun(test_function, total_time, _) ->
          Ok(test_function.name <> ": " <> int.to_string(total_time) <> " ms")
        _ -> Error(Nil)
      }
    })
  let execution_times_report =
    all_test_execution_time_reports
    |> string.join("\n")

  let all_tests_count =
    all_test_runs
    |> list.length()
  let ignored_tests_count =
    ignored_test_runs
    |> list.length()
  let failed_tests_count =
    failed_test_runs
    |> list.length()

  let passed =
    bold_green()(
      int.to_string(all_tests_count - failed_tests_count - ignored_tests_count) <> " passed",
    )
  let failed =
    bold_red()(
      int.to_string(all_tests_count - ignored_tests_count) <> " failed",
    )
  let ignored = case ignored_tests_count {
    0 -> ""
    _ -> ", " <> bold_yellow()(int.to_string(ignored_tests_count) <> " ignored")
  }

  let failed_tests_table =
    Table(None, failed_tests_report)
    |> align_table()
    |> to_string()

  "\n" <> failed_tests_table <> "\n" <> passed <> ", " <> failed <> ignored
}

fn erlang_error_to_unified(error_details: List(ReasonDetail), message: String) {
  error_details
  |> list.fold(
    UnifiedError(None, "not_set", message, "", ""),
    fn(unified, reason) {
      case reason {
        Expression(expression) -> UnifiedError(..unified, reason: expression)
        Expected(value) ->
          UnifiedError(..unified, expected: bold_green()(string.inspect(value)))
        Value(value) ->
          UnifiedError(..unified, got: bold_red()(string.inspect(value)))
        _ -> unified
      }
    },
  )
}

fn gleam_error_to_unified(gleam_error: GleamErrorDetail) -> UnifiedError {
  case gleam_error {
    Assert(module, function, line_no, message, value) -> {
      let result: Result(Dynamic, Assertion(Dynamic)) =
        dynamic.unsafe_coerce(value)
      assert Error(assertion) = result
      case assertion {
        Eq(expected, got, meta) -> {
          let d = do_compare(expected, got)
          let #(annotated_expected, annotated_got) = case d {
            Diff(expected, got) -> #(
              format_diff(expected, bold_green()),
              format_diff(got, bold_red()),
            )
            Literal(expected, got) -> #(
              bold_green()(string.inspect(expected)),
              bold_red()(string.inspect(got)),
            )
            LiteralString(expected, got) -> #(expected, got)
          }
          UnifiedError(
            meta,
            "assert",
            "Assert equal",
            annotated_expected,
            annotated_got,
          )
        }
        NotEq(expected, got) ->
          UnifiedError(
            None,
            "assert",
            "Assert not equal",
            "not " <> string.inspect(expected),
            string.inspect(got),
          )
      }
    }
  }
}

fn format_diff(values: List(Annotated), unique_styler) -> String {
  let formatted_values =
    values
    |> list.map(fn(value) {
      case value {
        InBoth(value) -> string.inspect(value)
        Unique(value) -> unique_styler(string.inspect(value))
      }
    })
    |> string.join(", ")
  "[" <> formatted_values <> "]"
}

fn format_reason(error: UnifiedError, module: String, function: String) {
  let meta = case error.meta {
    Some(meta) ->
      Some([
        AlignRight(Content("Description", module_style("Description")), 2),
        Separator(": "),
        AlignLeft(Content(meta.description, meta.description), 0),
      ])

    None -> None
  }

  let arrow =
    string.join(
      list.repeat("-", string.length(module) + 1 + string.length(function) / 2),
      "",
    ) <> "⌄"
  let test = module <> "." <> function
  let standard_table_rows = [
    Some([
      AlignRight(Content("Error", error_style("Error")), 2),
      Separator(": "),
      AlignLeft(Content(arrow, arrow), 0),
    ]),
    Some([
      AlignRight(Content("Test", module_style("Test")), 2),
      Separator(": "),
      AlignLeft(
        Content(
          module <> "." <> function,
          module <> "." <> module_style(function),
        ),
        0,
      ),
    ]),
    meta,
    Some([
      AlignRight(Content("Expected", module_style("Expected")), 2),
      Separator(": "),
      AlignLeft(Content(error.expected, error.expected), 0),
    ]),
    Some([
      AlignRight(Content("Got", module_style("Got")), 2),
      Separator(": "),
      AlignLeft(Content(error.got, error.got), 0),
    ]),
    Some([
      AlignRight(Content("", ""), 0),
      AlignRight(Content("", ""), 0),
      AlignRight(Content("", ""), 0),
    ]),
  ]
  // meta <> error_style("   Error") <> " in: " <> module_style(
  //   module <> "." <> function,
  // ) <> "\n" <> message_style("    Message: ") <> error.message <> "\n" <> expected_style(
  //   "   Expected: ",
  // ) <> error.expected <> "\n" <> got_style("        Got: ") <> error.got <> "\n"
  // let table_rows =
  standard_table_rows
  |> list.filter_map(fn(row) { option.to_result(row, Nil) })
  // Table(None, table_rows)
  // |> align_table()
  // |> to_string()
}

pub type Content {
  Content(unstyled: String, styled: String)
}

pub type Col {
  AlignRight(content: Content, margin: Int)
  AlignLeft(content: Content, margin: Int)
  Separator(char: String)
  Aligned(content: String)
}

pub type Table {
  Table(header: Option(String), rows: List(List(Col)))
}

pub fn to_string(table: Table) -> String {
  let rows =
    table.rows
    |> list.map(fn(row) {
      row
      |> list.filter_map(fn(col) {
        case col {
          Separator(char) -> Ok(char)
          Aligned(content) -> Ok(content)
          _ -> Error(Nil)
        }
      })
      |> string.join("")
    })
    |> string.join("\n")
  let header =
    table.header
    |> option.map(fn(header) { header <> "\n" })
    |> option.unwrap("")
  header <> rows
}

pub fn align_table(table: Table) -> Table {
  let cols =
    table.rows
    |> list.transpose()
  let col_width =
    cols
    |> list.map(fn(col) {
      col
      |> list.map(fn(content) {
        case content {
          AlignRight(Content(unstyled, _), _) -> unstyled
          AlignLeft(Content(unstyled, _), _) -> unstyled
          Separator(char) -> char
          Aligned(content) -> content
        }
      })
      |> list.fold(0, fn(max, str) { int.max(max, string.length(str)) })
    })
  let aligned_col =
    cols
    |> list.zip(col_width)
    |> list.map(fn(col_and_width) {
      let #(col, width) = col_and_width
      col
      |> list.map(fn(content) {
        case content {
          AlignRight(Content(unstyled, styled), margin) ->
            Aligned(pad_left(
              styled,
              width + margin - string.length(unstyled),
              " ",
            ))
          AlignLeft(Content(unstyled, styled), margin) ->
            Aligned(pad_right(
              styled,
              width + margin - string.length(unstyled),
              " ",
            ))
          Separator(char) -> Separator(char)
          Aligned(content) -> Aligned(content)
        }
      })
    })
  let aligned_rows =
    aligned_col
    |> list.transpose()
  Table(..table, rows: aligned_rows)
}

fn pad_left(str: String, num: Int, char: String) {
  let padding =
    list.repeat(char, num)
    |> string.join("")
  padding <> str
}

fn pad_right(str: String, num: Int, char: String) {
  let padding =
    list.repeat(char, num)
    |> string.join("")
  str <> padding
}
