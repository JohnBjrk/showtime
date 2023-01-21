import gleam/io
import gleam/int
import gleam/list
import gleam/string
import gleam/map.{Map}
import gleam/dynamic.{Dynamic}
import showtime/common/test_result.{
  Assert, AssertEqual, Expected, Expression, GleamError, GleamErrorDetail,
  ReasonDetail, Value,
}
import showtime/common/test_suite.{CompletedTestRun, TestRun}
import showtime/tests/should.{Assertion, Eq, NotEq}
import showtime/reports/styles.{
  bold_green, bold_red, error_style, expected_style, got_style, message_style,
  module_style,
}
import showtime/reports/compare.{
  Annotated, Diff, InBoth, Literal, Unique, do_compare,
}

type ModuleAndTest {
  ModuleAndTestRun(module_name: String, test_run: TestRun)
}

type UnifiedError {
  UnifiedError(reason: String, message: String, expected: String, got: String)
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
          CompletedTestRun(_, _, result) ->
            case result {
              Error(_) -> Ok(ModuleAndTestRun(module_name, test_run))
              Ok(_) -> Error(Nil)
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
    |> string.join("\n")
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
  let failed_tests_count =
    failed_test_runs
    |> list.length()
  "\n" <> execution_times_report <> "\n\n" <> failed_tests_report <> "\n" <> int.to_string(
    all_tests_count - failed_tests_count,
  ) <> "/" <> int.to_string(all_tests_count) <> " passed"
}

fn erlang_error_to_unified(error_details: List(ReasonDetail), message: String) {
  error_details
  |> list.fold(
    UnifiedError("not_set", message, "", ""),
    fn(unified, reason) {
      case reason {
        Expression(expression) -> UnifiedError(..unified, reason: expression)
        Expected(value) ->
          UnifiedError(..unified, expected: string.inspect(value))
        Value(value) -> UnifiedError(..unified, got: string.inspect(value))
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
        Eq(expected, got) -> {
          let d = do_compare(expected, got)
          let #(annotated_expected, annotated_got) = case d {
            Diff(expected, got) -> #(
              format_diff(expected, bold_red()),
              format_diff(got, bold_green()),
            )
            Literal(expected, got) -> #(
              bold_red()(string.inspect(expected)),
              bold_green()(string.inspect(got)),
            )
          }
          UnifiedError(
            "assert",
            "Assert equal",
            annotated_expected,
            annotated_got,
          )
        }
        NotEq(expected, got) ->
          UnifiedError(
            "assert",
            "Assert not equal",
            "!" <> string.inspect(expected),
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
  error_style("   Error") <> " in: " <> module_style(module <> "." <> function) <> "\n" <> message_style(
    "    Message: ",
  ) <> error.message <> "\n" <> expected_style("   Expected: ") <> error.expected <> "\n" <> got_style(
    "        Got: ",
  ) <> error.got <> "\n"
}
