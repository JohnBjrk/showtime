import gleam/io
import gleam/int
import gleam/list
import gleam/string
import gleam/option.{None, Option, Some}
import gleam/map.{Map}
import gleam/dynamic.{Dynamic}
import showtime/internal/common/test_result.{
  Assert, AssertEqual, AssertMatch, AssertNotEqual, Expected, Expression,
  GleamError, GleamErrorDetail, Ignored, Pattern, ReasonDetail, Trace,
  TraceModule, Value,
}
import showtime/internal/common/test_suite.{CompletedTestRun, TestRun}
import showtime/tests/should.{Assertion, Eq, Fail, IsError, IsOk, NotEq}
import showtime/internal/reports/styles.{
  error_style, expected_highlight, failed_style, function_style, got_highlight,
  heading_style, ignored_style, not_style, passed_style, stacktrace_style,
}
import showtime/internal/reports/compare.{compare}
import showtime/internal/reports/table.{
  AlignLeft, AlignRight, Content, Separator, StyledContent, Table, align_table,
  to_string,
}
import showtime/tests/meta.{Meta}

type GleeUnitAssertionType {
  GleeUnitAssertEqual(message: String)
  GleeUnitAssertNotEqual(message: String)
  GleeUnitAssertMatch(message: String)
}

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
    stacktrace: List(Trace),
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
          CompletedTestRun(_test_function, _, result) ->
            case result {
              Error(_) -> Ok(ModuleAndTestRun(module_name, test_run))
              Ok(_) -> Error(Nil)
              Ok(Ignored(_)) -> Error(Nil)
            }
          _ -> {
            test_run
            |> io.debug()
            Error(Nil)
          }
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
        CompletedTestRun(test_function, _total_time, result) ->
          case result {
            Error(exception) ->
              case exception.reason {
                AssertEqual(reason_details) ->
                  Ok(format_reason(
                    erlang_error_to_unified(
                      reason_details,
                      GleeUnitAssertEqual("Assert equal"),
                      exception.stacktrace.traces,
                    ),
                    module_and_test_run.module_name,
                    test_function.name,
                  ))
                AssertNotEqual(reason_details) ->
                  Ok(format_reason(
                    erlang_error_to_unified(
                      reason_details,
                      GleeUnitAssertNotEqual("Assert not equal"),
                      exception.stacktrace.traces,
                    ),
                    module_and_test_run.module_name,
                    test_function.name,
                  ))
                AssertMatch(reason_details) ->
                  Ok(format_reason(
                    erlang_error_to_unified(
                      reason_details,
                      GleeUnitAssertMatch("Assert match"),
                      exception.stacktrace.traces,
                    ),
                    module_and_test_run.module_name,
                    test_function.name,
                  ))
                GleamError(reason) ->
                  Ok(format_reason(
                    gleam_error_to_unified(reason, exception.stacktrace.traces),
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
  let _execution_times_report =
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
    passed_style(
      int.to_string(all_tests_count - failed_tests_count - ignored_tests_count) <> " passed",
    )
  let failed = failed_style(int.to_string(failed_tests_count) <> " failed")
  let ignored = case ignored_tests_count {
    0 -> ""
    _ -> ", " <> ignored_style(int.to_string(ignored_tests_count) <> " ignored")
  }

  let failed_tests_table =
    Table(None, failed_tests_report)
    |> align_table()
    |> to_string()

  "\n" <> failed_tests_table <> "\n" <> passed <> ", " <> failed <> ignored
}

fn erlang_error_to_unified(
  error_details: List(ReasonDetail),
  assertion_type: GleeUnitAssertionType,
  stacktrace: List(Trace),
) {
  error_details
  |> list.fold(
    UnifiedError(None, "not_set", assertion_type.message, "", "", stacktrace),
    fn(unified, reason) {
      case reason {
        Expression(expression) -> UnifiedError(..unified, reason: expression)
        Expected(value) ->
          case assertion_type {
            GleeUnitAssertEqual(_messaged) ->
              UnifiedError(
                ..unified,
                expected: expected_highlight(string.inspect(value)),
              )
            _ -> unified
          }
        Value(value) ->
          case assertion_type {
            GleeUnitAssertNotEqual(_message) ->
              UnifiedError(
                ..unified,
                expected: not_style("not ") <> string.inspect(value),
                got: got_highlight(string.inspect(value)),
              )
            _ ->
              UnifiedError(..unified, got: got_highlight(string.inspect(value)))
          }
        Pattern(pattern) ->
          case pattern {
            "{ ok , _ }" ->
              UnifiedError(..unified, expected: expected_highlight("Ok(_)"))
            "{ error , _ }" ->
              UnifiedError(..unified, expected: expected_highlight("Error(_)"))
            _ -> unified
          }
        _ -> unified
      }
    },
  )
}

fn gleam_error_to_unified(
  gleam_error: GleamErrorDetail,
  stacktrace: List(Trace),
) -> UnifiedError {
  case gleam_error {
    Assert(_module, _function, _line_no, _message, value) -> {
      let result: Result(Dynamic, Assertion(Dynamic, Dynamic)) =
        dynamic.unsafe_coerce(value)
      assert Error(assertion) = result
      case assertion {
        Eq(got, expected, meta) -> {
          let #(expected, got) = compare(expected, got)
          UnifiedError(
            meta,
            "assert",
            "Assert equal",
            expected,
            got,
            stacktrace,
          )
        }
        NotEq(got, expected, meta) ->
          UnifiedError(
            meta,
            "assert",
            "Assert not equal",
            not_style("not ") <> string.inspect(expected),
            string.inspect(got),
            stacktrace,
          )
        IsOk(got, meta) ->
          UnifiedError(
            meta,
            "assert",
            "Assert is Ok",
            expected_highlight("Ok(_)"),
            got_highlight(string.inspect(got)),
            stacktrace,
          )
        IsError(got, meta) ->
          UnifiedError(
            meta,
            "assert",
            "Assert is Ok",
            expected_highlight("Error(_)"),
            got_highlight(string.inspect(got)),
            stacktrace,
          )
        Fail(meta) ->
          UnifiedError(
            meta,
            "assert",
            "Assert is Ok",
            got_highlight("should.fail()"),
            got_highlight("N/A - test always expected to fail"),
            stacktrace,
          )
      }
    }
  }
}

fn format_reason(error: UnifiedError, module: String, function: String) {
  let meta = case error.meta {
    Some(meta) ->
      Some([
        AlignRight(StyledContent(heading_style("Description")), 2),
        Separator(": "),
        AlignLeft(Content(meta.description), 0),
      ])

    None -> None
  }

  let stacktrace =
    error.stacktrace
    |> list.map(fn(trace) {
      case trace {
        Trace(function, _, _) if function == "" -> "(anonymous)"
        TraceModule(module, function, _, _) if function == "" -> module <> "." <> "(anonymous)"
        Trace(function, _, _) -> function
        TraceModule(module, function, _, _) -> module <> "." <> function
      }
    })
  let stacktrace_rows = case stacktrace {
    [] -> []
    [first, ..rest] -> {
      let first_row =
        Some([
          AlignRight(StyledContent(heading_style("Stacktrace")), 2),
          Separator(": "),
          AlignLeft(StyledContent(stacktrace_style(first)), 0),
        ])
      let rest_rows =
        rest
        |> list.map(fn(row) {
          Some([
            AlignRight(Content(""), 2),
            Separator("  "),
            AlignLeft(StyledContent(stacktrace_style(row)), 0),
          ])
        })
      [first_row, ..rest_rows]
    }
  }

  let arrow =
    string.join(
      list.repeat("-", string.length(module) + 1 + string.length(function) / 2),
      "",
    ) <> "⌄"
  let standard_table_rows = [
    Some([
      AlignRight(StyledContent(error_style("Failed")), 2),
      Separator(": "),
      AlignLeft(Content(arrow), 0),
    ]),
    Some([
      AlignRight(StyledContent(heading_style("Test")), 2),
      Separator(": "),
      AlignLeft(StyledContent(module <> "." <> function_style(function)), 0),
    ]),
    meta,
    Some([
      AlignRight(StyledContent(heading_style("Expected")), 2),
      Separator(": "),
      AlignLeft(StyledContent(error.expected), 0),
    ]),
    Some([
      AlignRight(StyledContent(heading_style("Got")), 2),
      Separator(": "),
      AlignLeft(StyledContent(error.got), 0),
    ]),
  ]
  standard_table_rows
  |> list.append(stacktrace_rows)
  |> list.append([
    Some([
      AlignRight(Content(""), 0),
      AlignRight(Content(""), 0),
      AlignRight(Content(""), 0),
    ]),
  ])
  |> list.filter_map(fn(row) { option.to_result(row, Nil) })
}
