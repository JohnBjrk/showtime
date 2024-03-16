@target(erlang)
import gleam/list
@target(erlang)
import gleam/erlang/atom.{type Atom}
@target(erlang)
import showtime/internal/common/test_suite.{
  type TestEventHandler, type TestSuite, EndTest, StartTest,
}
@target(erlang)
import showtime/internal/common/test_result.{type TestResult}
@target(erlang)
import showtime/internal/common/cli.{type Capture}

// Runs all tests in a test suite
@target(erlang)
pub fn run_test_suite(
  test_suite: TestSuite,
  test_event_handler: TestEventHandler,
  ignore_tags: List(String),
  capture: Capture,
) {
  test_suite.tests
  |> list.each(fn(test_case) {
    test_event_handler(StartTest(test_suite.module, test_case))
    let result =
      run_test(test_suite.module.name, test_case.name, ignore_tags, capture)
    test_event_handler(EndTest(test_suite.module, test_case, result))
  })
}

// Wrapper around the ffi function that converts names to atoms
@target(erlang)
pub fn run_test(
  module_name: String,
  test_name: String,
  ignore_tags: List(String),
  capture: Capture,
) -> TestResult {
  let result =
    run_test_ffi(
      atom.create_from_string(module_name),
      atom.create_from_string(test_name),
      ignore_tags,
      capture,
    )
  result
}

// Calls ffi for running a test function
// The ffi will take care of mapping the result and exception to the data-types
// used in gleam
@target(erlang)
@external(erlang, "showtime_ffi", "run_test")
fn run_test_ffi(
  module module: Atom,
  function function: Atom,
  ignore_tags ignore_tags: List(String),
  capture capture: Capture,
) -> TestResult
