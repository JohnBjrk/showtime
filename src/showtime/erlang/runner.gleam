import gleam/io
import gleam/list
import gleam/dynamic.{Dynamic}
import gleam/erlang/atom.{Atom}
import showtime/common/test_suite.{
  EndTest, StartTest, TestEventHandler, TestSuite,
}
import showtime/common/test_result.{Ignored, TestFunctionReturn, TestResult}

pub fn run_test_suite(
  test_suite: TestSuite,
  test_event_handler: TestEventHandler,
) {
  test_suite.tests
  |> list.each(fn(test) {
    test_event_handler(StartTest(test_suite.module, test))
    let result = run_test(test_suite.module.name, test.name)
    test_event_handler(EndTest(test_suite.module, test, result))
  })
}

pub fn run_test(module_name: String, test_name: String) -> TestResult {
  run_test_ffi(
    atom.create_from_string(module_name),
    atom.create_from_string(test_name),
    [],
  )
}

external fn run_test_ffi(
  module: Atom,
  function: Atom,
  args: List(Dynamic),
) -> TestResult =
  "showtime_ffi" "run_test"
