import gleam/list
import showtime/common/test_suite.{EndTestRun, StartTestRun}
import showtime/erlang/event_handler
import showtime/erlang/module_handler
import showtime/erlang/runner
import showtime/erlang/discover.{collect_modules, collect_test_functions}

pub fn main() {
  let test_event_handler = event_handler.start()
  let test_module_handler =
    module_handler.start(
      test_event_handler,
      collect_test_functions,
      runner.run_test_suite,
    )
  test_event_handler(StartTestRun)
  let modules = collect_modules(test_module_handler)
  test_event_handler(EndTestRun(
    modules
    |> list.length(),
  ))
  Nil
}
