import glint.{CommandInput}
import glint/flag.{LS, S}
import gleam/result
import gleam/string
import gleam/io
import snag
import showtime/internal/common/cli.{Capture, Mixed, No, Yes}

if erlang {
  import gleam/list
  import gleam/option.{None, Option, Some}
  import gleam/erlang.{start_arguments}
  import showtime/internal/common/test_suite.{EndTestRun, StartTestRun}
  import showtime/internal/erlang/event_handler
  import showtime/internal/erlang/module_handler
  import showtime/internal/erlang/runner
  import showtime/internal/erlang/discover.{
    collect_modules, collect_test_functions,
  }

  pub fn main() {
    start_with_args(start_arguments(), run)
  }

  fn run(
    module_list: Option(List(String)),
    ignore_tags: List(String),
    capture: Capture,
  ) {
    // Start event handler which will collect test-results and eventually
    // print test report
    let test_event_handler = event_handler.start()
    // Start module handler which receives msg about modules to test and
    // runs the test-suite for the module
    let test_module_handler =
      module_handler.start(
        test_event_handler,
        collect_test_functions,
        runner.run_test_suite,
        ignore_tags,
        capture,
      )

    test_event_handler(StartTestRun)
    // Collect modules and notify the module handler to start the test-suites
    let modules = collect_modules(test_module_handler, module_list)
    test_event_handler(EndTestRun(
      modules
      |> list.length(),
    ))
    Nil
  }
}

if javascript {
  import gleam/map
  import gleam/option.{None, Option, Some}
  import showtime/internal/common/test_suite.{TestEvent}
  import showtime/internal/common/common_event_handler.{
    Finished, HandlerState, NotStarted, handle_event,
  }
  import showtime/internal/reports/formatter.{create_test_report}

  pub fn main() {
    start_with_args(start_arguments(), run)
  }

  fn run(
    module_list: Option(List(String)),
    ignore_tags: List(String),
    capture: Capture,
  ) {
    // Find test modules and run the tests using the event-handler for
    // collecting test-results and eventually print a test-report
    run_tests(
      event_handler,
      HandlerState(NotStarted, 0, map.new()),
      module_list,
      ignore_tags,
      capture,
    )
  }

  fn event_handler(event: TestEvent, state: HandlerState) {
    let new_state = handle_event(event, system_time, state)
    case new_state {
      HandlerState(Finished(_), _, events) -> {
        let #(report, num_failed) = create_test_report(events)
        io.println(report)
        case num_failed > 0 {
          True -> exit(1)
          False -> exit(0)
        }
      }
      _ -> Nil
    }
    new_state
  }

  external fn run_tests(
    fn(TestEvent, HandlerState) -> HandlerState,
    HandlerState,
    Option(List(String)),
    List(String),
    Capture,
  ) -> Nil =
    "./showtime_ffi.mjs" "run"

  external fn start_arguments() -> List(String) =
    "./showtime_ffi.mjs" "start_args"

  external fn exit(Int) -> Nil =
    "./showtime_ffi.mjs" "exit"

  external fn system_time() -> Int =
    "./showtime_ffi.mjs" "system_time"
}

fn start_with_args(args, func) {
  glint.new()
  |> glint.add_command(
    at: [],
    do: mk_runner(func),
    with: [
      flag.strings(
        called: "modules",
        default: [],
        explained: "Run only tests in the modules in this list",
      ),
      flag.strings(
        called: "ignore",
        default: [],
        explained: "Ignore tests that are have tags matching a tag in this list",
      ),
      flag.string(
        called: "capture",
        default: "no",
        explained: "Capture output: no (default) - output when tests are run, yes - output is captured and shown in report, mixed - output when run and in report",
      ),
    ],
    described: "Runs test",
  )
  |> glint.run(args)
}

fn mk_runner(func) {
  fn(command: CommandInput) {
    let module_list = case
      command.flags
      |> flag.get("modules")
      |> result.unwrap(LS([]))
    {
      LS(module_list) ->
        case module_list {
          [] -> None
          _ -> Some(module_list)
        }
      _ -> None
    }
    let ignore_tags = case
      command.flags
      |> flag.get("ignore")
      |> result.unwrap(LS([]))
    {
      LS(ignore_tags) -> ignore_tags
      _ -> []
    }
    let capture_output_result = case
      command.flags
      |> flag.get("capture")
      |> result.map(fn(arg) {
        assert S(string_arg) = arg
        S(string.lowercase(string_arg))
      })
      |> result.unwrap(S("no"))
    {
      S("no") -> Ok(No)
      S("yes") -> Ok(Yes)
      S("mixed") -> Ok(Mixed)
      _ -> snag.error("Expected capture to be one of: ['yes', 'no', 'mixed']")
    }

    case capture_output_result {
      Ok(capture_output) -> func(module_list, ignore_tags, capture_output)
      Error(error) ->
        error
        |> snag.pretty_print()
        |> io.println()
    }
  }
  // func(module_list, ignore_tags)
}
