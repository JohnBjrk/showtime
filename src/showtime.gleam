if erlang {
  import gleam/list
  import gleam/option.{None, Option, Some}
  import gleam/result
  import gleam/erlang.{start_arguments}
  import glint.{CommandInput}
  import glint/flag.{LS}
  import showtime/common/test_suite.{EndTestRun, StartTestRun}
  import showtime/erlang/event_handler
  import showtime/erlang/module_handler
  import showtime/erlang/runner
  import showtime/erlang/discover.{collect_modules, collect_test_functions}

  pub fn main() {
    start_with_args(start_arguments(), run)
  }

  pub fn run(module_list: Option(List(String)), ignore_tags: List(String)) {
    let test_event_handler = event_handler.start()
    let test_module_handler =
      module_handler.start(
        test_event_handler,
        collect_test_functions,
        runner.run_test_suite,
        ignore_tags,
      )
    test_event_handler(StartTestRun)
    let modules = collect_modules(test_module_handler, module_list)
    test_event_handler(EndTestRun(
      modules
      |> list.length(),
    ))
    Nil
  }
}

if javascript {
  import gleam/io
  import gleam/map
  import gleam/result
  import gleam/option.{None, Option, Some}
  import glint.{CommandInput}
  import glint/flag.{LS}
  import showtime/common/test_suite.{TestEvent}
  import showtime/common/common_event_handler.{
    Finished, HandlerState, NotStarted, handle_event,
  }
  import showtime/reports/formatter.{create_test_report}

  pub fn main() {
    start_with_args(start_arguments(), run)
  }

  pub fn run(module_list: Option(List(String)), ignore_tags: List(String)) {
    io.println("Module list")
    module_list
    |> io.debug()
    io.println("Ignore tags")
    ignore_tags
    |> io.debug()
    run_tests(
      event_handler,
      HandlerState(NotStarted, 0, map.new()),
      module_list,
      ignore_tags,
    )
  }

  fn event_handler(event: TestEvent, state: HandlerState) {
    let new_state = handle_event(event, state)
    case new_state {
      HandlerState(Finished(_), _, events) ->
        io.println(create_test_report(events))
      _ -> Nil
    }
    new_state
  }

  external fn run_tests(
    fn(TestEvent, HandlerState) -> HandlerState,
    HandlerState,
    Option(List(String)),
    List(String),
  ) -> Nil =
    "./showtime_ffi.mjs" "run"

  external fn start_arguments() -> List(String) =
    "./showtime_ffi.mjs" "start_args"
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
    func(module_list, ignore_tags)
  }
}
