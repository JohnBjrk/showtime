if erlang {
  import gleam/io
  import gleam/list
  import gleam/option.{None, Some}
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
    glint.new()
    |> glint.add_command(
      at: [],
      do: run,
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
    |> glint.run(start_arguments())
  }

  pub fn run(command: CommandInput) {
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
  import gleam/list
  import gleam/map
  import gleam/dynamic.{Dynamic}
  import showtime/common/test_suite.{
    CompletedTestRun, EndTest, EndTestRun, StartTestRun, StartTestSuite,
    TestEvent, TestModule, TestRun,
  }
  import showtime/common/common_event_handler.{
    Finished, HandlerState, NotStarted, handle_event,
  }
  import showtime/reports/formatter.{create_test_report}

  pub fn main() {
    io.println("Hello from JavaScript")
    test(event_handler, HandlerState(NotStarted, 0, map.new()))
  }

  fn event_handler(event: TestEvent, state: HandlerState) {
    // state
    // |> io.debug()
    // event
    // |> io.debug()

    // value
    // |> io.debug()
    let new_state = handle_event(event, state)
    case new_state {
      HandlerState(Finished(num_modules), num_done, events) ->
        io.println(create_test_report(events))
      // Nil
      _ -> Nil
    }
    new_state
    // let new_state = case event {
    //   StartTestRun -> {
    //     io.println("Start test-run")
    //     state
    //   }
    //   StartTestSuite(TestModule(name, path)) -> {
    //     io.println("Start Module: " <> name)
    //     state
    //   }
    //   EndTest(test_module, test_function, result) -> {
    //     io.println("End Test")
    //     result
    //     |> io.debug()
    //     [CompletedTestRun(test_function, 0, result), ..state]
    //   }
    //   EndTestRun(num_modules) -> {
    //     let result_map =
    //       state
    //       |> list.map(fn(test_run) { #(test_run.test_function.name, test_run) })
    //       |> map.from_list()
    //     let module_map = map.from_list([#("SUPERDUMMY MODULE", result_map)])
    //     io.println(create_test_report(module_map))
    //     state
    //   }
    //   _ -> {
    //     io.println("Unknown event")
    //     state
    //   }
    // }
    // new_state
    // |> io.debug()
  }

  external fn test(
    fn(TestEvent, HandlerState) -> HandlerState,
    HandlerState,
  ) -> Nil =
    "./showtime_ffi.mjs" "test"
}
