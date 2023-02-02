import gleam/map.{Map}
import gleam/option.{None, Some}
import showtime/common/test_suite.{
  CompletedTestRun, EndTest, EndTestRun, EndTestSuite, OngoingTestRun, StartTest,
  StartTestRun, StartTestSuite, TestEvent, TestRun,
}

pub type TestState {
  NotStarted
  Running
  Finished(num_modules: Int)
}

pub type HandlerState {
  HandlerState(
    test_state: TestState,
    num_done: Int,
    events: Map(String, Map(String, TestRun)),
  )
}

if erlang {
  import gleam/erlang.{Millisecond}

  fn system_time() {
    erlang.system_time(Millisecond)
  }
}

if javascript {
  external fn system_time() -> Int =
    "../../showtime_ffi.mjs" "system_time"
}

pub fn handle_event(msg: TestEvent, state: HandlerState) {
  let test_state = state.test_state
  let num_done = state.num_done
  let events = state.events
  let #(updated_test_state, updated_num_done, updated_events) = case msg {
    StartTestRun -> #(Running, num_done, events)
    StartTestSuite(module) -> {
      let new_events =
        events
        |> map.insert(module.name, map.new())
      #(test_state, num_done, new_events)
    }
    StartTest(module, test) -> {
      let current_time = system_time()
      let new_events =
        events
        |> map.update(
          module.name,
          fn(module_event) {
            case module_event {
              Some(module_event) ->
                module_event
                |> map.insert(test.name, OngoingTestRun(test, current_time))
              None -> map.new()
            }
          },
        )
      #(test_state, num_done, new_events)
    }
    EndTest(module, test, result) -> {
      let current_time = system_time()
      let new_events =
        map.update(
          events,
          module.name,
          fn(maybe_module_event) {
            let module_event = option.unwrap(maybe_module_event, map.new())
            let maybe_test_run =
              module_event
              |> map.get(test.name)
            case maybe_test_run {
              Ok(OngoingTestRun(test_function, started_at)) ->
                module_event
                |> map.insert(
                  test.name,
                  CompletedTestRun(
                    test_function,
                    current_time - started_at,
                    result,
                  ),
                )
              Error(_) -> module_event
            }
          },
        )
      #(test_state, num_done, new_events)
    }
    EndTestSuite(_) -> #(test_state, num_done + 1, events)
    EndTestRun(num_modules) -> #(Finished(num_modules), num_done, events)
    _ -> #(Running, num_done, events)
  }
  HandlerState(updated_test_state, updated_num_done, updated_events)
}
