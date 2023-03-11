import gleam/map.{Map}
import showtime/internal/common/test_suite.{
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

// This is the common event-handler (shared between erlang/JS targets)
// The main strategy is to collect the test-results in a map of maps:
// module_name ->
//   test_name -> test_result
// It will also keep track of if it is running (i.e. did it receive the EndTestRun)
// so that the caller can determine when to print test-results
pub fn handle_event(
  msg: TestEvent,
  system_time: fn() -> Int,
  state: HandlerState,
) {
  let test_state = state.test_state
  let num_done = state.num_done
  let events = state.events
  let #(updated_test_state, updated_num_done, updated_events) = case msg {
    StartTestRun -> #(Running, num_done, events)
    StartTestSuite(module) -> {
      let maybe_module_events = map.get(events, module.name)
      let new_events = case maybe_module_events {
        Ok(_) -> events
        Error(_) ->
          events
          |> map.insert(module.name, map.new())
      }
      #(test_state, num_done, new_events)
    }
    StartTest(module, test) -> {
      let current_time = system_time()
      let maybe_module_events = map.get(events, module.name)
      let new_events = case maybe_module_events {
        Ok(module_events) -> {
          let maybe_test_event = map.get(module_events, test.name)
          case maybe_test_event {
            Error(_) ->
              events
              |> map.insert(
                module.name,
                module_events
                |> map.insert(test.name, OngoingTestRun(test, current_time)),
              )
            Ok(_) -> events
          }
        }
        Error(_) -> events
      }
      #(test_state, num_done, new_events)
    }
    EndTest(module, test, result) -> {
      let current_time = system_time()
      let maybe_module_events = map.get(events, module.name)
      let new_events = case maybe_module_events {
        Ok(module_events) -> {
          let maybe_test_run =
            module_events
            |> map.get(test.name)
          let updated_module_events = case maybe_test_run {
            Ok(OngoingTestRun(test_function, started_at)) ->
              module_events
              |> map.insert(
                test.name,
                CompletedTestRun(
                  test_function,
                  current_time - started_at,
                  result,
                ),
              )
            Error(_) -> module_events
          }
          events
          |> map.insert(module.name, updated_module_events)
        }
        Error(_) -> events
      }
      #(test_state, num_done, new_events)
    }
    EndTestSuite(_) -> #(test_state, num_done + 1, events)
    EndTestRun(num_modules) -> #(Finished(num_modules), num_done, events)
    _ -> #(Running, num_done, events)
  }
  HandlerState(updated_test_state, updated_num_done, updated_events)
}
