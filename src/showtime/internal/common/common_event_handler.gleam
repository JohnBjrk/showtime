import gleam/dict.{type Dict}
import showtime/internal/common/test_suite.{
  type TestEvent, type TestRun, CompletedTestRun, EndTest, EndTestRun,
  EndTestSuite, OngoingTestRun, StartTest, StartTestRun, StartTestSuite,
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
    events: Dict(String, Dict(String, TestRun)),
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
      let maybe_module_events = dict.get(events, module.name)
      let new_events = case maybe_module_events {
        Ok(_) -> events
        Error(_) ->
          events
          |> dict.insert(module.name, dict.new())
      }
      #(test_state, num_done, new_events)
    }
    StartTest(module, test_case) -> {
      let current_time = system_time()
      let maybe_module_events = dict.get(events, module.name)
      let new_events = case maybe_module_events {
        Ok(module_events) -> {
          let maybe_test_event = dict.get(module_events, test_case.name)
          case maybe_test_event {
            Error(_) ->
              events
              |> dict.insert(
                module.name,
                module_events
                  |> dict.insert(
                    test_case.name,
                    OngoingTestRun(test_case, current_time),
                  ),
              )
            Ok(_) -> events
          }
        }
        Error(_) -> events
      }
      #(test_state, num_done, new_events)
    }
    EndTest(module, test_case, result) -> {
      let current_time = system_time()
      let maybe_module_events = dict.get(events, module.name)
      let new_events = case maybe_module_events {
        Ok(module_events) -> {
          let maybe_test_run =
            module_events
            |> dict.get(test_case.name)
          let updated_module_events = case maybe_test_run {
            Ok(OngoingTestRun(test_function, started_at)) ->
              module_events
              |> dict.insert(
                test_case.name,
                CompletedTestRun(
                  test_function,
                  current_time - started_at,
                  result,
                ),
              )
            Ok(CompletedTestRun(_, _, _)) ->
              panic as "Should not receive end test event for completed test run"
            Error(_) -> module_events
          }
          events
          |> dict.insert(module.name, updated_module_events)
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
