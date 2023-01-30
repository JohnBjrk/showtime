if erlang {
  import gleam/io
  import gleam/option.{None, Some}
  import gleam/otp/actor.{Continue, Stop}
  import gleam/erlang.{Millisecond}
  import gleam/erlang/process.{Normal}
  import gleam/map
  import showtime/common/test_suite.{
    CompletedTestRun, EndTest, EndTestRun, EndTestSuite, OngoingTestRun,
    StartTest, StartTestRun, StartTestSuite, TestEvent,
  }
  import showtime/reports/formatter.{create_test_report}

  type TestState {
    NotStarted
    Running
    Finished(num_modules: Int)
  }

  pub fn start() {
    assert Ok(subject) =
      actor.start(
        #(NotStarted, 0, map.new()),
        fn(msg: TestEvent, state) {
          let #(test_state, num_done, events) = state
          let next = case msg {
            StartTestRun -> Continue(#(Running, num_done, events))
            StartTestSuite(module) -> {
              let new_events =
                events
                |> map.insert(module.name, map.new())
              Continue(#(test_state, num_done, new_events))
            }
            StartTest(module, test) -> {
              let current_time = erlang.system_time(Millisecond)
              let new_events =
                events
                |> map.update(
                  module.name,
                  fn(module_event) {
                    case module_event {
                      Some(module_event) ->
                        module_event
                        |> map.insert(
                          test.name,
                          OngoingTestRun(test, current_time),
                        )
                      None -> map.new()
                    }
                  },
                )
              Continue(#(test_state, num_done, new_events))
            }
            EndTest(module, test, result) -> {
              let current_time = erlang.system_time(Millisecond)
              let new_events =
                map.update(
                  events,
                  module.name,
                  fn(maybe_module_event) {
                    let module_event =
                      option.unwrap(maybe_module_event, map.new())
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
              Continue(#(test_state, num_done, new_events))
            }
            EndTestSuite(_) -> Continue(#(test_state, num_done + 1, events))
            EndTestRun(num_modules) ->
              Continue(#(Finished(num_modules), num_done, events))
            _ -> Continue(state)
          }
          case next {
            Continue(#(Finished(num_modules), num_done, events)) if num_done == num_modules -> {
              io.println(create_test_report(events))
              Stop(Normal)
            }
            _ -> next
          }
        },
      )
    let monitor =
      process.monitor_process(
        subject
        |> process.subject_owner(),
      )
    let exit_subject =
      process.new_selector()
      |> process.selecting_process_down(monitor, fn(_process_down) { Nil })

    fn(test_event: TestEvent) {
      case test_event {
        EndTestRun(..) -> {
          process.send(subject, test_event)
          process.select_forever(exit_subject)
        }

        _ -> process.send(subject, test_event)
      }
    }
  }
}
