if erlang {
  import gleam/io
  import gleam/otp/actor.{Continue, Stop}
  import gleam/erlang/process.{Normal}
  import gleam/map
  import showtime/internal/common/test_suite.{EndTestRun, TestEvent}
  import showtime/internal/common/common_event_handler.{
    Finished, HandlerState, NotStarted, handle_event,
  }
  import showtime/internal/reports/formatter.{create_test_report}

  // Starts an actor that receives test events and forwards the to the event handler
  // When handler updates the state to `Finished` the actor will wait until handler
  // reports that all modules are done and the stop
  pub fn start() {
    assert Ok(subject) =
      actor.start(
        #(NotStarted, 0, map.new()),
        fn(msg: TestEvent, state) {
          let #(test_state, num_done, events) = state
          let updated_state =
            handle_event(msg, HandlerState(test_state, num_done, events))
          case updated_state {
            HandlerState(Finished(num_modules), num_done, events) if num_done == num_modules -> {
              io.println(create_test_report(events))
              Stop(Normal)
            }
            HandlerState(test_state, num_done, events) ->
              Continue(#(test_state, num_done, events))
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

    // Returns a callback that can receive test events
    fn(test_event: TestEvent) {
      case test_event {
        EndTestRun(..) -> {
          // When EndTestRun has been received the callback will wait until the
          // actor has stopped
          // TODO: Use a timeout?
          process.send(subject, test_event)
          process.select_forever(exit_subject)
        }

        // Normally just send the test event to the actor
        _ -> process.send(subject, test_event)
      }
    }
  }
}
