@target(erlang)
import gleam/io
@target(erlang)
import gleam/otp/actor.{Stop}
@target(erlang)
import gleam/erlang/process.{type Subject, Normal}
@target(erlang)
import gleam/dict
@target(erlang)
import showtime/internal/common/test_suite.{type TestEvent, EndTestRun}
@target(erlang)
import showtime/internal/common/common_event_handler.{
  Finished, HandlerState, NotStarted, handle_event,
}
@target(erlang)
import showtime/internal/reports/formatter.{create_test_report}
@target(erlang)
import gleam/erlang.{Millisecond}

@target(erlang)
type EventHandlerMessage {
  EventHandlerMessage(test_event: TestEvent, reply_to: Subject(Int))
}

// Starts an actor that receives test events and forwards the to the event handler
// When handler updates the state to `Finished` the actor will wait until handler
// reports that all modules are done and the stop
@target(erlang)
pub fn start() {
  let assert Ok(subject) =
    actor.start(
      #(NotStarted, 0, dict.new()),
      fn(msg: EventHandlerMessage, state) {
        let EventHandlerMessage(test_event, reply_to) = msg
        let #(test_state, num_done, events) = state
        let updated_state =
          handle_event(
            test_event,
            system_time,
            HandlerState(test_state, num_done, events),
          )
        case updated_state {
          HandlerState(Finished(num_modules), num_done, events)
            if num_done == num_modules
          -> {
            let #(test_report, num_failed) = create_test_report(events)
            io.println(test_report)
            process.send(reply_to, num_failed)
            Stop(Normal)
          }
          HandlerState(test_state, num_done, events) ->
            actor.continue(#(test_state, num_done, events))
        }
      },
    )

  let parent_subject = process.new_subject()

  let selector =
    process.new_selector()
    |> process.selecting(parent_subject, fn(x) { x })

  // Returns a callback that can receive test events
  fn(test_event: TestEvent) {
    case test_event {
      EndTestRun(..) -> {
        // When EndTestRun has been received the callback will wait until the
        // actor has stopped
        // TODO: Use a timeout?
        process.send(subject, EventHandlerMessage(test_event, parent_subject))
        let num_failed = process.select_forever(selector)
        case num_failed > 0 {
          True -> halt(1)
          False -> halt(0)
        }
      }

      // Normally just send the test event to the actor
      _ ->
        process.send(subject, EventHandlerMessage(test_event, parent_subject))
    }
  }
}

@target(erlang)
@external(erlang, "erlang", "halt")
fn halt(a: Int) -> Nil

@target(erlang)
fn system_time() {
  erlang.system_time(Millisecond)
}
