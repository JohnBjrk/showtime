import gleam/io
import gleam/list
import gleam/otp/actor
import gleam/erlang/process

pub fn run() {
  let event_handler = start_event_handler()
  let module_handler = start_module_handler(event_handler)

  ["test1", "test2", "last"]
  |> list.each(module_handler)
  process.sleep_forever()
}

fn start_event_handler() {
  assert Ok(subject) =
    actor.start(
      [],
      fn(msg, state) {
        case msg {
          "last" -> {
            [msg, ..state]
            |> io.debug()
            actor.Stop(process.Normal)
          }
          module -> actor.Continue([module, ..state])
        }
      },
    )
  fn(event) { process.send(subject, event) }
}

fn start_module_handler(event_handler) {
  assert Ok(subject) =
    actor.start(
      [],
      fn(msg, state) {
        case msg {
          test -> {
            process.start(
              fn() {
                io.println("Running test: " <> test)
                event_handler(test)
              },
              True,
            )
            actor.Continue(state)
          }
        }
      },
    )
  fn(module) { process.send(subject, module) }
}
