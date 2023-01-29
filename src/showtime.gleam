import gleam/io
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
