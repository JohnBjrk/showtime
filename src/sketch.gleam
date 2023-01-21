import gleam/io
import gleam/option.{None, Option, Some}
import gleam/list
import gleam/otp/actor.{Continue, Stop}
import gleam/erlang.{Millisecond}
import gleam/erlang/file
import gleam/erlang/process.{Normal, Subject}
import gleam/map.{Map}
import gleam/set.{Set}
import gleam/string
import test_suite.{
  CompletedTestRun, EndTest, EndTestRun, EndTestSuite, OngoingTestRun, StartTest,
  StartTestRun, StartTestSuite, TestEvent, TestEventHandler, TestFunction,
  TestFunctionCollector, TestModule, TestRunner, TestSuite,
}
import discovery_erlang.{collect_modules, collect_test_functions, run_test}
import formatter.{create_test_report}

type TestState {
  NotStarted
  Running
  Finished(num_modules: Int)
}

// if javascript {
//   fn do_main(
//     collect_modules: ModuleCollector,
//     collect_test_functions: TestFunctionCollector,
//     run_test_suite: TestRunner,
//     test_module_handler: TestModuleHandler,
//     test_event_handler: TestEventHandler,
//   ) {
//     collect_modules(test_event_handler)
//     |> list.map(collect_test_functions(_, test_event_handler))
//     |> list.each(run_test_suite(_, test_event_handler))
//   }
// }

pub fn main() {
  let test_event_handler = start_test_event_handler()
  let test_module_handler =
    start_test_module_handler(
      test_event_handler,
      collect_test_functions,
      run_test_suite_impl,
    )
  test_event_handler(StartTestRun)
  let modules = collect_modules(test_module_handler)
  test_event_handler(EndTestRun(
    modules
    |> list.length(),
  ))
  Nil
}

fn start_test_event_handler() {
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

fn start_test_module_handler(
  test_event_handler: TestEventHandler,
  test_function_collector: TestFunctionCollector,
  run_test_suite: TestRunner,
) {
  assert Ok(subject) =
    actor.start(
      Nil,
      fn(module: TestModule, state) {
        process.start(
          fn() {
            let test_suite = test_function_collector(module)
            test_event_handler(StartTestSuite(module))
            run_test_suite(test_suite, test_event_handler)
            test_event_handler(EndTestSuite(module))
          },
          True,
        )
        Continue(state)
      },
    )
  fn(test_module: TestModule) {
    process.send(subject, test_module)
    Nil
  }
}

// Target specific
fn run_test_suite_impl(
  test_suite: TestSuite,
  test_event_handler: TestEventHandler,
) {
  test_suite.tests
  |> list.each(fn(test) {
    test_event_handler(StartTest(test_suite.module, test))
    let result = run_test(test_suite.module.name, test.name)
    test_event_handler(EndTest(test_suite.module, test, result))
  })
}
