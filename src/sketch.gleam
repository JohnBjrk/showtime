import gleam/io
import gleam/option.{None, Option, Some}
import gleam/list
import gleam/otp/actor.{Continue, Stop}
import gleam/erlang/process.{Normal, Subject}
import gleam/map.{Map}
import gleam/set.{Set}

type TestModule {
  TestModule(name: String, path: Option(String))
}

type TestFunction {
  TestFunction(name: String)
}

type TestSuite {
  TestSuite(module: TestModule, tests: List(TestFunction))
}

type TestEvent {
  StartTestRun
  StartTestSuite(TestModule)
  StartTest(TestModule, TestFunction)
  EndTest(TestModule, test_function: TestFunction, result: TestResult)
  EndTestSuite(TestModule)
  EndTestRun(num_modules: Int)
}

type TestState {
  NotStarted
  Running
  Finished(num_modules: Int)
}

type TestModuleHandler =
  fn(TestModule) -> Nil

type TestEventHandler =
  fn(TestEvent) -> Nil

type ModuleCollector =
  fn(TestModuleHandler) -> List(TestModule)

type TestFunctionCollector =
  fn(TestModule) -> TestSuite

type TestRunner =
  fn(TestSuite, TestEventHandler) -> Nil

type TestOk {
  TestOk
}

type FailedTest {
  FailedTest(reason: String)
}

type TestResult =
  Result(TestOk, FailedTest)

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
      test_function_collector_impl,
      run_test_suite_impl,
    )
  test_event_handler(StartTestRun)
  let modules = module_collector_impl(test_module_handler)
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
        // msg
        // |> io.debug()
        let #(test_state, num_done, events) = state
        let next = case msg {
          StartTestRun -> Continue(#(Running, num_done, events))
          StartTestSuite(module) -> {
            let new_events =
              events
              |> map.insert(module.name, [])
            Continue(#(test_state, num_done, new_events))
          }
          StartTest(module, test) -> {
            assert Ok(module_events) =
              events
              |> map.get(module.name)
            let new_events =
              events
              |> map.insert(
                module.name,
                ["start " <> module.name <> " - " <> test.name, ..module_events],
              )
            Continue(#(test_state, num_done, new_events))
          }
          EndTest(module, test, _) -> {
            assert Ok(module_events) =
              events
              |> map.get(module.name)
            let new_events =
              events
              |> map.insert(
                module.name,
                ["end " <> module.name <> " - " <> test.name, ..module_events],
              )
            Continue(#(test_state, num_done, new_events))
          }
          EndTestSuite(module) -> Continue(#(test_state, num_done + 1, events))
          EndTestRun(num_modules) ->
            Continue(#(Finished(num_modules), num_done, events))
          _ -> Continue(state)
        }
        case next {
          Continue(#(Finished(num_modules), num_done, events)) if num_done == num_modules -> {
            io.println("Done")
            events
            |> io.debug()
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
        io.println("Waiting for exit")
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
            io.println("Got module event " <> module.name)
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
    io.println("Found module " <> test_module.name)
    process.send(subject, test_module)
    Nil
  }
}

// Target specific
fn module_collector_impl(test_module_handler: TestModuleHandler) {
  let modules = [TestModule("module_a", None), TestModule("module_b", None)]
  modules
  |> list.each(test_module_handler)
  modules
}

// Target specific
fn test_function_collector_impl(module: TestModule) {
  case module {
    TestModule("module_a", _) ->
      TestSuite(module, [TestFunction("test1"), TestFunction("test2")])
    TestModule("module_b", _) ->
      TestSuite(
        module,
        [TestFunction("test1"), TestFunction("test2"), TestFunction("test3")],
      )
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
    test_event_handler(EndTest(test_suite.module, test, Ok(TestOk)))
  })
}

fn test_module_handler_impl(test_module: TestModule) {
  todo
}

// Can be shared between targets but probably the erlang one will
// be wrapped in an actor
fn test_event_handler_impl(test_event: TestEvent) {
  todo
}
