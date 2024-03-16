@target(erlang)
import gleam/otp/actor
@target(erlang)
import gleam/erlang/process
@target(erlang)
import showtime/internal/common/test_suite.{
  type TestEventHandler, type TestFunctionCollector, type TestModule,
  type TestRunner, EndTestSuite, StartTestSuite,
}
@target(erlang)
import showtime/internal/common/cli.{type Capture}

@target(erlang)
pub fn start(
  test_event_handler: TestEventHandler,
  test_function_collector: TestFunctionCollector,
  run_test_suite: TestRunner,
  ignore_tags: List(String),
  capture: Capture,
) {
  let assert Ok(subject) =
    actor.start(Nil, fn(module: TestModule, state) {
      process.start(
        fn() {
          let test_suite = test_function_collector(module)
          test_event_handler(StartTestSuite(module))
          run_test_suite(test_suite, test_event_handler, ignore_tags, capture)
          test_event_handler(EndTestSuite(module))
        },
        False,
      )
      actor.continue(state)
    })
  fn(test_module: TestModule) {
    process.send(subject, test_module)
    Nil
  }
}
