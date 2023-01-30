if erlang {
  import gleam/otp/actor.{Continue}
  import gleam/erlang/process
  import showtime/common/test_suite.{
    EndTestSuite, StartTestSuite, TestEventHandler, TestFunctionCollector,
    TestModule, TestRunner,
  }

  pub fn start(
    test_event_handler: TestEventHandler,
    test_function_collector: TestFunctionCollector,
    run_test_suite: TestRunner,
    ignore_tags: List(String),
  ) {
    assert Ok(subject) =
      actor.start(
        Nil,
        fn(module: TestModule, state) {
          process.start(
            fn() {
              let test_suite = test_function_collector(module)
              test_event_handler(StartTestSuite(module))
              run_test_suite(test_suite, test_event_handler, ignore_tags)
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
}
