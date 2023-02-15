import gleam/option.{Option}
import showtime/internal/common/test_result.{TestResult}
import showtime/internal/common/cli.{Capture}

// The state (and result) of a test function
pub type TestRun {
  OngoingTestRun(test_function: TestFunction, started_at: Int)
  CompletedTestRun(
    test_function: TestFunction,
    total_time: Int,
    result: TestResult,
  )
}

// A test module (found by discovery)
pub type TestModule {
  TestModule(name: String, path: Option(String))
}

// A test function
pub type TestFunction {
  TestFunction(name: String)
}

// A test suite is a test module together with the test functions
// that were collected from that module
pub type TestSuite {
  TestSuite(module: TestModule, tests: List(TestFunction))
}

// Test event for the event handler
pub type TestEvent {
  StartTestRun
  StartTestSuite(test_module: TestModule)
  StartTest(test_module: TestModule, test_function: TestFunction)
  EndTest(
    test_module: TestModule,
    test_function: TestFunction,
    result: TestResult,
  )
  EndTestSuite(test_module: TestModule)
  EndTestRun(num_modules: Int)
}

// Interface for the module handler
pub type TestModuleHandler =
  fn(TestModule) -> Nil

// Interface for the event handler
pub type TestEventHandler =
  fn(TestEvent) -> Nil

// Interface for the module collector
pub type ModuleCollector =
  fn(TestModuleHandler) -> List(TestModule)

// Interface for the function collector
pub type TestFunctionCollector =
  fn(TestModule) -> TestSuite

// Interface for the test runner
pub type TestRunner =
  fn(TestSuite, TestEventHandler, List(String), Capture) -> Nil
