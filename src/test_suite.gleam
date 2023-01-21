import gleam/option.{Option}
import test_result.{TestResult}

pub type TestRun {
  OngoingTestRun(test_function: TestFunction, started_at: Int)
  CompletedTestRun(
    test_function: TestFunction,
    total_time: Int,
    result: TestResult,
  )
}

pub type TestModule {
  TestModule(name: String, path: Option(String))
}

pub type TestFunction {
  TestFunction(name: String)
}

pub type TestSuite {
  TestSuite(module: TestModule, tests: List(TestFunction))
}

pub type TestEvent {
  StartTestRun
  StartTestSuite(TestModule)
  StartTest(TestModule, TestFunction)
  EndTest(TestModule, test_function: TestFunction, result: TestResult)
  EndTestSuite(TestModule)
  EndTestRun(num_modules: Int)
}

pub type TestModuleHandler =
  fn(TestModule) -> Nil

pub type TestEventHandler =
  fn(TestEvent) -> Nil

pub type ModuleCollector =
  fn(TestModuleHandler) -> List(TestModule)

pub type TestFunctionCollector =
  fn(TestModule) -> TestSuite

pub type TestRunner =
  fn(TestSuite, TestEventHandler) -> Nil

pub type TestOk {
  TestOk
}

pub type FailedTest {
  FailedTest(reason: String)
}
