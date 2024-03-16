import showtime/tests/do_test
import showtime/tests/meta.{Meta}
import showtime/internal/common/test_suite.{
  CompletedTestRun, EndTest, EndTestSuite, OngoingTestRun, StartTest,
  StartTestRun, StartTestSuite, TestFunction, TestModule,
}
import showtime/internal/common/common_event_handler.{
  Finished, HandlerState, NotStarted, Running,
}
import showtime/internal/common/test_result.{TestFunctionReturn}
import gleam/dict
import gleam/option.{None}
import gleam/dynamic

const module_foo = TestModule("foo", None)

const function_bar = TestFunction("bar")

fn time(time) {
  fn() { time }
}

fn run_test_foo() {
  let handler_state = HandlerState(Running, 0, dict.new())
  common_event_handler.handle_event(
    StartTestSuite(module_foo),
    time(0),
    handler_state,
  )
  |> common_event_handler.handle_event(
    StartTest(module_foo, function_bar),
    time(0),
    _,
  )
  |> common_event_handler.handle_event(
    EndTest(
      module_foo,
      function_bar,
      Ok(TestFunctionReturn(dynamic.from(Nil), [])),
    ),
    time(100),
    _,
  )
}

pub fn start_handler_normal_test() {
  use should <- do_test.with_meta(Meta("StartTestRun -> Running", ["showtime"]))
  let handler_state = HandlerState(NotStarted, 0, dict.new())
  let test_event = StartTestRun
  let updated_handler_state =
    common_event_handler.handle_event(test_event, time(0), handler_state)
  updated_handler_state
  |> should.equal(HandlerState(Running, 0, dict.new()))
}

pub fn start_handler_twice_test() {
  use should <- do_test.with_meta(
    Meta("StartTestRun x 2-> Running", ["showtime"]),
  )
  let handler_state = HandlerState(NotStarted, 0, dict.new())
  let updated_handler_state =
    common_event_handler.handle_event(StartTestRun, time(0), handler_state)
    |> common_event_handler.handle_event(StartTestRun, time(0), _)
  updated_handler_state
  |> should.equal(HandlerState(Running, 0, dict.new()))
}

pub fn start_finished_handler_test() {
  use should <- do_test.with_meta(
    Meta("Finished -> StartTestRun -> Running", ["showtime"]),
  )
  let handler_state = HandlerState(Finished(1), 0, dict.new())
  let updated_handler_state =
    common_event_handler.handle_event(StartTestRun, time(0), handler_state)
  updated_handler_state
  |> should.equal(HandlerState(Running, 0, dict.new()))
}

pub fn end_not_started_test_test() {
  use should <- do_test.with_meta(Meta("StartTestRun -> EndTest", ["showtime"]))
  let handler_state = HandlerState(Running, 0, dict.new())
  let updated_handler_state =
    common_event_handler.handle_event(
      EndTest(
        module_foo,
        function_bar,
        Ok(TestFunctionReturn(dynamic.from(Nil), [])),
      ),
      time(0),
      handler_state,
    )
  updated_handler_state
  |> should.equal(HandlerState(Running, 0, dict.new()))
}

pub fn start_test_suite_normal_test() {
  use should <- do_test.with_meta(
    Meta("StartTestRun -> StartTestSuite -> Running", ["showtime"]),
  )
  let handler_state = HandlerState(Running, 0, dict.new())
  let updated_handler_state =
    common_event_handler.handle_event(
      StartTestSuite(module_foo),
      time(0),
      handler_state,
    )
  updated_handler_state
  |> should.equal(HandlerState(
    Running,
    0,
    dict.from_list([#("foo", dict.new())]),
  ))
}

pub fn start_test_suite_twice_test() {
  use should <- do_test.with_meta(
    Meta("StartTestRun -> StartTestSuite x 2 -> Running", ["showtime"]),
  )
  let handler_state = HandlerState(Running, 0, dict.new())
  let updated_handler_state =
    common_event_handler.handle_event(
      StartTestSuite(module_foo),
      time(0),
      handler_state,
    )
    |> common_event_handler.handle_event(StartTestSuite(module_foo), time(0), _)
  updated_handler_state
  |> should.equal(HandlerState(
    Running,
    0,
    dict.from_list([#("foo", dict.new())]),
  ))
}

pub fn start_test_suite_test_run_normal_test() {
  use should <- do_test.with_meta(
    Meta("StartTestRun -> StartTestSuite -> StartTest -> EndTest", ["showtime"]),
  )
  let handler_state = HandlerState(Running, 0, dict.new())
  let updated_handler_state =
    common_event_handler.handle_event(
      StartTestSuite(module_foo),
      time(0),
      handler_state,
    )
    |> common_event_handler.handle_event(
      StartTest(module_foo, function_bar),
      time(0),
      _,
    )
    |> common_event_handler.handle_event(
      EndTest(
        module_foo,
        function_bar,
        Ok(TestFunctionReturn(dynamic.from(Nil), [])),
      ),
      time(100),
      _,
    )
  updated_handler_state
  |> should.equal(HandlerState(
    Running,
    0,
    dict.from_list([
      #(
        "foo",
        dict.from_list([
          #(
            "bar",
            CompletedTestRun(
              function_bar,
              100,
              Ok(TestFunctionReturn(dynamic.from(Nil), [])),
            ),
          ),
        ]),
      ),
    ]),
  ))
}

pub fn run_test_after_ended_test() {
  use should <- do_test.with_meta(
    Meta(
      "StartTestRun -> StartTestSuite -> StartTest -> EndTest -> StartTest (again)",
      ["showtime"],
    ),
  )
  let updated_handler_state =
    run_test_foo()
    |> common_event_handler.handle_event(
      StartTest(module_foo, function_bar),
      time(120),
      _,
    )

  updated_handler_state
  |> should.equal(HandlerState(
    Running,
    0,
    dict.from_list([
      #(
        "foo",
        dict.from_list([
          #(
            "bar",
            CompletedTestRun(
              function_bar,
              100,
              Ok(TestFunctionReturn(dynamic.from(Nil), [])),
            ),
          ),
        ]),
      ),
    ]),
  ))
}

pub fn start_test_suite_after_ended_test() {
  use should <- do_test.with_meta(
    Meta(
      "StartTestRun -> StartTestSuite -> StartTest -> EndTest -> StartTestSuite (again)",
      ["showtime"],
    ),
  )
  let updated_handler_state =
    run_test_foo()
    |> common_event_handler.handle_event(
      StartTestSuite(module_foo),
      time(120),
      _,
    )

  updated_handler_state
  |> should.equal(HandlerState(
    Running,
    0,
    dict.from_list([
      #(
        "foo",
        dict.from_list([
          #(
            "bar",
            CompletedTestRun(
              function_bar,
              100,
              Ok(TestFunctionReturn(dynamic.from(Nil), [])),
            ),
          ),
        ]),
      ),
    ]),
  ))
}

pub fn end_test_suite_before_end_test_test() {
  use should <- do_test.with_meta(
    Meta("StartTestRun -> StartTestSuite -> StartTest -> EndTessSuite", [
      "showtime",
    ]),
  )
  let handler_state = HandlerState(Running, 0, dict.new())
  let updated_handler_state =
    common_event_handler.handle_event(
      StartTestSuite(module_foo),
      time(0),
      handler_state,
    )
    |> common_event_handler.handle_event(
      StartTest(module_foo, function_bar),
      time(0),
      _,
    )
    |> common_event_handler.handle_event(EndTestSuite(module_foo), time(100), _)
  updated_handler_state
  |> should.equal(HandlerState(
    Running,
    1,
    dict.from_list([
      #("foo", dict.from_list([#("bar", OngoingTestRun(function_bar, 0))])),
    ]),
  ))
}
