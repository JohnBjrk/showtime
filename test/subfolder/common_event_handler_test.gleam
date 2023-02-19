import showtime/tests/test
import showtime/tests/meta.{Meta}
import showtime/internal/common/test_suite.{
  CompletedTestRun, EndTest, StartTest, StartTestRun, StartTestSuite, TestEvent,
  TestEventHandler, TestFunction, TestModule, TestSuite,
}
import showtime/internal/common/common_event_handler.{
  Finished, HandlerState, NotStarted, Running,
}
import showtime/internal/common/test_result.{TestFunctionReturn}
import gleam/io
import gleam/map
import gleam/option.{None}
import gleam/dynamic

pub fn start_handler_normal_test() {
  use should <- test.with_meta(Meta("StartTestRun -> Running", ["showtime"]))
  let handler_state = HandlerState(NotStarted, 0, map.new())
  let test_event = StartTestRun
  let updated_handler_state =
    common_event_handler.handle_event(test_event, handler_state)
  updated_handler_state
  |> should.equal(HandlerState(Running, 0, map.new()))
}

pub fn start_handler_twice_test() {
  use should <- test.with_meta(Meta("StartTestRun x 2-> Running", ["showtime"]))
  let handler_state = HandlerState(NotStarted, 0, map.new())
  let updated_handler_state =
    common_event_handler.handle_event(StartTestRun, handler_state)
    |> common_event_handler.handle_event(StartTestRun, _)
  updated_handler_state
  |> should.equal(HandlerState(Running, 0, map.new()))
}

pub fn start_finished_handler_test() {
  use should <- test.with_meta(Meta(
    "Finished -> StartTestRun -> Running",
    ["showtime"],
  ))
  let handler_state = HandlerState(Finished(1), 0, map.new())
  let updated_handler_state =
    common_event_handler.handle_event(StartTestRun, handler_state)
  updated_handler_state
  |> should.equal(HandlerState(Running, 0, map.new()))
}

pub fn end_not_started_test_test() {
  use should <- test.with_meta(Meta("StartTestRun -> EndTest", ["showtime"]))
  let handler_state = HandlerState(Running, 0, map.new())
  let updated_handler_state =
    common_event_handler.handle_event(
      EndTest(
        TestModule("foo", None),
        TestFunction("bar"),
        Ok(TestFunctionReturn(dynamic.from(Nil), [])),
      ),
      handler_state,
    )
  updated_handler_state
  |> should.equal(HandlerState(Running, 0, map.new()))
}

pub fn start_test_suite_normal_test() {
  use should <- test.with_meta(Meta(
    "StartTestRun -> StartTestSuite -> Running",
    ["showtime"],
  ))
  let handler_state = HandlerState(Running, 0, map.new())
  let updated_handler_state =
    common_event_handler.handle_event(
      StartTestSuite(TestModule("foo", None)),
      handler_state,
    )
  updated_handler_state
  |> should.equal(HandlerState(Running, 0, map.from_list([#("foo", map.new())])))
}

pub fn start_test_suite_twice_test() {
  use should <- test.with_meta(Meta(
    "StartTestRun -> StartTestSuite x 2 -> Running",
    ["showtime"],
  ))
  let handler_state = HandlerState(Running, 0, map.new())
  let updated_handler_state =
    common_event_handler.handle_event(
      StartTestSuite(TestModule("foo", None)),
      handler_state,
    )
    |> common_event_handler.handle_event(
      StartTestSuite(TestModule("foo", None)),
      _,
    )
  updated_handler_state
  |> should.equal(HandlerState(Running, 0, map.from_list([#("foo", map.new())])))
}

pub fn start_test_suite_test_run_normal_test() {
  // TODO: System time should be passed as a callback to handle_event to be able to mock time
  use should <- test.with_meta(Meta(
    "StartTestRun -> StartTestSuite -> StartTest -> EndTest -> Running",
    ["showtime"],
  ))
  let handler_state = HandlerState(Running, 0, map.new())
  let updated_handler_state =
    common_event_handler.handle_event(
      StartTestSuite(TestModule("foo", None)),
      handler_state,
    )
    |> common_event_handler.handle_event(
      StartTest(TestModule("foo", None), TestFunction("bar")),
      _,
    )
    |> common_event_handler.handle_event(
      EndTest(
        TestModule("foo", None),
        TestFunction("bar"),
        Ok(TestFunctionReturn(dynamic.from(Nil), [])),
      ),
      _,
    )
  updated_handler_state
  |> should.equal(HandlerState(
    Running,
    0,
    map.from_list([
      #(
        "foo",
        map.from_list([
          #(
            "bar",
            CompletedTestRun(
              TestFunction("bar"),
              0,
              Ok(TestFunctionReturn(dynamic.from(Nil), [])),
            ),
          ),
        ]),
      ),
    ]),
  ))
}
