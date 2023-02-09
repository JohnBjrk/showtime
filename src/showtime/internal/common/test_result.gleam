import gleam/dynamic.{Dynamic}
import gleam/map.{Map}

// These are all the types used for test-results
// NOTE: These are heavily used in the erlang/js ffi:s
// so any changes here are likely to break the ffi:s unless
// the corresponding change is introduced there
// 
// Futhermore this has some erlang related names that should
// probably be cleaned up, but it is used by both the ffi:s

// Currently only one reason, but could be possible to support
// more reasons in the future
pub type IgnoreReason {
  Ignore
}

// This is the return value from running the test-function
// or ignored if the test was ignored
pub type TestReturn {
  TestFunctionReturn(value: Dynamic)
  Ignored(reason: IgnoreReason)
}

// All data about an exception in the test function is captured
// in this type.
// This is also where the data about the assertions will end up (in reason)
pub type Exception {
  ErlangException(class: Class, reason: Reason, stacktrace: TraceList)
}

// Alias for the test-result which is either a TestResult (passed test, ignored or a test-definition)
// or an Exception (failed test)
pub type TestResult =
  Result(TestReturn, Exception)

// Reason is either an assert equal (which is if the error was produced by gleeunit should)
// TODO: Add other asserts
// or it is a gleam error meaning that is was produced by showtime should
// TODO: Rename GleamError to ShowtimeError
pub type Reason {
  AssertEqual(details: List(ReasonDetail))
  AssertNotEqual(details: List(ReasonDetail))
  AssertMatch(details: List(ReasonDetail))
  GleamError(details: GleamErrorDetail)
  GleamAssert(value: Dynamic, line_no: Int)
  GenericException(value: Dynamic)
}

// ReasonDetail is the union-type used in erlang-exceptions where the reason
// is a list of such details
pub type ReasonDetail {
  Module(name: String)
  ReasonLine(line_no: Int)
  Expression(expression: String)
  Expected(value: Dynamic)
  Value(value: Dynamic)
  Pattern(pattern: String)
}

// Gleam error detail is produced by showtime should and will hold all the information
// about the assertion (both expected and got)
pub type GleamErrorDetail {
  Assert(
    module: String,
    function: String,
    line_no: Int,
    message: String,
    value: Dynamic,
  )
}

// Class is a part of standard erlang exceptions, but also used on js-side
// TODO: Extend to include a JS specific constructor
pub type Class {
  ErlangError
  Exit
  Throw
}

// The trace list is part of the standard erlang exception, but is also
// emulated on js-side.
// TODO: Maybe we need a js-version that contain some js-specific trace-elements
pub type TraceList {
  TraceList(traces: List(Trace))
}

// Trace are the elements in the trace list in an erlang exception
// TODO: Maybe add a js-specific trace (since arity is not really a js attribute)
pub type Trace {
  Trace(function: String, arity: Arity, extra_info: List(ExtraInfo))
  TraceModule(
    module: String,
    function: String,
    arity: Arity,
    extra_info: List(ExtraInfo),
  )
}

// Extra info holds information about the file and line
// as well as some dynamic data in a map
// This is currently not used in the reporter
pub type ExtraInfo {
  ErrorInfo(error_info: Map(Dynamic, Dynamic))
  File(filename: String)
  Line(line_no: Int)
}

// Arity is the erlang type for arity
// Can be either a number, or a list of arguments
pub type Arity {
  Num(arity: Int)
  ArgList(arg_list: List(Dynamic))
}
