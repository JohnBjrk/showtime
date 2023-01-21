import gleam/dynamic.{Dynamic}
import gleam/map.{Map}

pub type Exception {
  ErlangException(class: Class, reason: Reason, stacktrace: TraceList)
}

pub type TestResult =
  Result(Dynamic, Exception)

pub type Reason {
  AssertEqual(details: List(ReasonDetail))
  // AssertEqual(details: Dynamic)
  GleamError(details: GleamErrorDetail)
}

pub type ReasonDetail {
  Module(name: String)
  ReasonLine(line_no: Int)
  Expression(expression: String)
  Expected(value: Dynamic)
  Value(value: Dynamic)
}

pub type GleamErrorDetail {
  Assert(
    module: String,
    function: String,
    line_no: Int,
    message: String,
    value: Dynamic,
  )
}

pub type Class {
  ErlangError
  Exit
  Throw
}

pub type TraceList {
  TraceList(traces: List(Trace))
}

pub type Trace {
  Trace(function: String, arity: Arity, extra_info: List(ExtraInfo))
  TraceModule(
    module: String,
    function: String,
    arity: Arity,
    extra_info: List(ExtraInfo),
  )
}

pub type ExtraInfo {
  ErrorInfo(error_info: Map(Dynamic, Dynamic))
  File(filename: String)
  Line(line_no: Int)
}

pub type Arity {
  Num(arity: Int)
  ArgList(arg_list: List(Dynamic))
}
