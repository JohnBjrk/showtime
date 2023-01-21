import gleam/io
import gleam/list
import gleam/function
import gleam/map.{Map}
import gleam/string
import gleam/int
import gleam/dynamic.{Dynamic}
import gleam/erlang/atom.{Atom}
import gleam/erlang/file
import gleam/otp/task.{async, await}

pub type TestFunction {
  TestFunction(module_name: String, function_name: String)
}

pub type ModuleTests {
  ModuleTests(module_name: String, test_functions: List(TestFunction))
}

pub type TestEvent(a) {
  ModuleTestStarted(module_name: String)
  ModuleTestEnded(module_name: String)
  TestStarted(test_name: String)
  TestEnded(test_name: String, test_function_result: TestFunctionResult(a))
}

pub type TestEventHandler(a) =
  fn(TestEvent(a)) -> Nil

pub fn main() {
  // assertions.run()
  // sketch.main()
  assert Ok(files) = file.list_directory("./test")
  files
  |> list.filter(string.ends_with(_, "_test.gleam"))
  |> list.map(string.replace(_, each: ".gleam", with: ""))
  |> test_modules(dummy_test_event_handler)
}

pub fn dummy_test_event_handler(test_event: TestEvent(a)) {
  case test_event {
    ModuleTestStarted(module_name) ->
      io.println("Module test started: " <> module_name)
    ModuleTestEnded(module_name) ->
      io.println("Module test ended: " <> module_name)
    TestStarted(test_name) -> io.println("Test started: " <> test_name)
    TestEnded(test_name, _) -> io.println("Test ended: " <> test_name)
  }
}

pub fn test_modules(
  modules: List(String),
  test_event_handler: TestEventHandler(a),
) {
  let module_test_results =
    modules
    |> list.map(discover_test_functions)
    |> list.map(fn(module_tests) {
      async(fn() { run_tests(module_tests, test_event_handler) })
    })
    |> list.map(await(_, 1000))
  module_test_results
  |> list.flat_map(fn(result_list) {
    result_list
    |> list.map(fn(result) {
      case result {
        Normal(_) -> "Ok"
        ErlangException(class, reason, stacktrace) ->
          string.inspect(class) <> ": " <> string.inspect(reason) <> "\n" <> string.inspect(
            stacktrace,
          ) <> "\n"
      }
    })
  })
  |> io.debug()

  let test_results =
    module_test_results
    |> list.flatten()

  let num_tests =
    test_results
    |> list.length()
  let failed =
    test_results
    |> list.filter_map(fn(test_function_result) {
      case test_function_result {
        Normal(return_value) -> Error(return_value)
        ErlangException(..) -> Ok(test_function_result)
      }
    })
  let num_failed =
    failed
    |> list.length()

  io.println(int.to_string(num_tests) <> " test(s) run")
  io.println("Successful: " <> int.to_string(num_tests - num_failed))
  io.println("Failed: " <> int.to_string(num_failed))
}

pub fn discover_test_functions(module_name: String) -> ModuleTests {
  let test_functions: List(#(Atom, Int)) =
    apply(
      atom.create_from_string(module_name),
      atom.create_from_string("module_info"),
      [dynamic.from(atom.create_from_string("exports"))],
    )
    |> dynamic.unsafe_coerce()

  let test_functions_filtered =
    test_functions
    |> list.map(fn(entry) {
      assert #(name, arity) = entry
      #(
        name
        |> atom.to_string(),
        arity,
      )
    })
    |> list.filter_map(fn(entry) {
      assert #(name, arity) = entry
      case string.ends_with(name, "_test") {
        True ->
          case arity {
            0 -> Ok(name)
            _ -> {
              io.println(
                "WARNING: function \"" <> name <> "\" has arity: " <> int.to_string(
                  arity,
                ) <> " - cannot be used as test (needs to be 0)",
              )
              Error("Wrong arity")
            }
          }
        False -> Error("Non matching name")
      }
    })
    |> list.filter(string.ends_with(_, "_test"))
    |> list.map(fn(function_name) { TestFunction(module_name, function_name) })
  ModuleTests(module_name, test_functions_filtered)
}

pub fn run_tests(
  module_tests: ModuleTests,
  test_event_handler: TestEventHandler(a),
) -> List(TestFunctionResult(a)) {
  test_event_handler(ModuleTestStarted(module_tests.module_name))
  let test_results =
    module_tests.test_functions
    |> list.map(function.tap(_, fn(fun: TestFunction) {
      test_event_handler(TestStarted(fun.function_name))
    }))
    |> list.map(fn(fun) {
      let test_result =
        run_test(
          atom.create_from_string(fun.module_name),
          atom.create_from_string(fun.function_name),
          [],
        )
      test_event_handler(TestEnded(fun.function_name, test_result))
      test_result
    })
  test_event_handler(ModuleTestEnded(module_tests.module_name))
  test_results
}

pub type TestFunctionResult(a) {
  Normal(result: Dynamic)
  ErlangException(class: Class, reason: Reason(a), stacktrace: TraceList)
}

pub type Reason(a) {
  AssertEqual(List(ReasonDetail(a)))
  GleamError(details: GleamErrorDetail)
}

pub type ReasonDetail(a) {
  Module(name: Atom)
  ReasonLine(line_no: Int)
  Expression(List(String))
  Expected(value: a)
  Value(value: a)
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

pub external fn run_test(
  module: Atom,
  function: Atom,
  args: List(Dynamic),
) -> TestFunctionResult(a) =
  "showtime_ffi" "run_test"

external fn apply(module: Atom, function: Atom, args: List(Dynamic)) -> Dynamic =
  "erlang" "apply"
