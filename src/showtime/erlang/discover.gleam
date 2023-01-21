import gleam/io
import gleam/dynamic.{Dynamic}
import gleam/list
import gleam/string
import gleam/int
import gleam/option.{Some}
import gleam/erlang/file
import gleam/erlang/atom.{Atom}
import test_suite.{TestFunction, TestModule, TestModuleHandler, TestSuite}

pub fn collect_modules(
  test_module_handler: TestModuleHandler,
) -> List(TestModule) {
  assert Ok(files) = file.list_directory("./test")
  files
  |> list.filter(string.ends_with(_, "_test.gleam"))
  |> list.map(fn(test_module_file) {
    let module_name =
      test_module_file
      |> string.replace(".gleam", "")
    let test_module = TestModule(module_name, Some(test_module_file))
    test_module_handler(test_module)
    test_module
  })
}

pub fn collect_test_functions(module: TestModule) -> TestSuite {
  let test_functions: List(#(Atom, Int)) =
    apply(
      atom.create_from_string(module.name),
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
    |> list.map(fn(function_name) { TestFunction(function_name) })
  TestSuite(module, test_functions_filtered)
}

external fn apply(module: Atom, function: Atom, args: List(Dynamic)) -> Dynamic =
  "erlang" "apply"
