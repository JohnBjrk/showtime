@target(erlang)
import gleam/io
@target(erlang)
import gleam/dynamic.{type Dynamic}
@target(erlang)
import gleam/list
@target(erlang)
import gleam/string
@target(erlang)
import gleam/int
@target(erlang)
import gleam/option.{type Option, None, Some}
@target(erlang)
import gleam/result
@target(erlang)
import simplifile
@target(erlang)
import gleam/erlang/atom.{type Atom}
@target(erlang)
import showtime/internal/common/test_suite.{
  type TestModule, type TestModuleHandler, type TestSuite, TestFunction,
  TestModule, TestSuite,
}

// Module collector for erlang
// Will search the test folder for files ending with _test and notify
// the module handler about each module it finds
@target(erlang)
pub fn collect_modules(
  test_module_handler: TestModuleHandler,
  only_modules: Option(List(String)),
) -> List(TestModule) {
  collect_modules_in_folder("./test", test_module_handler, only_modules)
}

@target(erlang)
fn collect_modules_in_folder(
  path: String,
  test_module_handler: TestModuleHandler,
  only_modules: Option(List(String)),
) {
  let module_prefix = get_module_prefix(path)
  let assert Ok(files) = simplifile.read_directory(path)
  let test_modules_in_folder =
    files
    |> list.filter(string.ends_with(_, "_test.gleam"))
    |> list.filter_map(fn(test_module_file) {
      let module_name =
        module_prefix
        <> {
          test_module_file
          |> string.replace(".gleam", "")
        }
      case only_modules {
        Some(only_modules_list) -> {
          let module_in_list =
            only_modules_list
            |> list.any(fn(only_module_name) {
              only_module_name
              == module_name
              |> string.replace("@", "/")
            })
          case module_in_list {
            True -> {
              let test_module = TestModule(module_name, Some(test_module_file))
              test_module_handler(test_module)
              Ok(test_module)
            }

            False -> Error(Nil)
          }
        }
        None -> {
          let test_module = TestModule(module_name, Some(test_module_file))
          test_module_handler(test_module)
          Ok(test_module)
        }
      }
    })
  let test_modules_in_subfolders =
    files
    |> list.map(fn(filename) { path <> "/" <> filename })
    |> list.filter(fn(file) {
      simplifile.verify_is_directory(file)
      |> result.unwrap(False)
    })
    |> list.fold([], fn(modules, subfolder) {
      modules
      |> list.append(collect_modules_in_folder(
        subfolder,
        test_module_handler,
        only_modules,
      ))
    })
  test_modules_in_folder
  |> list.append(test_modules_in_subfolders)
}

@target(erlang)
fn get_module_prefix(path) {
  let path_without_test =
    path
    |> string.replace("./test", "")

  let path_without_leading_slash = case
    string.starts_with(path_without_test, "/")
  {
    True -> string.drop_left(path_without_test, 1)
    False -> path_without_test
  }
  let module_prefix =
    path_without_leading_slash
    |> string.replace("/", "@")
  case string.length(module_prefix) {
    0 -> module_prefix
    _ -> module_prefix <> "@"
  }
}

// Test function collector for erlang
// Uses erlang `apply` to run `module_info` for the test module
// and collects all the exports ending with _test into a `TestSuite`
@target(erlang)
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
      let assert #(name, arity) = entry
      #(
        name
          |> atom.to_string(),
        arity,
      )
    })
    |> list.filter_map(fn(entry) {
      let assert #(name, arity) = entry
      case string.ends_with(name, "_test") {
        True ->
          case arity {
            0 -> Ok(name)
            _ -> {
              io.println(
                "WARNING: function \""
                <> name
                <> "\" has arity: "
                <> int.to_string(arity)
                <> " - cannot be used as test (needs to be 0)",
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

@target(erlang)
@external(erlang, "erlang", "apply")
fn apply(
  module module: Atom,
  function function: Atom,
  args args: List(Dynamic),
) -> Dynamic
