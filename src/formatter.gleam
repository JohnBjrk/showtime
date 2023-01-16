import gleam/int
import gleam/list
import gleam/map.{Map}
import gleam/dynamic.{Dynamic}
import test_result.{Exception}
import test_suite.{CompletedTestRun, TestRun}

pub fn create_test_report(test_results: Map(String, Map(String, TestRun))) {
  let total_num_tests =
    test_results
    |> map.values()
    |> list.fold(
      0,
      fn(total, test_module_results) { total + map.size(test_module_results) },
    )
  let failed_tests =
    test_results
    |> map.values()
    |> list.flat_map(fn(test_module_results) {
      test_module_results
      |> map.values()
      |> list.filter_map(fn(test_run) {
        case test_run {
          CompletedTestRun(_, _, result) ->
            case result {
              Ok(_) -> Ok(test_run)
              Error(_) -> Error(Nil)
            }
          _ -> Error(Nil)
        }
      })
    })

  int.to_string(
    failed_tests
    |> list.length(),
  ) <> "/" <> int.to_string(total_num_tests) <> " passed"
}
