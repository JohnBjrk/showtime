import gleam/io
import gleam/function

pub type Assertion(t) {
  Unreachable
  Eq(a: t, b: t)
}

pub type AssertionResult(t) =
  Result(Assertion(t), Assertion(t))

pub fn eq(should, a: t, b: t) {
  should(Eq(a, b))
}

pub fn should() {
  fn(assertion: Assertion(t)) { evaluate(assertion) }
}

pub fn run() {
  should()
  |> eq(3, 4)
  should()
  |> eq(4, 4)
  should()
  |> eq(["apa", "bepa"], ["apa", "depa"])
}

pub fn evaluate(assertion) -> AssertionResult(t) {
  case assertion {
    Eq(a, b) ->
      case a == b {
        True -> {
          assert Error(Unreachable) = Ok(assertion)
        }
        False -> {
          assert Ok(Unreachable) = Error(assertion)
        }
      }
    Unreachable -> {
      assert Ok(Unreachable) = Error(Unreachable)
    }
  }
}
// pub fn evaluate2(arg1, arg2) {
//   #(evaluate(arg1), evaluate(arg2))
// }

// pub fn evaluate3(arg1, arg2, arg3) {
//   #(arg1(), arg2(), arg3())
// }
