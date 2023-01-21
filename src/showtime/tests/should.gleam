pub type Assertion(t) {
  Eq(a: t, b: t)
  NotEq(a: t, b: t)
}

pub fn equal(a: t, b: t) {
  evaluate(Eq(a, b))
}

pub fn not_equal(a: t, b: t) {
  evaluate(NotEq(a, b))
}

pub fn evaluate(assertion) -> Nil {
  case assertion {
    Eq(a, b) ->
      case a == b {
        True -> Ok(assertion)
        False -> {
          assert Ok(_assertion) = Error(assertion)
        }
      }
    NotEq(a, b) ->
      case a != b {
        True -> Ok(assertion)
        False -> {
          assert Ok(_assertion) = Error(assertion)
        }
      }
  }
  Nil
}
