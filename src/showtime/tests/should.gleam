import gleam/io
import gleam/option.{None, Option, Some}
import showtime/tests/meta.{Meta}

pub type Assertion(t) {
  Eq(a: t, b: t, Option(Meta))
  NotEq(a: t, b: t)
}

pub fn equal(a: t, b: t) {
  evaluate(Eq(a, b, None))
}

pub fn equal_meta(a: t, b: t, meta: Meta) {
  evaluate(Eq(a, b, Some(meta)))
}

pub fn not_equal(a: t, b: t) {
  evaluate(NotEq(a, b))
}

pub fn evaluate(assertion) -> Nil {
  case assertion {
    Eq(a, b, _meta) ->
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
