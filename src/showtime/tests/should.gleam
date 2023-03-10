import gleam/option.{None, Option, Some}
import showtime/tests/meta.{Meta}

pub type Assertion(t, e) {
  Eq(a: t, b: t, meta: Option(Meta))
  NotEq(a: t, b: t, meta: Option(Meta))
  IsOk(a: Result(t, e), meta: Option(Meta))
  IsError(a: Result(t, e), meta: Option(Meta))
  Fail(meta: Option(Meta))
}

pub fn equal(a: t, b: t) {
  evaluate(Eq(a, b, None))
}

pub fn equal_meta(a: t, b: t, meta: Meta) {
  evaluate(Eq(a, b, Some(meta)))
}

pub fn not_equal(a: t, b: t) {
  evaluate(NotEq(a, b, None))
}

pub fn not_equal_meta(a: t, b: t, meta: Meta) {
  evaluate(NotEq(a, b, Some(meta)))
}

pub fn be_ok(a: Result(o, e)) {
  evaluate(IsOk(a, None))
  a
}

pub fn be_ok_meta(a: Result(o, e), meta: Meta) {
  evaluate(IsOk(a, Some(meta)))
}

pub fn be_error(a: Result(o, e)) {
  evaluate(IsError(a, None))
  a
}

pub fn be_error_meta(a: Result(o, e), meta: Meta) {
  evaluate(IsError(a, Some(meta)))
}

pub fn fail() {
  evaluate(Fail(None))
}

pub fn fail_meta(meta: Meta) {
  evaluate(Fail(Some(meta)))
}

pub fn be_true(a: Bool) {
  a
  |> equal(True)
}

pub fn be_true_meta(a: Bool, meta: Meta) {
  a
  |> equal_meta(True, meta)
}

pub fn be_false(a: Bool) {
  a
  |> equal(False)
}

pub fn be_false_meta(a: Bool, meta: Meta) {
  a
  |> equal_meta(False, meta)
}

pub fn evaluate(assertion) -> Nil {
  case assertion {
    Eq(a, b, _meta) ->
      case a == b {
        True -> Ok(assertion)
        False -> {
          let assert Ok(_assertion) = Error(assertion)
        }
      }
    NotEq(a, b, _meta) ->
      case a != b {
        True -> Ok(assertion)
        False -> {
          let assert Ok(_assertion) = Error(assertion)
        }
      }
    IsOk(a, _meta) ->
      case a {
        Ok(_) -> Ok(assertion)
        Error(_) -> {
          let assert Ok(_assertion) = Error(assertion)
        }
      }
    IsError(a, _meta) ->
      case a {
        Error(_) -> Ok(assertion)
        Ok(_) -> {
          let assert Ok(_assertion) = Error(assertion)
        }
      }
    Fail(_meta) -> {
      let assert Ok(_assertion) = Error(assertion)
    }
  }
  Nil
}
