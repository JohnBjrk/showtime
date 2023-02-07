import showtime/tests/should
import showtime/tests/meta.{Meta}

pub type Test {
  Test(meta: Meta, test_function: fn() -> Nil)
}

pub type MetaShould(t) {
  MetaShould(equal: fn(t, t) -> Nil, not_equal: fn(t, t) -> Nil)
}

pub fn test(meta: Meta, test_function: fn(Meta) -> Nil) {
  Test(meta, fn() { test_function(meta) })
}

pub fn with_meta(meta: Meta, test_function: fn(MetaShould(a)) -> Nil) {
  Test(
    meta,
    fn() {
      test_function(MetaShould(
        fn(a, b) { equal(a, b, meta) },
        fn(a, b) { not_equal(a, b, meta) },
      ))
    },
  )
}

pub fn equal(a: t, b: t, meta: Meta) {
  should.equal_meta(a, b, meta)
}

pub fn not_equal(a: t, b: t, meta: Meta) {
  should.equal_meta(a, b, meta)
}

pub fn be_ok(a: Result(o, e), meta: Meta) {
  should.be_ok_meta(a, meta)
}

pub fn be_error(a: Result(o, e), meta: Meta) {
  should.be_error_meta(a, meta)
}

pub fn fail(meta: Meta) {
  should.fail_meta(meta)
}

pub fn be_true(a: Bool, meta: Meta) {
  should.be_true_meta(a, meta)
}

pub fn be_false(a: Bool, meta: Meta) {
  should.be_false_meta(a, meta)
}
