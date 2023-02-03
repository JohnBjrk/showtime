import showtime/tests/should
import showtime/tests/meta.{Meta}

pub type Test {
  Test(meta: Meta, test_function: fn() -> Nil)
}

pub fn test(meta: Meta, test_function: fn(Meta) -> Nil) {
  Test(meta, fn() { test_function(meta) })
}

pub fn equal(a: t, b: t, meta: Meta) {
  should.equal_meta(a, b, meta)
}

pub fn not_equal(a: t, b: t, meta: Meta) {
  should.equal_meta(a, b, meta)
}
