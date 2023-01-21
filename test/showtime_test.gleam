import gleam/io
import gleeunit
import gleeunit/should as gshould
import showtime
import other_module.{call_me}
import should
import sketch

pub type TestType {
  TestType(arg1: String, arg2: List(String))
  Variant
}

pub fn main() {
  sketch.main()
  // run("showtime_test")
  // gleeunit.main()
}

pub fn i_am_test() {
  1
  |> gshould.equal(2)
}

pub fn many_gleeunit_should_test() {
  1
  |> gshould.equal(1)
  1
  |> gshould.equal(1)
  1
  |> gshould.equal(1)
  1
  |> gshould.equal(1)
  1
  |> gshould.equal(1)
  1
  |> gshould.equal(1)
  1
  |> gshould.equal(1)
  1
  |> gshould.equal(1)
  1
  |> gshould.equal(1)
  1
  |> gshould.equal(2)
}

pub fn many_showtime_should_test() {
  1
  |> should.equal(1)
  1
  |> should.equal(1)
  1
  |> should.equal(1)
  1
  |> should.equal(1)
  1
  |> should.equal(1)
  1
  |> should.equal(1)
  1
  |> should.equal(1)
  1
  |> should.equal(1)
  1
  |> should.equal(1)
  1
  |> should.equal(2)
}

pub fn other_test() {
  Variant
  |> should.equal(TestType("apa", ["älg", "skog"]))
}

pub fn list_test() {
  [1, 2, 3]
  |> should.equal([3, 4, 5])
}

pub fn djur_test() {
  TestType("apa", ["ren", "tur"])
  |> should.not_equal(TestType("apa", ["ren", "tur"]))
}
