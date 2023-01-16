import gleam/io
import gleeunit
import gleeunit/should
import showtime
import other_module.{call_me}
import assertions
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
  io.println("Testing")
  1
  |> should.equal(call_me("Sal"))
}

pub fn other_test() {
  io.println("Other test")
  // assert TestType("nope", []) = TestType("apa", ["bepa", "depa"])
  // assertions.should()
  // |> assertions.eq(["apa"], ["apa", "bepa"])
  assertions.should()
  |> assertions.eq(Variant, TestType("apa", ["älg", "skog"]))
}
