import gleam/io
import gleam/option.{None}
import gleam/list
import gleeunit
import gleeunit/should as gshould
import showtime
import showtime/tests/should
import showtime/tests/test.{test}
import showtime/tests/meta.{Meta}
import showtime/internal/reports/table.{
  AlignLeft, AlignRight, Content, Separator, StyledContent, Table,
}
import gleam_community/ansi.{cyan, green, red}

pub type TestType {
  TestType(arg1: String, arg2: List(String))
  Variant
}

pub fn main() {
  showtime.main()
  // gleeunit.main()
}

pub fn i_am_test() {
  io.println("Some test output")
  io.println(
    "And the some really really really really really really really really really really really really really really really really really really really really long output",
  )
  Variant
  |> gshould.equal(TestType("name", ["some", "name"]))
  TestType("name", ["other", "names"])
  |> gshould.equal(TestType("name", ["some", "name"]))
}

pub fn is_ok_test() {
  Error(TestType("error", ["caused", "by"]))
  |> should.be_ok()
}

pub fn is_ok_meta_test() {
  test(
    Meta("Test failing be_ok assertion with meta-data", ["meta"]),
    fn(meta) {
      Error(TestType("error", ["caused", "by"]))
      |> test.be_ok(meta)
    },
  )
}

pub fn is_error_test() {
  io.println("This test")
  io.println("has som lines of output")
  io.println("Which is also followed by an io.debug()")
  TestType("Im the infamous TestType", ["we", "are", "on", "the", "list"])
  |> io.debug()
  Ok(TestType("ok", ["result"]))
  |> should.be_error()
}

pub fn is_error_meta_test() {
  test(
    Meta("Test failing be_error assertion with meta-data", ["meta"]),
    fn(meta) {
      io.println("This test")
      io.println("has som lines of output")
      io.println("Which is also followed by an io.debug()")
      TestType("Im the infamous TestType", ["we", "are", "on", "the", "list"])
      |> io.debug()
      Ok(TestType("ok", ["result"]))
      |> test.be_error(meta)
    },
  )
}

pub fn fail_test() {
  should.fail()
}

pub fn fail_meta_test() {
  test(Meta("Test fail with meta-data", ["meta"]), fn(meta) { test.fail(meta) })
}

pub fn is_true_test() {
  False
  |> should.be_true()
}

pub fn is_true_meta_test() {
  test(
    Meta("Test is true with meta", ["meta"]),
    fn(meta) {
      False
      |> test.be_true(meta)
    },
  )
}

pub fn is_false_test() {
  True
  |> should.be_false()
}

pub fn is_false_meta_test() {
  test(
    Meta("Test is false with meta", ["meta"]),
    fn(meta) {
      True
      |> test.be_false(meta)
    },
  )
}

pub fn gleeunit_assert_not_equal_test() {
  1
  |> gshould.not_equal(1)
}

pub fn gleeunit_should_be_ok_test() {
  Error("Wrong")
  |> gshould.be_ok()
}

pub fn gleeunit_should_be_error_test() {
  Ok(TestType("Done", ["good", "result"]))
  |> gshould.be_error()
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

pub fn not_equal_meta_test() {
  test(
    Meta("Test if meta works for not_equal", []),
    fn(meta) {
      TestType("apa", ["ren", "tur"])
      |> test.not_equal(TestType("apa", ["ren", "flax"]), meta)
    },
  )
}

pub fn meta_test() {
  test(
    Meta("This is a test description", ["ignore"]),
    fn(meta) {
      ["apa", "bepa"]
      |> test.equal(["bepa", "depa"], meta)
      2
      |> test.equal(3, meta)
    },
  )
}

pub fn assert_test() {
  let assert Ok("apa") = Error("bepa")
}

pub fn use_meta_test() {
  use meta <- test(Meta("This test is defined using use", ["meta"]))
  "meta"
  |> test.equal("universe", meta)
}

pub fn with_meta_test() {
  use should <- test.with_meta(Meta("This test is defined using use", ["meta"]))
  "meta"
  |> should.equal("universe")
}

fn add(a, b) {
  a + b
}

pub fn multi_test() {
  use should <- test.with_meta(Meta("Test multiple param values", ["meta"]))
  let as_and_bs =
    list.range(0, 5)
    |> list.zip(list.range(0, 5))
  use a_and_b <- list.each(as_and_bs)
  let #(a, b) = a_and_b
  add(a, b)
  |> should.equal(a + b)
}

pub fn diff_string_test() {
  test(
    Meta("Test diffing of strings", []),
    fn(meta) {
      "a test string"
      |> test.equal("the test thing", meta)
    },
  )
}

pub fn diff_custom_test() {
  test(
    Meta("Testing diffs of custom types", ["diff"]),
    fn(meta) {
      [TestType("first", ["in", "array"]), TestType("second", ["in", "array"])]
      |> test.equal(
        [
          TestType("second", ["in", "array"]),
          TestType("first", ["other", "array"]),
          Variant,
        ],
        meta,
      )
    },
  )
}

pub fn diff_long_test() {
  test(
    Meta("Testing long expected and got", ["diff"]),
    fn(meta) {
      [
        TestType("first", ["in", "array"]),
        TestType("second", ["in", "array"]),
        TestType("first", ["in", "array"]),
        TestType("second", ["in", "array"]),
        TestType("first", ["in", "array"]),
        TestType("second", ["in", "array"]),
      ]
      |> test.equal(
        [
          TestType("second", ["in", "array"]),
          TestType("first", ["other", "array"]),
          Variant,
          TestType("second", ["in", "array"]),
          TestType("first", ["other", "array"]),
          TestType("second", ["in", "array"]),
          TestType("first", ["other", "array"]),
        ],
        meta,
      )
    },
  )
}

pub fn formatted_table_test() {
  test(
    Meta("This is a test description", ["showtime", "ignore"]),
    fn(_meta) {
      Table(
        None,
        [
          [
            AlignRight(StyledContent(green("###")), 2),
            Separator(green("# ")),
            AlignLeft(StyledContent(green("Test Header ####")), 0),
            AlignLeft(Content(""), 0),
            AlignLeft(Content(""), 0),
          ],
          [
            AlignRight(StyledContent(cyan("first")), 2),
            Separator(": "),
            AlignLeft(StyledContent(green("second")), 1),
            Separator("| "),
            AlignLeft(Content("third"), 1),
          ],
          [
            AlignRight(StyledContent(cyan("firstlong")), 2),
            Separator(": "),
            AlignLeft(StyledContent(red("sh")), 1),
            Separator("| "),
            AlignLeft(Content("very long content"), 1),
          ],
        ],
      )
      |> table.align_table()
      |> table.to_string()
      |> io.println()
      Nil
    },
  )
}

pub fn table_test() {
  test(
    Meta("This is a test description", ["showtime"]),
    fn(meta) {
      let table_string =
        Table(
          None,
          [
            [
              AlignRight(Content("###"), 2),
              Separator("# "),
              AlignLeft(Content("Test Header ####"), 0),
              AlignLeft(Content(""), 0),
              AlignLeft(Content(""), 0),
            ],
            [
              AlignRight(Content("first"), 2),
              Separator(": "),
              AlignLeft(Content("second"), 1),
              Separator("| "),
              AlignLeft(Content("third"), 1),
            ],
            [
              AlignRight(Content("firstlong"), 2),
              Separator(": "),
              AlignLeft(Content("sh"), 1),
              Separator("| "),
              AlignLeft(Content("very long content"), 1),
            ],
          ],
        )
        |> table.align_table()
        |> table.to_string()
      table_string
      |> test.equal(
        "        #### Test Header ####                   \n      first: second           | third             \n  firstlong: sh               | very long content ",
        meta,
      )
      Nil
    },
  )
}

if javascript {
  import gleam/dynamic.{Dynamic}

  pub fn generic_exception_test() {
    throw_exception(dynamic.from(TestType("Generic", [])))
  }

  external fn throw_exception(exception: Dynamic) -> Nil =
    "./test_ffi.mjs" "throwException"
}

if erlang {
  import gleam/dynamic.{Dynamic}

  pub fn generic_exception_test() {
    throw_exception(dynamic.from(TestType("Generic", [])))
  }

  external fn throw_exception(exception: Dynamic) -> Nil =
    "test_ffi" "throw_exception"
}
