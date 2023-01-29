import gleam/io
import gleam/option.{None}
import gleeunit
import gleeunit/should as gshould
import showtime
import showtime/tests/should
import showtime/tests/test.{test}
import showtime/tests/meta.{Meta}
import showtime/reports/formatter.{
  AlignLeft, AlignRight, Col, Content, Separator, Table,
}
import gleam_community/ansi.{cyan, green, red}

pub type TestType {
  TestType(arg1: String, arg2: List(String))
  Variant
}

pub fn main() {
  showtime.main()
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

pub fn formatted_table_test() {
  test(
    Meta("This is a test description", ["showtime", "ignore"]),
    fn(meta) {
      Table(
        None,
        [
          [
            AlignRight(Content("###", green("###")), 2),
            Separator(green("# ")),
            AlignLeft(Content("Test Header ####", green("Test Header ####")), 0),
            AlignLeft(Content("", ""), 0),
            AlignLeft(Content("", ""), 0),
          ],
          [
            AlignRight(Content("first", cyan("first")), 2),
            Separator(": "),
            AlignLeft(Content("second", green("second")), 1),
            Separator("| "),
            AlignLeft(Content("third", "third"), 1),
          ],
          [
            AlignRight(Content("firstlong", cyan("firstlong")), 2),
            Separator(": "),
            AlignLeft(Content("sh", red("sh")), 1),
            Separator("| "),
            AlignLeft(Content("very long content", "very long content"), 1),
          ],
        ],
      )
      |> formatter.align_table()
      |> formatter.to_string()
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
              AlignRight(Content("###", "###"), 2),
              Separator("# "),
              AlignLeft(Content("Test Header ####", "Test Header ####"), 0),
              AlignLeft(Content("", ""), 0),
              AlignLeft(Content("", ""), 0),
            ],
            [
              AlignRight(Content("first", "first"), 2),
              Separator(": "),
              AlignLeft(Content("second", "second"), 1),
              Separator("| "),
              AlignLeft(Content("third", "third"), 1),
            ],
            [
              AlignRight(Content("firstlong", "firstlong"), 2),
              Separator(": "),
              AlignLeft(Content("sh", "sh"), 1),
              Separator("| "),
              AlignLeft(Content("very long content", "very long content"), 1),
            ],
          ],
        )
        |> formatter.align_table()
        |> formatter.to_string()
      table_string
      |> test.equal(
        "        #### Test Header ####                   \n      first: second           | third             \n  firstlong: sh               | very long content ",
        meta,
      )
      Nil
    },
  )
}
