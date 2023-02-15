import showtime/tests/should

pub fn in_subfolder_test() {
  ["I", "am", "in", "test"]
  |> should.equal(["I", "am", "in", "subfolder"])
}
