import gleam/dynamic.{Dynamic}

pub type Annotated {
  InBoth(value: Dynamic)
  Unique(value: Dynamic)
}

pub type Diff {
  Diff(expected: List(Annotated), got: List(Annotated))
  Literal(expected: Dynamic, got: Dynamic)
}

pub fn do_compare(expected: Dynamic, got: Dynamic) -> Diff {
  diff(expected, got)
}

pub external fn diff(expected: Dynamic, got: Dynamic) -> Diff =
  "showtime_ffi" "diff"
