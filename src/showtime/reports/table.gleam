import gleam/list
import gleam/string
import gleam/int
import gleam/option.{Option}
import showtime/reports/styles.{strip_style}

pub type Content {
  Content(unstyled_text: String)
  StyledContent(styled_text: String)
}

pub type Col {
  AlignRight(content: Content, margin: Int)
  AlignLeft(content: Content, margin: Int)
  Separator(char: String)
  Aligned(content: String)
}

pub type Table {
  Table(header: Option(String), rows: List(List(Col)))
}

pub fn to_string(table: Table) -> String {
  let rows =
    table.rows
    |> list.map(fn(row) {
      row
      |> list.filter_map(fn(col) {
        case col {
          Separator(char) -> Ok(char)
          Aligned(content) -> Ok(content)
          _ -> Error(Nil)
        }
      })
      |> string.join("")
    })
    |> string.join("\n")
  let header =
    table.header
    |> option.map(fn(header) { header <> "\n" })
    |> option.unwrap("")
  header <> rows
}

pub fn align_table(table: Table) -> Table {
  let cols =
    table.rows
    |> list.transpose()
  let col_width =
    cols
    |> list.map(fn(col) {
      col
      |> list.map(fn(content) {
        case content {
          AlignRight(Content(unstyled), _) -> unstyled
          AlignRight(StyledContent(styled), _) -> strip_style(styled)
          AlignLeft(Content(unstyled), _) -> unstyled
          AlignLeft(StyledContent(styled), _) -> strip_style(styled)
          Separator(char) -> char
          Aligned(content) -> content
        }
      })
      |> list.fold(0, fn(max, str) { int.max(max, string.length(str)) })
    })
  let aligned_col =
    cols
    |> list.zip(col_width)
    |> list.map(fn(col_and_width) {
      let #(col, width) = col_and_width
      col
      |> list.map(fn(content) {
        case content {
          AlignRight(Content(unstyled), margin) ->
            Aligned(pad_left(
              unstyled,
              width + margin - string.length(unstyled),
              " ",
            ))
          AlignRight(StyledContent(styled), margin) ->
            Aligned(pad_left(
              styled,
              width + margin - string.length(strip_style(styled)),
              " ",
            ))
          AlignLeft(Content(unstyled), margin) ->
            Aligned(pad_right(
              unstyled,
              width + margin - string.length(unstyled),
              " ",
            ))
          AlignLeft(StyledContent(styled), margin) ->
            Aligned(pad_right(
              styled,
              width + margin - string.length(strip_style(styled)),
              " ",
            ))
          Separator(char) -> Separator(char)
          Aligned(content) -> Aligned(content)
        }
      })
    })
  let aligned_rows =
    aligned_col
    |> list.transpose()
  Table(..table, rows: aligned_rows)
}

fn pad_left(str: String, num: Int, char: String) {
  let padding =
    list.repeat(char, num)
    |> string.join("")
  padding <> str
}

fn pad_right(str: String, num: Int, char: String) {
  let padding =
    list.repeat(char, num)
    |> string.join("")
  str <> padding
}
