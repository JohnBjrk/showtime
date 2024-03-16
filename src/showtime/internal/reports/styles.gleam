import gleam_community/ansi
import gleam/list
import gleam/string
import gleam/bit_array

pub fn passed_style(text) {
  bold_green(text)
}

pub fn failed_style(text) {
  bold_red(text)
}

pub fn ignored_style(text) {
  bold_yellow(text)
}

pub fn error_style(text) {
  bold_red(text)
}

pub fn expected_highlight(text) {
  bold_green(text)
}

pub fn got_highlight(text) {
  bold_red(text)
}

pub fn not_style(text) {
  ansi.bold(text)
}

pub fn module_style(text: String) {
  ansi.cyan(text)
}

pub fn heading_style(text: String) {
  ansi.cyan(text)
}

pub fn function_style(text: String) {
  bold_cyan(text)
}

pub fn stacktrace_style(text: String) {
  text
}

fn bold_red(text: String) {
  ansi.bold(ansi.red(text))
}

fn bold_green(text) {
  ansi.bold(ansi.green(text))
}

fn bold_yellow(text) {
  ansi.bold(ansi.yellow(text))
}

fn bold_cyan(text) {
  ansi.bold(ansi.cyan(text))
}

pub fn strip_style(text) {
  let #(new_text, _) =
    text
    |> string.to_graphemes()
    |> list.fold(#("", False), fn(acc, char) {
      let #(str, removing) = acc
      let bit_char = bit_array.from_string(char)
      case bit_char, removing {
        <<0x1b>>, _ -> #(str, True)
        <<0x6d>>, True -> #(str, False)
        _, True -> #(str, True)
        _, False -> #(str <> char, False)
      }
    })
  new_text
}
