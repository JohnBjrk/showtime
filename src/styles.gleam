import galant.{
  cyan, magenta, open, placeholder, start_bold, start_cyan, start_dim,
  start_green, start_magenta, start_red, text, to_string, to_string_styler,
}

pub fn error_style(text) {
  bold_red()(text)
}

pub fn module_style(text: String) {
  open()
  |> cyan(text)
  |> to_string()
}

pub fn message_style(text) {
  dim_magenta()(text)
}

pub fn expected_style(text) {
  dim_magenta()(text)
}

pub fn got_style(text) {
  dim_magenta()(text)
}

pub fn bold_red() {
  open()
  |> start_bold()
  |> start_red()
  |> placeholder()
  |> to_string_styler()
}

pub fn bold_green() {
  open()
  |> start_bold()
  |> start_green()
  |> placeholder()
  |> to_string_styler()
}

pub fn bold_cyan() {
  open()
  |> start_bold()
  |> start_cyan()
  |> placeholder()
  |> to_string_styler()
}

pub fn bold_magenta() {
  open()
  |> start_bold()
  |> start_magenta()
  |> placeholder()
  |> to_string_styler()
}

pub fn dim_magenta() {
  open()
  |> start_cyan()
  |> placeholder()
  |> to_string_styler()
}
