import gleam_community/ansi

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

pub fn module_style(text: String) {
  ansi.cyan(text)
}

pub fn heading_style(text: String) {
  ansi.cyan(text)
}

pub fn function_style(text: String) {
  bold_cyan(text)
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
