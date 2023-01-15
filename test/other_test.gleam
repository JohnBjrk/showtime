import gleam/io

pub fn other_main_test() {
  io.println("Hello from other main")
}

pub fn wrong_arity_test(_param: String) {
  io.println("Should not be run")
}
