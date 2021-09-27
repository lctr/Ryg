#![allow(warnings)]
mod core;
mod evaluating;
mod lexing;
mod parsing;
mod repl;
mod util;

#[allow(dead_code)]
fn main() {
  repl::run()
}
