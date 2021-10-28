#![allow(warnings)]
mod core;
mod evaluating;
mod interacting;
mod parsing;
mod tok;
mod util;

#[allow(dead_code)]
fn main() {
    use std::env;
    use util::fs::load_file;
    let mut args = env::args();
    match args.len() {
        0 | 1 => {
            println!("Loading REPL...");
            interacting::repl::run()
        }
        2 => match args.nth(1) {
            Some(c) if &c == "repl" => {
                println!("Loading REPL...");
                interacting::repl::run()
            }
            Some(c) => load_file(c.as_str()),
            _ => {}
        },
        _ => {
            println!("{:?}", args);
            todo!()
        }
    }
}
