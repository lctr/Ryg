mod evaluating;
mod lexing;
mod parsing;

mod halt;

use std::{
  fs::File,
  io::{BufReader, Read, Result},
  path::PathBuf,
};

#[allow(dead_code)]
fn main() {}

fn read_text_file(path: PathBuf) -> Result<String> {
  let file = File::open(path)?;
  let mut buf = BufReader::new(file);
  let mut program = String::new();
  match buf.read_to_string(&mut program) {
    Ok(_) => Ok(program),
    Err(e) => {
      println!("{:?}", e);
      Ok("".to_string())
    }
  }
}
