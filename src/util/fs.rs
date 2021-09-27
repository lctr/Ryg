use std::{
  env, error, fmt,
  fs::File,
  io::{self, BufReader, Read, Result, Write},
  path::PathBuf,
};

#[allow(unused)]
pub fn read_text_file(path: PathBuf) -> Result<String> {
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

// IO
// ansi color support more specific with *nix terminals, look into windows
// later
#[allow(unused)]
#[cfg(not(windows))]
fn colors_allowed() -> bool {
  match env::var_os("TERM") {
    None => false,
    Some(k) if k == "dumb" => false,
    _ => {
      if let Some(_) = env::var_os("NO_COLOR") {
        false
      } else {
        true
      }
    }
  }
}

// formatting/coloring
pub enum Style {
  Fg(),
}

impl Style {
  fn color(&mut self) {
    // let fg = concat!("\x1B[3", "", "m"););
    // let bg = |color| concat!("\x1B[4", color, "m")
  }
}
