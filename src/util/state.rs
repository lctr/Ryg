use std::{
  collections::HashSet,
  fmt::{self, Display, Error},
  rc::Rc,
};

use crate::{core::rygval::RygVal, util::misc::Paint};

pub trait Streaming<T>
where
  T: Clone, {
  fn peek(&mut self) -> T;
  fn next(&mut self) -> T;
  fn done(&mut self) -> bool;
  fn get_pos(&mut self) -> Pos;
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos {
  pub pos: usize,
  pub row: usize,
  pub col: usize,
  lines: Vec<(char, Rc<Pos>)>,
  pub new_line: bool,
}

#[allow(unused)]
impl Pos {
  pub fn new() -> Pos {
    Pos {
      pos: 0,
      row: 1,
      col: 0,
      lines: Vec::new(),
      new_line: true,
    }
  }
  // non-existent positions in source file (such as when modifying tokens/token
  // trees during analysis, etc) Pos starts counting rows (=lines) at 1,
  // while position and column are 0 indexed. A nonvalid Pos struct will have a
  // row property of 0.
  pub fn faux() -> Pos {
    Pos {
      pos: 0,
      row: 0,
      col: 0,
      lines: Vec::new(),
      new_line: true,
    }
  }
  pub fn next(&mut self, c: &char) {
    self.new_line = false;
    match *c {
      '\0' => {}
      '\n' => {
        self.lines.push((*c, Rc::new(self.clone())));
        self.eol();
      }
      _ => {
        self.pos += 1;
        self.col += 1;
      }
    }
  }

  pub fn get_lines(&self) -> &Vec<(char, Rc<Pos>)> {
    &self.lines
  }

  pub fn eol(&mut self) {
    self.pos += 1;
    self.col = 0;
    self.row += 1;
    self.new_line = true;
  }
}

impl fmt::Debug for Pos {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "at {}:{}", self.row, self.col)
  }
}

impl fmt::Display for Pos {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "at {}:{}", self.row, self.col)
  }
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum StreamState<A> {
  Ready(Option<A>),
  Pending(A),
  Halted(A),
  Done(A),
}

#[allow(unused)]
impl<A> StreamState<A> {
  pub fn new(init: Option<A>) -> Self {
    Self::Ready(init)
  }
  /// Returns `true` if the parser status is [`Ready`].
  ///
  /// [`Ready`]: ParserStatus::Ready
  pub fn is_ready(&self) -> bool {
    matches!(self, Self::Ready(_))
  }

  /// Returns `true` if the parser status is [`Pending`].
  ///
  /// [`Pending`]: ParserStatus::Pending
  pub fn is_pending(&self) -> bool {
    matches!(self, Self::Pending(..))
  }

  /// Returns `true` if the parser status is [`Halted`].
  ///
  /// [`Halted`]: ParserStatus::Halted
  pub fn is_halted(&self) -> bool {
    matches!(self, Self::Halted(..))
  }

  /// Returns `true` if the parser status is [`Complete`].
  ///
  /// [`Complete`]: ParserStatus::Complete
  pub fn is_complete(&self) -> bool {
    matches!(self, Self::Done(_))
  }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Halt {
  Evaluating(String),
  UnknownError(String),
  InvalidInput(String),
  Unexpected(String),
  DivisionByZero(String),
  ReplError(String),
  Many(Vec<Halt>),
  RefGetError(String),
  RefSetError(String),
  InvalidType(String),
}

impl Halt {
  pub fn is_many(&self) -> bool {
    if let Self::Many(_) = self {
      true
    } else {
      false
    }
  }
  pub fn get_many(&self) -> Vec<Halt> {
    if let Self::Many(v) = self {
      v.to_owned()
    } else {
      vec![self.to_owned()]
    }
  }
  pub fn append(&self, halt: Halt) {
    if self.is_many() {
      self.get_many().push(halt);
    }
  }
  pub fn as_val(self) -> RygVal {
    RygVal::Error(Box::new(self))
  }
}

impl Display for Halt {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self {
      Halt::UnknownError(m)
      | Halt::DivisionByZero(m)
      | Halt::Unexpected(m)
      | Halt::InvalidType(m)
      | Halt::InvalidInput(m) => write!(f, "{}", m),
      Halt::RefGetError(m) | Halt::RefSetError(m) => {
        write!(f, "{} is out of scope!", m)
      }
      Halt::Evaluating(m) => write!(f, "Evaluator error! {}", m),
      Halt::ReplError(e) => {
        write!(f, "{} {}", Paint::fg_red("Error!".to_string()), e)
      }
      Halt::Many(v) => f.debug_list().entries(v.iter()).finish(), /* write!(f, "Errors: {:?}", v), */
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  #[test]
  fn test_append() {
    println!("{}", "\\");
    let h1 = Halt::Unexpected("Error 1".to_owned());
    let h2 = Halt::Unexpected("Error 2".to_owned());
    let h3 = Halt::Many(vec![h1]);
    h3.append(h2);
    println!("{}", h3)
  }
}
