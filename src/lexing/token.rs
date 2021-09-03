use std::{fmt, iter::Peekable, str::Chars};

pub trait Streaming<T> {
  fn peek(&mut self) -> T;
  fn next(&mut self) -> T;
  fn eof(&mut self) -> bool;
}

#[derive(Debug)]

pub struct Stream<'a> {
  pub pos: Pos,
  chars: Peekable<Chars<'a>>,
}

impl<'a> Stream<'a> {
  pub fn new(src: &str) -> Stream {
    Stream {
      chars: src.trim_end().chars().peekable(),
      pos: Pos::new(),
    }
  }

  pub fn peek_iter(&mut self) -> Option<&char> {
    self.chars.peek()
  }
}

impl<'a> Streaming<char> for Stream<'a> {
  fn peek(&mut self) -> char {
    let c = self.chars.peek();
    match c {
      Some(ch) => *ch,
      None => '\0',
    }
  }

  fn next(&mut self) -> char {
    if self.eof() {
      '\0'
    } else {
      let c = self.peek();
      self.pos.tick(&c);
      self.chars.next();
      c
    }
  }

  fn eof(&mut self) -> bool {
    self.peek() == '\0'
  }
}

#[allow(unused)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
  String(String),
  Number(String, u8),
  Identifier(String),
  Symbol(String),
  Boolean(String),
  Operator(String),
  Keyword(String),
  Punct(char),
  Eof(),
  Empty(),
}

// impl fmt::Display for Token {
//   fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//     match self {
//         Token::String(_) => todo!(),
//         Token::Number(_, _) => todo!(),
//         Token::Variable(_) => todo!(),
//         Token::Symbol(_) => todo!(),
//         Token::Boolean(_) => todo!(),
//         Token::Operator(_) => todo!(),
//         Token::Keyword(_) => todo!(),
//         Token::Punct(_) => todo!(),
//         Token::Eof() => todo!(),
//         Token::Empty() => todo!(),
//     }
//     // write!(f, "{}:{}", self.line, self.col)
//   }
// }

#[allow(unused)]
impl Token {
  pub fn literal(&self) -> String {
    let mut word = String::new();

    match &self {
      Token::Number(b, _)
      | Token::String(b)
      | Token::Boolean(b)
      | Token::Identifier(b)
      | Token::Symbol(b)
      | Token::Operator(b)
      | Token::Keyword(b) => word = b.to_string(),
      Token::Punct(b) => word = b.to_string(),
      Token::Eof() => word = "\0".to_string(),
      Token::Empty() => word = "".to_string(),
    }
    word
  }

  pub fn literal_owned(&mut self) -> String {
    self.literal().to_owned()
  }

  pub fn match_literal(&mut self, word: &str) -> bool {
    self.literal() == word
  }

  pub fn is_atomic(&self) -> bool {
    matches!(self, Self::Boolean(_) | Self::Number(..) | Self::String(_))
  }

  /// Returns `true` if the token is [`Number`].
  ///
  /// [`Number`]: Token::Number
  pub fn is_number(&self) -> bool {
    matches!(self, Self::Number(..))
  }

  /// Returns `true` if the token is [`Keyword`].
  ///
  /// [`Keyword`]: Token::Keyword
  pub fn is_keyword(&self) -> bool {
    matches!(self, Self::Keyword(..))
  }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos {
  pos: usize,
  line: usize,
  col: usize,
}

impl Pos {
  pub fn new() -> Pos {
    Pos {
      pos: 0,
      line: 1,
      col: 0,
    }
  }

  pub fn tick(&mut self, c: &char) {
    match *c {
      '\0' => {}
      '\n' => {
        self.eol();
      }
      _ => {
        self.pos += 1;

        self.col += 1;
      }
    }
  }

  pub fn eol(&mut self) {
    self.pos += 1;

    self.col = 0;

    self.line += 1;
  }
}
impl fmt::Debug for Pos {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}:{}", self.line, self.col)
  }
}
impl fmt::Display for Pos {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}:{}", self.line, self.col)
  }
}

#[cfg(test)]
mod test {
  use super::*;
  fn inspect_chars(src: &str) {
    let mut stream = Stream::new(src);
    while !stream.eof() {
      println!("char '{}' at {}", stream.next(), stream.pos)
    }
  }

  #[test]
  fn all_chars() {
    inspect_chars(
      "12 34 this is a line
    break, ~ éé ʃ",
    );
    assert_eq!(Token::Number("1".to_string(), 0).is_number(), true)
  }

  #[test]
  fn test_atomics() {
    let number = Token::Number("10".to_owned(), 0);
    let string = Token::String("hi".to_owned());
    let boolean = Token::Boolean("true".to_owned());
    for t in [number, string, boolean].iter() {
      assert_eq!(t.is_atomic(), true);
    }
  }
}
