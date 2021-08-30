use std::{fmt, iter::Peekable, str::Chars};

#[allow(unused)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
  String(String),
  Number(String, u8),
  Variable(String),
  Symbol(String),
  Boolean(String),
  Operator(String),
  Keyword(String),
  Punct(char),
  Eof(),
  Empty(),
}

#[allow(unused)]
impl Token {
  pub fn literal(&self) -> String {
    let mut word = String::new();
    match &self {
      Token::Number(b, _) => word = b.to_string(),
      Token::String(b)
      | Token::Boolean(b)
      | Token::Variable(b)
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
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
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
  pub fn pair(&mut self) -> (usize, usize) {
    (self.line, self.col)
  }
}

impl fmt::Display for Pos {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}:{}", self.line, self.col)
  }
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
  pub fn peek(&mut self) -> &char {
    let c = self.chars.peek();
    match c {
      Some(ch) => ch,
      None => &'\0',
    }
  }
  pub fn next(&mut self) -> char {
    if self.eof() {
      '\0'
    } else {
      let c = *self.peek();
      self.pos.tick(&c);
      self.chars.next();
      c
    }
  }
  pub fn eof(&mut self) -> bool {
    self.peek() == &'\0'
  }
  pub fn peek_iter(&mut self) -> Option<&char> {
    self.chars.peek()
  }
}

mod test {
  use super::Stream;
  fn inspect_chars(src: &str) {
    let mut stream = Stream::new(src);
    while !stream.eof() {
      println!("char '{}' at {}", stream.next(), stream.pos)
    }
  }

  #[test]
  fn all_chars() {
    inspect_chars(
      "12
    34
    this is a line
    break, ~",
    )
  }
}
