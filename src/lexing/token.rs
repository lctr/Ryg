use std::{collections::HashMap, fmt, iter::Peekable, str::Chars};

use crate::{
  new_map,
  util::state::{Pos, Streaming},
};

#[derive(Debug)]
pub struct CharStream<'a> {
  pub pos: Pos,
  chars: Peekable<Chars<'a>>,
  src: &'a str,
}

impl<'a> CharStream<'a> {
  pub fn new(src: &str) -> CharStream {
    CharStream {
      chars: src.trim_end().chars().peekable(),
      pos: Pos::new(),
      src,
    }
  }
}

impl<'a> Streaming<Option<char>> for CharStream<'a> {
  fn peek(&mut self) -> Option<char> {
    let c = self.chars.peek();
    c.copied()
  }

  fn next(&mut self) -> Option<char> {
    if self.done() {
      None
    } else {
      if let Some(c) = self.peek() {
        self.pos.next(&c);
        self.chars.next();
        Some(c)
      } else {
        None
      }
    }
  }

  fn done(&mut self) -> bool {
    self.peek().is_none()
  }

  fn get_pos(&mut self) -> Pos {
    self.pos.clone()
  }
}

#[allow(unused)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
  Char(char, bool, Pos),
  String(String, Pos),
  Number(String, u8, Pos),
  Identifier(String, Pos),
  Symbol(String, Pos),
  Boolean(String, Pos),
  Operator(String, Pos),
  Keyword(String, Pos),
  Punct(char, Pos),
  Meta(String, Pos),
  Eof(Pos),
  Empty(),
  Invalid(String, Pos),
}

#[allow(unused)]
impl Token {
  pub fn literal(&self) -> String {
    let mut word = String::new();
    match &self {
      Token::Number(b, _, _)
      | Token::String(b, _)
      | Token::Boolean(b, _)
      | Token::Identifier(b, _)
      | Token::Symbol(b, _)
      | Token::Operator(b, _)
      | Token::Invalid(b, _)
      | Token::Meta(b, _)
      | Token::Keyword(b, _) => word = b.to_string(),
      Token::Char(b, e, _) => {
        word = format!("'{}{}'", if *e { "\\" } else { "" }, *b)
      }
      Token::Punct(b, _) => word = b.to_string(),
      Token::Eof(_) => word = "\0".to_string(),
      Token::Empty() => word = "".to_string(),
    }
    word
  }

  pub fn precedence(&self) -> Option<usize> {
    if let Some(Token::Operator(a, _)) = self.as_operator() {
      let prec = match a.as_ref() {
        "." | "@" | "<|" => 0,
        "=" | "<-" | "=<" => 1,
        "||" => 2,
        "&&" => 3,
        "|" => 4,
        "^" => 5,
        "&" => 6,
        "==" | "!=" => 7,
        "<" | "<=" | ">" | ">=" => 8,
        "<>" => 9,
        "++" => 9,
        "+" => 10,
        "-" => 10,
        "*" => 11,
        "/" => 11,
        "%" => 11,
        "**" => 12,
        "->" => 14,
        "|>" => 15,
        "::" => 20,
        _ => 50,
      } as usize;
      if prec < 50 {
        Some(prec)
      } else {
        None
      }
    } else {
      None
    }
  }

  /// Irregular lexicographic semantemes
  pub fn as_operator(&self) -> Option<Self> {
    let pos = if let Some(p) = self.get_pos() {
      p.clone()
    } else {
      Pos::faux()
    };

    match self {
      Self::Keyword(a, _)
        if ["mod", "and", "or", "xor", "nor", "not"]
          .iter()
          .as_slice()
          .contains(&a.as_ref()) =>
      {
        Self::Operator(a.to_owned(), pos).as_some()
      }
      Self::Symbol(a, _) if a.as_str() == "@" => {
        Self::Operator(a.to_string(), pos).as_some()
      }
      Self::Punct(a @ '|', _) => Self::Operator(a.to_string(), pos).as_some(),
      Self::Operator(..) => self.as_some(),
      _ => None,
    }
  }

  pub fn as_punct(&self) -> Option<Self> {
    let pos = if let Some(p) = self.get_pos() {
      p.clone()
    } else {
      Pos::faux()
    };
    match &self {
      Self::Operator(o, _) if matches!(o.as_str(), "|" | "<" | ">" | ".") => {
        let ch = o.clone();
        if ch.len() > 1 {
          None
        } else {
          ch.chars()
            .next()
            .and_then(|c| Self::Punct(c, pos).as_some())
        }
      }
      _ => None,
    }
  }
  pub fn as_some(&self) -> Option<Self> {
    Some(self.clone())
  }
  pub fn to_string(&self) -> String {
    self.literal().to_owned()
  }

  pub fn match_literal(&self, word: &str) -> bool {
    self.literal() == word
  }

  pub fn match_any_of(&self, literals: &[&str]) -> bool {
    literals.iter().any(|&word| self.match_literal(word))
  }

  pub fn get_pos(&self) -> Option<&Pos> {
    match self {
      Token::String(_, p)
      | Token::Char(_, _, p)
      | Token::Number(_, _, p)
      | Token::Identifier(_, p)
      | Token::Symbol(_, p)
      | Token::Boolean(_, p)
      | Token::Operator(_, p)
      | Token::Keyword(_, p)
      | Token::Punct(_, p)
      | Token::Meta(_, p)
      | Token::Invalid(_, p)
      | Token::Eof(p) => Some(p),
      Token::Empty() => None,
    }
  }

  pub fn is_left_punct(&self) -> bool {
    match self {
      Token::Punct('(' | '[' | '{', _) => true,
      _ => false,
    }
  }
  pub fn is_unary(&self) -> bool {
    self.match_any_of(&["!", "-"])
  }

  /// Returns `true` if the token is [`Eof`].
  ///
  /// [`Eof`]: Token::Eof
  pub fn is_eof(&self) -> bool {
    matches!(self, Self::Eof(_))
  }

  /// Returns `true` if the token is [`Boolean`], [`Number`], or [`String`].
  ///
  /// [`Boolean`]: Token::Boolean
  /// [`Number`]: Token::Number
  /// [`String`]: Token::String
  pub fn is_atomic(&self) -> bool {
    matches!(
      self,
      Self::Boolean(..) | Self::Number(..) | Self::String(..)
    )
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

  /// Returns `true` if the token is [`Identifier`].
  ///
  /// [`Identifier`]: Token::Identifier
  pub fn is_identifier(&self) -> bool {
    matches!(self, Self::Identifier(..))
  }

  /// Returns `true` if the token is [`Meta`].
  ///
  /// [`Meta`]: Token::Meta
  pub fn is_meta(&self) -> bool {
    matches!(self, Self::Meta(..))
  }

  /// Returns `true` if the token is [`Invalid`].
  ///
  /// [`Invalid`]: Token::Invalid
  pub fn is_invalid(&self) -> bool {
    matches!(self, Self::Invalid(..))
  }

  /// Returns `true` if the token is [`Empty`].
  ///
  /// [`Empty`]: Token::Empty
  pub fn is_empty(&self) -> bool {
    matches!(self, Self::Empty(..))
  }
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Token::Char(c, e, p) => {
        write!(f, "(Char '{}{}' {})", if *e { "\\" } else { "" }, c, p)
      }
      Token::String(s, p) => write!(f, "(String {} {})", s, p),
      Token::Number(s, b, p) => write!(f, "(Number {}\\{} {})", s, b, p),
      Token::Symbol(s, p) => write!(f, "(Symbol {} {})", s, p),
      Token::Boolean(s, p) => write!(f, "(Bool {} {})", s, p),
      Token::Operator(s, p) => write!(f, "(Operator {} {})", s, p),
      Token::Keyword(s, p) => write!(f, "(Keyword {} {})", s, p),
      Token::Identifier(s, p) => write!(f, "(Identifier {} {})", s, p),
      Token::Invalid(s, p) => write!(f, "(Invalid {} {})", s, p),
      Token::Empty() => write!(f, "(··)"),
      Token::Eof(p) => write!(f, "(Eof {})", p),
      Token::Punct(c, p) => write!(f, "(Punct {} {})", c, p),
      Token::Meta(s, p) => write!(f, "(Meta {} {})", s, p),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  fn inspect_chars(src: &str) {
    let mut stream = CharStream::new(src);
    while !stream.done() {
      println!("char '{}' {}", stream.next().unwrap(), stream.pos)
    }
  }

  #[test]
  fn all_chars() {
    let src = "12 34 this is a line
    break, ~ éé ʃ";
    let tok = Token::String("5".to_string(), Pos::faux());
    println!("{:?}", tok.match_any_of(&["4", "5", "6"]));

    inspect_chars(src);
    assert_eq!(
      Token::Number("1".to_string(), 0, Pos::new()).is_number(),
      true
    )
  }

  #[test]
  fn test_atomics() {
    let number = Token::Number("10".to_owned(), 0, Pos::new());
    let string = Token::String("hi".to_owned(), Pos::new());
    let boolean = Token::Boolean("true".to_owned(), Pos::new());
    for t in [number, string, boolean].iter() {
      assert_eq!(t.is_atomic(), true);
    }
  }
}
