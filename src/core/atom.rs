use crate::{
  lexing::token::Token,
  util::{state::Halt, types::Either},
};

#[derive(Clone, Debug, PartialEq)]
pub enum Atom {
  Nil(),
  Empty(),
  Bool(bool),
  Byte(u8),
  Int(i32),
  Float(f64),
  Char(char, Option<char>),
  String(String),
}

impl From<Token> for Atom {
  fn from(token: Token) -> Self {
    match token {
      Token::Empty() => Atom::Nil(),
      Token::Boolean(b, _) => Atom::Bool(boolean(&b)),
      Token::Char(c, e, _) => {
        if e {
          Atom::Char('\'', Some(c))
        } else {
          Atom::Char(c, None)
        }
      }
      Token::String(s, _) => Atom::String(s),
      Token::Number(w, n, _) => {
        if n == 8 {
          Atom::Byte(byte(&w))
        } else {
          if let Ok(parsed) = parse_number(w, n) {
            match parsed {
              Either::Left(i) => Atom::Int(i),
              Either::Right(j) => Atom::Float(j),
              _ => Atom::Nil(),
            }
          } else {
            Atom::Nil()
          }
        }
      }
      _ => Atom::Empty(),
    }
  }
}

pub fn token_to_atom(t: Token) -> Atom {
  Atom::from(t)
}

pub fn byte(word: &str) -> u8 {
  match word.parse::<u8>() {
    Ok(b) => b,
    Err(_) => 0,
  }
}

pub fn boolean(word: &str) -> bool {
  word == "true"
}

pub fn integer(word: &str, base: u8) -> i32 {
  match i32::from_str_radix(word, base.into()) {
    Ok(n) => n as i32,
    Err(_) => 0,
  }
}

pub fn floating(word: &String) -> f64 {
  match word.parse::<f64>() {
    Ok(n) => n,
    Err(_) => 0.0,
  }
}

pub fn parse_number(
  number: String,
  base: u8,
) -> Result<Either<i32, f64>, Halt> {
  match base {
    2 | 8 | 16 => Ok(Either::Left(integer(&number, base.clone()))),
    x if x == 0 => Ok(Either::Left(
      number
        .parse::<i32>()
        .unwrap_or_else(|_| integer(&number, 10))
        .to_owned(),
    )),
    x if x == 10 => Ok(Either::Right(floating(&number))),
    _ => Err(Halt::InvalidInput(format!(
      "Unable to parse string {:?} to number of base {}",
      &number, base
    ))),
  }
}
