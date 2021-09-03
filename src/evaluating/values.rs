// use std::fmt::{Display, Formatter, Result};

use std::error::Error;
use std::ops::Add;
use std::rc::Rc;

use crate::lexing::token::Token;
use crate::parsing::expression::Expr;

#[derive(Clone, Debug, PartialEq)]
pub enum Atom {
  Nil(),
  Bool(bool),
  Int(i32),
  Float(f64),
  String(String),
  Other {
    expr: Option<Expr>,
    token: Option<Token>,
  },
}

pub fn token_to_atom(t: Token) -> Atom {
  match t {
    Token::Boolean(b) => Atom::Bool(boolean(&b)),
    Token::String(s) => Atom::String(s),
    Token::Number(w, n) => match parse_number(w, n) {
      (Some(i), None) => Atom::Int(i),
      (None, Some(j)) => Atom::Float(j),
      _ => Atom::Nil(),
    },
    _ => Atom::Nil(),
  }
}

pub fn boolean(word: &str) -> bool {
  word == "true"
}

pub fn integer(word: &str, base: u8) -> i32 {
  match i64::from_str_radix(word, base.into()) {
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

pub fn parse_number(number: String, base: u8) -> (Option<i32>, Option<f64>) {
  if base > 0 && base != 10 {
    (Some(integer(&number, base.into())), None)
  } else {
    (None, Some(floating(&number)))
  }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Lambda {
  pub name: Option<String>,
  pub prams: Rc<Vec<String>>,
  pub body: Rc<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum RygVal {
  Nil(),
  Bool(bool),
  Int(i32),
  Float(f64),
  String(String),
  Vector(Vec<RygVal>),
  Lambda(Lambda),
  /* Function(fn(&[RygVal]) -> Result<RygVal<'a>, Halt<'a>>),
   */
}

impl RygVal {
  pub fn from_token(token: Token) -> RygVal {
    RygVal::from_atom(token_to_atom(token))
  }

  pub fn from_atom(atom: Atom) -> RygVal {
    match atom {
      Atom::Nil() => RygVal::Nil(),
      Atom::Bool(b) => RygVal::Bool(b),
      Atom::Int(i) => RygVal::Int(i),
      Atom::Float(q) => RygVal::Float(q),
      Atom::String(s) => RygVal::String(s),
      _ => panic!(
        "Unable to convert from provided potentially invalid atom {:?}",
        atom
      ),
    }
  }
  fn type_mismatch(&self) {
    panic!("Type mismatch on value from {:?}", self)
  }

  pub fn get_nil<T>(&self) {
    if let RygVal::Nil() = self {
    } else {
      panic!("Trying to get nil from a nonnil type {:?}", self)
    }
  }
  pub fn get_bool(&self) -> bool {
    if let RygVal::Bool(b) = self {
      b.clone()
    } else {
      panic!("Type mismatch on value from {:?}", self)
    }
  }
  pub fn get_int(&self) -> i32 {
    if let RygVal::Int(n) = self {
      n.clone()
    } else {
      panic!("Type mismatch on value from {:?}", self)
    }
  }
  pub fn get_float(&self) -> f64 {
    match self {
      RygVal::Float(q) => q.clone(),
      RygVal::Int(m) => m.clone() as f64,
      _ => panic!("Type mismatch on value from {:?}", self),
    }
  }
  pub fn get_string(&self) -> String {
    if let RygVal::String(s) = self {
      s.clone()
    } else {
      panic!("Type mismatch on value from {:?}", self)
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn make_int() {
    let token = Token::Number("10".to_owned(), 0);
    assert_eq!(token_to_atom(token), Atom::Float(10.0))
  }
}
