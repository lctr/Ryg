use std::fmt;

use super::parser::Defn;
use crate::lexing::token::Token;

#[derive(Clone, Debug, PartialEq)]
pub struct Binding {
  name: String,
  def: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Argm {
  def: Expr,
}

#[allow(unused)]
#[derive(Clone, PartialEq)]
pub enum Expr {
  Assign(Token, Box<Expr>, Box<Expr>),
  Binary(Token, Box<Expr>, Box<Expr>),
  Block(Vec<Expr>),
  Call(Box<Expr>, Vec<Expr>),
  Case(Box<Expr>, Vec<(Expr, Expr)>, Box<Expr>),
  Conditional(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
  Index(Box<Expr>, Box<Expr>),
  Iter(Box<Expr>),
  Range(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
  Lambda(Option<String>, Vec<String>, Box<Expr>),
  Literal(Token),
  Loop(Box<Expr>, Vec<Expr>),
  Tuple(Vec<Expr>),
  Unary(Token, Box<Expr>),
  Variable(Vec<Defn>, Box<Expr>),
  Vector(Vec<Expr>),
}

impl Expr {
  pub fn from_token(token: Token) -> Expr {
    match token {
      Token::Boolean(_) | Token::String(_) | Token::Number(..) | Token::Identifier(_) => Expr::Literal(token),
      _ => panic!("Only booleans, strings, numbers, and variables may be parsed as Literal expressions!")
    }
  }
}

impl fmt::Debug for Expr {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Expr::Literal(t) => write!(f, "{:?}", t),
      Expr::Unary(a, b) => write!(f, "(Unary {:?} {:?})", a, b),
      Expr::Binary(a, b, c) => {
        write!(f, "(Binary {:?} {:?} {:>15?})", a, b, c)
      }
      Expr::Assign(a, b, c) => write!(f, "(Assign {:?} {:?} {:?})", a, b, c),
      Expr::Block(a) => write!(f, "(Block {:?})", a),
      Expr::Call(a, b) => write!(f, "(Call {:?} {:?})", a, b),
      Expr::Case(a, b, c) => write!(f, "(Case {:?} {:?} {:?})", a, b, c),
      Expr::Conditional(a, b, c) => {
        write!(f, "(Conditional {:?} {:?} {:?})", a, b, c)
      }
      Expr::Lambda(a, b, c) => write!(f, "(Lambda {:?} {:?} {:?})", a, b, c),
      Expr::Variable(a, b) => write!(f, "(Let {:?} {:?})", a, b),
      Expr::Vector(a) => write!(f, "(Vector {:?})", a),
      Expr::Tuple(b) => write!(f, "(Tuple {:?})", b),
      Expr::Index(a, b) => write!(f, "(Index {:?} {:?})", a, b),
      Expr::Iter(a) => write!(f, "(Iter {:?})", a),
      Expr::Range(a, b, c) => write!(f, "(Range {:?} {:?} {:?})", a, b, c),
      Expr::Loop(a, b) => write!(f, "(Loop {:?} {:?})", a, b),
    }
  }
}
