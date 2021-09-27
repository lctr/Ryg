use std::{
  fmt::{self, Debug},
  rc::Rc,
};

use crate::{
  evaluating::environment::Field,
  lexing::token::Token,
  util::{
    misc::map_join,
    state::Halt,
    types::{Either, Kind},
  },
};

#[derive(Clone, Debug, PartialEq)]
pub struct Definition {
  pub item: Expr,
  pub ranges: Vec<(Expr, Expr)>,
  pub fixed: Vec<(Expr, Expr)>,
  pub conds: Vec<Expr>,
}

impl Definition {
  pub fn new(
    item: Expr,
    ranges: Vec<(Expr, Expr)>,
    fixed: Vec<(Expr, Expr)>,
    conds: Vec<Expr>,
  ) -> Self {
    Self {
      item,
      ranges,
      fixed,
      conds,
    }
  }
}

impl Definition {
  pub fn get_shape(&self) -> Shape {
    match self.item {
      Expr::Nil => Shape::Empty,
      Expr::Vector(_) => Shape::Vector,
      Expr::Literal(_) => Shape::Atom,
      Expr::Tuple(_) => Shape::Tuple,
      Expr::List(_) => Shape::List,
      Expr::Record(_, _) => Shape::Record,
      Expr::Unary(_, _) => Shape::Atom,
      Expr::Binary(_, _, _) => Shape::Holder,
      Expr::Lambda(..) => Shape::Closure,
      _ => Shape::Unknown,
    }
  }
  fn vec_gen(&self) {}
  fn tup_gen(&self) {}
  fn list_gen(&self) {}
  fn map_gen(&self) {}
  pub fn generators(&self) {
    match self.get_shape() {
      Shape::Empty => todo!(),
      Shape::Vector => self.vec_gen(),
      Shape::Tuple => self.tup_gen(),
      Shape::List => self.list_gen(),
      _ => {}
    };
    // for (ident, span) in self.range[..] {

    // }
  }
}

#[derive(Clone, Debug, PartialEq)]
// Variables bound to values currently in scope
// Let expressions handle these as arguments in a homomorphic manner to lambdas
pub struct Binding {
  pub pram: Parameter,
  pub expr: Expr,
}

#[derive(Clone, Debug, PartialEq)]
// left hand side of bindings, found in function/lambda args
pub struct Parameter {
  pub name: Token,
  // for destructuring
  pub pattern: TokPattern,
  pub shape: Shape,
  pub kind: Vec<Token>,
}

impl Parameter {
  pub fn new(
    name: Token,
    pattern: TokPattern,
    shape: Shape,
    kind: Vec<Token>,
  ) -> Self {
    Parameter {
      name,
      pattern,
      shape,
      kind,
    }
  }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Shape {
  Atom,
  Tuple,
  Vector,
  List,
  Record,
  Closure,
  Holder,
  Unknown,
  Empty,
  Nested(Rc<Shape>),
  // FOR GENERICS AND STUFF
  A,
  B,
  C,
  X,
  Y,
  Z,
  U,
  V,
  W,
  // special ryg generic, aka for "any"
  R,
}

impl Shape {
  pub fn from_tok_ref(tok: &Token) -> Shape {
    Shape::from(tok.to_owned())
  }
}

impl From<Token> for Shape {
  fn from(token: Token) -> Shape {
    if matches!(token.clone(), Token::Identifier(..) | Token::Meta(..)) {
      Shape::Atom
    } else {
      match token.clone().to_string().as_str() {
        "(" => Shape::Tuple,
        "[" => Shape::Vector,
        "{" => Shape::Record,
        "|" => Shape::Closure,
        "A" => Shape::A,
        "B" => Shape::B,
        "C" => Shape::C,
        "X" => Shape::X,
        "Y" => Shape::Y,
        "Z" => Shape::Z,
        "U" => Shape::U,
        "V" => Shape::V,
        "W" => Shape::W,
        _ => Shape::Unknown,
      }
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokPattern {
  Empty,
  Atom(Token),
  Tuple(Vec<TokPattern>),
  Vector(Vec<TokPattern>),
  List(Vec<TokPattern>),
  Record(Vec<TokPattern>),
  Call(Box<TokPattern>, Vec<TokPattern>),
  Unary(Token, Box<TokPattern>),
  Binary(Token, Box<TokPattern>, Box<TokPattern>),
  Ternary(Token, Box<TokPattern>, Box<TokPattern>, Box<TokPattern>),
  Map(Box<TokPattern>, Box<TokPattern>),
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
  Nil,
  // (Assign Left Right)
  Assign(Token, Box<Expr>, Box<Expr>),
  // (BinOp Left Right)
  Binary(Token, Box<Expr>, Box<Expr>),
  // (Expr[..] Last**) **for optimization
  Block(Vec<Expr>),
  // Fn Args
  Call(Box<Expr>, Vec<Expr>, Option<String>),
  Case(Box<Expr>, Vec<(Expr, Expr)>, Box<Expr>),
  Conditional(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
  Index(Box<Expr>, Box<Expr>),
  Iter(Box<Expr>, Option<Box<Expr>>),
  List(Box<Definition>),
  Range(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
  Lambda(Option<String>, Vec<Parameter>, Box<Expr>),
  Literal(Token),
  // cond body
  Loop(Box<Expr>, Box<Expr>),
  Tuple(Vec<Expr>),
  Unary(Token, Box<Expr>),
  Variable(Vec<Binding>, Box<Expr>),
  Vector(Vec<Expr>),
  Error(Halt, Token),
  Return(Box<Expr>, Box<Expr>),
  Named(Kind<String>, Box<Expr>),
  Record(Kind<Token>, Vec<Binding>),
}

#[derive(Clone, PartialEq)]
pub struct Sequence {
  pub body: Vec<Expr>,
  index: usize,
  current: Option<Expr>,
}

impl Sequence {
  pub fn new(exprs: Vec<Expr>) -> Self {
    Self {
      body: exprs,
      index: 0,
      current: None,
    }
  }
  pub fn len(&self) -> usize {
    self.body.len()
  }
}

impl From<Expr> for Sequence {
  fn from(expr: Expr) -> Self {
    match expr {
      Expr::Block(v) | Expr::Tuple(v) | Expr::Vector(v) => Sequence::new(v),
      _ => Sequence::new(vec![expr]),
    }
  }
}

impl Iterator for Sequence {
  type Item = Expr;
  fn next(&mut self) -> Option<Self::Item> {
    let idx = self.index;
    if idx < self.body.len() {
      self.index += 1;
      let res = if let Some(expr) = self.body.get(idx) {
        Some(expr.clone())
      } else {
        None
      };
      self.current = res.clone();
      res
    } else {
      None
    }
  }
}

impl From<Token> for Expr {
  fn from(token: Token) -> Self {
    match token {
      Token::Boolean(..) | Token::String(..) | Token::Number(..) | Token::Identifier(..) => Self::Literal(token),
      Token::Empty() => Expr::Nil,
      _ => Expr::Error(Halt::InvalidInput("Only booleans, strings, numbers, and variables may be parsed as Literal Expressions!".to_string()), token)
    }
  }
}

impl Expr {
  pub fn from_token(token: Token) -> Expr {
    Self::from(token)
  }
  pub fn is_error(&self) -> bool {
    matches!(self, Self::Error(..))
  }
}

impl fmt::Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Expr::Nil => write!(f, "()"),
      Expr::Literal(t) => write!(f, "{:>}", t),
      Expr::Unary(a, b) => write!(f, "(Unary\n\t {}\n\t {})", a, b),
      Expr::Binary(a, b, c) => {
        write!(f, "(Binary\n\t {}\n\t {}\n\t {})", a, *b, *c)
      }
      Expr::Assign(a, b, c) => {
        write!(f, "(Assign\n {}\n\t {}\n\t {})", a, *b, *c)
      }
      Expr::Block(a) => write!(f, "(Block {:#?})", a),
      Expr::Call(a, b, c) => write!(
        f,
        "(Call\n\t {}\n\t {:#?}\n\t {})",
        *a,
        b,
        if let Some(name) = &c {
          name.as_str()
        } else {
          ""
        }
      ),
      Expr::Case(a, b, c) => {
        write!(f, "(Case\n\t {}\n\t {:#?}\n\t {})", *a, b, *c)
      }
      Expr::Conditional(a, b, c) => {
        write!(
          f,
          "(Conditional\n\t {:?}\n\t {:?}\n\t {:?})",
          *a,
          *b,
          if let Some(expr) = c { expr } else { &Expr::Nil }
        )
      }
      Expr::Lambda(a, b, c) => {
        write!(
          f,
          "(Lambda\n\t {}\n\t {:#?}\n\t {})",
          if let Some(name) = a { name } else { "ʎ" },
          b,
          *c
        )
      }
      Expr::Variable(a, b) => write!(f, "(Let\n\t {:#?}\n\t {})", a, *b),
      Expr::Vector(a) => write!(f, "(Vector\n\t {:#?})", a),
      Expr::List(a) => write!(f, "(List\n\t {:#?})", a),
      Expr::Tuple(b) => write!(f, "(Tuple\n\t {:#?})", b),
      Expr::Index(a, b) => write!(f, "(Index\n\t {}\n\t {})", *a, *b),
      Expr::Iter(a, b) => write!(
        f,
        "(Iter\n\t {:?}\n\t {:?})",
        *a,
        if let Some(expr) = b { expr } else { &Expr::Nil }
      ),
      Expr::Range(a, b, c) => {
        write!(
          f,
          "(Range\n\t {:?}\n\t {:?}\n\t {:?})",
          *a,
          *b,
          if let Some(expr) = c { expr } else { &Expr::Nil }
        )
      }
      Expr::Loop(a, b) => write!(f, "(Loop\n\t {:?}\n\t {:?})", *a, *b),
      Expr::Error(a, b) => write!(f, "(Error\n\t {}\n\t {}", a, b),
      Expr::Return(a, b) => write!(f, "(Return\n\t {}\n\t {} )", *a, *b),
      Expr::Named(a, b) => write!(f, "(Named {}\n\t {})", a, *b),
      // Expr::Pattern(a, b) => write!(f, "(Pattern {:?} {:?})", a, b),
      Expr::Record(a, b) => write!(f, "(Record {}\n\t {:#?})", a, b),
    }
  }
}

fn fmt_kind<T: Debug>(
  kind: &Kind<T>,
  f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
  f.debug_tuple("Kind").field(&kind.0).finish()
}

impl fmt::Display for Kind<Token> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt_kind(self, f)
  }
}

impl fmt::Display for Kind<String> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt_kind(self, f)
  }
}
