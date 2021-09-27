use std::{fmt::Display, rc::Rc};

use crate::{
  evaluating::{
    environment::{Envr, Field},
    evaluator::walk,
  },
  parsing::expression::{Expr, Parameter, Shape},
  util::{state::Halt, types::Maybe},
};

use super::{rygtype::RygType, rygval::RygVal};

#[derive(Clone, Debug, PartialEq)]
pub struct Lambda {
  pub name: Option<String>,
  pub args: Rc<Vec<Parameter>>,
  pub body: Box<Expr>,
  pub envr: Envr,
}

impl Lambda {
  pub fn new(
    name: Option<String>,
    args: Vec<Parameter>,
    body: Expr,
    envr: Envr,
  ) -> Self {
    Lambda {
      name,
      args: Rc::new(args),
      body: Box::new(body),
      envr,
    }
  }
  // pub fn arg_types(args: Vec<Parameter>) {}
  pub fn set_envr(&mut self, envr: Envr) {
    self.envr = envr.clone();
  }

  // pub fn validate_args(&self, args: Rc<Vec<Expr>>) {
  //   self.args.iter().map(|p| {
  //     p.
  //   })
  // }

  pub fn call(
    &self,
    args: Rc<Vec<Expr>>,
    env: &mut Envr,
  ) -> Result<RygVal, Halt> {
    let scope = &mut env.extend();
    if self.args.len() != args.len() {
      return Err(Halt::InvalidInput(format!(
        "mismatch between prams {:?} and args {:?}",
        self.args, args
      )));
    };
    for (p, a) in self.args.iter().zip(args.iter()) {
      let res = walk(a.to_owned(), scope)?;
      scope.def(p.clone().name.to_string(), Some(&res));
    }
    let b = self.body.clone();
    walk(*b, &mut scope.to_owned())
  }
}

impl Display for Lambda {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    let name = if let Some(name) = &self.name {
      name.as_str()
    } else {
      "ʎ"
    };
    let args = self
      .args
      .iter()
      .map(|p| p.clone().name.to_string())
      .collect::<Vec<_>>()
      .join(", ");
    let result = &self.body;
    write!(f, "{}: {} -> {:?}", name, args, result)
  }
}

#[derive(Clone, PartialEq)]
pub struct RygFn {
  pub name: String,
  pub meta_in: (Shape, Rc<Vec<RygType>>),
  pub meta_out: Option<(Shape, Rc<RygType>)>,
  pub this: fn(Vec<RygVal>) -> Maybe<RygVal>,
}

impl RygFn {
  pub fn new(
    name: &str,
    meta_in: (Shape, Vec<RygType>),
    meta_out: Option<(Shape, RygType)>,
    this: fn(args: Vec<RygVal>) -> Maybe<RygVal>,
  ) -> RygFn {
    RygFn {
      name: name.to_owned(),
      meta_in: (meta_in.0, Rc::new(meta_in.1)),
      meta_out: if let Some((sh, rt)) = meta_out {
        Some((sh, Rc::new(rt)))
      } else {
        None
      },
      this,
    }
  }
  pub fn validate_args(&self, args: &Vec<RygVal>) -> bool {
    let (shape, types) = &self.meta_in;
    if matches!(shape, &Shape::Unknown | &Shape::Empty | &Shape::R) {
      true
    } else {
      types.iter().zip(args.iter()).all(|(rygtype, rygval)| {
        rygtype.matches_variant(&RygType::from(rygval.clone())).0
      })
    }
  }
  pub fn call(&self, args: Vec<RygVal>) -> Maybe<RygVal> {
    let f = self.this;
    if self.validate_args(&args) {
      f(args)
    } else {
      Err(Halt::InvalidInput(format!(
        "Function {:?} expects {:?} arguments, but was provided {:?}",
        self.name, self.meta_in.1, args
      )))
    }
  }
  // pub fn apply<'a, D, E, F>(
  //   &self,
  //   scope: &mut Envr,
  //   args: E,
  //   cont: F,
  // ) -> (&'a mut Envr, Maybe<D>)
  // where
  //   D: Clone,
  //   E: Iterator<Item = D>,
  //   F: FnMut(&'a mut Envr) -> (&'a mut Envr, Maybe<D>), {
  //   args.iter
  // }

  pub fn as_rygval(self) -> RygVal {
    RygVal::from(self)
  }
  fn check_args(&self, args: Vec<RygVal>) {}
}

impl From<RygFn> for RygVal {
  fn from(rygfn: RygFn) -> Self {
    RygVal::Function(rygfn)
  }
}

pub trait Apply {
  // type Applicative =
  fn get() {}
  fn walk() {}
}

mod test {
  use crate::{core::rygtype::RygType, parsing::parser::parse_input};

  use super::*;

  fn assert_annotated_lambda_prams(src: &str, rygtype: RygType) {
    let ast = parse_input(src);
    if let Expr::Block(mut x) = ast.clone() {
      if let Some(y) = x.pop() {
        if let Expr::Lambda(_a, b, _c) = y {
          let types = b
            .iter()
            .map(|p| RygType::from(p.clone()))
            .collect::<Vec<_>>();
          println!("parsed: {:#?}", ast);
          println!("typing: {:?}", &types);
          assert_eq!(types, vec![rygtype])
        }
      }
    }
  }

  #[test]
  fn parameter_types() {
    let src = "|[a, b]: [Int, Int]| a + b";
    let rygtype = RygType::Vector(vec![RygType::Int, RygType::Int]);
    assert_annotated_lambda_prams(src, rygtype)
  }
}
