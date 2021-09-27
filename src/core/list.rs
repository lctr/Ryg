use std::{collections::HashMap, ops::Add, rc::Rc};

use crate::{
  evaluating::{
    environment::{Envr, Field},
    evaluator::walk,
  },
  lexing::token::Token,
  parsing::expression::{Definition, Expr, Shape},
  util::{
    state::Halt,
    types::{Maybe, Name},
  },
};

use super::{function::RygFn, rygtype::RygType, rygval::RygVal};

#[macro_export]
macro_rules! list_compr {
  ($id1: ident | $id2: ident <- [$start: expr , $end: expr] , $cond: expr) => {{
    let mut vec = Vec::new();

    for num in $start..$end + 1 {
      if $cond(num) {
        vec.push(num);
      }
    }

    vec
  }};
}

#[derive(Clone, Debug)]
pub struct Element(pub usize, pub RygVal);

#[derive(Clone, Debug)]
pub struct Variable<X, Y, Z> {
  pub ident: X,
  pub index: usize,
  pub start: Y,
  pub limit: Y,
  pub value: Z,
}

#[derive(Clone, Debug)]
pub struct Range<R>(pub usize, pub Option<usize>, pub Option<R>);

#[derive(Clone, Debug)]
pub struct RygList {
  vars: Vec<Range<(RygType, Maybe<Vec<RygVal>>)>>,
  ranges: Vec<usize>,
  index: Option<usize>,
  start: usize,
  limit: Option<usize>,
  scope: Envr,
  pub kind: RygType,
  current: Option<Rc<Element>>,
  conds: Vec<Expr>,
  item: Expr,
}

impl RygList {
  pub fn new(
    env: Envr,
    vars: Vec<(RygType, Maybe<Vec<RygVal>>)>,
    fixed: Vec<(Field, RygVal)>,
    conds: Vec<Expr>,
    item: Expr,
  ) -> Self {
    let mut kind = RygType::Unknown;
    // let mut limit = vars.len();
    let iters = vars.clone();
    let vars = vars
      .iter()
      .map(|(a, b)| {
        let start = 0;
        let len = b
          .clone()
          .and_then(|v| Ok(v.len()))
          .map_or(None, |lim| Some(lim));
        let body = Some((a.to_owned(), b.to_owned()));
        Range(start, len, body)
        // Range(0, b.and_then(|o| o.len())), (a.to_owned(), b.to_owned())
      })
      .collect::<Vec<_>>();
    let ranges = iters
      .clone()
      .iter()
      .filter_map(|(rt, rv)| {
        kind = rt.clone();
        rv.to_owned().ok()
      })
      .map(|v| v.len())
      .collect::<Vec<_>>();

    Self {
      vars,
      ranges,
      index: Some(0),
      start: 0,
      limit: None,
      scope: env,
      current: None,
      conds,
      kind,
      item,
    }
  }
}

// impl Iterator for RygList {
//   type Item = RygVal;
//   fn next(&mut self) -> Option<Self::Item> {
//     match (self.current, self.index) {}
//   }
// }

pub trait Walk {
  fn walk<X, Y, Z>(node: X, scope: Y, cont: Z) -> (Y, Z);
}

fn _walk_list(defn: Definition, env: &mut Envr) -> Maybe<RygVal> {
  let Definition {
    // expression to be called iteratively
    item,
    ranges: range,
    fixed,
    conds,
  } = defn;
  println!("{:?}", (item, range, fixed, conds));
  Ok(RygVal::Bool(true))
}

fn walk_list(defn: Definition, env: &mut Envr) -> Maybe<RygVal> {
  let mut scope = env.extend();
  let mut starts = vec![];
  let vars = defn.ranges.iter().map(|(var, rng)| {
    if let Expr::Literal(Token::Identifier(ident, pos)) = var {
      if let Expr::Vector(ref els) = rng {
        els.iter().enumerate().fold((RygType::Unknown, Ok(Vec::new())), |a, c| {
          let res = walk(c.1.clone(), env);
          if c.0 == 0 {
            if let Ok(val) = res.clone() {
              let kind = RygType::from(val.clone());
              let fld = Field::new(ident, &kind);
              let defined = scope.def(fld.name, Some(&val));
              if defined.is_none() {
                starts.push((kind, val))
              };
            };
          };
          match (a.clone(), res) {
            ((a_t, Ok(mut a_vs)), Ok(rv)) => {
              if RygType::from(rv.clone()) == a_t.clone() || matches!(a_t, RygType::Any | RygType::Unknown) {
                a_vs.push(rv);
                return (a_t.clone(), Ok(a_vs));
              } else {
                (a_t.clone(), Err(Halt::InvalidType(format!("List requires all types to be the same! Expected type {}, but found {} {:?}", a_t, rv, pos))))
              }
            }
            (_, Ok(v)) => (a.clone().0, a.clone().1.and_then(|mut op| {op.push(v); Ok(op)})),
            (_, Err(e)) => (a.clone().0, Err(e)),
          }
        })
      } else  // if let Expr::Literal(Token::Number(n, b, _)) = var
       {
        (RygType::Halt, Err(Halt::InvalidInput(format!("The provided token {} is not a valid generator identifier!", var))))
      }
    } else {
      (RygType::Halt, Err(Halt::InvalidInput(format!("The provided token {} is not a valid generator identifier!", var))))
    }
  }).collect::<Vec<_>>();
  let mut err: Maybe<RygVal> = Err(Halt::InvalidInput(format!(
    "Unable to assign non-variables as constant! {:?}",
    defn
  )));
  let mut fixed = vec![];
  for (cnst, base) in defn.fixed.iter() {
    let evaled = walk(base.to_owned(), &mut scope);
    if let Ok(rv) = evaled {
      if let Expr::Literal(Token::Identifier(n, _)) = cnst {
        let field = Field {
          name: n.to_string(),
          kind: RygType::from(rv.clone()),
        };
        if let Some(x) = scope.def(n.to_owned(), Some(&rv)) {
          fixed.push((field, x));
        };
      } else {
        return Err(Halt::InvalidInput(format!(
          "Unable to assign non-variables as constant! {:?}",
          defn
        )));
      }
    }
  }

  println!("vars: {:?}", &vars);
  println!("scope: {:?}", &scope);
  Ok(RygVal::List(RygList::new(
    scope, vars, fixed, defn.conds, defn.item,
  )))
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::parsing::parser::parse_input;
  use crate::{
    evaluating::{
      environment::Envr,
      evaluator::{walk, Interpreter},
    },
    util::types::Either,
  };

  #[test]
  fn inspect_list_1<'a, 'b: 'a>() {
    let expr = parse_input("[x | x <- [1, 2, 3]]");
    let interpreter = Interpreter::new();
    let env = &mut Envr::new();
    let list = walk(expr, env);
    println!("{:#?}", list)
  }
}
