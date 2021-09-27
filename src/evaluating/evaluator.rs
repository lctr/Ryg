use std::{
  cell::{Cell, RefCell},
  collections::HashMap,
  ops::BitOr,
  rc::Rc,
};

use crate::{
  core::{
    function::Lambda,
    list::RygList,
    record::{Record, RecordKey},
    rygtype::RygType,
    rygval::{RygVal, FALSE, NIL},
  },
  lexing::token::Token,
  list_compr,
  parsing::expression::{
    Binding, Definition, Expr, Parameter, Shape, TokPattern,
  },
  util::state::Halt,
  util::types::{Either, Kind, Maybe},
};

use super::{
  builtin::{load_core, load_math},
  environment::{self, Envr, Field},
};

#[derive(Debug)]
pub struct Context<'a>(pub &'a mut Rc<RefCell<Envr>>, pub Maybe<RygVal>);

#[derive(Debug)]
pub struct Interpreter<'a> {
  global_scope: RefCell<Envr>,
  cache: Vec<&'a mut Context<'a>>,
  next_scope: Option<&'a mut Envr>,
  last: Option<Maybe<RygVal>>,
}

impl<'a> Interpreter<'a> {
  pub fn new() -> Self {
    let mut global_scope = RefCell::new(Envr::new());
    load_core(global_scope.get_mut());
    load_math(global_scope.get_mut());
    Interpreter {
      global_scope,
      cache: vec![],
      next_scope: None,
      last: None,
    }
  }
  pub fn get_by_name(
    &mut self,
    env: &mut Envr,
    name: String,
  ) -> Maybe<RygVal> {
    if let Some(e) = env.lookup(&name) {
      e.clone().get(&name).ok_or(Halt::RefGetError(name))
    } else {
      Err(Halt::RefGetError(name))
    }
  }

  pub fn get_global(&mut self) -> &mut Envr {
    self.global_scope.get_mut()
  }
  pub fn eval_error(expr: Expr) -> Halt {
    Halt::Evaluating(format!("Unable to evaluate {:?}", expr))
  }
  pub fn test_walk(&mut self, expr: Expr) {
    let my_scope = &mut self.global_scope;
    let cc = my_scope.get_mut();
    let res = walk(expr.clone(), cc);
    match res {
      Ok(rv) => {
        println!("Expr: {:#?}\n\nScopes: {:#?}\n\nResult: {:?}", expr, cc, rv)
      }
      Err(e) => println!("{:?}", e),
    }
  }
  pub fn walk(&'a mut self, expr: Expr) -> Maybe<RygVal> {
    let my_scope = &mut self.global_scope;
    let cc = my_scope.get_mut();
    let res = walk(expr.clone(), cc);
    self.next_scope = Some(cc);
    self.last = Some(res.clone());
    res
  }
}

pub fn walk(expr: Expr, mut env: &mut Envr) -> Maybe<RygVal> {
  match expr {
    Expr::Nil => Ok(NIL),
    Expr::Literal(t) => walk_literal(t, &mut env),
    Expr::Assign(o, l, r) => walk_assign(o, l, r, &mut env),
    Expr::Unary(o, r) => walk_unary(o, r, &mut env),
    Expr::Binary(o, l, r) => walk_binary(o, l, r, &mut env),
    Expr::Lambda(n, p, b) => walk_lambda(n, p, b, &mut env),
    Expr::Conditional(i, t, d) => walk_conditional(i, t, d, &mut env),
    Expr::Block(p) => walk_block(p, &mut env),
    Expr::Call(a, f, n) => walk_call(a, f, n, &mut env),
    Expr::Variable(a, b) => walk_let(a, b, &mut env),
    Expr::Tuple(b) => walk_tuple(b, &mut env),
    Expr::Vector(b) => walk_vector(b, &mut env),
    Expr::Index(b, i) => walk_index(b, i, &mut env),
    Expr::Iter(h, t) => walk_iter(h, t, &mut env),
    Expr::Range(i, a, b) => walk_range(i, a, b, &mut env),
    Expr::Loop(c, b) => walk_loop(c, b, &mut env),
    Expr::Case(p, c, d) => walk_case(p, c, d, &mut env),
    Expr::Record(k, b) => walk_record(k, b, &mut env),
    Expr::List(defn) => walk_list(*defn, &mut env),
    _ => Err(Halt::UnknownError(format!("Interrupted at {:?}", expr))),
  }
}

fn walk_assign(
  op: Token,
  l: Box<Expr>,
  r: Box<Expr>,
  env: &mut Envr,
) -> Maybe<RygVal> {
  let err = || -> Maybe<RygVal> {
    Err(Halt::RefSetError(format!(
      ",
    operator: {},
    left: {},
    right: {}",
      &op, &l, &r
    )))
  };

  if let Expr::Literal(Token::Identifier(x, _)) = *l.clone() {
    let val = walk(*(r.clone()), env)?;
    if op.match_literal("<-") {
      if let Some(scope) = env.lookup(&x) {
        match scope.set(&x, val) {
          Some(v) => Ok(v),
          None => err(),
        }
      } else {
        env.def(x, Some(&val));
        Ok(val)
      }
    } else {
      match env.set(&x, val) {
        Some(v) => Ok(v),
        None => err(),
      }
    }
  } else {
    err()
  }
}

fn walk_record(
  kind: Kind<Token>,
  bindings: Vec<Binding>,
  env: &mut Envr,
) -> Maybe<RygVal> {
  let map = &mut HashMap::new();
  // for bnd in bindings.iter() {
  //   let key_name = bnd.pram.name.to_string();
  //   let value = &RygVal::String(key_name);
  // }
  let mut pairs = vec![];
  let body = bindings.iter().for_each(|bnd| {
    let key = RecordKey::new(&RygVal::String(bnd.pram.name.to_string()))
      .unwrap()
      .clone();
    let val = if let Ok(rygval) = walk(bnd.expr.clone(), env) {
      rygval
    } else {
      RygVal::Error(Box::new(Halt::Evaluating(format!(
        "Unable to evaluate {:?}",
        bnd.expr
      ))))
    };
    map.insert(key.to_owned(), val.to_owned());
    pairs.push((key, val));
  });
  // println!("{:?}", map.clone());
  let record = RygVal::Object(Record { map: map.clone() });
  match kind.0 {
    Token::Identifier(n, ..) | Token::Meta(n, ..) => {
      env.def(n, Some(&record));
    }
    _ => {}
  };
  Ok(record)
}

fn walk_loop(
  cond: Box<Expr>,
  body: Box<Expr>,
  env: &mut Envr,
) -> Maybe<RygVal> {
  let mut scope = &mut env.extend();
  let mut value = NIL;
  let mut err: Maybe<RygVal>;
  while match walk(*cond.clone(), &mut scope) {
    Ok(r) => match r {
      RygVal::Bool(b) => b,
      RygVal::Nil() => false,
      _ => {
        err = Err(RygType::invalid_type(&r, "Bool or Nil"));
        false
      }
    },
    Err(e) => return Err(e),
  } {
    value = walk(*body.clone(), &mut scope)?
  }

  // loop {
  //   match walk(*cond.clone(), &scope)? {
  //     RygVal::Bool(true) => value = walk_block(body.clone(),
  // scope.clone())?,     _ => break,
  //   };
  // }
  Ok(value)
}

fn walk_range(
  iter: Box<Expr>,
  start: Box<Expr>,
  end: Option<Box<Expr>>,
  env: &mut Envr,
) -> Maybe<RygVal> {
  todo!()
  // let a = walk(*start, env)?;
  // let b = if let Some(expr) = end {
  //   walk(*expr, env)?
  // } else {
  //   NIL
  // };
  // match a {
  //   RygVal::Int(idx0) if b.is_int() => {
  //     let arr: Vec<RygVal> = vec![];
  //     for i in idx0..b.get_int() {
  //       arr.push(walk_index(*iter, env))
  //     }
  //   }
  // }
}

fn walk_iter(
  x: Box<Expr>,
  t: Option<Box<Expr>>,
  env: &mut Envr,
) -> Maybe<RygVal> {
  // if matches!(x.clone(), Expr::Vector(..) | Expr::Tuple(..) |
  // Expr::String(..)) {

  // }
  match walk(*x.clone(), env) {
    Ok(r) => match r {
      RygVal::String(s) => Ok(RygVal::Vector(
        s.split("")
          .map(|c| RygVal::String(c.to_owned()))
          .collect::<Vec<_>>(),
      )),
      RygVal::Vector(v) => Ok(RygVal::Vector(v)),
      _ => Err(RygType::invalid_type(&walk(*x, env)?, "Iter")),
    },
    Err(e) => Err(e),
  }
}

fn walk_index(
  body: Box<Expr>,
  index: Box<Expr>,
  env: &mut Envr,
) -> Maybe<RygVal> {
  let err = |index: RygVal, whole: RygVal| {
    Err(Halt::Evaluating(format!(
      "Invalid range error! {:?} is not a valid range for {:?}",
      &index, &whole
    )))
  };
  let body = walk(*body.clone(), env)?;
  if body.is_vector() || body.is_string() {
    let v = body.get_vector()?;
    let idx = walk(*index.clone(), env)?;
    let len = v.len();
    let m = match idx {
      RygVal::Float(q) if idx.is_float() => q.floor(),
      RygVal::Int(m) if idx.is_int() => m as f64,
      _ => return err(idx, body),
    };
    if m >= 0.0 && m < len as f64 {
      Ok(v.get(m as usize).unwrap().clone())
    } else {
      err(RygVal::Float(m), body)
    }
  } else {
    Err(Halt::Evaluating(format!(
      "Invalid range error! The object {:?} is not a vector!",
      body
    )))
  }
}

fn walk_literal(t: Token, env: &mut Envr) -> Maybe<RygVal> {
  if !t.is_identifier() {
    Ok(RygVal::from_token(t))
  } else {
    let scope = env.lookup(&t.clone().literal());
    match scope {
      Some(ctx) => ctx
        .to_owned()
        .get(&t.to_string())
        .ok_or(Halt::RefGetError(t.to_string())),
      None => Err(Halt::RefGetError(t.to_string())),
    }
  }
  // if let Token::Identifier(v, _) = t.clone() {
  //   let scope = env.lookup(v.clone());
  //   if let Some(Envr { ancestor, bindings }) = scope {
  //     if let Some(res) = bindings.get(&Field::from(t.to_owned())) {
  //       return Ok(res.clone());
  //     } else {
  //       if let Some(outer) = ancestor {
  //         if let Some(res) = outer.bindings.get(&Field::from(t.to_owned()))
  // {           return Ok(res.clone());
  //         } else {
  //           return Err(Halt::RefGetError(v));
  //         }
  //       } else {
  //         return Err(Halt::RefGetError(v));
  //       }
  //     }
  //   } else {
  //     return Err(Halt::RefGetError(v));
  //   }
  // } else {
  //   Ok(RygVal::from_token(t))
  // }
}

fn walk_case(
  p: Box<Expr>,
  c: Vec<(Expr, Expr)>,
  d: Box<Expr>,
  env: &mut Envr,
) -> Maybe<RygVal> {
  let test = walk(*p, env);
  for (pat, branch) in c.iter() {
    if walk(pat.to_owned(), env) == test {
      return walk(branch.to_owned(), env);
    }
  }
  walk(*d, env)
}

fn walk_vector(b: Vec<Expr>, env: &mut Envr) -> Maybe<RygVal> {
  let scope = env;
  let mut vector: Vec<RygVal> = vec![];
  for v in b.iter() {
    match walk(v.clone(), scope) {
      Ok(rv) => vector.push(rv),
      Err(e) => return Err(e),
    };
  }
  Ok(RygVal::Vector(vector))
}

fn walk_tuple(b: Vec<Expr>, env: &mut Envr) -> Maybe<RygVal> {
  let scope = env;
  let mut tuple: Vec<RygVal> = vec![];
  for v in b.iter() {
    match walk(v.clone(), scope) {
      Ok(rv) => tuple.push(rv),
      Err(e) => return Err(e),
    };
  }
  Ok(RygVal::Tuple(tuple.len(), tuple))
}

fn walk_list(defn: Definition, env: &mut Envr) -> Maybe<RygVal> {
  let mut scope = env.extend();
  let mut starts = vec![];
  // let mut step;
  let vars = defn.ranges.iter().map(|(var, rng)| {
    if let Expr::Literal(Token::Identifier(ident, pos)) = var {
      if let Expr::Vector(ref els) = rng {
        els.iter().enumerate().fold((RygType::Unknown, Ok(Vec::new())), |a, c| {
          let res = walk(c.1.clone(), env);
          if c.0 == 0 {
            if let Ok(val) = res.clone() {
              let kind = RygType::from(val.clone());
              let defined = scope.def(ident.to_owned(), Some(&val));
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
            (_, Ok(v)) => (RygType::from(v.clone()), a.clone().1.and_then(|mut op| {op.push(v); Ok(op)})),
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
  let err: Maybe<RygVal> = Err(Halt::InvalidInput(format!(
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
        // fixed.push(field);
      } else {
        return err;
      }
    }
  }

  // println!("vars: {:?}", &vars);
  // println!("scope: {:?}", &scope);
  Ok(RygVal::List(RygList::new(
    scope, vars, fixed, defn.conds, defn.item,
  )))
}

fn unravel(pattern: TokPattern, rygval: RygVal, mut scope: &mut Envr) {
  match (pattern, rygval.clone()) {
    (TokPattern::Empty, _) => {
      RygVal::Nil();
    }
    (TokPattern::Atom(tok), _) => {
      scope.def(tok.literal(), Some(&rygval));
    }
    (TokPattern::Tuple(vt), RygVal::Tuple(_, rhs))
    | (TokPattern::Vector(vt), RygVal::Vector(rhs)) => vt
      .iter()
      .zip(rhs.iter())
      .for_each(|(pat, val)| match (pat, val) {
        (TokPattern::Atom(x), _) => {
          &scope.def(x.literal(), Some(&val));
        }
        (TokPattern::Tuple(tu), RygVal::Tuple(_, rh))
        | (TokPattern::Vector(tu), RygVal::Vector(rh)) => {
          tu.iter().zip(rh.iter()).for_each(|(a, b)| {
            if let TokPattern::Atom(tt) = a {
              scope.def(tt.literal(), Some(&b));
            } else {
              unravel(a.to_owned(), b.to_owned(), &mut scope)
            };
          });
        }
        (TokPattern::Record(_), _) => todo!(),
        _ => todo!(),
      }),
    (TokPattern::Record(vt), RygVal::Object(rhs)) => todo!(),
    _ => todo!(),
  };
}

fn walk_let<'a, 'b: 'a>(
  defs: Vec<Binding>,
  body: Box<Expr>,
  env: &mut Envr,
) -> Maybe<RygVal> {
  let mut scope = &mut env.extend();
  for arg in defs {
    let Parameter {
      name,
      shape,
      kind,
      pattern,
    } = arg.pram.clone();
    let binding = walk(arg.expr, &mut scope);
    // let mut insertions = vec![];
    if let Ok(rval) = binding {
      match (shape, rval.clone()) {
        (Shape::Atom, _) => {
          scope.def(name.literal(), Some(&rval));
        }
        (Shape::Vector, RygVal::Vector(v))
        | (Shape::Tuple, RygVal::Tuple(_, v)) => {
          unravel(pattern, rval, scope)
          // do something about flagging mistakes later
          // let i_ = inner
          //   .iter()
          //   .zip(v.iter())
          //   .map(|(t, r)| {
          //     insertions.push(scope.def(t.literal(), Some(r)));
          //     r
          //   })
          //   .collect::<Vec<_>>();
          // // println!("{:?}\n\ninsertions: {:?}", i_, insertions);
        }
        (..) => {
          scope.def(name.literal(), Some(&rval));
        }
      }
    }
  }
  // println!("scope: {}\n\n\nenv: {}", &scope, &env);
  walk(*body, &mut scope)
}

fn walk_call(
  fnc: Box<Expr>,
  args: Vec<Expr>,
  _name: Option<String>,
  env: &mut Envr,
) -> Maybe<RygVal> {
  let fun = walk(*fnc.clone(), env)?;
  match fun {
    RygVal::Lambda(lam) => Ok(lam.call(Rc::new(args), env)?),
    RygVal::Function(fnc) => (fnc.this)(
      args
        .iter()
        .map(|x| (walk(x.clone(), env)).unwrap())
        .collect::<Vec<_>>(),
    ),
    _ => Err(RygType::invalid_type(&fun, "Function or Lambda")),
  }
}

fn walk_block(body: Vec<Expr>, mut env: &mut Envr) -> Maybe<RygVal> {
  if body.len() == 1 {
    return walk(body.clone().get(0).unwrap().to_owned(), &mut env);
  };
  let mut result = NIL;
  let scope = env;

  for expr in body {
    result = walk(expr, scope)?;
  }
  // body.iter().fold(init, f)
  Ok(result)
}

fn walk_conditional(
  cond: Box<Expr>,
  then: Box<Expr>,
  deft: Option<Box<Expr>>,
  env: &mut Envr,
) -> Maybe<RygVal> {
  let test = &mut || walk(*cond.clone(), env);
  let pass;
  if let Ok(t) = test() {
    match t {
      RygVal::Bool(b) => pass = b,
      RygVal::Nil() => pass = false,
      _ => {
        return Err(RygType::invalid_type(&walk(*cond, env)?, "Bool or Nil"))
      }
    };
  } else {
    return Err(Halt::UnknownError(format!(
      "Unknown error for {:#?}, {:#?}, {:#?}",
      cond, then, deft
    )));
  };
  if pass {
    walk(*then, env)
  } else {
    match deft {
      Some(x) => walk(*x, env),
      None => Ok(RygVal::Nil()),
    }
  }
}

fn walk_lambda(
  name: Option<String>,
  prams: Vec<Parameter>,
  body: Box<Expr>,
  mut env: &mut Envr,
) -> Maybe<RygVal> {
  let mut lam_scope = env.extend();
  let mut types: Vec<Field> = vec![];
  let _types = prams
    .iter()
    .map(|p| {
      if matches!(p.name, Token::Empty()) {
        Field::new("", &RygType::Unit)
      } else {
        let rygtype = RygType::from(p.to_owned());
        let field = Field::new(p.name.literal().as_str(), &rygtype);
        lam_scope.def(field.clone().name, None);
        field
      }
    })
    .collect::<Vec<_>>();

  let mut lam = Lambda {
    name: name.clone(),
    args: Rc::new(prams), // types),
    body,
    envr: lam_scope.to_owned(),
  };
  let lambda = RygVal::Lambda(lam.to_owned());

  if let Some(name) = &name {
    env.def(name.clone(), Some(&lambda));
    [&mut lam.envr, &mut env].iter_mut().for_each(|ctx| {
      ctx.def(name.to_owned(), Some(&lambda));
    });
  }

  Ok(lambda)
}

fn walk_binary(
  o: Token,
  l: Box<Expr>,
  r: Box<Expr>,
  env: &mut Envr,
) -> Maybe<RygVal> {
  apply_op(o, walk(*l, env)?, walk(*r, env)?)
}

fn apply_conc(left: RygVal, right: RygVal) -> Maybe<RygVal> {
  let on_both =
    |(l, r): (RygVal, RygVal), f: fn(RygVal) -> Vec<RygVal>| (f(l), f(r));
  // on_both((left, right), |rv| rv.get_vector()?);
  // on_both((left, right), |rv| rv.to_vec()?);
  if [left.clone()].iter().all(|r| r.is_vector()) {
    Ok(RygVal::Vector(
      left
        .get_vector()?
        .iter()
        .chain(right.get_vector()?.iter())
        .map(|x| x.clone())
        .collect::<Vec<_>>(),
    ))
  } else {
    if left.is_vector() {
      Ok(RygVal::Vector(
        left
          .to_vec()
          .iter()
          .chain(right.to_vec().iter())
          .map(|x| x.clone())
          .collect::<Vec<_>>(),
      ))
    } else {
      Ok(RygVal::Vector(vec![left, right]))
    }
  }
}

fn apply_op(operator: Token, left: RygVal, right: RygVal) -> Maybe<RygVal> {
  let invalid = &mut || -> Maybe<RygVal> {
    Err(Halt::InvalidInput(format!(
      "{:?} is not a valid binary operator!",
      operator.clone()
    )))
  };
  if let Token::Operator(ref op, _) = operator {
    match op.as_str() {
      "+" => left + right,
      "-" => left - right,
      "*" => left * right,
      "%" => left % right,
      "/" => left / right,
      "<" => Ok(RygVal::Bool(left < right)),
      ">" => Ok(RygVal::Bool(left > right)),
      "==" => Ok(RygVal::Bool(left == right)),
      ">=" => Ok(RygVal::Bool(left >= right)),
      "<=" => Ok(RygVal::Bool(left <= right)),
      "&" => Ok(RygVal::Byte(left.get_byte()? & right.get_byte()?)),
      "|" => Ok(RygVal::Byte(left.get_byte()? | right.get_byte()?)),
      "^" => Ok(RygVal::Byte(left.get_byte()? ^ right.get_byte()?)),
      "<>" => apply_conc(left, right),
      "++" => (|l: RygVal, r: RygVal| {
        return Ok((|arr: Vec<RygVal>| {
          if matches!(
            (l.clone(), r.clone()),
            (RygVal::Tuple(..), RygVal::Tuple(..))
          ) {
            return RygVal::Tuple(arr.len(), arr);
          } else {
            return RygVal::Vector(arr);
          }
        })(
          l.to_vec()
            .iter()
            .chain(r.to_vec().iter())
            .map(|v| v.clone())
            .collect::<Vec<_>>(),
        ));
      })(left.clone(), right.clone()),
      _ => invalid(),
    }
  } else {
    invalid()
  }
}

fn apply_append(left: RygVal, right: RygVal) {
  // if left.is
}

fn walk_unary(o: Token, r: Box<Expr>, env: &mut Envr) -> Maybe<RygVal> {
  let right = &mut || walk(*r.clone(), env);
  match &o {
    Token::Operator(op, pos) => match op.as_str() {
      "-" => apply_op(
        Token::Operator(String::from("-"), pos.clone()),
        RygVal::Float(0.0),
        right()?,
      ),
      "!" => apply_op(
        Token::Operator(String::from("=="), pos.clone()),
        FALSE,
        right()?,
      ),
      _ => Err(Halt::InvalidInput(format!(
        "Unable to handle unary operator {:?}",
        o
      ))),
    },
    _ => Err(Halt::Evaluating(format!(
      "Unable to evaluate unary expression with {:?} and {:?}",
      o, *r
    ))),
  }
}

#[cfg(test)]
mod test {
  use std::rc::Rc;

  use crate::{
    core::{rygtype::RygType, rygval::RygVal},
    evaluating::{
      environment::{Envr, Field},
      evaluator::walk,
    },
    parsing::parser::parse_input,
    util::types::{Either, Maybe},
  };

  use super::Interpreter;

  fn run(src: &str) -> Maybe<RygVal> {
    println!("src: \n> {}\n", src.clone());
    let ast = parse_input(src);
    let mut env = Envr::new();
    let val = walk(ast, &mut env);
    println!(
      "> {:?}\n\nenv: {:?}\n",
      match val.clone() {
        Ok(v) => v,
        Err(e) => RygVal::Error(Box::new(e)),
      },
      env
    );
    val
  }

  #[test]
  fn basic_arithmetic() {
    let src = "3 + 4";
    let result = run(src);
    [("3 + 4", "7"), ("1 + 4 / 2 - 3 * 5", "-12")]
      .map(|(x, y)| assert_eq!(run(x), run(y)));

    let _ = run("100 / 50");
  }

  #[test]
  fn test_conditional() {
    let src = "if case 3 * 3 + 4 of {
      _ => false
    } then 1 else 2;";
    let result = run(src);
    assert_eq!(result.unwrap(), RygVal::Int(2))
  }

  #[test]
  fn test_assignment() {
    let src = "x = 4; x; y = x";
    let result = run(src);
    assert_eq!(result.unwrap(), RygVal::Float(4.0))
  }

  #[test]
  fn test_lambda() {
    let src = "(fn summation |sum| sum + 1)(3.4)";
    let result = run(src);
    println!("{}", result.clone().unwrap());
    assert_eq!(result.unwrap(), RygVal::Float(4.4))
  }

  #[test]
  fn test_cases() {
    let src = "case 1 + 2 of {
      30 => -5,
      4 => 6,
      _ => 7
    };";
    run(src);
  }

  #[test]
  fn test_assign() {
    let src = "
    let x' = 2, 
      y = 3.0,
    in {
      x' = 2 * y;
      x'
    }; fn sum |a, b| a + b;";
    let ast = parse_input(src);
    // run(src);
    let mut interpreter = Interpreter::new();
    interpreter.test_walk(ast)
  }

  #[test]
  fn test_let_pats() {
    let src = "let [a, b] = [0, 1] in a + b;";
    let ast = parse_input(src);
    let mut interpreter = Interpreter::new();
    interpreter.test_walk(ast)
  }

  #[test]
  fn test_index() {
    let src = "[[1], 2, 3][0][0]";
    run(src);
    let mut env = Envr::new();
    let key = String::from("foo");
    env.def(key.clone(), Some(&RygVal::String(String::from("bar"))));
    let mut scope = env.extend();
    println!("{:?}, {:?}", scope.clone(), scope.get(&String::from("foo")));

    let res = walk(parse_input("foo = \"hi\"; foo"), &mut scope);
    println!("{:?}      ... {:#?}", &res, scope);
    let val = Envr::set(&mut scope, &key, res.clone().expect("Asd"));
    println!("{:?}      ... {:#?}", res, scope);
    println!("{:?}", val)
  }

  #[test]
  fn test_conc() {
    println!("{:?}", run("1 <> 2"));
    let src = "
      let a = 2, b = 3, c = 4
      in a <> b <> c";
    run(src);
  }

  #[test]
  fn test_record() {
    let src = "#{a -> \"cat\", is -> \"cute\"};";
    println!("{:?}", run(src));
  }

  #[test]
  fn test_interpreter() {
    let intr = Interpreter::new();
    println!("{:?}", intr);
    let ctx = intr.global_scope;
    let ast = parse_input("[a | a <- [1, 2, 3], a > 0]");
    let aa = &mut *ctx.borrow_mut();
    println!("{:#?}", walk(ast, aa).unwrap());

    // println!("{:?}", aa)
  }
}
