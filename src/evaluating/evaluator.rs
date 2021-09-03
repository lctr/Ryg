use std::rc::Rc;

use crate::{
  lexing::token::Token,
  parsing::{expression::Expr, parser::Defn},
};

use super::{
  environment::Envr,
  values::{Lambda, RygVal},
};

pub fn walk(expr: Expr, env: Envr) -> RygVal {
  let v = match expr {
    Expr::Literal(t) => walk_literal(t, &env),
    Expr::Assign(o, l, r) => walk_assign(l, r, &env),
    Expr::Unary(o, r) => walk_unary(o, r, &env),
    Expr::Binary(o, l, r) => walk_binary(o, l, r, &env),
    Expr::Lambda(n, p, b) => walk_lambda(n, p, b, &env),
    Expr::Conditional(i, t, d) => walk_conditional(i, t, d, &env),
    Expr::Block(p) => walk_block(p, &env),
    Expr::Call(a, f) => walk_call(a, f, &env),
    Expr::Variable(a, b) => walk_let(a, b, &env),
    Expr::Tuple(b) => walk_tuple(b, &env),
    Expr::Vector(b) => walk_vector(b, &env),
    Expr::Index(b, i) => walk_index(b, i, &env),
    Expr::Iter(x) => walk_iter(x, &env),
    Expr::Range(i, a, b) => walk_range(i, a, b, &env),
    Expr::Loop(c, b) => walk_loop(c, b, &env),
    Expr::Case(p, c, d) => walk_case(p, c, d, &env),
    _ => {
      panic!("Interrupted at {:?}", expr)
    }
  };
  v
}

fn walk_loop(c: Box<Expr>, b: Vec<Expr>, env: &Envr) -> RygVal {
  todo!()
}

fn walk_range(
  i: Box<Expr>,
  a: Box<Expr>,
  b: Option<Box<Expr>>,
  env: &Envr,
) -> RygVal {
  todo!()
}

fn walk_iter(x: Box<Expr>, env: &Envr) -> RygVal {
  todo!()
}

fn walk_index(b: Box<Expr>, i: Box<Expr>, env: &Envr) -> RygVal {
  todo!()
}

fn walk_literal(t: Token, env: &Envr) -> RygVal {
  RygVal::from_token(t)
}

fn walk_case(
  p: Box<Expr>,
  c: Vec<(Expr, Expr)>,
  d: Box<Expr>,
  env: &Envr,
) -> RygVal {
  let test = match p {
    x => {
      let node = x.clone();
      return walk(*node, env.clone());
    }
    _ => RygVal::Nil(),
  };
  for (pat, branch) in c.iter() {
    if walk(pat.to_owned(), env.clone()) == walk(*p.clone(), env.clone()) {
      return walk(branch.clone(), env.clone());
    }
  }
  walk(*d, env.clone())
}

fn walk_vector(b: Vec<Expr>, env: &Envr) -> RygVal {
  if b.len() > 0 {
    RygVal::Vector(b.iter().map(|x| walk(x.clone(), env.clone())).collect())
  } else {
    RygVal::Vector(vec![RygVal::Nil()])
  }
}

fn walk_tuple(b: Vec<Expr>, env: &Envr) -> RygVal {
  walk_vector(b, env)
}

fn walk_let(a: Vec<Defn>, b: Box<Expr>, env: &Envr) -> RygVal {
  // let mut scope = env.extend();
  // for arg in a {
  //   scope
  // }
  todo!()
}

fn walk_call(a: Box<Expr>, f: Vec<Expr>, env: &Envr) -> RygVal {
  todo!()
}

fn walk_block(p: Vec<Expr>, env: &Envr) -> RygVal {
  let mut result = RygVal::Nil();
  for expr in p {
    result = walk(expr, env.clone());
  }
  result
}

fn walk_conditional(
  i: Box<Expr>,
  t: Box<Expr>,
  d: Option<Box<Expr>>,
  env: &Envr,
) -> RygVal {
  if match walk(*i, env.clone()) {
    RygVal::Nil() => false,
    RygVal::Bool(b) => b,
    _ => true,
  } {
    walk(*t, env.clone())
  } else {
    match d {
      Some(x) => walk(*x, env.clone()),
      None => RygVal::Nil(),
    }
  }
}

fn walk_lambda(
  n: Option<String>,
  p: Vec<String>,
  b: Box<Expr>,
  env: &Envr,
) -> RygVal {
  RygVal::Lambda(Lambda {
    name: n,
    prams: Rc::new(p),
    body: Rc::new(*b),
  })
}

fn walk_binary(o: Token, l: Box<Expr>, r: Box<Expr>, env: &Envr) -> RygVal {
  apply_op(o, walk(*l, env.clone()), walk(*r, env.clone()), env)
}

fn apply_op(
  operator: Token,
  left: RygVal,
  right: RygVal,
  env: &Envr,
) -> RygVal {
  if let Token::Operator(op) = operator {
    match op.as_str() {
      "+" => RygVal::Float(left.get_float() + right.get_float()),
      "-" => RygVal::Float(left.get_float() - right.get_float()),
      "*" => RygVal::Float(left.get_float() * right.get_float()),
      "/" => RygVal::Float({
        let r = right.get_float();
        if r == 0.0 {
          panic!("Division by zero!")
        } else {
          left.get_float() / right.get_float()
        }
      }),
      "<" => RygVal::Bool(left.get_float() < right.get_float()),
      ">" => RygVal::Bool(left.get_float() > right.get_float()),
      "==" => RygVal::Bool(left == right),
      ">=" => RygVal::Bool(left.get_float() >= right.get_float()),
      "<=" => RygVal::Bool(left.get_float() <= right.get_float()),
      _ => RygVal::Nil(),
    }
  } else {
    panic!("Invalid operator {:?}", operator)
  }
}

fn walk_unary(o: Token, r: Box<Expr>, env: &Envr) -> RygVal {
  todo!()
}

fn walk_assign(l: Box<Expr>, r: Box<Expr>, env: &Envr) -> RygVal {
  todo!()
}

#[cfg(test)]
mod test {
  use crate::{
    evaluating::{
      environment::Envr,
      evaluator::{apply_op, walk},
      values::RygVal,
    },
    lexing::token::Token,
    parsing::{expression::Expr, parser::get_ast},
  };
  fn run(src: &str) -> Option<RygVal> {
    let ast = get_ast(src);
    let env = Envr::new();
    let val = walk(ast, env);
    println!("{:?}", &val);
    Some(val)
  }

  #[test]
  fn sum_floats() {
    let src = "3 + 4";
    let result = run(src);
    assert_eq!(result.unwrap(), RygVal::Float(7.0))
  }
}
