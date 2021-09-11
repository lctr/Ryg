use std::rc::Rc;

use crate::{
  lexing::token::Token,
  parsing::{expression::Expr, parser::Defn},
};

use super::{
  environment::Envr,
  values::{Lambda, RygVal, EMPTY, FALSE, NIL, TRUE},
};

pub fn walk<'a>(expr: Expr, scope: &Envr<'a>) -> RygVal {
  // let v =
  let env = scope.clone();
  match expr {
    Expr::Literal(t) => walk_literal(t, env),
    Expr::Assign(o, l, r) => walk_assign(l, r, env),
    Expr::Unary(o, r) => walk_unary(o, r, env),
    Expr::Binary(o, l, r) => walk_binary(o, l, r, env),
    Expr::Lambda(n, p, b) => walk_lambda(n, p, b, env),
    Expr::Conditional(i, t, d) => walk_conditional(i, t, d, env),
    Expr::Block(p) => walk_block(p, env),
    Expr::Call(a, f) => walk_call(a, f, env),
    Expr::Variable(a, b) => walk_let(a, b, env),
    Expr::Tuple(b) => walk_tuple(b, env),
    Expr::Vector(b) => walk_vector(b, env),
    Expr::Index(b, i) => walk_index(b, i, env),
    Expr::Iter(x) => walk_iter(x, env),
    Expr::Range(i, a, b) => walk_range(i, a, b, env),
    Expr::Loop(c, b) => walk_loop(c, b, &env),
    Expr::Case(p, c, d) => walk_case(p, c, d, env),
    _ => {
      panic!("Interrupted at {:?}", expr)
    }
  }
  // v
}

fn walk_loop<'a>(c: Box<Expr>, b: Vec<Expr>, env: &Envr) -> RygVal {
  todo!()
}

fn walk_range<'a>(
  i: Box<Expr>,
  a: Box<Expr>,
  b: Option<Box<Expr>>,
  ref mut env: Envr<'a>,
) -> RygVal {
  todo!()
}

fn walk_iter<'a>(x: Box<Expr>, ref mut env: Envr<'a>) -> RygVal {
  todo!()
}

fn walk_index<'a>(
  b: Box<Expr>,
  i: Box<Expr>,
  ref mut env: Envr<'a>,
) -> RygVal {
  todo!()
}

fn walk_literal<'a>(t: Token, ref mut env: Envr<'a>) -> RygVal {
  if let Token::Identifier(v) = t {
    match env.get(&v) {
      Some(x) => return x.clone(),
      None => NIL,
    }
  } else {
    RygVal::from_token(t)
  }
}

fn walk_case<'a>(
  p: Box<Expr>,
  c: Vec<(Expr, Expr)>,
  d: Box<Expr>,
  env: Envr<'a>,
) -> RygVal {
  let test = walk(*p, &env);
  for (pat, branch) in c.iter() {
    if walk(pat.clone(), &env) == test {
      return walk(branch.to_owned(), &env);
    }
  }
  walk(*d, &env)
}

fn walk_vector<'a>(b: Vec<Expr>, ref mut env: Envr<'a>) -> RygVal {
  todo!()
}

fn walk_tuple<'a>(b: Vec<Expr>, ref mut env: Envr<'a>) -> RygVal {
  let scope = env.clone();
  walk_vector(b, scope)
}

fn walk_let<'a>(a: Vec<Defn>, b: Box<Expr>, ref mut env: Envr<'a>) -> RygVal {
  // let mut scope = env.extend();
  // for arg in a {
  //   scope
  // }
  todo!()
}

fn walk_call<'a>(f: Box<Expr>, a: Vec<Expr>, env: Envr<'a>) -> RygVal {
  let fun = walk(*f.clone(), &env);
  if let RygVal::Lambda(lam) = fun {
    lam.call(a, &mut env.clone())
  } else {
    panic!("{:?} is not callable!", f)
  }
  // fun
}

fn walk_block<'a>(p: Vec<Expr>, ref mut env: Envr<'a>) -> RygVal {
  let mut result = RygVal::Nil();
  let scope = env.clone();
  for expr in p {
    result = walk(expr, &scope);
  }
  result
}

fn walk_conditional<'a>(
  i: Box<Expr>,
  t: Box<Expr>,
  d: Option<Box<Expr>>,
  ref mut env: Envr<'a>,
) -> RygVal {
  if match walk(*i, env) {
    RygVal::Bool(b) => b,
    _ => false,
  } {
    walk(*t, env)
  } else {
    match d {
      Some(x) => walk(*x, env),
      None => RygVal::Nil(),
    }
  }
}

impl Lambda {
  pub fn call<'a>(&self, args: Vec<Expr>, env: &'a mut Envr) -> RygVal {
    let mut scope = env.extend();
    if self.prams.len() != args.len() {
      panic!(
        "mismatch between prams {:?} and args {:?}",
        self.prams, args
      )
    };
    for (p, a) in self.prams.iter().zip(args.iter()) {
      scope.def(p.to_owned(), walk(a.to_owned(), &scope));
    }
    let b = self.body.clone();
    walk(*b, &scope)
  }
}

fn walk_lambda<'a>(
  name: Option<String>,
  prams: Vec<String>,
  body: Box<Expr>,
  env: Envr<'a>,
) -> RygVal {
  RygVal::Lambda(Lambda {
    name,
    prams: Rc::new(prams),
    body,
  })
}

fn walk_binary<'a>(
  o: Token,
  l: Box<Expr>,
  r: Box<Expr>,
  ref mut env: Envr<'a>,
) -> RygVal {
  apply_op(o, walk(*l, env), walk(*r, env))
}

fn apply_op<'a>(operator: Token, left: RygVal, right: RygVal) -> RygVal {
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

fn walk_unary<'a>(o: Token, r: Box<Expr>, ref mut env: Envr<'a>) -> RygVal {
  let right = || walk(*r.clone(), env);
  match &o {
    Token::Operator(op) => match op.as_str() {
      "-" => apply_op(
        Token::Operator(String::from("-")),
        RygVal::Float(0.0),
        right(),
      ),
      "!" => apply_op(Token::Operator(String::from("==")), FALSE, right()),
      _ => panic!("Unable to handle unary operator {:?}", o),
    },
    _ => panic!(
      "Unable to evaluate unary expression with {:?} and {:?}",
      o, *r
    ),
  }
}

fn walk_assign<'a>(
  l: Box<Expr>,
  r: Box<Expr>,
  ref mut env: Envr<'a>,
) -> RygVal {
  if let Expr::Literal(Token::Identifier(x)) = *l {
    let val = walk(*r, env);
    env.set(x, val.clone());
    println!("[assign] env: {:?}", &env);
    val
    // env.set(x, walk(*r, env))
  } else {
    panic!("Unable to set value to undefined variable {:?}", l)
  }
}

#[cfg(test)]
mod test {
  use crate::{
    evaluating::{environment::Envr, evaluator::walk, values::RygVal},
    lexing::token::Token,
    parsing::{expression::Expr, parser::get_ast},
  };
  fn run(src: &str) -> Option<RygVal> {
    let ast = get_ast(src);
    let env = Envr::new();
    let val = walk(ast, &env);
    println!("> {:?}\nenv:\n {:?}", &val, env);
    Some(val)
  }

  #[test]
  fn sum_floats() {
    let src = "3 + 4";
    let result = run(src);
    assert_eq!(result.unwrap(), RygVal::Float(7.0))
  }

  #[test]
  fn test_conditional() {
    let src = "if false then 1 else 2";
    let result = run(src);
    assert_eq!(result.unwrap(), RygVal::Float(2.0))
  }

  #[test]
  fn test_assignment() {
    let src = "x = 4; x; y = x";
    let result = run(src);
    assert_eq!(result.unwrap(), RygVal::Float(4.0))
  }

  #[test]
  fn test_lambda() {
    let src = "(|sum| sum + 1)(0)";
    let result = run(src);
    println!("{:#?}", result.unwrap())
    // assert_eq!(result.unwrap(), RygVal::Float( 4.0))
  }

  #[test]
  fn test_cases() {
    let src = "case 1 + 2 of {
      3 => -5,
      4 => 6,
      _ => 7
    };";
    let result = run(src);
    println!("{:?}", result.unwrap())
  }

  #[test]
  fn test_assign() {
    let src = " ";
    let result = run(src);
    println!("{:?}", result.unwrap())
  }
}
