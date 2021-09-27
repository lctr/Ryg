use std::rc::Rc;

use super::environment::Envr;

use crate::{
  core::{
    function::RygFn,
    rygtype::RygType,
    rygval::{RygVal, NIL},
  },
  evaluating::environment::Field,
  parsing::expression::Shape,
  util::{constant::PI, state::Halt, types::Maybe},
};

pub fn get_type(args: Vec<RygVal>) -> Maybe<RygVal> {
  if args.len() > 0 {
    let res = args
      .clone()
      .iter()
      .map(|arg| RygVal::Symbol(arg.type_string()))
      .collect::<Vec<RygVal>>();
    if args.len() == 1 {
      Ok(res.get(0).unwrap().to_owned())
    } else {
      Ok(RygVal::Vector(res.to_owned()))
    }
  } else {
    return Err(Halt::InvalidInput(
      "Expected 1 argument, but was provided none!".to_string(),
    ));
  }
}

pub fn load_core(env: &mut Envr) {
  let type_of = RygFn::new(
    "type'of",
    (Shape::Holder, vec![RygType::Any]),
    Some((Shape::Atom, RygType::Symbol(String::from("Type")))),
    get_type,
  );

  let length = RygFn::new(
    "len'",
    (Shape::Vector, vec![RygType::Vector(vec![RygType::Any])]),
    Some((Shape::Atom, RygType::Int)),
    |args| {
      match args.len() {
        1 => {
          if let Some(rv) = args.get(0) {
            match rv {
              RygVal::String(st) | RygVal::Symbol(st) => {
                Ok(RygVal::from(st.len()))
              }
              RygVal::Vector(vs) | RygVal::Tuple(_, vs) => {
                Ok(RygVal::from(vs.len()))
              }
              // RygVal::List(_) => todo!(),
              RygVal::Lambda(l) => Ok(RygVal::from(l.args.len())),
              RygVal::Error(h) => Err(*h.clone()),
              RygVal::Function(rf) => Ok(RygVal::from(rf.meta_in.1.len())),
              RygVal::Object(rc) => Ok(RygVal::from(rc.map.len())),
              _ => Err(Halt::InvalidType(format!(
                "Cannot take the length of {}",
                rv
              ))),
            }
          } else {
            Ok(RygVal::from(0))
          }
        }
        _ => {
          return Err(Halt::InvalidInput(
            "Expected 1 argument, but was provided none!".to_string(),
          ));
        }
      }
    },
  );

  let print_0 = RygFn::new(
    "print",
    (Shape::R, vec![RygType::R]),
    Some((Shape::Atom, RygType::R)),
    |args| {
      if let Some(rv) = args.get(0) {
        println!("{}", rv);
        Ok(rv.to_owned())
      } else {
        return Err(Halt::InvalidInput(
          "Expected 1 argument, but was provided none!".to_string(),
        ));
      }
    },
  );

  let print_ln = RygFn::new(
    "print'ln",
    (Shape::R, vec![RygType::R]),
    Some((Shape::Empty, RygType::Nil)),
    |args| {
      args.iter().for_each(|a| println!("{}", a));
      Ok(RygVal::Nil())
    },
  );

  let to_string = RygFn::new(
    "to'string",
    (Shape::R, vec![RygType::R]),
    Some((Shape::Atom, RygType::String)),
    |arg| {
      if arg.len() > 0 {
        if let Some(rv) = arg.get(0) {
          Ok(RygVal::String(rv.clone().to_string()))
        } else {
          Ok(RygVal::String(String::new()))
        }
      } else {
        Ok(RygVal::String(String::new()))
      }
    },
  );

  let print_env = RygFn::new(
    "print'env",
    (Shape::R, vec![RygType::R]),
    Some((Shape::Empty, RygType::Nil)),
    |args| {
      args.iter().for_each(|a| println!("{:?}", a));
      Ok(RygVal::Nil())
    },
  );

  [("nil", NIL)].iter().for_each(|(name, val)| {
    env.def(String::from(*name), Some(val));
  });

  [type_of, length, print_0, print_ln, to_string]
    .iter()
    .for_each(|f| {
      env.def(f.clone().name, Some(&RygVal::Function(f.to_owned())));
    });
}

pub fn load_math(env: &mut Envr) {
  let math_sin = RygFn::new(
    "sin'",
    (Shape::Atom, vec![RygType::Float]),
    Some((Shape::Atom, RygType::Float)),
    |a| {
      if let Some(n) = a.get(0) {
        match n {
          RygVal::Float(q) => Ok(RygVal::Float(q.sin())),
          RygVal::Int(q) => Ok(RygVal::Float((q.clone() as f64).sin())),
          x => Err(RygType::invalid_type(x, "Int or Float")),
        }
      } else {
        Err(Halt::InvalidInput(format!(
          "Function {:?} expects {:?} arguments, but {:?} was/were provided.",
          "sin'".to_owned(),
          1,
          a.len()
        )))
      }
    },
  );
  let math_cos = RygFn::new(
    "cos'",
    (Shape::Atom, vec![RygType::Float]),
    Some((Shape::Atom, RygType::Float)),
    |a| {
      if let Some(n) = a.get(0) {
        match n {
          RygVal::Float(q) => Ok(RygVal::Float(q.cos())),
          RygVal::Int(q) => Ok(RygVal::Float((q.clone() as f64).cos())),
          x => Err(RygType::invalid_type(x, "Int or Float")),
        }
      } else {
        Err(Halt::InvalidInput(format!(
          "Function {:?} expects {:?} arguments, but {:?} was/were provided.",
          "cos'".to_owned(),
          1,
          a.len()
        )))
      }
    },
  );
  env.def(String::from("PI"), Some(&RygVal::Float(PI)));
  env.def(math_sin.name.clone(), Some(&RygVal::Function(math_sin)));
  env.def(math_cos.name.clone(), Some(&RygVal::Function(math_cos)));
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::evaluating::evaluator::walk;
  use crate::parsing::parser::parse_input;

  fn run<'a>(src: &str, ctx: &mut Envr) -> Maybe<RygVal> {
    let res = walk(parse_input(src), ctx);
    println!("src: {}", src);
    println!(
      "> {:?}",
      match res.clone() {
        Ok(v) => v,
        Err(h) => h.as_val(),
      }
    );
    res
  }

  #[test]
  fn test_sin() {
    let mut ctx = &mut Envr::new();
    load_math(&mut ctx);
    let src = "sin'(PI);";
    let res = walk(parse_input(src), ctx);
    println!("{:?}", res.unwrap())
  }

  #[test]
  fn test_type_of() {
    let mut ctx = &mut Envr::new();
    load_core(&mut ctx);
    let src = "type'of(3)";
    let _ = run(src, ctx);
  }
}
