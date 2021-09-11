use std::{collections::HashMap, rc::Rc};

use crate::{
  evaluating::values::{Atom, RygVal},
  main,
};

pub type Scope<'a> = HashMap<String, RygVal>;

#[derive(Clone, Debug, PartialEq)]
pub struct Envr<'a> {
  scope: Scope<'a>,
  parent: Option<&'a Envr<'a>>,
}

impl<'a> Envr<'a> {
  pub fn new() -> Envr<'a> {
    let mut scope: Scope = HashMap::new();
    Envr {
      scope,
      parent: None,
    }
  }

  pub fn extend(&mut self) -> Envr<'_> {
    Envr {
      scope: self.scope.clone(),
      parent: Some(self),
    }
  }

  pub fn get(&self, name: &String) -> Option<&RygVal> {
    if self.scope.contains_key(name) {
      self.scope.get(name)
    } else {
      match self.parent {
        Some(envr) => envr.get(name),
        None => panic!(
          "Unable to get undefined variable 
  {:?}\nwith env
  {:?}",
          name, self
        ),
      }
    }
  }

  pub fn lookup(&self, name: &String) -> Option<&Envr<'a>> {
    let mut env = self;
    loop {
      if env.scope.contains_key(name) {
        return Some(env);
      } else {
        match env.parent {
          None => return None,
          Some(parent) => {
            env = parent;
          }
        }
      }
    }
    // while let s = env {
    //   if s.scope.contains_key(name) {
    //     return Some(env.scope.clone());
    //   } else {
    //     env = env.parent.unwrap();
    //   }
    // }
    // None
  }
  pub fn set(&self, name: String, value: RygVal) -> RygVal {
    // let (a1, a2) = (self.lookup(&name), self.parent);
    // if let (None, None) = (a1, a2)
    let scope = match self.lookup(&name) {
      Some(env) => env,
      None => {
        if self.parent == None {
          panic!(
            "Unable to set value 
  {:?}\nto undefined variable 
  {:?}\nwith env
  {:?}",
            value, name, self
          )
        }
        self
      }
    };
    // let scope = match (self.lookup(&name), self.parent) {
    //   (Some(ctx), Some(p)) => ctx,
    //   (None, Some(_)) => {
    //     panic!(
    //       "Unable to set value to undefined variable {:?}
    //     env: {:?}",
    //       name, self
    //     )
    //   }
    //   (Some(ctx), None) => ctx,
    //   _ => &self,
    // };
    scope.clone().scope.insert(name, value.clone());
    value.clone()
  }

  pub fn def(&mut self, name: String, value: RygVal) -> RygVal {
    let v = value.clone();
    self.scope.insert(name, value);
    v
  }

  pub fn clone(&self) -> Envr {
    Envr {
      scope: self.scope.clone().to_owned(),
      parent: self.parent.clone().to_owned(),
    }
  }
}

mod test {
  use super::*;
  use crate::{
    evaluating::evaluator::walk,
    parsing::{expression::Expr, parser},
  };

  fn load_env() -> Envr<'static> {
    let mut env = Envr::new();
    env.def(String::from("foo"), RygVal::String(String::from("bar")));
    env
  }

  #[test]
  fn test_env() {
    let src = "3 + 4";
    let ast = parser::get_ast(src);
    println!("{:?}", &ast);
    println!("{:?}", RygVal::Int(3 + 4));
    let mut envr = Envr::new();
    println!("{:?}", walk(ast, &envr));
    let name = String::from("sum");
    envr.scope.insert(name.clone(), RygVal::Int(7));
    envr.scope.insert(name.clone(), RygVal::Int(7));
    println!("{:?}", envr);
    println!("{:?}", envr.get(&name))
  }

  #[test]
  fn test_lookup() {
    let mut env = load_env();
    env.def(String::from("foo"), RygVal::String(String::from("bar")));
    println!("{:?}", env.get(&String::from("foo")));
    println!("{:?}", env)
  }

  #[test]
  fn test_set() {
    let mut env = load_env();
    env.set(String::from("x"), RygVal::Int(8));
    println!("{:?}", env.get(&String::from("x")))
  }
}
