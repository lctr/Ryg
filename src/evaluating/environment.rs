use std::{collections::HashMap, rc::Rc};

use crate::evaluating::values::{Atom, RygVal};

#[derive(Clone, Debug)]

pub struct Envr<'a> {
  scope: HashMap<String, RygVal>,
  parent: Option<&'a Envr<'a>>,
}

impl Envr<'_> {
  pub fn new<'a>() -> Envr<'a> {
    let mut scope: HashMap<String, RygVal> = HashMap::new();
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
        None => None,
      }
    }
    // match self.parent {
    //   Some(ref env) => match env.scope.get(name) {
    //     Some(v) => Some(v),
    //     None => self.scope.get(name),
    //   },
    //   None => self.scope.get(name),
    // }
  }

  pub fn set(&mut self, name: String, value: RygVal) -> RygVal {
    self.scope.insert(name, value.clone());
    value
  }
}

mod test_envr {
  use super::*;
  use crate::{
    evaluating::evaluator::walk,
    parsing::{expression::Expr, parser},
  };

  #[test]
  fn test_env() {
    let src = "3 + 4";
    let ast = parser::get_ast(src);
    println!("{:?}", &ast);
    println!("{:?}", RygVal::Int(3 + 4));
    let mut envr = Envr::new();
    println!("{:?}", walk(ast, envr.clone()));
    let name = String::from("sum");
    envr.scope.insert(name.clone(), RygVal::Int(7));
    envr.scope.insert(name.clone(), RygVal::Int(7));
    println!("{:?}", envr);
    println!("{:?}", envr.get(&name))
  }
}
