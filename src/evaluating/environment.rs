use std::{
  collections::HashMap,
  fmt::{self, Debug, Write},
  hash::{Hash, Hasher},
};

use crate::{
  core::{
    rygtype::RygType,
    rygval::{RygVal, NIL, UNIT},
  },
  lexing::token::Token,
  main,
  util::{
    display::{Paint, Table},
    state::Halt,
    types::{Either, Kind, Maybe},
  },
};

impl Eq for RygVal {}

#[derive(Clone, PartialEq, Eq)]
pub struct Envr {
  store: HashMap<String, Variable>,
  parent: Option<Box<Envr>>,
}

impl Envr {
  pub fn new() -> Self {
    Self {
      store: HashMap::new(),
      parent: None,
    }
  }
  pub fn extend<'a>(&'a mut self) -> Self {
    Self {
      store: HashMap::new(),
      parent: Some(Box::new(self.clone())),
    }
  }
  pub fn def(
    &mut self,
    name: String,
    value: Option<&RygVal>,
  ) -> Option<RygVal> {
    self.store.insert(name, Variable::new(value));
    if let Some(va) = value {
      Some(va.clone())
    } else {
      None
    }
  }
  pub fn has(&self, name: &String) -> bool {
    self.store.contains_key(name)
  }
  pub fn lookup<'a: 'b, 'b: 'a>(
    &'a mut self,
    name: &String,
  ) -> Option<&'b mut Envr> {
    if self.has(name) {
      Some(self)
    } else {
      match &mut self.parent {
        Some(parent) => parent.lookup(name),
        None => None,
      }
    }
  }
  pub fn get(&mut self, name: &String) -> Option<RygVal> {
    if self.store.contains_key(name) {
      self.store.get(name).and_then(|pr| Some(pr.get_value()))
    } else {
      if let Some(scope) = self.lookup(name) {
        scope.get(name)
      } else {
        None
      }
    }
  }
  pub fn get_mut(&mut self, name: &String) -> Option<RygVal> {
    if self.store.contains_key(name) {
      self.store.get_mut(name).and_then(|pr| Some(pr.get_value()))
    } else {
      if let Some(scope) = self.lookup(name) {
        scope.get_mut(name)
      } else {
        None
      }
    }
  }
  pub fn set(&mut self, name: &String, value: RygVal) -> Option<RygVal> {
    if let Some(scope) = self.lookup(name) {
      if scope.has(name) {
        scope
          .store
          .insert(name.to_owned(), Variable::new(Some(&value)))
          .and_then(|p| Some(p.get_value()))
      } else {
        scope.set(name, value)
      }
    } else {
      None
    }
  }
}

impl fmt::Display for Envr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(env) = &self.parent {
      f.debug_struct("Envr")
        .field("scope", &self.store)
        .field("parent", &env)
        .finish()
    } else {
      f.debug_struct("Envr").field("scope", &self.store).finish()
    }
  }
}

impl fmt::Debug for Envr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let tbl = self.store.iter().map(|(k, v)| (k, v)).collect::<Vec<_>>();
    let val = Variable::new(None);
    let table = Table(tbl.as_slice());
    f.debug_struct("Envr")
      .field("scope", &table)
      .field(
        "parent",
        if let Some(w) = &self.parent {
          w.as_ref()
        } else {
          &None as &Option<&Envr>
        },
      )
      .finish()
    // write!(f, "{:?}", table)
  }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Variable {
  pub kind: RygType,
  value: RygVal,
  is_fixed: bool,
  initialized: bool,
}

impl fmt::Debug for Variable {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.initialized {
      write!(
        f,
        "{}{}",
        if self.is_fixed { "const " } else { "" },
        self.value
      )
    } else {
      write!(f, "_",)
    }
  }
}

impl fmt::Display for Variable {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.initialized {
      write!(f, "{}", self.value)
    } else {
      write!(f, "",)
    }
  }
}

impl Variable {
  pub fn new(value: Option<&RygVal>) -> Self {
    let (value, initialized) = if let Some(value) = value {
      (value.clone(), true)
    } else {
      (UNIT, false)
    };
    Self {
      kind: RygType::from(value.clone()),
      value,
      is_fixed: false,
      initialized,
    }
  }
  pub fn new_constant(value: &RygVal) -> Self {
    let value = value.clone();
    Self {
      kind: RygType::from(value.clone()),
      value,
      is_fixed: true,
      initialized: true,
    }
  }
  pub fn init(&mut self, value: &RygVal) -> Maybe<RygVal> {
    if !self.initialized && self.value == UNIT && self.kind == RygType::Unit {
      self.initialized = true;
      self.value = value.clone();
      self.kind = RygType::from(value.to_owned());
      Ok(value.to_owned())
    } else {
      Err(Halt::InvalidType(format!(
        "Expected type {}, but was provided with {}",
        self.kind, value
      )))
    }
  }
  pub fn hashmap_insert(
    &self,
    name: String,
    map: &mut HashMap<String, Self>,
  ) -> Option<Variable> {
    map.insert(name, self.clone())
  }
  pub fn get_value_mut(&mut self) -> &mut RygVal {
    &mut self.value
  }
  pub fn get_value_ref(&self) -> &RygVal {
    &self.value
  }
  pub fn get_value(&self) -> RygVal {
    self.value.clone()
  }
  pub fn match_type(&self, other: &RygVal) -> bool {
    match (self.kind.clone(), RygType::from(other.clone())) {
      (x, y) if x == y || matches!(x, RygType::Any | RygType::Unit) => true,
      (_, _) => false,
    }
  }
  pub fn set_value(&mut self, value: &RygVal) -> Maybe<RygVal> {
    if self.initialized && self.match_type(value) {
      Err(Halt::RefSetError(value.type_string()))
    } else {
      let val = value.to_owned();
      self.value = val.to_owned();
      Ok(value.clone())
    }
  }
  pub fn is_ok(&self) -> bool {
    let value = self.value.clone();
    self.kind != RygType::Halt && !matches!(value.clone(), RygVal::Error(_))
      || (self.initialized && self.kind != RygType::from(value))
  }
}

impl From<RygVal> for Variable {
  fn from(value: RygVal) -> Self {
    Variable::new(Some(&value))
  }
}

mod test {
  use crate::{
    evaluating::{environment, evaluator::walk},
    log_do,
    parsing::parser::{parse_input, Parser},
  };

  use super::*;

  fn load_env() -> Envr {
    let mut env = Envr::new();
    env.def(
      String::from("foo"),
      Some(&RygVal::String(String::from("bar"))),
    );
    env
  }

  #[test]
  fn test_env() {}

  #[test]
  fn test_define() {
    let mut env = Envr::new();
    let foo = RygVal::Bool(true);
    let bar = RygVal::String(String::from("foo"));
    let baz = RygVal::Int(8);
    log_do! {
      "env loaded" => &env,
      "variable 1" => &foo,
      "varable 2" => &bar,
      "variable 3" => &baz,
      "defining variable 1" => env.def(String::from("foo"), Some(&foo)),
      "env" => &env,
      "extend and replace" => {let tmp = env.extend(); env = tmp.clone().to_owned(); env.clone()},
      "define variable 2" => env.def(String::from("bar"), Some(&bar)),
      "define variable 3" => env.def(String::from("baz"), Some(&baz)),
      "final env" => &env,
      "get variable 1" => env.get(&String::from("foo"))
    };
    let expected = Envr {
      store: HashMap::from([
        (String::from("bar"), Variable::new(Some(&bar))),
        (String::from("baz"), Variable::new(Some(&baz))),
      ]),
      parent: Some(Box::new(Envr {
        store: HashMap::from([(
          String::from("foo"),
          Variable::new(Some(&foo)),
        )]),
        parent: None,
      })),
    };
    assert_eq!(env, expected)
  }

  #[test]
  fn test_lookup() {
    let env = &mut load_env();
    let rygstr = |name: &str| RygVal::String(String::from(name));
    let mut layer = &mut env.clone();
    log_do! {
      "env loaded" => &env,
      "variable 1" => rygstr("bar"),
      "defining variable 1" => env.def(String::from("bar"), Some(&rygstr("bar"))),
      "env" => &env,
      "variable 1 lookup" => { let tmp = if let Some(lr) = env.lookup(&String::from("bar")) { lr} else {env}; layer = tmp; layer.clone()},
      "get variable 1" => layer.get(&String::from("bar"))
    };
    // println!("env loaded: {:#?}", &env);
    // println!("new variable 1: {} := {:?}", "foo", rygstr("foo"));
    // println!("Defining new variable...");
    // env.def(String::from("foo2"), rygstr("foo"));
    // println!("env: {:#?}", &env);
    // println!("getting variable 1 from env...");
    // println!("{:#?}", env.get(Either::Right(String::from("foo"))));
    // println!("env: {:?}", env)
  }

  #[test]
  fn test_set() {
    let mut global = Envr::new();
    global.def(String::from("foo1"), Some(&RygVal::Int(8)));
    let mut scope = global.extend();
    println!("{:?}", &global);
    println!("{:?}", &scope);
    println!(
      "{:?}",
      scope.def(String::from("foo"), Some(&RygVal::Bool(false)))
    );
    println!("{:?}", &scope.extend().get(&String::from("foo1")));
    // println!("{:?}", scope)
  }
}
