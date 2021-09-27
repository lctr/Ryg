use std::{
  cell::Cell,
  collections::{hash_map::Keys, HashMap, HashSet},
  fmt::Debug,
  hash::{Hash, Hasher},
};

use crate::util::{state::Halt, types::Maybe};

use super::rygval::RygVal;

#[macro_export]
macro_rules! new_map {
    ($($key: expr => $val: expr),*) => {
      {
        let mut map = HashMap::new();
        $(
          map.insert($key, $val);
        )*
        map
      }
    };
  }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RecordKey {
  Int(i32),
  Str(String),
  Bool(bool),
  Sym(String),
}

impl RecordKey {
  pub fn new(key: &RygVal) -> Option<Self> {
    match key {
      RygVal::Int(ref n) => Some(RecordKey::Int(n.clone())),
      RygVal::Bool(ref b) => Some(RecordKey::Bool(b.clone())),
      RygVal::String(ref s) => Some(RecordKey::Str(s.clone())),
      RygVal::Symbol(ref s) => Some(RecordKey::Sym(s.clone())),
      _ => None,
    }
  }
  pub fn get_val(&self) -> RygVal {
    match self {
      Self::Int(n) => RygVal::Int(n.clone()),
      Self::Str(s) => RygVal::String(s.clone()),
      Self::Bool(b) => RygVal::Bool(b.clone()),
      Self::Sym(b) => RygVal::Symbol(b.clone()),
    }
  }
  pub fn to_string(&self) -> String {
    match self {
      Self::Int(n) => format!("{}", n),
      Self::Str(s) | Self::Sym(s) => s.clone(),
      Self::Bool(b) => format!("{}", b),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record {
  pub map: HashMap<RecordKey, RygVal>,
}

impl Record {
  pub fn new(map: HashMap<RecordKey, RygVal>) -> Self {
    Self { map }
  }

  pub fn get_keys(&self) -> Keys<'_, RecordKey, RygVal> {
    self.map.keys()
  }
  pub fn contains_val(&self, val: RygVal) -> bool {
    for (_, v) in self.map.iter() {
      if val == *v {
        return true;
      }
    }
    false
  }
  pub fn contains_key(&self, key: &RecordKey) -> bool {
    self.map.contains_key(key) || {
      for k in self.map.keys() {
        if k == key || k.get_val() == key.get_val() {
          return true;
        }
      }
      false
    }
  }

  pub fn get(&self, key: &RecordKey) -> Option<&RygVal> {
    if self.contains_key(key) {
      self.map.get(key)
    } else {
      None
    }
  }
  pub fn set(&mut self, key: &RecordKey, val: RygVal) -> Maybe<RygVal> {
    match self.map.insert(key.clone(), val.clone()) {
      Some(v) => Ok(v),
      None => Ok(val),
    }
  }
}

impl std::fmt::Display for RecordKey {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      RecordKey::Int(i) => write!(f, "[{}]", i),
      RecordKey::Str(s) | RecordKey::Sym(s) => write!(f, "{}", s),
      RecordKey::Bool(b) => write!(f, "{}", b),
    }
  }
}

impl std::fmt::Display for Record {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{{\n\t");
    for (k, v) in self.map.clone() {
      write!(f, "\t{}: {}\n", k, v);
    }
    write!(f, "}}")
  }
}
