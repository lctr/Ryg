use std::{
    cell::{Cell, RefCell},
    collections::{hash_map::Keys, HashMap, HashSet},
    fmt::{self, Debug, Write},
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::{
    evaluating::environment::{Envr, Variable},
    tok::token::Token,
    util::{
        state::Halt,
        types::{Kind, Maybe},
    },
};

use super::rygval::{RygVal, NIL};

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Object {
    this: Envr,
    class: Option<Kind<String>>,
}
impl Object {
    pub fn new(this: Envr) -> Self {
        Self { this, class: None }
    }
    pub fn get(&mut self, name: &String) -> Option<RygVal> {
        self.this.get(name)
    }
    pub fn get_mut(&mut self, name: &String) -> Option<&mut RygVal> {
        self.this.get_mut(name)
    }

    pub fn get_var(&mut self, name: &String) -> Option<&Variable> {
        self.this.get_var(name)
    }
    pub fn set(&mut self, name: &String, value: &RygVal) -> Option<RygVal> {
        if let Some(v) = self.get_mut(name) {
            *v = value.to_owned();
            self.get(name)
        } else {
            None
        }
    }
    pub fn contains_key(&self, name: &String) -> bool {
        self.this.has(name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record {
    pub entries: HashMap<String, RygVal>,
    // pub context: Envr,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dict<K, V>
where
    K: Debug + Clone + PartialEq + Eq + Hash,
    V: Debug + Clone + PartialEq + Eq + Hash, {
    pub map: HashMap<K, V>,
}

impl Record {
    pub fn new(entries: HashMap<String, RygVal>) -> Self {
        Self { entries }
    }

    // pub fn get_this(self) -> Rc<RefCell<RygVal>> {
    //     Rc::new(RefCell::new(RygVal::Dict(self)))
    // }

    pub fn get_keys(&self) -> Keys<'_, String, RygVal> {
        self.entries.keys()
    }

    pub fn contains_val(&self, val: RygVal) -> bool {
        self.entries.iter().any(|(_, v)| val == *v)
    }

    pub fn contains_key(&self, key: &String) -> bool {
        self.get_keys().any(|k| k == key)
    }

    pub fn get(&self, key: &String) -> Option<&RygVal> {
        self.entries.get(key)
    }

    pub fn get_mut(&mut self, key: &String) -> Option<&mut RygVal> {
        if self.contains_key(key) {
            self.entries.get_mut(key)
        } else {
            None
        }
    }

    pub fn set(&mut self, key: &String, val: RygVal) -> Maybe<RygVal> {
        if matches!(key.as_str(), "self" | "this" | "super") {
            Err(Halt::Evaluating(format!(
                "Invalid assignment! Unable to assign to `{}` keyword.",
                key
            )))
        } else {
            // let val = RygVal::Object(Rc::new(RefCell::new(val)));
            match self.entries.insert(key.into(), val.clone()) {
                Some(v) => Ok(val),
                None => Ok(val),
            }
        }
    }
}

// impl From<Envr> for Record {
//     fn from(env: Envr) -> Self {
//         let store = env.store;
//     }
// }

impl std::fmt::Display for Record {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let inner = self
            .entries
            .iter()
            .map(|(a, b)| format!("{}: {}", a, b))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{{ {} }}", inner)
    }
}

#[cfg(test)]
mod test {
    use crate::{log_do, tok::stream::Pos};

    use super::*;

    #[test]
    fn test_lookup() {
        let mut record = Record::new(HashMap::new());
        let tok = Token::String("cat".to_string(), Pos::faux());
        let val_from_tok = RygVal::from_token(tok.clone());
        let val_from_str = RygVal::from("cat");
        let key_from_tok = tok.literal();
        let key_from_val = val_from_str.to_string();

        log_do!(
          "record" => &record,
          "tok" => &tok,
          "val_from_tok" => &val_from_tok,
          "val_from_str" => &val_from_str,
          "set key_from_tok with val_from_tok" => record.set(&key_from_tok, val_from_tok.clone()),
          "has key_from_tok" => &record.contains_key(&key_from_tok),
          "has key_from_val" => &record.contains_key(&key_from_val)
        )
    }
}
