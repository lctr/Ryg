use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{self, Debug, Write},
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::{
    core::{
        record::Record,
        rygtype::{self, RygType},
        rygval::{RygVal, NIL, UNIT},
    },
    main,
    tok::token::Token,
    util::{
        display::{Paint, Table},
        state::Halt,
        types::{Either, Kind, Maybe},
    },
};

impl Eq for RygVal {}

pub trait Scoped<T> {
    fn get(&mut self, name: &String) -> Option<T>;
    fn set();
    fn lookup<'a: 'b, 'b: 'a>(
        &'a mut self,
        id: &String,
    ) -> Option<&'b mut Self>;
    fn def(self: &mut Self, id: &String, val: Option<&T>);
}

#[derive(Clone, PartialEq, Eq)]
pub struct Variable {
    pub kind: RygType,
    value: RygVal,
    is_fixed: bool,
    initialized: bool,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Envr {
    store: HashMap<String, Variable>,
    parent: Option<Box<Envr>>,
    stack: Vec<RygVal>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Scope {
    id: usize,
    context: Rc<RefCell<Envr>>,
}

impl Envr {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            parent: None,
            stack: vec![],
        }
    }
    pub fn is_orphan(&self) -> bool {
        self.parent.is_none()
    }
    pub fn extend<'a>(&'a mut self) -> Self {
        Self {
            store: HashMap::new(),
            parent: Some(Box::new(self.clone())),
            stack: if self.stack.is_empty() {
                vec![]
            } else {
                self.stack.clone()
            },
        }
    }
    pub fn extend_with<'a>(&'a mut self, ancestor: &Envr) -> Self {
        let mut ev = self.extend();
        ev.parent = Some(Box::new(ancestor.clone()));
        ev
    }
    pub fn inject(&mut self, envr: &Envr) {
        self.store.extend(envr.store.clone().into_iter())
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
    pub fn def_const(
        &mut self,
        name: &String,
        value: RygVal,
    ) -> Maybe<RygVal> {
        if self.store.contains_key(name) {
            Err(Halt::Evaluating(format!(
                "Unable to assign value to constant {}!",
                name
            )))
        } else {
            self.store
                .insert(name.to_string(), Variable::new_constant(&value));
            match self.get(name) {
                Some(v) => Ok(v),
                None => Err(Halt::UnknownError(format!("Unable to define constant {} with value {} in environment {}", name, &value, &self)))
            }
        }
    }
    pub fn has(&self, name: &String) -> bool {
        self.store.contains_key(name)
            || match &self.parent {
                Some(env) => env.has(name),
                None => false,
            }
    }
    pub fn lookup<'a: 'b, 'b: 'a>(
        &'a mut self,
        name: &String,
    ) -> Option<&'b mut Envr> {
        if self.is_orphan() {
            if self.store.contains_key(name) {
                Some(self)
            } else {
                None
            }
        } else if self.has(name) {
            Some(self)
        } else {
            match &mut self.parent {
                Some(parent) => parent.lookup(name),
                None => None,
            }
        }
    }
    pub fn get_var(&mut self, name: &String) -> Option<&Variable> {
        if self.is_orphan() {
            self.store.get(name)
        } else if let Some(ctx) = self.lookup(name) {
            if ctx.store.contains_key(name) {
                ctx.store.get(name)
            } else {
                match &mut ctx.parent {
                    Some(parent) => parent.get_var(name),
                    _ => None,
                }
            }
        } else {
            None
        }
    }
    pub fn get(&mut self, name: &String) -> Option<RygVal> {
        self.get_var(name).and_then(|var| Some(var.get_value()))
    }
    pub fn get_mut(&mut self, name: &String) -> Option<&mut RygVal> {
        if self.is_orphan() {
            self.store
                .get_mut(name)
                .and_then(|pr| Some(pr.get_value_mut()))
        } else if let Some(ctx) = self.lookup(name) {
            if ctx.store.contains_key(name) {
                ctx.store
                    .get_mut(name)
                    .and_then(|pr| Some(pr.get_value_mut()))
            } else {
                match &mut ctx.parent {
                    Some(parent) => parent.get_mut(name),
                    _ => None,
                }
            }
        } else {
            None
        }
    }
    pub fn set(&mut self, name: &String, value: RygVal) -> Option<RygVal> {
        if let Some(mut scope) = self.lookup(name) {
            if let Some(mut var) = scope.get_var(name) {
                if var.is_fixed {
                    Some(RygVal::Error(Halt::RefSetError(format!(
                        "const {}",
                        name
                    ))))
                } else {
                    let mut rv = self.get_mut(name)?;
                    *rv = value;
                    None
                }
            } else {
                scope.set(name, value);
                None
            }
        } else {
            None
        }
    }

    pub fn modify<F: FnMut(bool, &mut RygVal)>(
        &mut self,
        name: &String,
        action: &mut F,
    ) {
        let mut val_out = self.get_mut(name);
        if let Some(mut val) = val_out {
            action(true, &mut val);
        } else {
            action(false, &mut NIL);
        }
    }
    pub fn walk<Y, F: FnMut(&mut Self) -> Y>(&mut self, mut eval: F) {
        eval(self);
    }
    pub fn is_single(&mut self) -> bool {
        self.store.len() == 1
    }
    pub fn resolve_ty(env: &mut Envr, rygtype: RygType) -> RygType {
        match &rygtype {
            RygType::Lambda(args, b) => {
                if let Some(t) = b {
                    *t.clone()
                } else {
                    rygtype
                }
            }
            RygType::Function(_, _) => todo!(),
            RygType::Var(name) => env
                .get_var(&name)
                .and_then(|v| Some(v.kind.clone()))
                .unwrap_or(RygType::Unknown),
            RygType::Given(_, _) => todo!(),
            _ => rygtype,
        }
    }
}

impl From<HashMap<String, RygVal>> for Envr {
    fn from(store: HashMap<String, RygVal>) -> Self {
        let mut map = HashMap::new();
        store.into_iter().for_each(|(key, val)| {
            map.insert(key, Variable::from(val));
        });

        Self {
            store: map,
            parent: None,
            stack: vec![],
        }
    }
}

// impl Iterator for Envr {
//     type Item = Self;
//     fn next(&mut self) -> Option<Self::Item> {
//         if self.is_orphan() {
//             return None;
//         };
//         if let Some(parent) = (&self.parent).clone() {
//             Some(*parent)
//         } else {
//             None
//         }
//     }
// }

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
        if !self.initialized
            && self.value == UNIT
            && self.kind == RygType::Unknown
        {
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
            (x, y)
                if x == y || matches!(x, RygType::Any | RygType::Unknown) =>
            {
                true
            }
            (_, _) => false,
        }
    }
    pub fn set_value(&mut self, value: &RygVal) -> Maybe<RygVal> {
        if self.is_fixed {
            Err(Halt::Evaluating(format!(
                "Unable to assign value to constant"
            )))
        } else if self.initialized && !self.match_type(value) {
            Err(Halt::RefSetError(value.type_string()))
        } else {
            let val = value.to_owned();
            self.value = val.to_owned();
            Ok(value.clone())
        }
    }
    pub fn is_ok(&self) -> bool {
        let value = self.value.clone();
        self.kind != RygType::Halt
            && !matches!(value.clone(), RygVal::Error(_))
            || (self.initialized && self.kind != RygType::from(value))
    }
}

impl From<RygVal> for Variable {
    fn from(value: RygVal) -> Self {
        Variable::new(Some(&value))
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.initialized {
            write!(
                f,
                "{}{}",
                if self.is_fixed { "const " } else { "" },
                self.kind
            )
        } else {
            write!(f, "",)
        }
    }
}

impl fmt::Debug for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:?}", &self)
        } else {
            write!(f, "{:#?}", &self)
        }
    }
}

mod test {
    use std::cell::RefCell;

    use crate::{
        core::record::Record,
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
                stack: vec![],
            })),
            stack: vec![],
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
    }

    #[test]
    fn inspect_set() {
        let mut global = Envr::new();
        global.def(String::from("foo1"), Some(&RygVal::Int(8)));
        let mut scope = global.extend();
        log_do! {
            "root" => &global,
            "adding foo1" => global.def(String::from("foo1"), Some(&RygVal::Int(8))),
            "extended" => {scope = global.extend(); &scope},
            "adding foo" => scope.def(String::from("foo"), Some(&RygVal::Bool(false))),
            "getting foo1" => &scope.extend().get(&String::from("foo1"))
        };
    }

    #[test]
    fn inspect_modify() {
        let mut envr = Envr::new();
        let record = RygVal::Dict(Record::new(HashMap::from([
            ("name".to_string(), RygVal::from("kitty")),
            ("color".to_string(), RygVal::from("black")),
        ])));
        let name = "name".to_string();
        let updated = RygVal::from("coraline");
        envr.def("my'cat".to_string(), Some(&record));
        println!("envr before: {:#?}", &envr);
        envr.modify(&"my'cat".to_string(), &mut |b, val| {
            if let (true, RygVal::Dict(rec)) = (b, val) {
                let res = rec.get_mut(&"name".to_string());
                if let Some(rv) = res {
                    *rv = updated.to_owned();
                }
            }
        });
        println!("envr after: {:#?}", &envr);
    }
}
