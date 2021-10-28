use std::{
    borrow::Borrow,
    cell::RefCell,
    cmp,
    fmt::{self, Debug, Display, Formatter},
    ops::{
        Add, AddAssign, BitAnd, BitOr, BitXor, Div, Mul, MulAssign, Rem, Sub,
        SubAssign,
    },
    rc::Rc,
};

use crate::{
    parsing::expression::{Parameter, Shape},
    tok::token::Token,
    util::{
        display::Paint,
        state::Halt,
        types::{Either, Kind, Maybe},
    },
};

use super::{
    atom::*,
    function::{Lambda, RygFn},
    list::{RygIter, RygList},
    record::Record,
    rygtype::{Field, RygType},
    variant::RygEnum,
};

pub const TRUE: RygVal = RygVal::Bool(true);
pub const FALSE: RygVal = RygVal::Bool(false);
pub const NIL: RygVal = RygVal::Nil();
pub const UNIT: RygVal = RygVal::Unit();

#[derive(Clone)]
pub struct Container(pub Kind<String>, pub Box<RygVal>);

#[derive(Clone)] //, PartialEq)]
pub enum RygVal {
    Nil(),
    Unit(),
    Bool(bool),
    Byte(u8),
    Int(i32),
    Float(f64),
    Char(char),
    String(String),
    Symbol(String),
    Vector(Vec<RygVal>),
    Tuple(usize, Vec<RygVal>),
    List(RygList),
    Lambda(Lambda),
    Error(Halt),
    Function(RygFn),
    Dict(Record),
    Iter(Rc<RygIter<i32>>),
    Data(RygEnum),
    Holder(Container),
}

impl From<Atom> for RygVal {
    fn from(atom: Atom) -> Self {
        match atom {
            Atom::Nil => Self::Nil(),
            Atom::Bool(b) => Self::Bool(b),
            Atom::Byte(b) => Self::Byte(b),
            Atom::Int(i) => Self::Int(i),
            Atom::Float(q) => Self::Float(q),
            Atom::Char(c) => Self::Char(c),
            Atom::String(s) => Self::String(s),
            Atom::Symbol(s) => Self::Symbol(s),
            _ => Self::Error(Halt::InvalidInput(format!(
                "Unable to convert {:?} into RygValue",
                atom.clone()
            ))),
        }
    }
}

impl From<&str> for RygVal {
    fn from(src: &str) -> Self {
        RygVal::String(src.to_owned())
    }
}

macro_rules! from_rs_prim {
    ($t:ident) => {
        from_rs_prim!($t => $t);
    };
    ($rs:ty => $var:ident) => {
        impl From<$rs> for RygVal {
            fn from(item: $rs) -> Self {
                RygVal::$var(item)
            }
        }
        impl From<&$rs> for RygVal {
            fn from(item: &$rs) -> Self {
                RygVal::$var(item.to_owned())
            }
        }
    };
}

from_rs_prim!(char => Char);
from_rs_prim!(String);
from_rs_prim!(u8 => Byte);
from_rs_prim!(i32 => Int);
from_rs_prim!(f64 => Float);
from_rs_prim!(bool => Bool);

impl From<usize> for RygVal {
    fn from(item: usize) -> Self {
        RygVal::Int(item as i32)
    }
}

impl From<Vec<RygVal>> for RygVal {
    fn from(item: Vec<RygVal>) -> Self {
        RygVal::Vector(item)
    }
}

#[allow(unused)]
impl RygVal {
    pub fn from_token(token: Token) -> Self {
        Self::from_atom(Atom::from(token))
    }

    pub fn from_atom(atom: Atom) -> Self {
        Self::from(atom)
    }

    pub fn get_byte(&self) -> Result<u8, Halt> {
        if let Self::Byte(b) = self {
            Ok(b.clone())
        } else {
            Err(RygType::invalid_type(&self.clone(), "Byte"))
        }
    }
    pub fn get_bool(&self) -> Result<bool, Halt> {
        if let Self::Bool(b) = self {
            Ok(b.clone())
        } else {
            Err(RygType::invalid_type(&self.clone(), "Bool"))
        }
    }
    pub fn get_int(&self) -> Result<i32, Halt> {
        if let Self::Int(n) = self {
            Ok(n.clone())
        } else {
            Err(RygType::invalid_type(&self.clone(), "Int"))
        }
    }
    pub fn get_float(&self) -> Result<f64, Halt> {
        match self {
            Self::Float(q) => Ok(q.clone()),
            Self::Int(m) => Ok(m.clone() as f64),
            _ => Err(RygType::invalid_type(&self.clone(), "Float")),
        }
    }
    pub fn get_char(&self) -> Result<char, Halt> {
        match self {
            RygVal::Byte(u) => Ok(*u as char),
            RygVal::Int(b) => Ok((*b as u8) as char),
            RygVal::Char(c) => Ok(*c),
            RygVal::String(s) => s.chars().nth(0).map_or(
                Err(Halt::InvalidInput(format!(
                    "String {:?} does not have any chars!",
                    s
                ))),
                |c| Ok(c),
            ),
            _ => Err(RygType::invalid_type(&self.clone(), "Char")),
        }
    }
    pub fn get_string(&self) -> Result<String, Halt> {
        if let Self::String(s) = self {
            Ok(s.clone())
        } else if let Self::Char(c) = self {
            Ok(c.to_string())
        } else if let Self::Vector(v) = self {
            if v.clone().iter().all(|rs| matches!(rs, RygVal::Char(..))) {
                Ok(v.iter()
                    .filter_map(|rs| rs.get_string().ok())
                    .collect::<Vec<_>>()
                    .join(""))
            } else {
                Err(RygType::invalid_type(&self.clone(), "String"))
            }
        } else {
            Err(RygType::invalid_type(&self.clone(), "String"))
        }
    }
    pub fn get_vector(&self) -> Result<Vec<RygVal>, Halt> {
        if let Self::Vector(v) = self {
            Ok(v.clone())
        } else {
            Err(RygType::invalid_type(&self.clone(), "Vector"))
        }
    }
    pub fn is_nil(&self) -> bool {
        match self {
            Self::Nil() | Self::Unit() => true,
            Self::Tuple(n, _) => n == &0,
            _ => false,
        }
    }
    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool(..))
    }
    pub fn is_zero(&self) -> bool {
        match self {
            Self::Int(n) => n == &0,
            Self::Float(q) => q == &0.0,
            _ => false,
        }
    }
    pub fn is_empty(&self) -> bool {
        match self {
            Self::Vector(v) => v.is_empty(),
            _ => false,
        }
    }
    pub fn is_float(&self) -> bool {
        match self {
            Self::Float(_) => true,
            _ => false,
        }
    }
    pub fn is_int(&self) -> bool {
        match self {
            Self::Int(_) => true,
            _ => false,
        }
    }
    pub fn is_char(&self) -> bool {
        match self {
            Self::Char(..) => true,
            Self::String(s) => s.len() == 1 && s.chars().nth(0).is_some(),
            _ => false,
        }
    }
    pub fn is_string(&self) -> bool {
        match self {
            Self::String(_) => true,
            _ => false,
        }
    }
    pub fn is_vector(&self) -> bool {
        match self {
            Self::Vector(_) => true,
            _ => false,
        }
    }
    pub fn is_tuple(&self) -> bool {
        match self {
            Self::Tuple(..) => true,
            _ => false,
        }
    }
    pub fn is_list(&self) -> bool {
        match self {
            Self::List(_) => true,
            _ => false,
        }
    }
    pub fn is_function(&self) -> bool {
        match self {
            Self::Lambda(_) | Self::Function(_) => true,
            _ => false,
        }
    }
    pub fn to_float(&self) -> Maybe<f64> {
        self.get_float().and_then(|v| Ok(v.clone()))
    }
    pub fn to_vec(&self) -> Vec<Self> {
        match self {
            RygVal::String(s) => {
                s.chars().map(|c| RygVal::Char(c)).collect::<Vec<_>>()
            }
            RygVal::Vector(v) | RygVal::Tuple(_, v) => v.to_owned(),
            _ => vec![self.to_owned()],
            // RygVal::List(_) => todo!(),
        }
    }
    pub fn type_string(&self) -> String {
        RygType::from(self.clone()).to_string()
    }
    pub fn match_kind(&self, rhs: &Self) -> bool {
        // matches!((self, variant), )
        match (self, rhs.clone()) {
            (Self::Bool(..), Self::Bool(..))
            | (Self::Nil(), Self::Nil())
            | (Self::Int(..), Self::Int(..))
            | (Self::Float(..), Self::Float(..))
            | (Self::Function(..), Self::Function(..))
            | (Self::Error(..), Self::Error(..))
            | (Self::Lambda(..), Self::Lambda(..))
            | (Self::Dict(..), Self::Dict(..))
            | (Self::Char(..), Self::Char(..))
            | (Self::String(..), Self::String(..))
            | (Self::Symbol(..), Self::Symbol(..))
            | (Self::Tuple(..), Self::Tuple(..))
            | (Self::Unit(), Self::Unit())
            | (Self::List(_), Self::List(_))
            | (Self::Data(..), Self::Data(..))
            | (Self::Vector(..), Self::Vector(..)) => true,
            _ => false,
        }
    }
    pub fn as_usize(&self) -> Option<usize> {
        match self {
            Self::Int(n) => Some(*n as usize),
            Self::Float(q) => Some(q.floor() as usize),
            Self::Char(c) => Some((*c as u8) as usize),
            Self::Byte(b) => Some(*b as usize),
            Self::Tuple(l, vs) if l == &1 => {
                if let Some(rv) = vs.first() {
                    rv.as_usize()
                } else {
                    None
                }
            }
            Self::Vector(vs) if vs.len() == 1 => {
                if let Some(rv) = vs.first() {
                    rv.as_usize()
                } else {
                    None
                }
            }
            _ => None,
        }
    }
    pub fn coerce_nums(
        ref lhs: RygVal,
        ref rhs: RygVal,
    ) -> Maybe<Either<(i32, i32), (f64, f64)>> {
        match (lhs, rhs) {
            (RygVal::Int(a), RygVal::Int(b)) => Ok(Either::Left((*a, *b))),
            (RygVal::Int(a), RygVal::Float(b)) => {
                Ok(Either::Right((*a as f64, *b)))
            }
            (RygVal::Float(a), RygVal::Int(b)) => {
                Ok(Either::Right((*a, *b as f64)))
            }
            (RygVal::Float(a), RygVal::Float(b)) => {
                Ok(Either::Right((*a, *b)))
            }
            _ => Err(Halt::InvalidType(format!(
                "{} and/or {} is/are not coercible to Num types Int or Float!",
                lhs, rhs
            ))),
        }
    }
    pub fn is_indexed(&self) -> bool {
        matches!(
            self,
            Self::String(..)
                | Self::Vector(..)
                | Self::Tuple(..)
                | Self::Dict(..)
        )
    }
    pub fn dimension(&self) -> (usize, usize) {
        match self {
            RygVal::Nil() => (0, 0),
            RygVal::Unit() => (1, 0),
            RygVal::Bool(_) => (0, 1),
            RygVal::Byte(_) => (0, 1),
            RygVal::Int(_) => (0, 1),
            RygVal::Float(_) => (0, 1),
            RygVal::Char(_) => (0, 1),
            RygVal::String(s) | RygVal::Symbol(s) => (1, 1),
            RygVal::Vector(v) | RygVal::Tuple(_, v) => (1, v.len()),
            RygVal::List(_) => (2, 1),
            RygVal::Lambda(_) => (2, 2),
            RygVal::Error(_) => (2, 0),
            RygVal::Function(_) => (0, 2),
            RygVal::Dict(r) => (1, r.get_keys().len()),
            RygVal::Iter(m) => (1, 1),
            _ => todo!(),
        }
    }
    pub fn accepts_index(&self, index: &RygVal) -> bool {
        if self.is_indexed() {
            match (&self, index) {
                (RygVal::String(s), RygVal::Int(n)) => s.len() > (*n as usize),
                (RygVal::String(s), RygVal::Float(q)) => {
                    (q.floor() as usize) < s.len()
                }
                (RygVal::Symbol(s), RygVal::Unit()) => {
                    s.split(">").collect::<Vec<_>>().len() == 1
                }
                (RygVal::Symbol(sx), RygVal::Symbol(sy)) => {
                    sx.split(">").collect::<Vec<_>>().contains(&sy.as_str())
                }

                (RygVal::Vector(vs), RygVal::Int(l)) => {
                    (*l as usize) < vs.len()
                }
                (RygVal::Vector(vs), RygVal::Float(l)) => {
                    (l.floor() as usize) < vs.len()
                }
                // (RygVal::Vector(_), RygVal::Char(_, _)) => todo!(),
                (RygVal::Tuple(_, vs), RygVal::Int(l)) => {
                    (*l as usize) < vs.len()
                }
                (RygVal::Tuple(_, vs), RygVal::Float(l)) => {
                    (l.floor() as usize) < vs.len()
                }

                // (RygVal::Function(_), RygVal::Symbol(_)) => todo!(),
                (RygVal::Dict(rec), RygVal::String(s))
                | (RygVal::Dict(rec), RygVal::Symbol(s)) => {
                    rec.contains_key(s)
                }
                _ => false,
            }
        } else {
            false
        }
    }
}

impl From<Halt> for RygVal {
    fn from(halt: Halt) -> Self {
        Self::Error(halt)
    }
}

impl PartialEq for RygVal {
    fn eq(&self, other: &RygVal) -> bool {
        if *self.type_string() != other.type_string() {
            return false;
        } else {
            match (self, other) {
                (Self::Nil() | Self::Unit(), _) => true,
                (Self::Bool(b1), Self::Bool(b2)) => b1 == b2,
                (Self::Int(n1), Self::Int(n2)) => {
                    n1 == n2 || (n1 <= n2 && n2 <= n1)
                }
                (Self::Float(q1), Self::Float(q2)) => {
                    q1 == q2
                        || (q1 <= q2 && q2 <= q1)
                        || (q1.abs() - q2.abs()).abs() < 1e-10_f64
                }
                (Self::String(s1), Self::String(s2)) => s1 == s2,
                (Self::Symbol(s1), Self::Symbol(s2)) => s1 == s2,
                (Self::Vector(v1), Self::Vector(v2)) => {
                    v1.iter().zip(v2.iter()).all(|(u1, u2)| u1 == u2)
                }
                (Self::Tuple(len1, v1), Self::Tuple(len2, v2)) => {
                    if len1 != len2 {
                        false
                    } else {
                        v1.iter().zip(v2.iter()).all(|(u1, u2)| u1 == u2)
                    }
                }
                (Self::Lambda(lam1), Self::Lambda(lam2)) => lam1 == lam2,
                (Self::Error(h1), Self::Error(h2)) => *h1 == *h2,
                (Self::Function(rf1), Self::Function(rf2)) => {
                    rf1.name == rf2.name
                        && rf1.meta_in == rf2.meta_in
                        && rf1.meta_out == rf2.meta_out
                }
                (Self::Dict(ro1), Self::Dict(ro2)) => ro1 == ro2,
                (Self::Data(a1), Self::Data(a2)) => a1 == a2,
                _ => false,
            }
        }
    }
}

impl Add for &mut RygVal {
    type Output = Maybe<RygVal>;
    fn add(self, rhs: Self) -> Self::Output {
        let me = self.clone();
        let other = rhs.clone();
        let res = &mut (me + other);
        res.clone()
    }
}

impl Add for RygVal {
    type Output = Maybe<RygVal>;
    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Self::Nil() => Ok(rhs),
            Self::Bool(b) => Ok(Self::Bool(b || rhs.get_bool()?)),
            Self::Int(i) => Ok(if rhs.is_int() {
                Self::Int(i + rhs.get_int()?)
            } else {
                Self::Float((i as f64) + rhs.get_float()?)
            }),
            Self::Float(q) => Ok(Self::Float(
                q + if rhs.is_int() {
                    (rhs.get_int()?) as f64
                } else {
                    rhs.get_float()?
                },
            )),
            Self::String(s) => {
                Ok(Self::String([s, rhs.get_string()?].join("")))
            }
            Self::Char(c1) => match rhs {
                Self::Char(c2) => Ok(Self::String(
                    [c1, c2]
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(""),
                )),
                Self::String(mut s) => {
                    Ok(RygVal::String([c1.to_string(), s].join("")))
                }
                _ => Err(Halt::Evaluating(format!(
                    "Addition is not defined for {:?} and {:?}",
                    self.clone(),
                    rhs
                ))),
            },
            Self::Vector(v) => Ok(Self::Vector(
                v.iter()
                    .zip(rhs.get_vector()?)
                    .map(|(a, b)| (a.to_owned() + b.to_owned()))
                    .filter_map(|res| res.ok())
                    .collect::<Vec<_>>(),
            )),
            x => Err(Halt::Evaluating(format!(
                "Addition is not defined for {:?} and {:?}",
                x, rhs
            ))),
        }
    }
}

impl AddAssign for RygVal {
    fn add_assign(&mut self, mut rhs: Self) {
        let sum = self.clone() + rhs;
        *self = match sum {
            Ok(s) => s,
            Err(h) => Self::Error(h),
        }
    }
}

impl Sub for &mut RygVal {
    type Output = Maybe<RygVal>;
    fn sub(self, rhs: Self) -> Self::Output {
        let me = self.clone();
        let other = rhs.clone();
        let res = &mut (me - other);
        res.clone()
    }
}

impl Sub for RygVal {
    type Output = Maybe<RygVal>;
    fn sub(self, rhs: Self) -> Self::Output {
        let err = || {
            Err(Halt::Evaluating(format!(
                "Subtraction is not defined for {:?} and {:?}",
                self, rhs
            )))
        };
        match self {
            Self::Nil() => match rhs {
                Self::Nil() => Ok(NIL),
                Self::Bool(b) => Ok(Self::Bool(!b)),
                Self::Int(i) => Ok(Self::Int(-i)),
                Self::Float(q) => Ok(Self::Float(-q)),
                _ => err(),
            },
            Self::Bool(b) => Ok(Self::Bool(b && !(rhs.get_bool()?))),
            Self::Int(i) => Ok(Self::Int(i - rhs.get_int()?)),
            Self::Float(q) => Ok(Self::Float(q - rhs.get_float()?)),
            _ => err(),
            Self::Vector(ref v)
                if rhs.is_vector() && rhs.get_vector()?.len() == v.len() =>
            {
                let rs = rhs.get_vector()?;
                let vs = v.clone();
                let pairs = vs.iter().zip(rs.iter());
                let t_a = RygType::from(v.to_owned());
                let t_b = RygType::from(rhs.get_vector()?);
                if (t_a.is_float_vec() && t_b.is_float_vec())
                    || (t_a.is_int_vec() && t_b.is_int_vec())
                {
                    Ok(Self::Vector(
                        pairs
                            .map(|(a, b)| a.to_owned() - b.to_owned())
                            .filter_map(|res| res.ok())
                            .collect::<Vec<RygVal>>(),
                    ))
                } else {
                    err()
                }
            }
        }
    }
}

impl SubAssign for RygVal {
    fn sub_assign(&mut self, rhs: Self) {
        let sum = self.clone() - rhs;
        match sum {
            Ok(s) => *self = s,
            Err(h) => *self = Self::Error(h),
        }
    }
}

impl Mul for &mut RygVal {
    type Output = Maybe<RygVal>;
    fn mul(self, rhs: Self) -> Self::Output {
        let me = self.clone();
        let other = rhs.clone();
        let res = &mut (me * other);
        res.clone()
    }
}

impl Mul for RygVal {
    type Output = Maybe<RygVal>;
    fn mul(self, rhs: Self) -> Self::Output {
        match self.clone() {
            Self::Nil() => todo!(),
            Self::Bool(b) => Ok(Self::Bool(b && rhs.get_bool()?)),

            Self::Int(n) => match rhs {
                Self::Float(q) => Ok(Self::Float((n as f64) * q)),
                Self::Int(m) => {
                    Ok(Self::Int(if m == 0 || n == 0 { 0 } else { n * m }))
                }
                _ => Err(Halt::Evaluating(format!(
                    "Multiplication is not defined between {:?} and {:?}",
                    self, rhs
                ))),
            },

            Self::Float(q) => Ok(Self::Float(q * rhs.get_float()?)),

            Self::String(s) => {
                if let Self::Int(k) = rhs {
                    Ok(Self::String(s.repeat(k as usize)))
                } else {
                    Err(Halt::Evaluating(format!(
                        "Multiplication is not defined for {} and {}",
                        self, rhs
                    )))
                }
            }

            Self::Vector(v) => {
                let (w1, w2) = (&v.len(), &rhs.get_vector()?.len());
                if w1 == w2
                    && v.iter().all(|rv| rv.is_float() || rv.is_int())
                    && rhs
                        .get_vector()?
                        .iter()
                        .all(|rv| rv.is_float() || rv.is_int())
                {
                    return Ok(RygVal::Vector(
                        v.iter()
                            .zip(rhs.get_vector()?.iter())
                            .map(|(a, b)| {
                                (a.to_owned() * b.to_owned()).unwrap()
                            })
                            .collect::<Vec<_>>(),
                    ));
                } else {
                    return Err(Halt::Evaluating(format!(
                        "Unable to multiply {:?} by {:?}",
                        self, rhs
                    )));
                }
            }
            _ => Err(Halt::Evaluating(format!(
        "Multiplication not defined for a * b, where a = {:?} and b = {:?}",
        self, rhs
      ))),
        }
    }
}

impl MulAssign for RygVal {
    fn mul_assign(&mut self, rhs: Self) {
        let res = self.clone() * rhs;
        match res {
            Ok(s) => *self = s,
            Err(h) => *self = Self::Error(h),
        }
    }
}

impl Div for &mut RygVal {
    type Output = Maybe<RygVal>;
    fn div(self, rhs: Self) -> Self::Output {
        let me = self.clone();
        let other = rhs.clone();
        let res = &mut (me / other);
        res.clone()
    }
}

impl Div for RygVal {
    type Output = Maybe<RygVal>;
    fn div(self, rhs: Self) -> Self::Output {
        match (self.is_zero(), rhs.is_zero()) {
            (true, false) => Ok(self), // 0/x = 0 for all x != 0
            (_, true) => Err(Halt::DivisionByZero(format!(
                "Unable to divide {} by {}",
                self, rhs
            ))),
            _ => match self {
                Self::Nil() => Ok(NIL),
                Self::Bool(_) => todo!(),
                Self::Int(i) if self.match_kind(&rhs) => {
                    let r = rhs.get_int()?;
                    if i != 1 && (i % r == 0 || r % i == 0) {
                        Ok(Self::Int(i / r))
                    } else {
                        Ok(Self::Float(((i as f64) / (r as f64))))
                    }
                }
                Self::Float(r) => Ok(Self::Float(if r == 0.0 {
                    0.0
                } else {
                    r / rhs.get_float()?
                })),
                _ => Err(Halt::Evaluating(format!(
          "Division not defined for a / b, where a = {:?} and b = {:?}",
          self, rhs
        ))),
            },
        }
    }
}

impl Rem for RygVal {
    type Output = Maybe<RygVal>;
    fn rem(self, rhs: Self) -> Self::Output {
        match self {
            Self::Int(n) => Ok(Self::Int(n % rhs.get_int()?)),
            Self::Float(q) => Ok(Self::Float(q % rhs.get_float()?)),
            _ => Err(Halt::Evaluating(format!(
                "Remainder not defined for a % b, where a = {:?} and b = {:?}",
                self, rhs
            ))),
        }
    }
}

impl BitAnd for RygVal {
    type Output = Maybe<Self>;
    fn bitand(self, rhs: Self) -> Self::Output {
        if self.match_kind(&rhs) {
            match self {
                Self::Nil() | Self::Unit() => Ok(Self::Byte(0)),
                Self::Bool(b) => {
                    Ok(Self::Byte(if b & rhs.get_bool().unwrap() {
                        1 as u8
                    } else {
                        0 as u8
                    }))
                }
                Self::Char(c, ..) => match rhs {
                    Self::Char(d, ..) => Ok(Self::Byte((c as u8) & (d as u8))),
                    Self::Byte(b) => Ok(Self::Byte((c as u8) & b)),
                    _ => Err(Halt::InvalidType(format!(
                        "Bitwise And is not defined for {} and {}",
                        self, rhs
                    ))),
                },
                Self::Byte(b) => Ok(Self::Byte(b & rhs.get_byte().unwrap())),
                Self::Int(m) => match rhs {
                    Self::Int(n) => Ok(Self::Byte((m as u8) & (n as u8))),
                    _ => Err(Halt::InvalidType(format!(
                        "Bitwise And is not defined for {} and {}",
                        self, rhs
                    ))),
                },
                _ => Err(Halt::InvalidType(format!(
                    "Bitwise And is not defined for {} and {}",
                    self, rhs
                ))),
            }
        } else {
            Err(Halt::InvalidType(format!(
                "Bitwise And is not defined for {} and {}",
                self, rhs
            )))
        }
    }
}

impl BitOr for RygVal {
    type Output = Maybe<Self>;
    fn bitor(self, rhs: Self) -> Self::Output {
        if self.match_kind(&rhs) {
            match self {
                RygVal::Nil() | RygVal::Unit() => Ok(RygVal::Byte(0)),
                RygVal::Bool(b) => {
                    Ok(RygVal::Byte(if b | rhs.get_bool().unwrap() {
                        1 as u8
                    } else {
                        0 as u8
                    }))
                }
                RygVal::Byte(b) => {
                    Ok(RygVal::Byte(b | rhs.get_byte().unwrap()))
                }
                RygVal::Int(m) => Ok(RygVal::Int(m | rhs.get_int().unwrap())),
                _ => Err(Halt::InvalidType(format!(
                    "Bitwise Or is not defined for {} and {}",
                    self, rhs
                ))),
            }
        } else {
            Err(Halt::InvalidType(format!(
                "Bitwise Or is not defined for {} and {}",
                self, rhs
            )))
        }
    }
}

impl BitXor for RygVal {
    type Output = Maybe<RygVal>;
    fn bitxor(self, rhs: Self) -> Self::Output {
        if self.match_kind(&rhs) {
            match self {
                RygVal::Nil() | RygVal::Unit() => Ok(RygVal::Byte(0)),
                RygVal::Bool(b) => {
                    Ok(RygVal::Byte(if b ^ rhs.get_bool().unwrap() {
                        1 as u8
                    } else {
                        0 as u8
                    }))
                }
                RygVal::Byte(b) => {
                    Ok(RygVal::Byte(b ^ rhs.get_byte().unwrap()))
                }
                RygVal::Int(m) => Ok(RygVal::Int(m ^ rhs.get_int().unwrap())),
                _ => Err(Halt::InvalidType(format!(
                    "Bitwise Or is not defined for {} and {}",
                    self, rhs
                ))),
            }
        } else {
            Err(Halt::InvalidType(format!(
                "Bitwise Or is not defined for {} and {}",
                self, rhs
            )))
        }
    }
}

impl cmp::PartialOrd for RygVal {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        use cmp::Ordering;
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => {
                if a < b {
                    Some(Ordering::Less)
                } else if a > b {
                    Some(Ordering::Greater)
                } else {
                    Some(Ordering::Equal)
                }
            }
            (Self::Float(a), Self::Int(b)) => {
                if a < &(*b as f64) {
                    Some(Ordering::Less)
                } else if a > &(*b as f64) {
                    Some(Ordering::Greater)
                } else {
                    Some(Ordering::Equal)
                }
            }
            (Self::Int(a), Self::Float(b)) => {
                if &(*a as f64) < b {
                    Some(Ordering::Less)
                } else if &(*a as f64) > b {
                    Some(Ordering::Greater)
                } else {
                    Some(Ordering::Equal)
                }
            }
            (Self::Float(a), Self::Float(b)) => {
                if a < b {
                    Some(Ordering::Less)
                } else if a > b {
                    Some(Ordering::Greater)
                } else {
                    Some(Ordering::Equal)
                }
            }
            (Self::Char(a), Self::Char(b)) => {
                let a = *a as u8;
                let b = *b as u8;
                Some(if a < b {
                    Ordering::Less
                } else if a > b {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                })
            }
            _ => None,
        }
    }

    fn lt(&self, other: &Self) -> bool {
        matches!(self.partial_cmp(other), Some(cmp::Ordering::Less))
    }

    fn le(&self, other: &Self) -> bool {
        // Pattern `Some(Less | Eq)` optimizes worse than negating `None |
        // Some(Greater)`. FIXME: The root cause was fixed upstream in LLVM
        // with: https://github.com/llvm/llvm-project/commit/9bad7de9a3fb844f1ca2965f35d0c2a3d1e11775
        // Revert this workaround once support for LLVM 12 gets dropped.
        !matches!(self.partial_cmp(other), None | Some(cmp::Ordering::Greater))
    }

    fn gt(&self, other: &Self) -> bool {
        matches!(self.partial_cmp(other), Some(cmp::Ordering::Greater))
    }

    fn ge(&self, other: &Self) -> bool {
        matches!(
            self.partial_cmp(other),
            Some(cmp::Ordering::Greater | cmp::Ordering::Equal)
        )
    }
}

impl fmt::Debug for RygVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let map_join = |uv: &Vec<RygVal>| {
            uv.iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        };
        match self {
            Self::Nil() => write!(f, "()"),
            Self::Unit() => write!(f, "()"),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Byte(b) => write!(f, "{:b}", b),
            Self::Int(n) => write!(f, "Int {}", n),
            Self::Float(q) => write!(f, "Float {}", q),
            Self::Char(c) => {
                write!(f, "Char {:?}", c)
            }
            Self::String(s) => write!(f, "String {}", s),
            Self::Symbol(s) => write!(f, "Symbol {}", s),
            Self::Tuple(_, v) => {
                write!(f, "{}{}{}", "(", map_join(v), ")")
            }
            Self::Vector(v) => {
                write!(f, "{}{}{}", "[", map_join(v), "]")
            }
            Self::List(l) => {
                write!(f, "{:?}", l)
            }
            Self::Lambda(l) => write!(f, "λ. {:?}", l),
            Self::Error(e) => write!(f, "Error {:#?}", *e),
            Self::Function(RygFn {
                meta_in,
                meta_out,
                this: _,
                name: _,
            }) => {
                write!(f, "{} {} {}", meta_in, "->", meta_out)
            }
            Self::Dict(r) => write!(f, "{:?}", r),
            Self::Iter(v) => write!(f, "{:?}", v.range),
            Self::Data(data) => write!(f, "{:?}", data),
            Self::Holder(Container(k, b)) => write!(f, "{} {:?}", k, *b),
        }
    }
}

impl fmt::Display for Either<RygType, Vec<RygType>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Either::Left(t) => write!(f, "{}", t),
            Either::Right(ts) => write!(
                f,
                "{}",
                ts.into_iter()
                    .map(|c| format!("{}", c))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl fmt::Display for RygVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let red = |s: &str| Paint::fg_red(s);
        let yellow = |s: &str| Paint::fg_yellow(s);
        let cyan = |s: &str| Paint::fg_cyan(s);
        match self.clone() {
            Self::Nil() => write!(f, "nil"),
            Self::Unit() => write!(f, "()"),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Byte(b) => write!(f, "{:b}", b),
            Self::Int(n) => write!(f, "{}", n),
            Self::Float(q) => write!(f, "{}", q),
            Self::Char(c) => write!(f, "{:?}", c),
            Self::String(s) => write!(f, "{}{}{}", '"', s, '"'),
            Self::Symbol(s) => write!(f, "{}", s),
            Self::Tuple(_, u) => write!(
                f,
                "{}{}{}",
                cyan("("),
                u.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                cyan(")")
            ),
            Self::Vector(v) => write!(
                f,
                "{}{}{}",
                cyan("["),
                v.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                cyan("]")
            ),
            // Self::Lambda(l) => {
            //     let pram_types = {
            //         l.pram
            //             .iter()
            //             .map(|p| RygType::from(p.clone()).to_string())
            //             .collect::<Vec<_>>()
            //             .join(", ")
            //     };
            //     write!(
            //         f,
            //         "{} {}{}{}",
            //         if let Some(name) = &l.name { name } else { "λ" },
            //         red("|"),
            //         pram_types,
            //         red("|")
            //     )
            // }
            Self::Error(e) => write!(f, "{}", Paint::reset(format!("{}", e))),
            // Self::Dict(r) => write!(f, "{}", r),
            Self::List(l) => write!(f, "{:?}", l),
            Self::Iter(l) => write!(f, "{:?}", l),
            // Self::Object(obj) => write!(f, "{:?}", obj),
            Self::Dict(record) => write!(f, "{}", record),
            Self::Data(data) => write!(f, "{}", data),
            x @ _ => write!(f, "{}", x),
        }
    }
}
