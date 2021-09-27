use std::{
  cmp,
  fmt::{self, Debug, Display, Formatter},
  ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub},
  rc::Rc,
};

use crate::{
  evaluating::environment::Field,
  lexing::token::Token,
  parsing::expression::{Parameter, Shape},
  util::{misc::Paint, state::Halt, types::Maybe},
};

use super::{
  atom::*,
  function::{Lambda, RygFn},
  list::RygList,
  record::Record,
  rygtype::RygType,
};

pub const TRUE: RygVal = RygVal::Bool(true);
pub const FALSE: RygVal = RygVal::Bool(false);
pub const NIL: RygVal = RygVal::Nil();
pub const UNIT: RygVal = RygVal::Unit();

#[derive(Clone)] //, PartialEq)]
pub enum RygVal {
  Nil(),
  Unit(),
  Bool(bool),
  Byte(u8),
  Int(i32),
  Float(f64),
  Char(char, Option<char>),
  String(String),
  Vector(Vec<RygVal>),
  Tuple(usize, Vec<RygVal>),
  List(RygList),
  Lambda(Lambda),
  Error(Box<Halt>),
  Function(RygFn),
  Object(Record),
}

impl From<Atom> for RygVal {
  fn from(atom: Atom) -> Self {
    match atom {
      Atom::Nil() => Self::Nil(),
      Atom::Bool(b) => Self::Bool(b),
      Atom::Byte(b) => Self::Byte(b),
      Atom::Int(i) => Self::Int(i),
      Atom::Float(q) => Self::Float(q),
      Atom::Char(c, d) => Self::Char(c, d),
      Atom::String(s) => Self::String(s),
      _ => Self::Error(Box::new(Halt::InvalidInput(format!(
        "Unable to convert {:?} into RygValue",
        atom.clone()
      )))),
    }
  }
}

impl From<char> for RygVal {
  fn from(ch: char) -> Self {
    RygVal::Char(ch, None)
  }
}

impl From<String> for RygVal {
  fn from(item: String) -> Self {
    RygVal::String(item)
  }
}

impl From<u8> for RygVal {
  fn from(b: u8) -> Self {
    RygVal::Byte(b)
  }
}

impl From<i32> for RygVal {
  fn from(item: i32) -> Self {
    RygVal::Int(item)
  }
}

impl From<f64> for RygVal {
  fn from(item: f64) -> Self {
    RygVal::Float(item)
  }
}

impl From<bool> for RygVal {
  fn from(item: bool) -> Self {
    RygVal::Bool(item)
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
    Self::from_atom(token_to_atom(token))
  }

  pub fn from_atom(atom: Atom) -> Self {
    Self::from(atom)
  }

  pub fn get_nil<T>(&self) -> Result<(), Halt> {
    if let Self::Nil() = self {
      Ok(())
    } else {
      Err(Halt::Evaluating(format!(
        "Trying to get nil from a nonnil type {:?}",
        self
      )))
    }
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
      RygVal::Char(c, x @ None) => Ok(*c),
      RygVal::Char(c, x @ Some(d)) => Ok(*c),
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
    } else if let Self::Char(c, d) = self {
      Ok({
        let mut s = String::from(c.to_string());
        if let Some(dd) = d {
          s.push(*dd)
        };
        s
      })
    } else if let Self::Vector(v) = self {
      if v.clone().iter().all(|rs| matches!(rs, RygVal::Char(..))) {
        Ok(
          v.iter()
            .filter_map(|rs| rs.get_string().ok())
            .collect::<Vec<_>>()
            .join(""),
        )
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
        s.chars().map(|c| RygVal::Char(c, None)).collect::<Vec<_>>()
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
      | (Self::Object(..), Self::Object(..))
      | (Self::Char(..), Self::Char(..))
      | (Self::String(..), Self::String(..))
      // | (Self::Symbol(..), Self::Symbol(..))
      | (Self::Tuple(..), Self::Tuple(..))
      | (Self::Unit(), Self::Unit())
      | (Self::List(_), Self::List(_))
      | (Self::Vector(..), Self::Vector(..)) => true,
      _ => false,
    }
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
        (Self::Int(n1), Self::Int(n2)) => n1 == n2 || (n1 <= n2 && n2 <= n1),
        (Self::Float(q1), Self::Float(q2)) => {
          q1 == q2
            || (q1 <= q2 && q2 <= q1)
            || (q1.abs() - q2.abs()).abs() < 1e-10_f64
        }
        (Self::String(s1), Self::String(s2)) => s1 == s2,
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
        (Self::Object(ro1), Self::Object(ro2)) => ro1 == ro2,
        _ => false,
      }
    }
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
      Self::Float(q) => Ok(Self::Float(q + rhs.get_float()?)),
      Self::String(s) => Ok(Self::String([s, rhs.get_string()?].join(""))),
      Self::Char(c1, d1) => match rhs {
        Self::Char(c2, d2) => Ok(Self::String(
          [d1, Some(c1), d2, Some(c2)]
            .iter()
            .map(|x| {
              if let Some(y) = x {
                y.to_string()
              } else {
                String::new()
              }
            })
            .collect::<Vec<_>>()
            .join(""),
        )),
        Self::String(mut s) => {
          let mut ch = if let Some(dd) = d1 {
            let mut x = dd.to_string();
            x.push_str(&c1.to_string());
            x
          } else {
            c1.to_string()
          };
          ch.push_str(&s);
          Ok(RygVal::String(ch))
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
          return Ok(Self::String(s.repeat(k as usize)));
        } else {
          return Err(Halt::Evaluating(format!(
            "Multiplication is not defined for {} and {}",
            self, rhs
          )));
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
              .map(|(a, b)| (a.to_owned() * b.to_owned()).unwrap())
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
          Ok(if i % r == 0 || r % i == 0 {
            Self::Int(i / r)
          } else {
            Self::Float((i / r).into())
          })
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
        Self::Bool(b) => Ok(Self::Byte(if b & rhs.get_bool().unwrap() {
          1 as u8
        } else {
          0 as u8
        })),
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
        RygVal::Bool(b) => Ok(RygVal::Byte(if b | rhs.get_bool().unwrap() {
          1 as u8
        } else {
          0 as u8
        })),
        RygVal::Byte(b) => Ok(RygVal::Byte(b | rhs.get_byte().unwrap())),
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
        RygVal::Bool(b) => Ok(RygVal::Byte(if b ^ rhs.get_bool().unwrap() {
          1 as u8
        } else {
          0 as u8
        })),
        RygVal::Byte(b) => Ok(RygVal::Byte(b ^ rhs.get_byte().unwrap())),
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
    match (self, other) {
      (Self::Int(a), Self::Int(b)) => {
        if a < b {
          Some(cmp::Ordering::Less)
        } else {
          if a > b {
            Some(cmp::Ordering::Greater)
          } else {
            Some(cmp::Ordering::Equal)
          }
        }
      }
      (Self::Float(b), Self::Int(a)) | (Self::Int(a), Self::Float(b)) => {
        if &(*a as f64) < b {
          Some(cmp::Ordering::Less)
        } else {
          if &(*a as f64) > b {
            Some(cmp::Ordering::Greater)
          } else {
            Some(cmp::Ordering::Equal)
          }
        }
      }
      (Self::Float(a), Self::Float(b)) => {
        if a < b {
          Some(cmp::Ordering::Less)
        } else {
          if a > b {
            Some(cmp::Ordering::Greater)
          } else {
            Some(cmp::Ordering::Equal)
          }
        }
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
    // let red = |s: &str| Paint::fg_red(String::from(s));
    let blue = |s: &str| Paint::fg_blue(String::from(s));
    let green = |s: &str| Paint::fg_green(String::from(s));
    match self {
      Self::Nil() => write!(f, "nil"),
      Self::Bool(b) => write!(f, "{}", b),
      Self::Byte(b) => write!(f, "{:b}", b),
      Self::Int(n) => write!(f, "Int {}", n),
      Self::Float(q) => write!(f, "Float {}", q),
      Self::Char(c, d) => {
        write!(f, "Char {}{}", if let Some(e) = d { e } else { &'\0' }, c)
      }
      Self::String(s) => write!(f, "String {}", s),
      Self::Tuple(_, v) => {
        write!(f, "{}{}{}", blue("("), map_join(v), blue(")"))
      }
      Self::Vector(v) => {
        write!(f, "{}{}{}", green("["), map_join(v), green("]"))
      }
      Self::List(l) => {
        write!(f, "{:?}", l)
      }
      Self::Lambda(l) => write!(f, "Lambda {:?}", l),
      Self::Error(e) => write!(f, "Error {:#?}", *e),
      Self::Function(m) => {
        let type_in = match &m.meta_in {
          (Shape::Atom, rtv) | (Shape::A, rtv) => {
            if let Some(rt) = rtv.get(0) {
              rt.clone()
            } else {
              RygType::R
            }
          }
          (Shape::Vector, t) => RygType::Vector(t.to_vec()),
          (Shape::Tuple, t) => RygType::Tuple(t.to_vec()),
          (Shape::R, rt) => RygType::Custom(
            rt.to_vec()
              .iter()
              .map(|t| t.clone().to_string())
              .collect::<Vec<_>>()
              .join(", "),
          ),
          (Shape::Empty, _) => RygType::Unit,
          (_, rt) => RygType::Custom(
            rt.iter()
              .map(|t| t.to_string())
              .collect::<Vec<_>>()
              .join(", "),
          ),
        };
        let type_out = if let Some((_, rt)) = &m.meta_out {
          rt.to_string()
        } else {
          RygType::Unit.to_string()
        };
        write!(
          f,
          "{} {} {}",
          type_in,
          Paint::fg_red(String::from("->")),
          type_out
        )
      }
      Self::Unit() => write!(f, "()"),
      Self::Object(r) => write!(
        f,
        "{{{}}}",
        r.map
          .iter()
          .map(|(k, v)| format!("\t{}{}", k, v))
          .collect::<Vec<_>>()
          .join("\n")
      ),
    }
  }
}

impl fmt::Display for RygVal {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let red = |s: &str| Paint::fg_red(String::from(s));
    let yellow = |s: &str| Paint::fg_yellow(String::from(s));
    let green = |s: &str| Paint::fg_green(String::from(s));
    match self {
      Self::Nil() => write!(f, "nil"),
      Self::Unit() => write!(f, "()"),
      Self::Bool(b) => write!(f, "{}", b),
      Self::Byte(b) => write!(f, "{:b}", b),
      Self::Int(n) => write!(f, "{}", n),
      Self::Float(q) => write!(f, "{}", q),
      Self::Char(c, Some(d)) => {
        write!(f, "{}{}{}{}", yellow("'"), c, d, yellow("'"))
      }
      Self::Char(c, None) => write!(f, "'{}'", c),
      Self::String(s) => write!(f, "{}{}{}", '"', s, '"'),
      Self::Tuple(_, u) => write!(
        f,
        "{}{}{}",
        green("("),
        u.iter()
          .map(|x| x.to_string())
          .collect::<Vec<_>>()
          .join(", "),
        green(")")
      ),
      Self::Vector(v) => write!(
        f,
        "{}{}{}",
        green("["),
        v.iter()
          .map(|x| x.to_string())
          .collect::<Vec<_>>()
          .join(", "),
        green("]")
      ),
      // Self::List(rl) => todo!(),
      Self::Lambda(l) => {
        let pram_types = {
          l.args
            .iter()
            .map(|p| RygType::from(p.clone()).to_string())
            .collect::<Vec<_>>()
            .join(", ")
        };
        write!(
          f,
          "{} {}{}{}",
          if let Some(name) = &l.name { name } else { "ʎ" },
          red("|"),
          pram_types,
          red("|")
        )
      }
      Self::Error(e) => write!(f, "{}", *e),
      Self::Function(ph) => {
        write!(
          f,
          "{:?} {} {}",
          ph.meta_in.1,
          Paint::fg_red(String::from("->")),
          if let Some((_, b)) = &ph.meta_out {
            b.clone()
          } else {
            Rc::new(RygType::Unknown)
          }
        )
      }
      Self::Object(r) => write!(f, "{:?}", r),
      Self::List(l) => todo!(),
    }
  }
}
