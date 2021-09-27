use std::{
  fmt::{self, Display, Formatter},
  rc::Rc,
};

use crate::{
  lexing::token::Token,
  parsing::expression::{Parameter, Shape},
  util::{misc::Paint, state::Halt, types::Maybe},
};

use super::rygval::RygVal;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum RygType {
  // semi-boolean,
  Nil,
  Unit,
  Bool,
  Byte,
  Int,
  Float,
  Char,
  String,
  Symbol(String),
  Lambda,
  Function,
  Vector(Vec<RygType>),
  Tuple(Vec<RygType>),
  List(Box<RygType>),
  Record,
  Halt,
  Custom(String),
  // Compound(Kind, Vec<RygType>),
  Unknown,
  Dynamic(Option<Rc<RygType>>), // largely used internally
  R,
  Any,
}

impl Display for RygType {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    write!(f, "{}", self.to_string())
  }
}

impl From<Token> for RygType {
  fn from(token: Token) -> Self {
    match token {
      Token::String(..) => Self::String,
      Token::Char(..) => Self::Char,
      Token::Number(_, b, _) => {
        if b == 10 {
          Self::Float
        } else {
          Self::Int
        }
      }
      Token::Identifier(t, _) | Token::Meta(t, _) => match t.as_str() {
        "Int" => RygType::Int,
        "Float" => RygType::Float,
        "Bool" => RygType::Bool,
        "String" => RygType::String,
        "Nil" => RygType::Nil,
        _ => RygType::Custom(t),
      },
      Token::Symbol(s, _) => RygType::Symbol(s),
      Token::Boolean(..) => RygType::Bool,
      Token::Operator(_, _)
      | Token::Keyword(_, _)
      | Token::Punct(_, _)
      | Token::Invalid(_, _)
      | Token::Eof(_) => RygType::Unknown,
      Token::Empty() => RygType::Unit,
    }
  }
}

impl From<RygVal> for RygType {
  fn from(val: RygVal) -> Self {
    match val.clone() {
      RygVal::Nil() => Self::Nil,
      RygVal::Unit() => Self::Unit,
      RygVal::Bool(_) => Self::Bool,
      RygVal::Byte(_) => Self::Byte,
      RygVal::Int(_) => Self::Int,
      RygVal::Float(_) => Self::Float,
      RygVal::Char(..) => Self::Char,
      RygVal::String(_) => Self::String,
      RygVal::Symbol(s) => Self::Symbol(s),
      RygVal::Tuple(_, v) | RygVal::Vector(v) => {
        (if matches!(val, RygVal::Vector(_)) {
          Self::Vector
        } else {
          Self::Tuple
        })(
          v.iter()
            .map(|x| RygType::from(x.clone()))
            .collect::<Vec<_>>(),
        )
      }
      RygVal::Lambda(_) => Self::Lambda,
      RygVal::Error(_) => Self::Halt,
      RygVal::Function(_) => Self::Function,
      RygVal::Object(_) => Self::Record,
      RygVal::List(v) => Self::List(Box::new(v.kind)),
    }
  }
}

impl From<Vec<RygVal>> for RygType {
  fn from(v: Vec<RygVal>) -> Self {
    return RygType::Vector(
      v.iter()
        .map(|rv| RygType::from(rv.clone()))
        .collect::<Vec<_>>(),
    );
  }
}

impl From<(Shape, Vec<Token>)> for RygType {
  fn from((shape, tokens): (Shape, Vec<Token>)) -> Self {
    if tokens.len() == 0 {
      return RygType::Unit;
    };

    let mut rygtype = tokens
      .iter()
      .map(|t| RygType::from(t.clone()))
      .collect::<Vec<RygType>>();

    match shape {
      Shape::Vector => Self::Vector(rygtype),
      Shape::Tuple => Self::Tuple(rygtype),
      // Shape::List => Self::List()
      // Shape::Holder => todo!(),
      Shape::Unknown => RygType::Unknown,
      Shape::Empty => RygType::Unit,
      _ => rygtype.pop().unwrap_or(RygType::Nil),
    }
  }
}

impl From<Parameter> for RygType {
  fn from(pram: Parameter) -> Self {
    RygType::from((pram.shape, pram.kind.clone()))
  }
}

impl RygType {
  pub fn invalid_type(rygval: &RygVal, expected_type: &str) -> Halt {
    Halt::InvalidType(format!(
      "Invalid type! Expected a value of type {}, but was provided with {}, which is of type {}",
      expected_type,
      rygval,
      RygType::from(rygval.clone())
    ))
  }

  pub fn to_string(&self) -> String {
    String::from(match self {
      Self::Any => "Any".to_string(),
      Self::Nil => "Nil".to_string(),
      Self::Unit => "()".to_string(),
      Self::Bool => "Bool".to_string(),
      Self::Byte => "Byte".to_string(),
      Self::Int => "Int".to_string(),
      Self::Float => "Float".to_string(),
      Self::Char => "Char".to_string(),
      Self::String => "String".to_string(),
      Self::Symbol(x) => {
        format!("Symbol:{}", if x.len() == 0 { "_" } else { x })
      }
      Self::Lambda => "Lambda".to_string(),
      Self::Function => "Fn".to_string(),
      Self::Vector(v) | Self::Tuple(v) => v
        .iter()
        .map(|x| x.to_string())
        .collect::<Vec<String>>()
        .join(""),
      Self::Halt => "Halt".to_string(),
      Self::Record => "Record".to_string(),
      Self::Custom(s) => s.to_string(),
      Self::Dynamic(x) => {
        if let Some(t) = x {
          format!("Dynamic:{}", t)
        } else {
          "Dynamic:_".to_string()
        }
      }
      Self::Unknown => "X".to_string(),
      _ => stringify!(self).to_string(),
    })
  }

  pub fn is_int_vec(&self) -> bool {
    match self {
      Self::Vector(v) => v.iter().all(|t| matches!(t, Self::Int)),
      _ => false,
    }
  }
  pub fn is_int_tuple(&self) -> bool {
    match self {
      Self::Tuple(v) => v.iter().all(|t| matches!(t, Self::Int)),
      _ => false,
    }
  }
  pub fn is_float_vec(&self) -> bool {
    match self {
      Self::Vector(v) => v.iter().all(|t| matches!(t, Self::Float)),
      _ => false,
    }
  }
  pub fn is_float_tuple(&self) -> bool {
    match self {
      Self::Tuple(v) => v.iter().all(|t| matches!(t, Self::Float)),
      _ => false,
    }
  }
  pub fn is_num_vec(&self) -> bool {
    match self {
      Self::Vector(v) => {
        v.iter().all(|t| matches!(t, Self::Int | Self::Float))
      }
      _ => false,
    }
  }
  pub fn is_num_tuple(&self) -> bool {
    match self {
      Self::Tuple(v) => v.iter().all(|t| matches!(t, Self::Int | Self::Float)),
      _ => false,
    }
  }
  pub fn matches_variant(&self, variant: &Self) -> (bool, Option<RygType>) {
    // matches!((self, variant), )
    match (self, variant.clone()) {
      (Self::Nil, Self::Nil)
      | (Self::Bool, Self::Bool)
      | (Self::Unit, Self::Unit)
      | (Self::Byte, Self::Byte)
      | (Self::Int, Self::Int)
      | (Self::Float, Self::Float)
      | (Self::Function, Self::Function)
      | (Self::Halt, Self::Halt)
      | (Self::Lambda, Self::Lambda)
      | (Self::Record, Self::Record)
      | (Self::String, Self::String)
      | (Self::Symbol(_), Self::Symbol(_))
      | (Self::Tuple(_), Self::Tuple(_))
      | (Self::Custom(_), Self::Custom(_))
      | (Self::Vector(_), Self::Vector(_)) => (true, Some(self.clone())),
      _ => (false, None),
    }
  }
}

#[allow(unused)]
#[cfg(test)]
mod test {
  use std::collections::HashMap;

  use super::*;

  macro_rules! sub_expr_in {
    (x => $e:expr) => {
      println!("x: {}", $e)
    };
    (y => $e:expr) => {
      println!("y: {}", $e)
    };
    (z => $e:expr) => {
      println!("z: {}", stringify!($e))
    };
  }

  macro_rules! build_fn {
    ($func_name:ident) => {
      fn $func_name() {
        println!("You called {:?}()", stringify!($func_name))
      }
    };
  }

  macro_rules! print_expr {
    ($e:expr) => {
      println!("{:?} = {:?}", stringify!($e), $e)
    };
  }

  macro_rules! examine {
    ($l:expr; and $r:expr) => {
      println!(
        "{:?} and {:?} is {:?}",
        stringify!($l),
        stringify!($r),
        $l && $r
      )
    };
    ($l:expr; or $r:expr) => {
      println!(
        "{:?} or {:?} is {:?}",
        stringify!($l),
        stringify!($r),
        $l || $r
      )
    };
    ($l:expr; with $r:expr) => {
      println!(
        "{:?} with {:?} is {:?}",
        stringify!($l),
        stringify!($r),
        ($l, $r)
      )
    };
  }

  macro_rules! list_compr {
    ($id1: ident | $id2: ident <- [$start: expr , $end: expr] , $cond: expr) => {{
      let mut vec = Vec::new();

      for num in $start..$end + 1 {
        if $cond(num) {
          vec.push(num);
        }
      }

      vec
    }};
    ($id1: ident | $id2: ident <- [$start: expr , $end: expr] where $cond: expr) => {{
      let mut vec = Vec::new();

      for num in $start..$end + 1 {
        if $cond(num) {
          vec.push(num);
        }
      }

      vec
    }};
  }

  macro_rules! new_map {
    ($($key: expr => $val: expr)*) => {
      {
        let mut map = HashMap::new();
        $(
          map.insert($key, $val);
        )*
        map
      }
    };
  }

  macro_rules! compute {
    (eval $e:expr) => {{
      {
        let val: usize = $e;
        println!("{} = {}", stringify!($e), val)
      }
    }};
    (eval $e:expr, $($es:expr),+) => {
      {
        calc! { eval $e }
        calc! { $(eval $es),+ }
      }
    };
  }

  #[test]
  fn test_variants() {
    // sub_expr_in!(x => RygType::Byte);
    // sub_expr_in!(z => RygType::Byte);
    // build_fn!(ryggy);
    // ryggy();
    // print_expr!(RygType::Tuple(vec![RygType::Nil, RygType::Int]));
    // examine!(true; with false);
    // let list = list_compr!(x | x <- [1, 10] where |x| x > 0);
    // println!("{:?}", list);
    // let map = new_map!('4' => 4);
    // println!("{:?}", map);
    // compute! {eval 4 + 5};
    let val = RygVal::Int(4);
  }
}
