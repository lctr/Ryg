use std::{
    borrow::Borrow,
    cell::RefCell,
    fmt::{self, Display, Formatter},
    iter::Map,
    rc::Rc,
};

use crate::{
    parsing::expression::{
        Binding, DataDef, DataVariant, Expr, Morpheme, Parameter, Program,
        Shape, VariantArg, Visit,
    },
    tok::token::Token,
    util::{
        display::{map_join, Paint},
        state::Halt,
        types::{Either, Kind, Maybe},
    },
};

use super::{
    function::{Lambda, RygFn},
    rygval::RygVal,
    variant::RygEnum,
};

type Variadic = Either<Box<RygType>, Vec<RygType>>;
#[macro_export]
macro_rules! var_args {
  ($t:expr) => {
    {
      Either::Left(Box::new(t))
    }
  };
  ($t:expr, $( $x:expr ),*) => {{
    let mut temp = Vector::new();
    temp.push($t);
    $(
      temp.push($x)
    )*
    temp
  }}
}

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
    Lambda(Vec<RygType>, Option<Box<RygType>>),
    Function(Variadic, Box<RygType>),
    Vector(Vec<RygType>),
    Tuple(Vec<RygType>),
    List(Box<RygType>),
    Record(Vec<Field>),
    Struct(String, Vec<Field>),
    Union(Vec<RygType>),
    Custom(String, Vec<Field>),
    // Compound(Kind, Vec<RygType>),
    Kind(Kind<String>),
    Wrapped(String, Variadic),
    Data(Kind<String>, Kind<String>, Variadic),
    Dynamic(Box<RygType>), // generics?
    Var(String),           // or symbol-lookup
    Given(Box<RygType>, String),
    Halt,
    R,
    Any,
    Never,
    Unknown,
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
        let join =
            |vs: &Vec<RygType>| map_join(&vs, ", ", |c| format!("{}", c));
        String::from(match self {
            Self::Any => "*".to_string(),
            Self::Nil | // => "Nil".to_string(),
            Self::Unit => "()".to_string(),
            Self::Bool => "Bool".to_string(),
            Self::Byte => "Byte".to_string(),
            Self::Int => "Int".to_string(),
            Self::Float => "Float".to_string(),
            Self::Char => "Char".to_string(),
            Self::String => "String".to_string(),
            // -> stringify!(self)
            Self::Symbol(x) => format!("{}", x),
            Self::Lambda(a, Some(b)) => format!("{} -> {}", if a.len() > 0 {join(a)} else {"()".to_string()}, b),
            Self::Lambda(a, None) => format!("{} -> ()", if a.len() > 0 {join(a)} else { "()".to_string()}),
            Self::Vector(v) => format!("[{}]", join(v)),
            Self::Tuple(v) => format!("({})", join(v)),
            Self::Record(v) => format!("#{{ {} }}", v
                .iter()
                .map(|fld| format!("{}: {}", fld.get_name(), fld.get_type()))
                .collect::<Vec<_>>().join(", ")),
            Self::Halt => "Halt".to_string(),
            Self::Custom(s, _) => s.to_string(),
            Self::Dynamic(x) => { format!("{}", &self)
            }
            Self::Struct(a, b) => format!("{} {{ {} }}", a, b.iter().map(|fld| format!("{}", fld)).collect::<Vec<_>>().join(", ")), 
            Self::Wrapped(l, r) => match r {
                Either::Left(v) => format!("{} ({})", l, v),
                Either::Right(v) => {
                format!("{} ({})", l, join(v))
                }
            },
            Self::Unknown => "_".to_string(),
            Self::List(l) => l.to_string(),
            Self::R => "R".to_string(),
            Self::Never => "⟘".to_string(), /* _ => stringify!(self). */
            // to_string(),
            Self::Function(a, b) => match a {
                Either::Left(t) => format!("{} -> {}", t, b),
                Either::Right(ts) => format!("{} -> {}", join(ts), b),
            },
            Self::Union(vs) => format!("({})", vs.iter().map(|v| format!("{}", v)).collect::<Vec<_>>().join(" | ")),
            Self::Data(a, b, c) => {
                let inner = match c {
                    Either::Left(t1) => {*t1.to_owned()}
                    Either::Right(t2) => {RygType::Vector(t2.clone())}
                };
                format!("{} {} {}", a, b, inner)
            },
            Self::Kind(k) => format!("{}", k),
            Self::Var(label) => format!("@{}", label),
            Self::Given(ty, label) => format!("{} >- {}", label, ty)
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
            Self::Tuple(v) => {
                v.iter().all(|t| matches!(t, Self::Int | Self::Float))
            }
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
            | (Self::Function(..), Self::Function(..))
            | (Self::Halt, Self::Halt)
            | (Self::Lambda(..), Self::Lambda(..))
            | (Self::Record(_), Self::Record(_))
            | (Self::String, Self::String)
            | (Self::Symbol(_), Self::Symbol(_))
            | (Self::Tuple(_), Self::Tuple(_))
            | (Self::Custom(_, _), Self::Custom(_, _))
            | (Self::Struct(..), Self::Struct(..))
            | (Self::Var(_), Self::Var(_))
            | (Self::Data(..), Self::Data(..))
            | (Self::Vector(_), Self::Vector(_)) => (true, Some(self.clone())),
            _ => (false, None),
        }
    }

    pub fn has_deps(&self) -> bool {
        match self {
            RygType::Lambda(a, b) => {
                a.iter().any(|t| t.has_deps())
                    || match b {
                        Some(rt) => rt.has_deps(),
                        _ => false,
                    }
            }
            RygType::Function(a, b) => match (a, b.has_deps()) {
                (Either::Left(rt), false) => rt.has_deps(),
                (Either::Right(ts), false) => ts.iter().any(|t| t.has_deps()),
                (_, true) => true,
            },
            RygType::Vector(vs) | RygType::Tuple(vs) => {
                vs.iter().any(|t| t.has_deps())
            }
            RygType::List(ls) => ls.has_deps(),
            RygType::Record(fls) => fls.iter().any(|f| f.kind.has_deps()),
            RygType::Struct(_, _) => true,
            RygType::Union(tys) => tys.iter().any(|t| t.has_deps()),
            RygType::Var(_) | RygType::Given(..) => true,
            _ => false,
        }
    }
}

impl Display for RygType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let red = |s: &str| Paint::fg_red(s);
        let green = |s: &str| Paint::fg_green(s);
        let cyan = |s: &str| Paint::fg_cyan(s);
        let mut join = |rts: &Vec<RygType>, [left, right]: [&str; 2]| {
            write!(
                f,
                "{}{}{}",
                cyan(left),
                map_join(rts, ", ", |rt| format!("{}", rt)),
                cyan(right)
            )
        };
        match self {
            RygType::Int => {
                write!(f, "{}", "Int")
            }
            RygType::Float => {
                write!(f, "{}", "Float")
            }
            RygType::Symbol(s) => write!(f, "{}", s),
            RygType::Vector(ts) => join(ts, ["[", "]"]),
            RygType::Tuple(ts) => join(ts, ["(", ")"]),
            RygType::Record(ts) => {
                let (open, close) = (cyan("{"), cyan("}"));
                if ts.is_empty() {
                    write!(f, "#{} * {}", open, close)
                } else {
                    write!(
                        f,
                        "{} {} {}",
                        open,
                        ts.iter()
                            .map(|f| format!("{}: {}", f.name, f.kind))
                            .collect::<Vec<_>>()
                            .join(if ts.len() > 2 { ",\n\t  " } else { ", " }),
                        close,
                    )
                }
            }
            // RygType::List(_) => todo!(),
            RygType::Custom(x, _) => write!(f, "{}", green(x)),
            RygType::Dynamic(d) => write!(f, "{}", d.as_ref()),
            RygType::Wrapped(wr, Either::Left(t)) => {
                write!(f, "{} {}{}{}", green(wr), cyan("("), t, cyan(")"))
            }
            RygType::Wrapped(wr, Either::Right(ts)) => {
                write!(
                    f,
                    "{} {}{}{}",
                    wr,
                    cyan("("),
                    map_join(ts, ", ", |c| format!("{}", c)),
                    cyan(")")
                )
            }
            RygType::Function(a, b) => match a {
                Either::Left(t) => {
                    write!(
                        f,
                        "{}{} {} {}{}",
                        cyan("("),
                        t,
                        red("->"),
                        b,
                        cyan(")")
                    )
                }
                Either::Right(ts) => {
                    write!(
                        f,
                        "{}{} {} {}{}",
                        cyan("("),
                        map_join(ts, &red("/"), |c| format!("{}", c)),
                        red("->"),
                        b,
                        cyan(")")
                    )
                }
            },
            RygType::Lambda(ty_args, Some(ty_ret)) => {
                write!(
                    f,
                    "{} -> {}",
                    map_join(ty_args, " ", |c| format!("{}", c)),
                    ty_ret
                )
            }
            RygType::Lambda(ty_args, None) => write!(
                f,
                "{} ()",
                map_join(ty_args, " ", |c| format!("{}", c))
            ),
            RygType::Struct(a, b) => {
                write!(f, "{} {}{}", a, cyan("{"), cyan("}"))
            }
            RygType::Union(ts) => {
                write!(
                    f,
                    "{}",
                    map_join(ts, &red(" | "), |c| format!("{}", c))
                )
            }
            RygType::Never => write!(f, "{}", cyan("⟘")),
            RygType::Unknown => write!(f, "{}", green("_")),
            RygType::Data(a, b, c) => {
                let inner = match c {
                    Either::Left(t1) => *t1.to_owned(),
                    Either::Right(t2) => RygType::Vector(t2.clone()),
                };
                write!(f, "{} {} {}", a, b, inner)
            }
            _ => write!(f, "{}", self.to_string()),
        }
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
            Token::Meta(t, p) => match t.as_str() {
                "Int" => RygType::Int,
                "Float" => RygType::Float,
                "Bool" => RygType::Bool,
                "Char" => RygType::Char,
                "String" => RygType::String,
                "Nil" => RygType::Nil,
                _ => RygType::Kind(Kind(t)),
            },
            Token::Identifier(t, _) => RygType::Var(t),
            Token::Meta(ty, _) => RygType::Kind(Kind(ty)),
            Token::Symbol(s, _) => RygType::Symbol(s),
            Token::Bool(..) => RygType::Bool,
            Token::Operator(_, _)
            | Token::Keyword(_, _)
            | Token::Punct(_, _)
            | Token::Invalid(_, _)
            | Token::Eof(_) => RygType::Never,
            Token::Empty() => RygType::Unit,
        }
    }
}

impl From<&Token> for RygType {
    fn from(token: &Token) -> Self {
        Self::from(token.to_owned())
    }
}

impl From<&RygVal> for RygType {
    fn from(val: &RygVal) -> Self {
        Self::from(val.to_owned())
    }
}

impl From<&mut RygVal> for RygType {
    fn from(item: &mut RygVal) -> Self {
        Self::from(item.clone())
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
            RygVal::Lambda(Lambda { pram, body, .. }) => {
                let ty_args = pram
                    .iter()
                    .map(|p| RygType::from(p.kind.clone()))
                    .collect::<Vec<_>>();
                let ty_ret = RygType::from(*body);
                Self::Lambda(ty_args, Some(Box::new(ty_ret)))
            }
            RygVal::Error(_) => Self::Halt,
            RygVal::Function(RygFn {
                meta_in,
                meta_out,
                name: _,
                this: _,
            }) => Self::Function(
                match meta_in {
                    Either::Left(t) => Either::Left(Box::new(t)),
                    Either::Right(ts) => Either::Right(ts),
                },
                Box::new(meta_out),
            ),
            RygVal::Dict(rec) => Self::Record(
                rec.entries
                    .iter()
                    .map(|(a, b)| Field::new(a.to_string(), &RygType::from(b)))
                    .collect::<Vec<_>>(),
            ),
            RygVal::List(v) => Self::List(Box::new(v.kind)),
            RygVal::Iter(v) => {
                Self::Vector(vec![RygType::Vector(vec![RygType::Int])])
            }
            RygVal::Data(variant) => RygType::from(variant),
            _ => Self::Unknown,
        }
    }
}
impl From<Box<RygVal>> for RygType {
    fn from(item: Box<RygVal>) -> Self {
        Self::from(*item)
    }
}

impl From<Vec<RygVal>> for RygType {
    fn from(v: Vec<RygVal>) -> Self {
        return Self::Vector(
            v.iter()
                .map(|rv| Self::from(rv.clone()))
                .collect::<Vec<_>>(),
        );
    }
}

impl From<(Shape, Vec<Token>)> for RygType {
    fn from((shape, tokens): (Shape, Vec<Token>)) -> Self {
        if tokens.len() == 0 {
            return Self::Unit;
        };

        let mut rygtype = tokens
            .iter()
            .map(|t| Self::from(t.clone()))
            .collect::<Vec<RygType>>();

        match shape {
            Shape::Vector => Self::Vector(rygtype),
            Shape::Tuple => Self::Tuple(rygtype),
            Shape::Record => Self::Record(
                tokens
                    .into_iter()
                    .map(|tk| Field::new(tk.literal(), &RygType::from(tk)))
                    .collect::<Vec<_>>(),
            ),
            // Shape::List => Self::List()
            // Shape::Holder => todo!(),
            Shape::Unknown => RygType::Unknown,
            Shape::Empty => RygType::Unit,
            _ => rygtype.pop().unwrap_or(RygType::Unknown),
        }
    }
}

impl From<Morpheme> for RygType {
    fn from(morpheme: Morpheme) -> Self {
        match morpheme {
            Morpheme::Empty => RygType::Unknown,
            Morpheme::Meta(tk) => RygType::Kind(Kind(tk.literal())),
            Morpheme::Atom(a) => RygType::from(a),
            Morpheme::Tuple(ts) => RygType::Tuple(
                ts.iter()
                    .map(|lx| RygType::from(lx.to_owned()))
                    .collect::<Vec<_>>(),
            ),
            Morpheme::Vector(ts) => RygType::Vector({
                let mut ty = ts
                    .iter()
                    .map(|lx| RygType::from(lx.to_owned()))
                    .collect::<Vec<_>>();
                ty.dedup();
                ty
            }),
            Morpheme::Record(ls) => RygType::Record(
                ls.iter()
                    .flat_map(|lx| {
                        let toks = lx.get_tokens();
                        let rt = RygType::from(lx.to_owned());
                        match rt {
                            RygType::Vector(vs) | RygType::Tuple(vs) => toks
                                .iter()
                                .zip(vs.iter())
                                .map(|(vn, vt)| Field::new(vn.literal(), vt))
                                .collect::<Vec<_>>(),
                            _ => {
                                let k = if let Some(n) = toks.first() {
                                    n.literal()
                                } else {
                                    "_".to_string()
                                };
                                vec![Field::new(k, &rt)]
                            }
                        }
                    })
                    .collect::<Vec<_>>(),
            ),
            Morpheme::Iter(_) => todo!(),
            Morpheme::Call(_, _) => todo!(),
            Morpheme::Unary(_, _) => todo!(),
            Morpheme::Binary(_, _, _) => todo!(),
            Morpheme::Rest(t) => RygType::Vector({
                let mut ts = vec![RygType::from(t)];
                ts.dedup();
                ts
            }),
        }
    }
}

impl From<Parameter> for RygType {
    fn from(pram: Parameter) -> Self {
        let Parameter {
            name,
            kind,
            pattern,
            shape,
        } = pram;
        if matches!(name, Token::Empty()) {
            RygType::from(kind.clone())
        } else {
            RygType::from(name.clone())
        }
    }
}

impl From<&Parameter> for RygType {
    fn from(pram: &Parameter) -> Self {
        RygType::from(pram.clone())
    }
}

impl From<Kind<String>> for RygType {
    fn from(kind: Kind<String>) -> Self {
        RygType::Kind(kind)
    }
}

impl From<&Expr> for RygType {
    fn from(expr: &Expr) -> Self {
        Self::from(expr.clone())
    }
}

impl From<Expr> for RygType {
    fn from(expr: Expr) -> Self {
        // println!("tokens from from walking_expr: {:#?}", {
        //     let mut toks = vec![];
        //     &expr.visit(&mut |t| toks.push(t.clone()));
        //     toks
        // });
        match expr {
            Expr::Nil => RygType::Nil,
            Expr::Assign(_, l, r) => match RygType::from(*l) {
                RygType::Unknown => RygType::from(*r),
                x @ _ => x,
            },
            Expr::Binary(op, l, r) => match op.clone() {
                Token::Operator(o, ..) => match o.as_str() {
                    "||" | "&&" => RygType::Bool,
                    "/" => RygType::Float,
                    "%" => RygType::Int,
                    "*" | "-" => {
                        let ty_l = RygType::from(*l);
                        let ty_r = RygType::from(*r);
                        match (ty_l, ty_r) {
                            (RygType::Int, RygType::Int) => RygType::Int,
                            (RygType::Int, RygType::Float)
                            | (RygType::Float, RygType::Int)
                            | (RygType::Float, RygType::Float) => {
                                RygType::Float
                            }
                            (num @ (RygType::Float | RygType::Int), _)
                            | (_, num @ (RygType::Float | RygType::Int)) => {
                                num
                            }
                            _ => RygType::Halt,
                        }
                    }
                    "+" => {
                        let ty_l = RygType::from(*l);
                        let ty_r = RygType::from(*r.clone());
                        match (&ty_l, &ty_r) {
                            (RygType::Vector(xs), RygType::Int)
                                if xs
                                    .iter()
                                    .all(|x| matches!(x, RygType::Int)) =>
                            {
                                RygType::Vector(vec![RygType::Int])
                            }
                            (RygType::Vector(_), RygType::Float) => {
                                RygType::Vector(vec![RygType::Float])
                            }

                            (
                                RygType::String | RygType::Char,
                                RygType::String | RygType::Char,
                            ) => RygType::String,
                            (
                                RygType::Int | RygType::Byte,
                                RygType::Int | RygType::Byte,
                            ) => RygType::Int,
                            (RygType::Int, RygType::Float)
                            | (
                                RygType::Float,
                                RygType::Int | RygType::Float,
                            ) => RygType::Float,
                            (RygType::Int, RygType::Unknown) => RygType::Int,
                            (RygType::Float, RygType::Unknown) => {
                                RygType::Float
                            }
                            (
                                RygType::String | RygType::Char,
                                RygType::Unknown,
                            ) => RygType::String,
                            _ => RygType::Given(Box::new(RygType::Int), {
                                let mut s = String::new();
                                r.visit(&mut |t| s = t.literal());
                                s
                            }),
                        }
                    }
                    "++" => RygType::Tuple(vec![
                        RygType::from(*l),
                        RygType::from(*r),
                    ]),
                    "<>" => {
                        let ty_l = RygType::from(*l);
                        let ty_r = RygType::from(*r);
                        RygType::Vector(if ty_l.matches_variant(&ty_r).0 {
                            vec![ty_l]
                        } else {
                            vec![ty_l, ty_r]
                        })
                    }
                    "=" | "=<" | "<-" | "+=" | "-=" | "*=" | "/=" | "%=" => {
                        RygType::from(*r)
                    }
                    _ => RygType::Halt,
                },
                _ => RygType::Halt,
            },
            Expr::Block(_, xs) => xs
                .iter()
                .fold(RygType::Unit, |a, x| RygType::from(x.to_owned())),
            Expr::Call(b, ps, n) => RygType::Lambda(
                ps.iter().map(|p| RygType::from(p)).collect::<Vec<_>>(),
                Some(Box::new(RygType::from(*b))),
            ),
            Expr::Case(_, b, deft) => {
                let deft_ty = RygType::from(deft.as_ref());
                b.iter().fold(deft_ty, |a, (b, c)| {
                    if a == RygType::from(b) {
                        RygType::from(c)
                    } else {
                        RygType::Halt
                    }
                })
            }
            Expr::Conditional(cond, then, deft) => {
                if RygType::from(cond.as_ref()) != RygType::Bool {
                    RygType::Halt
                } else {
                    let ty_a = RygType::from(then.as_ref());
                    if let Some(b) = deft {
                        let ty_b = RygType::from(b.as_ref());
                        if ty_a == ty_b {
                            ty_a
                        } else {
                            RygType::Union(vec![ty_a, ty_b])
                        }
                    } else {
                        RygType::Union(vec![ty_a, RygType::Nil])
                    }
                }
            }
            Expr::Index(b, i) => RygType::Unknown,
            Expr::Iter(_, _) => RygType::List(Box::new(RygType::Int)),
            Expr::List(_) => RygType::Unknown,
            Expr::Range(_, _, _) => RygType::Unknown,
            Expr::Lambda(n, ps, b) => RygType::Lambda(
                ps.iter().map(|p| RygType::from(p)).collect::<Vec<_>>(),
                Some(Box::new(RygType::from(*b))),
            ),
            Expr::Literal(x) => RygType::from(x),
            Expr::Loop(_, x) => RygType::from(*x),
            Expr::Tuple(xs) => RygType::Tuple(
                xs.iter().map(|p| RygType::from(p)).collect::<Vec<_>>(),
            ),
            Expr::Unary(op, x) => {
                if op.match_literal("!") {
                    RygType::Bool
                } else {
                    RygType::from(*x)
                }
            }
            Expr::Variable(_, _) => RygType::Unknown,
            Expr::Vector(xs) => RygType::Vector(
                xs.iter().map(|p| RygType::from(p)).collect::<Vec<_>>(),
            ),
            Expr::Error(_, _) => RygType::Halt,
            Expr::Return(_, _) => RygType::Unknown,
            Expr::Named(n, x) => RygType::Wrapped(
                n.0.literal(),
                Either::Left(Box::new(RygType::from(*x))),
            ),
            Expr::Record(k, xs) => RygType::Record(
                xs.iter()
                    .map(|Binding { pram, expr }| {
                        Field::new(pram.name.literal(), &RygType::from(expr))
                    })
                    .collect::<Vec<_>>(),
            ),
            Expr::Program(Program { name, body, vocab }) => {
                body.iter().fold(RygType::Unit, |p, c| RygType::from(c))
            }
            Expr::Path(root, branch) => RygType::Unknown,
            _ => RygType::Unknown,
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Field {
    pub name: String,
    pub kind: RygType,
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let [l, m, r] = [
            Paint::fg_light_red("("),
            Paint::fg_red("::"),
            Paint::fg_light_red(")"),
        ];
        write!(
            f,
            "{}{:<9.*} {} {:>18.*}{}\n",
            l,
            36,
            m,
            self.name,
            36,
            self.kind.to_string(),
            r
        )
    }
}

impl fmt::Debug for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({} :: {})",
            self.name,
            self.kind,
            // " ".repeat(self.name.len())
        )
    }
}

impl From<Token> for Field {
    fn from(token: Token) -> Self {
        Field::new(token.to_string().as_str(), &RygType::from(token))
    }
}

impl From<(String, RygType)> for Field {
    fn from((name, kind): (String, RygType)) -> Self {
        Field::new(&name, &kind)
    }
}

impl From<(&str, &RygVal)> for Field {
    fn from((name, value): (&str, &RygVal)) -> Self {
        Field::new(name, &RygType::from(value.clone()))
    }
}

impl Field {
    pub fn new(name: impl std::fmt::Display, kind: &RygType) -> Field {
        Field {
            name: name.to_string(),
            kind: kind.to_owned(),
        }
    }
    pub fn from_val(name: &str, value: &RygVal) -> Field {
        Field::from((name, value))
    }
    pub fn get_name(&self) -> String {
        self.name.clone()
    }
    pub fn get_type(&self) -> RygType {
        self.kind.clone()
    }
}

#[allow(unused)]
#[cfg(test)]
mod test {
    use std::{any::TypeId, collections::HashMap};

    use crate::log_do;

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
        log_do!(
            "id_of" => format!("{:?}", TypeId::of::<RygType>())
        )
    }
}
