use std::rc::Rc;

use crate::{
    tok::{
        literal::Literal,
        stream::Pos,
        token::{AssignOp, BinOp, FromToken, Token, UnOp},
    },
    util::types::Kind,
};

use super::expression::{Binding, Expr, Parameter, Shape};

#[derive(Debug, Clone, PartialEq)]
pub enum Morpheme<T, X> {
    Empty,
    Wild(T),
    Rest(T),
    Meta(T),
    Atom(T),
    Spread(Rc<Self>),
    Tuple(Vec<Self>),
    Vector(Vec<Self>),
    Record(Vec<Self>),
    Iter(Vec<Self>),
    Struct(T, Vec<Self>),
    Call(T, Vec<Self>),
    Unary(T, Rc<Self>),
    Binary(T, Rc<Self>, Rc<Self>),
    Closure(Vec<Self>, Rc<Self>),
    Union(Vec<Self>),
    At(Rc<Self>, X),
}

impl Morpheme<Token, Expr> {
    pub fn get_tokens<'t>(&'t self) -> Vec<&'t Token> {
        // use crate::tok::token::
        let mut acc = vec![];
        match self {
            Morpheme::Empty => {}
            Morpheme::Meta(t) => acc.push(t),
            Morpheme::Atom(t) => {
                acc.push(t);
            }
            Morpheme::Tuple(ts)
            | Morpheme::Vector(ts)
            | Morpheme::Iter(ts)
            | Morpheme::Record(ts) => ts.iter().for_each(|tp| {
                tp.get_tokens().iter().for_each(|t| acc.push(*t))
            }),
            Morpheme::Struct(class, ts) => {
                acc.push(class);
                ts.iter().for_each(|t| {
                    t.get_tokens().iter().for_each(|&t| acc.push(t))
                });
            }
            Morpheme::Call(a, bs) => {
                acc.push(a);
                bs.iter().for_each(|tp| {
                    tp.get_tokens().into_iter().for_each(|t| acc.push(t));
                })
            }
            Morpheme::Unary(t, b) => {
                acc.push(t);
                b.get_tokens().into_iter().for_each(|t| acc.push(t));
            }
            Morpheme::Binary(t, b, c) => {
                acc.push(t);
                b.get_tokens().iter().for_each(|t| acc.push(*t));
                c.get_tokens().iter().for_each(|t| acc.push(*t));
            }
            Morpheme::Rest(t) => acc.push(t),
            Morpheme::Spread(m) => acc.extend(m.get_tokens().iter()),
            Morpheme::Closure(a, b) => {
                a.into_iter()
                    .for_each(|m| acc.extend(m.get_tokens().iter()));
                b.get_tokens().iter().for_each(|t| acc.push(t));
            }
            Morpheme::Union(ms) => ms
                .into_iter()
                .for_each(|m| acc.extend(m.get_tokens().iter())),
            Morpheme::At(ms, _xp) => acc.extend(ms.get_tokens().iter()),
            Morpheme::Wild(t) => acc.push(t),
        };
        acc
    }
    pub fn has_rest(&self) -> bool {
        match self {
            Self::Empty | Self::Wild(_) => false,
            Self::Rest(_) | Self::Spread(_) => true,
            Self::Meta(_) => false,
            Self::Atom(_) => false,
            Self::Tuple(ms)
            | Self::Vector(ms)
            | Self::Record(ms)
            | Self::Call(_, ms)
            | Self::Struct(_, ms)
            | Self::Union(ms)
            | Self::Iter(ms) => ms.iter().any(Self::has_rest),
            Self::Unary(o, m) => Self::has_rest(&**m),
            Self::Binary(o, n, m) => {
                Self::has_rest(&**n) || Self::has_rest(&**m)
            }
            Self::Closure(a, b) => {
                a.iter().any(Self::has_rest) || b.has_rest()
            }
            Self::At(a, _b) => a.has_rest(),
        }
    }
}

impl Literal for Morpheme<Token, Expr> {
    type Lit = String;
    fn literal(&self) -> Self::Lit {
        let mut lit = String::new();
        macro_rules! join {
            ($x:expr) => {
                join! {$x, ", "}
            };
            ($x:expr, <> $y:expr) => {
                &*$x.into_iter()
                    .map(|m| m.literal())
                    .collect::<Vec<_>>()
                    .join(format!("{}", &$y))
            };
            ($x:expr, $d:literal) => {
                &*$x.into_iter()
                    .map(|m| m.literal())
                    .collect::<Vec<_>>()
                    .join($d)
            };

            ($l:tt | $i:ident $d:literal | $r:tt) => {
                let _x = $i;
                let _s = join!(_x, stringify!($d));
                format!("{}{}{}", stringify($l), _s, stringify!($r))
            };
        }
        match self {
            Morpheme::Empty => {}
            Morpheme::Wild(t) => lit.push_str(&t.literal()),
            Morpheme::Rest(t) => {
                lit.push_str("..");
                lit.push_str(&t.literal());
            }
            Morpheme::Meta(t) | Morpheme::Atom(t) => {
                lit.push_str(&t.literal())
            }
            Morpheme::Spread(t) => {
                lit.push_str(&*["...", &t.literal()].concat());
                // lit.push_str("...");
                // lit.push_str(&t.literal());
            }
            Morpheme::Tuple(t) => {
                lit.push_str(&*["(", join!(t), ")"].concat());
            }
            Morpheme::Vector(t) => {
                lit.push_str(&*["[", join!(t), "]"].concat());
            }
            Morpheme::Record(t) => {
                lit.push_str(&*["{", join!(t), "}"].concat());
            }
            Morpheme::Iter(_) => todo!(),
            Morpheme::Struct(kind, ts) => {
                lit.push_str(
                    &*[&*kind.literal(), " {\n", join!(ts), "\n}"].concat(),
                );
            }
            Morpheme::Call(kind, ts) => {
                lit.push_str(
                    &*[&kind.literal(), "(", join!(ts), ")"].concat(),
                );
            }
            Morpheme::Unary(_, _) => todo!(),
            Morpheme::Binary(_, _, _) => todo!(),
            Morpheme::Closure(_, _) => todo!(),
            Morpheme::Union(ts) => {
                lit.push_str(&*["(", join!(ts, " | "), ")"].concat());
            }
            Morpheme::At(p, x) => {
                lit.push_str(&*p.literal());
                lit.push_str(" @ ");
                lit.push_str(&*x.literal())
            }
        };
        lit
    }
}

impl From<&Morpheme<Token, Expr>> for Expr {
    fn from(tp: &Morpheme<Token, Expr>) -> Self {
        Self::from(tp.clone())
    }
}

impl From<Morpheme<Token, Expr>> for Expr {
    fn from(tokpat: Morpheme<Token, Expr>) -> Self {
        match tokpat {
            Morpheme::Empty => Expr::Empty,
            Morpheme::Wild(t) => Expr::Literal(t),
            // figure this out for rest parameters
            Morpheme::Meta(t) => Expr::Literal(t),
            Morpheme::Atom(t) => Expr::Literal(t),
            Morpheme::Iter(vs) => Expr::Named(
                Kind(Token::from("Iter")),
                Box::new(Expr::Vector(
                    vs.iter().map(|tp| Expr::from(tp)).collect::<Vec<_>>(),
                )),
            ),
            Morpheme::Vector(vs) => Expr::Vector(
                vs.iter()
                    .map(|v| Expr::from(v.to_owned()))
                    .collect::<Vec<_>>(),
            ),
            Morpheme::Tuple(vs) => Expr::Tuple(
                vs.iter()
                    .map(|v| Expr::from(v.to_owned()))
                    .collect::<Vec<_>>(),
            ),
            Morpheme::Struct(class, vs) => Expr::Record(
                Kind(class),
                vs.iter()
                    .map(|v| {
                        let toks = v.get_tokens();
                        Binding {
                            pram: Parameter {
                                pattern: v.to_owned(),
                                shape: Shape::Record,
                                kind: v.clone(),
                            },
                            expr: Expr::from(v.to_owned()),
                        }
                    })
                    .collect::<Vec<_>>(),
            ),
            Morpheme::Record(vs) => Expr::Record(
                Kind(Token::Empty),
                vs.iter()
                    .map(|v| {
                        let toks = v.get_tokens();
                        Binding {
                            pram: Parameter {
                                pattern: v.to_owned(),
                                shape: Shape::Record,
                                kind: v.clone(),
                            },
                            expr: Expr::from(v.to_owned()),
                        }
                    })
                    .collect::<Vec<_>>(),
            ),
            Morpheme::Call(n, a) => Expr::Call(
                Box::new(Expr::Literal(n)),
                a.iter().map(|v| Expr::from(v.clone())).collect::<Vec<_>>(),
                None,
            ),
            Morpheme::Unary(t, p) => Expr::Unary(
                UnOp::from_token(&t).unwrap(),
                Box::new(Expr::from(p.as_ref())),
            ),
            Morpheme::Binary(t, a, b) => Expr::Binary(
                BinOp::from_token(&t).unwrap(),
                Box::new(Expr::from(a.as_ref())),
                Box::new(Expr::from(b.as_ref())),
            ),
            Morpheme::Spread(m) => Expr::from(m.as_ref()),
            Morpheme::Rest(t) => Expr::Literal(t),
            Morpheme::Closure(a, b) => Expr::Lambda(
                a.into_iter()
                    .map(|m| Parameter {
                        pattern: m.clone(),
                        kind: m.clone(),
                        shape: match &m {
                            Morpheme::Vector(_) => Shape::Vector,
                            Morpheme::Tuple(_) => Shape::Tuple,
                            Morpheme::Record(_) => Shape::Record,
                            Morpheme::Empty => Shape::Empty,
                            Morpheme::Struct(..) => Shape::Struct,
                            Morpheme::Closure(..) => Shape::Closure,
                            Morpheme::Atom(_) => Shape::Atom,
                            _ => Shape::Unknown,
                        },
                    })
                    .collect::<Vec<_>>(),
                Box::new(Expr::from(b.as_ref())),
            ),
            Morpheme::Union(ts) => ts.into_iter().fold(Expr::Empty, |a, c| {
                Expr::Binary(BinOp::Or, Box::new(a), Box::new(Expr::from(c)))
            }),
            Morpheme::At(a, b) => b,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        parsing::{expression::Program, parser::parse_input},
        tok::lexer::Pos,
    };

    use super::*;

    #[test]
    fn vector_morpheme_tokens() {
        let a = Token::Identifier(String::from("a"), Pos::faux());
        let b = Token::Identifier(String::from("b"), Pos::faux());
        let c = Token::Identifier(String::from("c"), Pos::faux());
        let rest = Token::Operator(String::from("..."), Pos::faux());
        let morpheme = Morpheme::Vector(vec![
            Morpheme::Atom(Token::Identifier(String::from("a"), Pos::faux())),
            Morpheme::Tuple(vec![
                Morpheme::Atom(Token::Identifier(
                    String::from("b"),
                    Pos::faux(),
                )),
                Morpheme::Atom(Token::Identifier(
                    String::from("c"),
                    Pos::faux(),
                )),
            ]),
            Morpheme::Rest(Token::Operator(String::from("..."), Pos::faux())),
        ]);
        let toks = morpheme.get_tokens();
        println!("{:?}", &toks);
        assert_eq!(toks, vec![&a, &b, &c, &rest])
    }

    #[test]
    fn morpheme_literal() {
        let morpheme = Morpheme::Vector(vec![
            Morpheme::Atom(Token::Identifier(String::from("a"), Pos::faux())),
            Morpheme::Tuple(vec![
                Morpheme::Atom(Token::Identifier(
                    String::from("b"),
                    Pos::faux(),
                )),
                Morpheme::Atom(Token::Identifier(
                    String::from("c"),
                    Pos::faux(),
                )),
            ]),
            Morpheme::Rest(Token::Identifier(String::from("c"), Pos::faux())),
        ]);
        let morphstr = morpheme.literal();
        println!("{}", morphstr);
    }

    #[test]
    fn morphemes_literal() {
        let src = "a, ...[b, c]";
        let ast = parse_input(&*format!("|{}| {{}}", &src));
        if let Expr::Program(Program { body, .. }) = ast {
            if let [Expr::Lambda(pram, _)] = &body[..] {
                let pp = pram
                    .iter()
                    .map(|p| p.pattern.literal())
                    .collect::<Vec<_>>()
                    .join(", ");
                println!("{}", &pp);
                assert_eq!(&pp, src)
            }
        }
    }
}
