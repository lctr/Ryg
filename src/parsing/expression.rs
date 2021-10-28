use std::{
    collections::HashSet,
    fmt::{self, Debug},
    rc::Rc,
};

use crate::{
    core::rygtype::Field,
    simple_pub_struct,
    tok::{
        lexer::{Pos, ToLiteral},
        token::{Assoc, Token},
    },
    util::{
        display::map_join,
        state::Halt,
        types::{Either, Kind},
    },
};

#[derive(Debug, Clone, PartialEq)]
pub enum Morpheme {
    Empty,
    Rest(Token),
    Meta(Token),
    Atom(Token),
    Tuple(Vec<Morpheme>),
    Vector(Vec<Morpheme>),
    Record(Vec<Morpheme>),
    Iter(Vec<Morpheme>),
    Call(Token, Vec<Morpheme>),
    Unary(Token, Box<Morpheme>),
    // for default parameters,
    Binary(Token, Box<Morpheme>, Box<Morpheme>),
}

impl Morpheme {
    pub fn get_tokens<'t>(&'t self) -> Vec<&'t Token> {
        let mut acc = vec![];
        match self {
            Morpheme::Empty => {}
            Morpheme::Meta(t) => {
                // for tk in t.get_tokens() {
                //     acc.push(tk)
                // }
                acc.push(t)
            }
            Morpheme::Atom(t) => {
                acc.push(t);
            }
            Morpheme::Tuple(ts)
            | Morpheme::Vector(ts)
            | Morpheme::Iter(ts)
            | Morpheme::Record(ts) => ts.iter().for_each(|tp| {
                tp.get_tokens().iter().for_each(|t| acc.push(*t))
            }),
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
        };
        acc
    }
    pub fn has_rest(&self) -> bool {
        match self {
            Self::Empty => false,
            Self::Rest(_) => true,
            Self::Meta(_) => false,
            Self::Atom(_) => false,
            Self::Tuple(ms)
            | Self::Vector(ms)
            | Self::Record(ms)
            | Self::Call(_, ms)
            | Self::Iter(ms) => ms.iter().any(Self::has_rest),
            Self::Unary(o, m) => Self::has_rest(&**m),
            Self::Binary(o, n, m) => {
                Self::has_rest(&**n) || Self::has_rest(&**m)
            }
        }
    }
}

impl From<&Morpheme> for Expr {
    fn from(tp: &Morpheme) -> Self {
        Self::from(tp.clone())
    }
}

impl From<Morpheme> for Expr {
    fn from(tokpat: Morpheme) -> Self {
        match tokpat {
            Morpheme::Empty => Expr::Nil,
            // figure this out for rest parameters
            Morpheme::Meta(t) => Expr::Vector(vec![]),
            Morpheme::Atom(t) => Expr::Literal(t),
            Morpheme::Iter(vs) => Expr::Named(
                Kind(Token::Meta("Iter".to_string(), Pos::faux())),
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
            Morpheme::Record(vs) => Expr::Record(
                Kind(Token::Empty()),
                vs.iter()
                    .map(|v| {
                        let toks = v.get_tokens();
                        Binding {
                            pram: Parameter {
                                name: Token::Empty(),
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
            Morpheme::Unary(t, p) => Expr::Unary(t, Box::new(Expr::from(*p))),
            Morpheme::Binary(t, a, b) => Expr::Binary(
                t,
                Box::new(Expr::from(*a)),
                Box::new(Expr::from(*b)),
            ),
            Morpheme::Rest(t) => Expr::Literal(t),
            /* TokPattern::Ternary(_, _, _, _) => todo!(),
             * TokPattern::Map(_, _) => todo!(), */
        }
    }
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub name: Option<String>,
    pub body: Vec<Expr>,
    pub vocab: HashSet<String>,
}

impl Program {
    pub fn new(
        name: Option<String>,
        body: Vec<Expr>,
        vocab: HashSet<String>,
    ) -> Self {
        Self { name, body, vocab }
    }
    pub fn set_name(&mut self, name: String) -> Option<&String> {
        if self.name.is_none() {
            self.name = Some(name)
        };
        self.name.as_ref()
    }
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Assign(Token, Box<Expr>, Box<Expr>),
    Binary(Token, Box<Expr>, Box<Expr>),
    Block(bool, Vec<Expr>),
    Call(Box<Expr>, Vec<Expr>, Option<Token>),
    // Case(Box<Expr>, Vec<(TokPattern, Expr)>, Box<Expr>),
    Case(Box<Expr>, Vec<(Expr, Expr)>, Box<Expr>),
    Conditional(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Constant(Vec<Binding>),
    Data(Kind<Token>, Vec<DataVariant>),
    Error(Halt, Token),
    Index(Box<Expr>, Box<Expr>),
    Iter(Box<Expr>, Option<Box<Expr>>),
    Lambda(Option<Token>, Vec<Parameter>, Box<Expr>),
    List(Box<Definition>),
    Literal(Token),
    Loop(Box<Expr>, Box<Expr>),
    Member(Box<Expr>, Token),
    Named(Kind<Token>, Box<Expr>),
    Nil,
    Path(Box<Expr>, Token),
    Pipe(Box<Expr>, Vec<Expr>),
    Program(Program),
    Range(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Record(Kind<Token>, Vec<Binding>),
    Return(Box<Expr>, Box<Expr>),
    Tuple(Vec<Expr>),
    Unary(Token, Box<Expr>),
    Variable(Vec<Binding>, Box<Expr>),
    Vector(Vec<Expr>),
}

impl From<Token> for Expr {
    fn from(token: Token) -> Self {
        match token {
      Token::Bool(..) | Token::String(..) | Token::Number(..) | Token::Identifier(..) => Self::Literal(token),
      Token::Empty() => Expr::Nil,
      _ => Expr::Error(Halt::InvalidInput("Only booleans, strings, numbers, and variables may be parsed as Literal Expressions!".to_string()), token)
    }
    }
}

impl Expr {
    pub fn from_token(token: Token) -> Expr {
        Self::from(token)
    }
    pub fn is_error(&self) -> bool {
        matches!(self, Self::Error(..))
    }
    pub fn from_operator(
        token: &Token,
    ) -> fn(Token, Box<Expr>, Box<Expr>) -> Expr {
        match token.operator_info() {
            // ALL ASSIGNMENT OPERATORS ARE RIGHT ASSOCIATIVE WITH PREC 1
            // "=" | "<-" | "=<" | "+=" | "-="
            Some((1, Assoc::Right)) => Self::Assign,
            Some((..)) | _ => Self::Binary,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Shape {
    Atom,
    Tuple,
    Vector,
    List,
    Record,
    Closure,
    Holder,
    Unknown,
    Empty,
    Nested(Rc<Shape>),
    // special ryg generic, aka for "any"
    R,
}

impl Shape {
    pub fn from_tok_ref(tok: &Token) -> Shape {
        Shape::from(tok.to_owned())
    }
}

impl From<Token> for Shape {
    fn from(token: Token) -> Shape {
        if matches!(token.clone(), Token::Identifier(..) | Token::Meta(..)) {
            Shape::Atom
        } else {
            match token.clone().to_string().as_str() {
                "(" => Shape::Tuple,
                "[" => Shape::Vector,
                "{" => Shape::Record,
                "|" => Shape::Closure,
                "_" => Shape::R,
                _ => Shape::Unknown,
            }
        }
    }
}

simple_pub_struct! {
    Definition :: {
        item: Expr,
        ranges: Vec<(Expr, Expr)>,
        fixed: Vec<(Expr, Expr)>,
        conds: Vec<Expr>
    } + Clone, Debug, PartialEq
}

impl From<Definition> for Shape {
    fn from(defn: Definition) -> Shape {
        match defn.item {
            Expr::Nil => Shape::Empty,
            Expr::Vector(_) => Shape::Vector,
            Expr::Literal(_) => Shape::Atom,
            Expr::Tuple(_) => Shape::Tuple,
            Expr::List(_) => Shape::List,
            Expr::Record(_, _) => Shape::Record,
            Expr::Unary(_, _) => Shape::Atom,
            Expr::Binary(_, _, _) => Shape::Holder,
            Expr::Lambda(..) => Shape::Closure,
            _ => Shape::Unknown,
        }
    }
}

simple_pub_struct! {
    Parameter :: {
        name: Token,
        pattern: Morpheme,
        shape: Shape,
        kind: Morpheme
    } + Clone, Debug, PartialEq
}

// Variables bound to values currently in scope
// Let expressions handle these as arguments in a homomorphic manner to lambdas
simple_pub_struct! {
        Binding :: {
            pram: Parameter,
            expr: Expr
    } + Clone, Debug, PartialEq
}

simple_pub_struct! {
    VariantArg :: {
        morpheme: Morpheme,
    } + Clone, Debug, PartialEq
}

simple_pub_struct! {
    DataVariant :: {
        ident: Kind<Token>,
        items: Vec<Morpheme>
    } + Clone, Debug, PartialEq
}

simple_pub_struct! {
    DataDef :: {
        kind: Kind<Token>,
        variants: Vec<DataVariant>
    } + Clone, Debug, PartialEq
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Nil => write!(f, "()"),
            Expr::Literal(t) => write!(f, "{}", t),
            Expr::Unary(a, b) => write!(f, "(Unary {} {})", a, b),
            Expr::Binary(a, b, c) => {
                write!(f, "(Binary {} {} {})", a, *b, *c)
            }
            Expr::Assign(a, b, c) => {
                write!(f, "(Assign {} {} {})", a, *b, *c)
            }
            Expr::Block(d, a) => {
                write!(f, "(Block {}{:#?})", if *d { "do " } else { "" }, a)
            }
            Expr::Call(a, b, c) => write!(
                f,
                "(Call {} {:#?} {})",
                *a,
                b,
                if let Some(name) = &c {
                    name.literal()
                } else {
                    String::new()
                }
            ),
            Expr::Case(a, b, c) => {
                write!(f, "(Case {} {:#?} {})", *a, b, *c)
            }
            Expr::Conditional(a, b, c) => {
                write!(
                    f,
                    "(Conditional {:?} {:?} {:?})",
                    *a,
                    *b,
                    if let Some(expr) = c { expr } else { &Expr::Nil }
                )
            }
            Expr::Lambda(a, b, c) => {
                write!(
                    f,
                    "(Lambda {} {:#?} {})",
                    if let Some(name) = a {
                        name.literal()
                    } else {
                        "ʎ".to_string()
                    },
                    b,
                    *c
                )
            }
            Expr::Variable(a, b) => write!(f, "(Let {:#?} {})", a, *b),
            Expr::Vector(a) => write!(f, "(Vector {:#?})", a),
            Expr::List(a) => write!(f, "(List {:#?})", a),
            Expr::Tuple(b) => write!(f, "(Tuple {:#?})", b),
            Expr::Index(a, b) => write!(f, "(Index {} {})", *a, *b),
            Expr::Iter(a, b) => write!(
                f,
                "(Iter {:?} {:?})",
                *a,
                if let Some(expr) = b { expr } else { &Expr::Nil }
            ),
            Expr::Range(a, b, c) => {
                write!(
                    f,
                    "(Range {:?} {:?} {:?})",
                    *a,
                    *b,
                    if let Some(expr) = c { expr } else { &Expr::Nil }
                )
            }
            Expr::Loop(a, b) => write!(f, "(Loop {:?} {:?})", *a, *b),
            Expr::Error(a, b) => write!(f, "(Error {} {}", a, b),
            Expr::Return(a, b) => write!(f, "(Return {} {} )", *a, *b),
            Expr::Named(a, b) => write!(f, "(Named {} {})", a, *b),
            // Expr::Pattern(a, b) => write!(f, "(Pattern {:?} {:?})", a, b),
            Expr::Record(a, b) => write!(f, "(Record {} {:#?})", a, b),
            _ => write!(f, "{:#?}", self),
        }
    }
}

impl fmt::Display for Kind<Token> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for Kind<String> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub trait Visit<X: Clone> {
    fn visit<F>(&self, cont: &mut F)
    where
        F: FnMut(&X);
}

impl Visit<Token> for Expr {
    fn visit<F: FnMut(&Token)>(&self, mut visit: &mut F) {
        match self {
            Expr::Nil => visit(&Token::Empty()),
            Expr::Assign(op, left, right) => {
                visit(op);
                left.visit(visit);
                right.visit(visit);
            }
            Expr::Binary(op, left, right) => {
                visit(op);
                right.visit(visit);
                left.visit(visit);
            }
            Expr::Block(is_do, xs) => {
                xs.iter().fold((), |a, c| c.visit(visit));
            }
            Expr::Call(func, args, name) => {
                func.visit(visit);
                args.iter().for_each(|expr| expr.visit(visit));
                if let Some(id) = name {
                    visit(&id);
                };
            }
            Expr::Case(subj, conds, deft) => {
                subj.visit(visit);
                deft.visit(visit);
                conds.iter().for_each(|(pat, branch)| {
                    pat.visit(visit);
                    branch.visit(visit);
                });
            }
            Expr::Conditional(cond, then, deft) => {
                cond.visit(visit);
                then.visit(visit);
                if let Some(expr) = deft {
                    expr.visit(visit);
                }
            }
            Expr::Index(item, index) => {
                item.visit(visit);
                index.visit(visit);
            }
            Expr::Member(item, field) => {
                item.visit(visit);
                visit(field);
            }
            Expr::Iter(start, end) => {
                start.visit(visit);
                if let Some(end_) = end {
                    end_.visit(visit);
                }
            }
            Expr::List(defn) => {
                let Definition {
                    ranges,
                    conds,
                    fixed,
                    item,
                } = defn.as_ref();
                ranges.iter().for_each(|(a, b)| {
                    a.visit(visit);
                    b.visit(visit);
                });
                conds.iter().for_each(|x| {
                    x.visit(visit);
                });
                fixed.iter().for_each(|(id, vardef)| {
                    id.visit(visit);
                    vardef.visit(visit)
                });
                item.visit(visit);
            }
            Expr::Range(body, start, end) => {
                body.visit(visit);
                start.visit(visit);
                if let Some(end_) = end {
                    end_.visit(visit);
                };
            }
            Expr::Lambda(name, prams, body) => {
                if let Some(id) = name {
                    visit(id);
                    prams.iter().for_each(
                        |Parameter {
                             name,
                             pattern,
                             shape,
                             kind,
                         }| {
                            visit(name);
                            pattern.get_tokens().iter().for_each(|tok| {
                                visit(*tok);
                            });
                            kind.get_tokens().iter().for_each(|tok| {
                                visit(*tok);
                            });
                        },
                    )
                }
            }
            Expr::Literal(tok) => {
                visit(tok);
            }
            Expr::Loop(cond, body) => {
                cond.visit(visit);
                body.visit(visit);
            }
            Expr::Tuple(items) => items.iter().for_each(|x| {
                x.visit(visit);
            }),
            Expr::Unary(op, rhs) => {
                visit(op);
                rhs.visit(visit);
            }
            Expr::Constant(bindings) => bindings.iter().for_each(
                |Binding {
                     pram:
                         Parameter {
                             name,
                             pattern,
                             shape,
                             kind,
                         },
                     expr,
                 }| {
                    visit(name);
                    pattern.get_tokens().iter().for_each(|tok| {
                        visit(*tok);
                    });
                    kind.get_tokens().iter().for_each(|tok| {
                        visit(*tok);
                    });
                    expr.visit(visit);
                },
            ),
            Expr::Variable(bindings, body) => {
                bindings.iter().for_each(
                    |Binding {
                         pram:
                             Parameter {
                                 name,
                                 pattern,
                                 shape,
                                 kind,
                             },
                         expr,
                     }| {
                        visit(name);
                        pattern.get_tokens().iter().for_each(|tok| {
                            visit(*tok);
                        });
                        kind.get_tokens().iter().for_each(|tok| {
                            visit(*tok);
                        });
                        expr.visit(visit);
                    },
                );
                body.visit(visit)
            }
            Expr::Vector(items) => items.iter().for_each(|x| {
                x.visit(visit);
            }),
            Expr::Error(_, _) => todo!(),
            Expr::Return(_, _) => todo!(),
            Expr::Named(_, _) => todo!(),
            Expr::Record(kind, bindings) => {
                visit(&kind.0);
                bindings.iter().for_each(
                    |Binding {
                         pram:
                             Parameter {
                                 name,
                                 pattern,
                                 shape,
                                 kind,
                             },
                         expr,
                     }| {
                        visit(name);
                        pattern.get_tokens().iter().for_each(|tok| {
                            visit(*tok);
                        });
                        kind.get_tokens().iter().for_each(|tok| {
                            visit(*tok);
                        });
                        expr.visit(visit);
                    },
                );
            }
            Expr::Data(name, dvs) => {
                visit(&name.0);
                dvs.iter().for_each(|DataVariant { ident, items }| {
                    visit(&ident.0);
                    items.iter().for_each(|pram| {
                        pram.get_tokens().iter().for_each(|tok| {
                            visit(*tok);
                        });
                    })
                })
            }
            Expr::Pipe(arg, pipes) => {
                arg.visit(visit);
                pipes.iter().for_each(|x| {
                    x.visit(visit);
                });
            }
            Expr::Program(Program {
                name: _,
                body,
                vocab: _,
            }) => {
                body.iter().for_each(|expr| {
                    expr.visit(visit);
                });
            }
            Expr::Path(root, branch) => {
                root.visit(visit);
                visit(branch);
            }
        };
    }
}

#[cfg(test)]
mod test {
    use crate::{parsing::parser::parse_input, tok::lexer::Pos};

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
    fn walk_expr() {
        let src1 = "if true then 1 else 0";

        let ast1 = parse_input(src1);
        let mut toks = vec![];
        let mut i = 0;
        ast1.visit(&mut |tok| {
            println!("(i, tok): ({}, {})", &i, tok);
            toks.push(tok.clone());
            i += 1;
        })
    }
}
