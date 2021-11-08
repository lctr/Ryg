use std::{
    collections::HashSet,
    fmt::{self, Debug},
    rc::{Rc, Weak},
};

pub use super::morpheme::Morpheme;

use crate::{
    core::{function::ArgList, rygtype::Field},
    quick_match, simple_pub_struct,
    tok::{
        lexer::{Literal, Pos, Token},
        token::{AssignOp, Assoc, BinOp, FromToken, UnOp},
    },
    util::{
        display::map_join,
        state::Halt,
        types::{Either, Kind, Maybe},
    },
};

#[allow(unused)]
#[derive(Clone, PartialEq)]
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

impl std::fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.body.iter()).finish()
    }
}

pub struct Binary<X> {
    op: BinOp,
    left: Rc<X>,
    right: Rc<X>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Unary<X> {
    op: UnOp,
    expr: Rc<X>,
}

impl<X> Unary<X> {
    pub fn visit<F, G, T>(&self, f: F) -> T
    where
        F: Fn(UnOp) -> G,
        G: Fn(Rc<X>) -> T, {
        let op = self.op;
        let expr = self.expr.clone();
        f(op)(expr)
    }

    pub fn walk<F, G, T, U>(&self, f: F, mut u: &mut U) -> T
    where
        F: Fn(UnOp) -> G,
        G: Fn(Rc<X>, &mut U) -> T, {
        let op = self.op;
        let expr = self.expr.clone();
        f(op)(expr, &mut u)
    }
}

impl From<(Token, Expr)> for Unary<Expr> {
    fn from((t, x): (Token, Expr)) -> Self {
        let op = match t.literal().as_str() {
            "!" => UnOp::Not,
            "-" => UnOp::Neg,
            "&" => UnOp::Ref,
            "*" => UnOp::Deref,
            "~" => UnOp::BitNot,
            // TODO: custom unary ops? or error handling
            _ => todo!(),
        };
        let expr = Rc::new(x);
        Self { op, expr }
    }
}

impl From<(UnOp, Expr)> for Unary<Expr> {
    fn from((t, x): (UnOp, Expr)) -> Self {
        let op = t;
        let expr = Rc::new(x);
        Self { op, expr }
    }
}

pub struct AssignExpr {}
pub struct BinaryExpr {
    op: BinOp,
    left: Rc<Expr>,
    right: Rc<Expr>,
}

#[derive(Debug, Clone)]
pub struct SupExpr(Weak<Expr>);
impl Literal for SupExpr {
    type Lit = String;
    fn literal(&self) -> Self::Lit {
        match self.0.upgrade() {
            Some(expr) => expr.literal(),
            None => String::from("\n~~ EMPTY SUPEXPR\n"),
        }
    }
}
impl std::cmp::PartialEq for SupExpr {
    fn ne(&self, other: &Self) -> bool {
        // self.0.ptr_eq(&other.0)
        // ???
        self.0.strong_count() == 0 || self.0.strong_count() == 0
    }

    fn eq(&self, other: &Self) -> bool {
        self.0.ptr_eq(&other.0)
            && self.0.strong_count() > 0
            && other.0.strong_count() > 0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Assign(AssignOp, Box<Self>, Box<Self>),
    Binary(BinOp, Box<Self>, Box<Self>),
    Block(Token, Vec<Self>),
    Call(Box<Self>, Vec<Self>, Option<Token>),
    Case(Box<Self>, Vec<(Self, Self)>, Box<Self>),
    Conditional(Box<Self>, Box<Self>, Option<Box<Self>>),
    Constant(Vec<Binding>),
    Data(Kind<Token>, Vec<Variant>),
    Empty,
    Error(Halt, Token),
    Ident(Token, SupExpr),
    Index(Box<Self>, Box<Self>),
    Instance(Token, Vec<(Token, Self)>),
    Iter(Box<Self>, Option<Box<Self>>),
    Lambda(Vec<Parameter>, Box<Self>),
    Function(
        Token,
        Vec<Parameter>,
        Option<Box<Morpheme<Token, Expr>>>,
        Box<Self>,
    ),
    List(Box<Definition>),
    Literal(Token),
    Loop(Box<Self>, Box<Self>),
    Member(Box<Self>, Token),
    Named(Kind<Token>, Box<Self>),
    Path(Box<Self>, Token),
    Pipe(Box<Self>, Vec<Self>),
    Program(Program),
    Range(Box<Self>, Box<Self>, Option<Box<Self>>),
    Record(Kind<Token>, Vec<Binding>),
    Return(Box<Self>, Box<Self>),
    Structure(Token, Shape, Vec<Parameter>),
    Tuple(Vec<Self>),
    Unary(UnOp, Box<Self>),
    Variable(Vec<Binding>, Box<Self>),
    Vector(Vec<Self>),
}

pub struct Type {}

impl From<Token> for Expr {
    fn from(token: Token) -> Self {
        match token {
      Token::Bool(..) | Token::String(..) | Token::Number(..) | Token::Identifier(..) => Self::Literal(token),
      Token::Empty => Expr::Empty,
      _ => Expr::Error(Halt::InvalidInput("Only booleans, strings, numbers, and identifiers may be parsed as Literal Expressions!".to_string()), token)
    }
    }
}

impl Expr {
    // pub fn from_operator(
    //     token: &Token,
    // ) -> fn(Token, Box<Self>, Box<Self>) -> Self {
    //     match token.operator_info() {
    //         // ALL ASSIGNMENT OPERATORS ARE RIGHT ASSOCIATIVE WITH PREC 1
    //         // "=" | "<-" | "=<" | "+=" | "-="
    //         Some((1, Assoc::Right)) => Self::Assign,
    //         Some((..)) | _ => Self::Binary,
    //     }
    // }
}

impl Literal<String> for Expr {
    type Lit = String;
    fn literal(&self) -> Self::Lit {
        let mut literal = String::new();
        macro_rules! join {
            ($x:expr) => {
                join! {$x, ", "}
            };
            ($x:expr, $d:literal) => {
                &*$x.into_iter()
                    .map(|m| m.literal())
                    .collect::<Vec<_>>()
                    .join($d)
            };
            ($left:literal -> $ex:expr => $sep:literal -> $right:literal) => {{
                let inner = &*join!($ex, $sep);
                format!("{}{}{}", stringify!($left), inner, stringify!($right))
                    .as_str()
            }};
        }
        match self {
            Expr::Assign(a, b, c) => literal.push_str(
                &*[&*b.literal(), " ", &*a.literal(), " ", &*c.literal()]
                    .concat(),
            ),
            Expr::Binary(a, b, c) => {
                literal.push_str(
                    &*[&*b.literal(), " ", &*a.literal(), " ", &*c.literal()]
                        .concat(),
                );
            }
            Expr::Block(t, ts) => literal.push_str(
                &*[&*t.literal(), " {\n", &*join!(ts, ";\n"), "\n}"].concat(),
            ),
            Expr::Call(a, b, c) => literal.push_str(
                &[&*a.literal(), join!("(" -> b => "," -> ")")].concat(),
            ),
            Expr::Case(_, _, _) => todo!(),
            Expr::Conditional(a, b, c) => {}
            Expr::Constant(a) => todo!(),
            Expr::Data(d, vs) => literal.push_str(
                &*[
                    "data",
                    d.0.literal().as_str(),
                    "::",
                    "{\n",
                    join!(vs, "\n\t| "),
                    "\n}",
                ]
                .concat(),
            ),
            Expr::Empty => {}
            Expr::Error(_, _) => todo!(),
            Expr::Ident(tok, _) => literal.push_str(tok.literal().as_str()),
            Expr::Index(a, b) => {
                literal.push_str(
                    &*[&*a.literal(), "[", &*b.literal(), "]"].concat(),
                );
            }
            Expr::Instance(a, b) => {
                literal.push_str(&*[&*a.literal(), " {\n"].concat());
                for (t, e) in b {
                    literal.push_str(
                        &*["\t", &*t.literal(), ": ", &*e.literal(), ",\n"]
                            .concat(),
                    )
                }
                literal.push_str("}");
            }
            Expr::Iter(a, b) => literal.push_str(
                &*[
                    "(",
                    &*a.literal(),
                    "..",
                    &*(match b {
                        Some(end) => end.literal(),
                        _ => String::new(),
                    }),
                    ")",
                ]
                .concat(),
            ),
            Expr::Lambda(b, c) => {
                // let mut nm = if let Some(name) = a {
                //     [String::from("fn "), name.literal(), " ".to_string()]
                //         .concat()
                // } else {
                //     String::new()
                // };
                literal.push_str(
                    &*[
                        join!(b
                            .iter()
                            .map(|p| &p.pattern)
                            .collect::<Vec<_>>()),
                        &*c.literal(),
                    ]
                    .concat(),
                );
            }
            Expr::Function(a, b, c, d) => literal.push_str(
                &*[
                    &*a.literal(),
                    join!(b.iter().map(|p| &p.pattern).collect::<Vec<_>>()),
                    &*match c {
                        Some(ret) => {
                            let mut rets = String::from(" -> ");
                            rets.push_str(&ret.literal());
                            rets.push_str(", ");
                            rets
                        }
                        None => String::from(" "),
                    },
                    &*d.literal(),
                ]
                .concat(),
            ),
            Expr::List(_) => todo!(),
            Expr::Literal(t) => literal.push_str(t.literal().as_str()),
            Expr::Loop(a, b) => literal.push_str(
                &*["loop (", &*a.literal(), ")\n\t(", &*b.literal()].concat(),
            ),
            Expr::Path(a, b) | Expr::Member(a, b) => literal
                .push_str(&*[&*a.literal(), ".", &*b.literal()].concat()),
            Expr::Named(_, _) => todo!(),
            Expr::Pipe(a, b) => literal.push_str(
                &*[&*a.literal(), " |> ", join!(b, " |> ")].concat(),
            ),
            Expr::Program(Program { body, .. }) => {
                literal.push_str(join!(body, ";\n"));
            }
            Expr::Range(a, b, c) => literal.push_str(
                &*[
                    &*a.literal(),
                    "[",
                    &*Expr::Iter(b.clone(), c.clone()).literal(),
                    "]",
                ]
                .concat(),
            ),
            Expr::Record(_, b) => literal.push_str(
                &*[
                    "#{ ",
                    &*b.into_iter()
                        .map(
                            |Binding {
                                 pram:
                                     Parameter {
                                         kind,
                                         pattern,
                                         shape,
                                     },
                                 expr,
                             }| {
                                let knd = kind.literal();
                                let prop = pattern.literal();
                                let left = if knd.is_empty() {
                                    prop
                                } else {
                                    [prop, ": ".to_string(), knd].concat()
                                };
                                let right = expr.literal();
                                if right.is_empty() {
                                    left
                                } else {
                                    [left, " = ".to_string(), right].concat()
                                }
                            },
                        )
                        .collect::<Vec<_>>()
                        .join(", "),
                    " }",
                ]
                .concat(),
            ),
            Expr::Return(_, _) => todo!(),
            Expr::Structure(a, b, c) => {
                let (left, right) = match b {
                    Shape::Tuple => ("(", ")"),
                    Shape::Vector => ("[", "]"),
                    Shape::Struct | Shape::Record => ("{", "}"),
                    Shape::Empty | _ => ("", ""),
                };
                let x = &*[
                    "struct ",
                    &*a.literal(),
                    left,
                    &*c.iter()
                        .map(|Parameter { kind, pattern, .. }| {
                            ["\t", &*pattern.literal(), ": ", &*kind.literal()]
                                .concat()
                        })
                        .collect::<Vec<_>>()
                        .join(",\n"),
                    right,
                ]
                .concat();
            }
            Expr::Tuple(ts) => {
                literal.push_str(join!("(" -> ts => ", " -> ")"))
            }
            Expr::Unary(a, b) => {
                literal.push_str(&*[a.literal(), b.literal()].concat())
            }
            Expr::Variable(a, b) => literal.push_str(
                &*[
                    "let\n\t",
                    &*a.into_iter()
                        .map(
                            |Binding {
                                 pram:
                                     Parameter {
                                         kind,
                                         pattern,
                                         shape,
                                     },
                                 expr,
                             }| {
                                let knd = kind.literal();
                                let prop = pattern.literal();
                                let left = if knd.is_empty() {
                                    prop
                                } else {
                                    [
                                        "\t".to_string(),
                                        prop,
                                        ": ".to_string(),
                                        knd,
                                    ]
                                    .concat()
                                };
                                let right = expr.literal();
                                if right.is_empty() {
                                    left
                                } else {
                                    [left, " = ".to_string(), right].concat()
                                }
                            },
                        )
                        .collect::<Vec<_>>()
                        .join(", "),
                    "\n\tin\n\t",
                    &*b.literal(),
                ]
                .concat(),
            ),
            Expr::Vector(_) => todo!(),
        };
        literal
    }
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Shape {
    Atom,
    Tuple,
    Vector,
    List,
    Record,
    Struct,
    Closure,
    Holder,
    Unknown,
    Empty,
    Binary,
    Unary,
    Union,
    At,
    Spread,
    Rest,
    Wild,
    Call,
    Meta,
    Iter,
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
        if matches!(&token, Token::Identifier(..)) {
            Shape::Atom
        } else if matches!(&token, Token::Meta(..)) {
            Shape::Struct
        } else {
            match token.to_string().as_str() {
                "(" => Shape::Tuple,
                "[" => Shape::Vector,
                "{" => Shape::Record,
                "|" => Shape::Empty,
                "_" => Shape::Empty,
                _ => Shape::Unknown,
            }
        }
    }
}

impl From<Morpheme<Token, Expr>> for Shape {
    fn from(morpheme: Morpheme<Token, Expr>) -> Self {
        match morpheme {
            Morpheme::Empty => Self::Empty,
            Morpheme::Wild(_) => Self::Wild,
            Morpheme::Rest(_) => Self::Rest,
            Morpheme::Meta(_) => Self::Meta,
            Morpheme::Atom(_) => Self::Record,
            Morpheme::Spread(_) => Self::Atom,
            Morpheme::Tuple(_) => Self::Spread,
            Morpheme::Vector(_) => Self::Tuple,
            Morpheme::Record(_) => Self::Vector,
            Morpheme::Iter(_) => Self::Iter,
            Morpheme::Struct(..) => Self::Struct,
            Morpheme::Call(..) => Self::Call,
            Morpheme::Unary(..) => Self::Unary,
            Morpheme::Binary(..) => Self::Binary,
            Morpheme::Closure(..) => Self::Closure,
            Morpheme::Union(_) => Self::Union,
            Morpheme::At(..) => Self::At,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ListDef {
    item: Expr,
    ranges: Vec<(Morpheme<Token, Expr>, Expr)>,
    preds: Vec<Expr>,
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
            Expr::Empty => Shape::Empty,
            Expr::Vector(_) => Shape::Vector,
            Expr::Literal(_) => Shape::Atom,
            Expr::Tuple(_) => Shape::Tuple,
            Expr::List(_) => Shape::List,
            Expr::Record(_, _) => Shape::Record,
            Expr::Instance(_, _) => Shape::Struct,
            Expr::Unary(_, _) => Shape::Atom,
            Expr::Binary(_, _, _) => Shape::Holder,
            Expr::Lambda(..) => Shape::Closure,
            _ => Shape::Unknown,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub pattern: Morpheme<Token, Expr>,
    pub shape: Shape,
    pub kind: Morpheme<Token, Expr>,
}

impl Parameter {
    pub fn new(
        pattern: Morpheme<Token, Expr>,
        shape: Shape,
        kind: Option<Morpheme<Token, Expr>>,
    ) -> Self {
        Self {
            pattern,
            shape,
            kind: match kind {
                Some(k) => k,
                _ => Morpheme::Empty,
            },
        }
    }
    pub fn get_tokens(&self) -> Vec<&Token> {
        self.pattern.get_tokens()
    }
    pub fn len(&self) -> usize {
        self.get_tokens().len()
    }
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
        morpheme: Morpheme<Token, Expr>,
    } + Clone, Debug, PartialEq
}

simple_pub_struct! {
    Variant :: {
        ident: Kind<Token>,
        items: Vec<Morpheme<Token, Expr>>
    } + Clone, Debug, PartialEq
}

impl Literal for Variant {
    type Lit = String;
    fn literal(&self) -> Self::Lit {
        let mut literal = String::new();
        literal.push_str(&*[self.ident.0.literal().as_str(), " "].concat());
        literal.push_str(
            &*self
                .items
                .iter()
                .map(|p| p.literal())
                .collect::<Vec<_>>()
                .join(", "),
        );
        literal
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // macro_rules! map {
        //     ($kind:tt )
        // }
        match self {
            Expr::Assign(a, b, c) => {
                write!(
                    f,
                    "(Assign {} {})",
                    a,
                    format_args!("\n\t{}\n\t{}", b.as_ref(), c.as_ref())
                )
            }
            Expr::Binary(a, b, c) => {
                write!(
                    f,
                    "(Bin {} {})",
                    a,
                    format_args!("\n\t{}\n\t{}", b.as_ref(), c.as_ref())
                )
            }
            Expr::Block(a, b) => {
                write!(f, "(Block {} {})", a, format_args!("\n\t{:#?}", b))
            }
            _ => write!(f, "{:#?}", &self),
            Expr::Program(Program { name, body, vocab }) => {
                if let Some(name) = name {
                    writeln!(f, "[Program::{}] {{", name);
                };
                writeln!(f, "\tbank: {:#?},", &vocab);
                writeln!(f, "\tbody: [");
                for (i, x) in body.iter().enumerate() {
                    writeln!(f, "\t [{}]:\t {}", i, x);
                }
                writeln!(f, "\t]\n}}")
            }
        }
    }
}

impl fmt::Display for Kind<Token> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.literal())
    }
}

impl fmt::Display for Kind<String> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod test {
    use crate::{parsing::parser::parse_input, tok::lexer::Pos};

    use super::*;

    use crate::{
        core::rygval::RygVal, evaluating::environment::Envr, log_do,
        util::types::Maybe,
    };

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

    #[test]
    fn expr_code() {
        let expr = Expr::Assign(
            AssignOp::from_token(&Token::from("<-")).unwrap(),
            Box::new(Expr::Literal(Token::from("x"))),
            Box::new(Expr::Literal(Token::from("y"))),
        );
        let prog = expr.literal();
        println!("{}", prog)
    }

    #[test]
    fn ops_from_token() {
        let pos = Pos::new();
        let tok1 = Token::Operator("+".to_string(), pos.clone());
        let tok2 = Token::Operator("<>".to_string(), pos.clone());
        let op1 = BinOp::from_token(&tok1);
        let op2 = BinOp::from_token(&tok2);
        log_do!(
            tok1 => tok1, tok2 => tok2, op1 => op1, op2 => op2
        )
    }
    #[test]
    fn unary() {
        let tok = Token::from("!");
        let expr = Expr::Literal(Token::Bool(true, {
            let mut pos = Pos::faux();
            pos.next(&'!');
            pos
        }));
        let unexpr = Unary::from((tok, expr));
        println!("{:?}", &unexpr);
        unexpr.visit(|op| {
            move |x| println!("visiting {:?} {:?}", op, x.clone())
            // match op {
            //     UnOp::Ref => |x| {println!("{:?}", &x); },
            //     UnOp::Deref => todo!(),
            //     UnOp::Neg => |x| ,
            //     UnOp::Not => todo!(),
            //     UnOp::BitNot => todo!(),
            // }
        })
    }
}
