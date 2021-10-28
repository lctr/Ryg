use std::{
    borrow::{Borrow, Cow},
    fmt::Display,
    rc::Rc,
};

use crate::{
    core::rygtype::Field,
    evaluating::{
        environment::Envr,
        evaluator::{pattern_match, walk},
    },
    parsing::expression::{Binding, Expr, Morpheme, Parameter, Shape, Visit},
    tok::{lexer::ToLiteral, token::Token},
    util::{
        state::Halt,
        types::{Either, Kind, Maybe},
    },
};

use super::{
    rygtype::RygType,
    rygval::{Container, RygVal},
};

pub trait Callable<R: Clone + std::fmt::Debug> {
    type Args;
    type Stack;
    fn call(&mut self, args: Self::Args, env: &mut Self::Stack) -> Maybe<R>;
}

#[derive(Clone, Debug, PartialEq)]
pub struct Args<T>(Option<T>, Vec<T>);

impl<T: Clone> Args<T> {
    pub fn new(this: Option<T>, args: Vec<T>) -> Self {
        Self(this, args)
    }
    pub fn len(&self) -> usize {
        self.1.len() + (if self.has_this() { 1 } else { 0 })
    }
    pub fn get_arglen(&self) -> usize {
        self.1.len()
    }
    pub fn get_args(self) -> Vec<T> {
        self.1
    }
    pub fn has_this(&self) -> bool {
        self.0.is_some()
    }
    pub fn get_this(self) -> Option<T> {
        self.0
    }
    pub fn get_this_ref(&self) -> Option<&T> {
        if let Some(ref t) = self.0 {
            Some(t)
        } else {
            None
        }
    }
    pub fn get_this_mut(&mut self) -> Option<&mut T> {
        if let Some(ref mut t) = &mut self.0 {
            Some(t)
        } else {
            None
        }
    }
    pub fn set_this(&mut self, this: T) {
        self.0 = Some(this)
    }
    pub fn to_vec(self) -> Vec<T> {
        let mut args = vec![];
        if let Some(this) = self.0 {
            args.push(this);
        };
        self.1.iter().for_each(|t| args.push(t.clone()));
        args
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Constructor(Kind<String>, Lambda);

impl Constructor {
    pub fn new(name: Kind<String>, lambda: Lambda) -> Self {
        Self(name, lambda)
    }
    pub fn call(
        &mut self,
        args: Vec<Expr>,
        mut envr: &mut Envr,
    ) -> Maybe<RygVal> {
        let Constructor(name, lam) = self;
        match &mut lam.call(args, envr) {
            Ok(rv) => Ok(RygVal::Holder(Container(
                name.clone(),
                Box::new(rv.to_owned()),
            ))),
            Err(h) => Err(h.to_owned()),
        }
    }
}

impl Callable<RygVal> for Constructor {
    type Args = Vec<Expr>;
    type Stack = Envr;
    fn call(
        &mut self,
        args: Self::Args,
        envr: &mut Self::Stack,
    ) -> Maybe<RygVal> {
        let Constructor(name, lam) = self;
        match &mut lam.call(args, envr) {
            Ok(rv) => Ok(RygVal::Holder(Container(
                name.clone(),
                Box::new(rv.to_owned()),
            ))),
            Err(h) => Err(h.to_owned()),
        }
    }
}

impl Display for Constructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", &(self.0), &(self.1))
    }
}

pub enum Function {
    Lambda(Lambda),
    Native(RygFn),
    // Method()
}

#[derive(Clone, PartialEq)]
pub struct Lambda {
    pub name: Option<String>,
    pub pram: Vec<Parameter>,
    pub body: Box<Expr>,
    pub envr: Envr,
    returns: Option<RygType>,
}

impl Lambda {
    pub fn new(
        name: Option<String>,
        pram: Vec<Parameter>,
        body: Expr,
        mut envr: Envr,
    ) -> Self {
        let returns = match RygType::from(body.clone()) {
            RygType::Var(id) => envr.get_var(&id).map(|t| t.kind.clone()),
            rt @ _ => Some(rt),
        };
        Lambda {
            name,
            pram,
            body: Box::new(body),
            envr,
            returns,
        }
    }
    fn as_var_expr(&mut self, args: Vec<Expr>) -> Expr {
        Expr::Variable(
            self.pram
                .iter()
                .zip(args.iter())
                .map(|x| Binding {
                    pram: x.0.to_owned(),
                    expr: x.1.to_owned(),
                })
                .collect::<Vec<_>>(),
            self.body.clone(),
        )
    }
    fn resolve_args(&mut self, args: Vec<Expr>, mut env: &mut Envr) -> Envr {
        let mut scope = &mut self.envr.clone();
        for (p, a) in self.pram.iter().zip(args.iter()) {
            if let Ok(res) = walk(a.to_owned(), &mut env) {
                pattern_match(p.clone().pattern, res.to_owned(), &mut scope);
            }
        }
        scope.to_owned()
    }
    pub fn call(
        &mut self,
        args: Vec<Expr>,
        mut env: &mut Envr,
    ) -> Maybe<RygVal> {
        let my_args = &self.pram;
        if my_args.len() != args.len() {
            return Err(Halt::InvalidInput(format!(
                "Invalid argument(s)! Expected {} args, but got {}. \nArgs: {:?}",
                my_args.len(),
                args.len(),
                args
            )));
        }
        walk(*(self.body.clone()), &mut self.resolve_args(args, &mut env))
    }
    pub fn apply(
        &mut self,
        args: Vec<RygVal>,
        mut env: &mut Envr,
    ) -> Maybe<RygVal> {
        let scope = &mut self.envr.clone();
        for (p, rv) in self.pram.iter().zip(args.iter()) {
            pattern_match(p.pattern.clone(), rv.to_owned(), scope);
        }
        scope.def("self".to_string(), Some(&RygVal::Lambda(self.clone())));
        let b = self.body.clone();
        walk(*b, scope)
    }
}

impl From<(Expr, Envr)> for Lambda {
    fn from((expr, envr): (Expr, Envr)) -> Self {
        match &expr {
            Expr::Lambda(name, pram, body) => Self::new(
                name.clone().map(|t| t.literal()),
                pram.clone(),
                *body.clone(),
                envr,
            ),
            _ => Self::new(None, vec![], expr, envr),
        }
    }
}

impl Display for Lambda {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let name = if let Some(name) = &self.name {
            name.as_str()
        } else {
            "ʎ"
        };
        let args = self
            .pram
            .iter()
            .map(|p| p.clone().name.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        let result = &self.body;
        write!(f, "{}: {} -> {:?}", name, args, result)
    }
}

impl std::fmt::Debug for Lambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            f.debug_struct("Lambda")
                .field("name", &self.name)
                .field("pram", &self.pram)
                .field("envr", &self.envr)
                .field("body", &self.body)
                .finish()
        } else {
            write!(f, "{:#?}", &self)
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct RygFn {
    pub name: String,
    pub meta_in: Either<RygType, Vec<RygType>>,
    pub meta_out: RygType,
    pub this: fn(Vec<RygVal>) -> Maybe<RygVal>,
}

impl RygFn {
    pub fn new(
        name: &str,
        meta_in: Either<RygType, Vec<RygType>>,
        meta_out: RygType,
        this: fn(args: Vec<RygVal>) -> Maybe<RygVal>,
    ) -> RygFn {
        RygFn {
            name: name.to_owned(),
            meta_in,
            meta_out,
            this,
        }
    }
    pub fn validate_args(&self, args: &Vec<RygVal>) -> bool {
        if matches!(
            self.meta_in,
            Either::Left(RygType::Any | RygType::Unknown)
        ) {
            true
        } else {
            match &self.meta_in {
                Either::Left(ty) => {
                    if args.len() == 1 {
                        if let Some(val) = args.first() {
                            ty.matches_variant(&RygType::from(val)).0
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }
                Either::Right(vs) => {
                    vs.len() == args.len()
                        && vs
                            .into_iter()
                            .zip(args.iter().map(RygType::from))
                            .all(|(a, b)| {
                                a == &b
                                    || matches!(
                                        a,
                                        RygType::Any | RygType::Unknown
                                    )
                            })
                }
            }
        }
    }
    pub fn call(&self, args: Vec<RygVal>) -> Maybe<RygVal> {
        let f = self.this;
        if self.validate_args(&args) {
            f(args)
        } else {
            Err(Halt::InvalidInput(format!(
                "Function {:?} expects {:?} arguments, but was provided {:?}",
                self.name, self.meta_in, args
            )))
        }
    }

    pub fn as_rygval(self) -> RygVal {
        RygVal::from(self)
    }
}

impl From<RygFn> for RygVal {
    fn from(rygfn: RygFn) -> Self {
        RygVal::Function(rygfn)
    }
}

impl std::fmt::Display for RygFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::util::display::Paint;
        let cyan = Paint::fg_cyan;
        let red = Paint::fg_red;
        write!(
            f,
            "{}{} {} {}{}",
            cyan("("),
            self.meta_in,
            red("->"),
            self.meta_out,
            cyan(")")
        )
    }
}

macro_rules! rygfn {
    ($n:ident :: $argts:ident => $rets:expr, $body: expr) => {{
        let name = stringify!($n);
        let arg_ty = Either::Left::<RygType, Vec<RygType>>(RygType::$argts);
        println!("{} :: {:?} => {}", name, arg_ty, $rets);
        RygFn::new(name, arg_ty, $rets, $body)
    }};
}

mod test {
    use crate::{
        core::rygtype::RygType, evaluating::evaluator::Interpreter, log_do,
        parsing::parser::parse_input,
    };

    use super::*;
    #[test]
    fn make() {
        let fun =
            rygfn!(womp :: Any => RygType::Any, |arg| Ok(RygVal::Int(4)));
    }
    fn run(src: &str) -> Maybe<RygVal> {
        let ast = parse_input(src);
        let mut interpreter = Interpreter::new();
        let res = interpreter.walk(&ast);
        res
        // let mut env = Envr::new();
        // let res = walk(ast, &mut env);
        // (res, env)
    }

    fn assert_annotated_lambda_prams(src: &str, rygtype: RygType) {
        let ast = parse_input(src);
        if let Expr::Block(_, mut x) = ast.clone() {
            if let Some(y) = x.pop() {
                if let Expr::Lambda(_a, b, _c) = y {
                    let types = b
                        .iter()
                        .map(|p| RygType::from(p.clone()))
                        .collect::<Vec<_>>();
                    println!("parsed: {:#?}", ast);
                    println!("typing: {:?}", &types);
                    assert_eq!(types, vec![rygtype])
                }
            }
        }
    }

    macro_rules! eval {
        ($src: expr) => {{
            let src = $src;
            let ast = parse_input(src);
            let mut interpreter = Interpreter::new();
            interpreter.walk(&ast)
        }};
        ($src: expr => $rhs:expr) => {{
            assert_eq!(eval! { $src }, $rhs)
        }};
    }

    #[test]
    fn parameter_types() {
        let src = "|[a, b]: [Int, Int]| a + b";
        let rygtype = RygType::Vector(vec![RygType::Int, RygType::Int]);
        assert_annotated_lambda_prams(src, rygtype)
    }

    #[test]
    fn tuple_as_1_arg() {
        let src = "(|(a, b)| a + b)((1, 2))";
        let res = run(src);
        assert_eq!(res, Ok(RygVal::from(3)))
    }

    #[test]
    fn atom_and_tuple_as_2_args() {
        eval! {
            "(|a, (b, c)| a + b + c)(1, (2, 3))" => Ok(RygVal::from(6))
        };
        // let src = "(|a, (b, c)| a + b + c)(1, (2, 3))";
        // let res = run(src);
        // assert_eq!(res, Ok(RygVal::from(6)))
    }

    #[test]
    fn vector_arg() {
        let src = "(|[a, b]| a + b)([20, 3])";
        let res = run(src);
        assert_eq!(res, Ok(RygVal::from(23)))
    }

    #[test]
    fn tuple_in_vector_as_1_arg() {
        let src = "(|[a, (b, c)]| a + b + c)([8, (2, 3)])";
        let res = run(src);
        assert_eq!(res, Ok(RygVal::from(13)))
    }

    #[test]
    fn record_as_1_arg() {
        let src = "(|{a, b}| a + b)(#{a = 1, b = 20})";
        let res = run(src);
        assert_eq!(res, Ok(RygVal::from(21)))
    }

    #[test]
    fn atom_and_record_in_tuple_as_1_arg() {
        let src = "(|(a, {b, c})| a + b + c)((10, #{b = 20, c = 30}))";
        let res = run(src);
        println!("{:?}", res);
        // assert_eq!(res, Ok(RygVal::from(60)))
    }

    #[test]
    fn closure_scopes() {
        let src = "
    testing =< 123;
    eq_assign_only_makes_new_vars_in_global_scope =< true;
    |n, f| if n < 0 then 0 else self(n - 1, f);
    fn womp || print'ln(\"hello\")";
        // let (res, env) =
        // println!("{:#?}", run(src));
        if let Ok(rv) = run(src) {
            if let RygVal::Lambda(lam) = &rv {
                log_do!("src" => &src, "result" => &rv, 
                "lambda-env" => lam.envr)
            }
        };
    }
}
