use std::fmt::Debug;

use crate::{
    log_do,
    parsing::expression::{
        Binding, DataVariant, Definition, Program, VariantArg,
    },
    tok::{
        lexer::{twin_of, Lexer, ToLiteral},
        stream::{Pos, Streaming},
        token::{Assoc, Token},
    },
    util::{
        state::{Halt, StreamState},
        types::{Kind, Maybe},
    },
};

use super::expression::{Expr, Morpheme, Parameter, Shape};

#[derive(Clone)]
pub struct Parsed<'a, T>(&'a [T]);
impl<'a, T: Debug> std::fmt::Debug for Parsed<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", &self.0)
    }
}

impl<'a> Parsed<'a, Expr> {
    pub fn as_block(&self) -> Expr {
        Expr::Block(false, (&(self.0)).to_vec())
    }
    pub fn as_do_block(&self) -> Expr {
        Expr::Block(true, (&(self.0)).to_vec())
    }
    pub fn iter(&self) -> std::slice::Iter<'a, Expr> {
        self.0.iter()
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl<'a> From<Parsed<'a, Expr>> for Expr {
    fn from(parsed: Parsed<'a, Expr>) -> Self {
        parsed.as_block()
    }
}

// pub enum ParserError {
//     Unexpected,
//     Invalid,
//     Unbalanced,
// }

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    state: StreamState<Token>,
    parsed: Vec<Expr>,
    has_error: bool,
}

impl<'a> Streaming for Parser<'a> {
    type T = Token;
    fn peek(&mut self) -> Self::T {
        // println!("{}", self.lexer.peek());
        self.lexer.peek()
    }

    fn next(&mut self) -> Token {
        if let StreamState::Halted(a) = &self.state {
            return a.clone();
        };
        let next = Streaming::next(&mut self.lexer);
        if let Token::Invalid(..) = &next {
            self.state = StreamState::Halted(next.clone());
            next
        } else {
            next
        }
    }

    fn done(&mut self) -> bool {
        self.peek().is_eof() || self.state.is_complete()
    }

    fn get_pos(&mut self) -> Pos {
        self.lexer.get_pos()
    }
}

#[allow(unused)]
impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            lexer: Lexer::new(src),
            state: StreamState::new(None),
            parsed: vec![],
            has_error: false,
        }
    }

    pub fn eat(&mut self, literal: &str) -> Maybe<Token> {
        if !self.state.is_halted() && self.match_literal(literal) {
            Ok(self.next())
        } else {
            let p = self.get_pos();
            let halt = Halt::Unexpected(format!(
                "Expected '{}', but instead found '{}' {:?}",
                literal,
                self.peek().literal(),
                p
            ));
            Err(halt)
        }
    }

    pub fn eat_match<K, F: FnMut(&mut Self, bool) -> K>(
        &mut self,
        literal: &str,
        parser: &mut F,
    ) -> K {
        if self.match_literal(literal) {
            self.eat(literal);
            parser(self, true)
        } else {
            parser(self, false)
        }
    }

    pub fn ignore(&mut self, literal: &str) {
        if self.match_literal(literal) {
            self.eat(literal);
        }
    }

    pub fn match_literal(&mut self, s: &str) -> bool {
        self.peek().match_literal(s)
    }

    pub fn match_any_of(&mut self, literals: &[&str]) -> bool {
        self.peek().match_any_of(literals)
    }

    pub fn guard_depth<K: std::fmt::Debug, F: 'a + FnMut(&mut Self) -> K>(
        &mut self,
        parse: &mut F,
    ) -> Maybe<K> {
        let start = self.lexer.get_depth();
        let parsed = parse(self);
        if start != self.lexer.get_depth() {
            println!("error from getting {:#?}", parsed);
            Err(Halt::Unexpected(
                "Unclosed parentheses/bracket/braces!".to_string(),
            ))
        } else {
            Ok(parsed)
        }
    }

    // unlike the algorithm used for where clauses, we allow for a single empty
    // trailing infix delimiter
    pub fn delimited<K, F: FnMut(&mut Self) -> K>(
        &mut self,
        [prefix, infix, suffix]: [&str; 3],
        parser: &mut F,
    ) -> Vec<K> {
        let mut nodes: Vec<K> = vec![];
        let mut first = true;
        self.eat(prefix);
        while !self.done() {
            if self.match_literal(suffix) {
                break;
            };
            if first {
                first = false;
            } else {
                self.eat(infix);
            };
            if self.match_literal(suffix) {
                break;
            };
            nodes.push(parser(self))
        }
        self.eat(suffix);
        nodes
    }

    pub fn parse(&mut self) -> Expr {
        let mut body: Vec<Expr> = vec![];
        while !self.done() && !self.state.is_halted() {
            let expr = self.expression();
            if let Expr::Error(h, t) = expr {
                panic!("{}; token: {}", h, t);
            } else {
                body.push(expr);
                if !self.done() {
                    self.eat(";");
                }
            };
        }
        Expr::Program(Program::new(None, body, self.lexer.take_meta()))
    }

    pub fn expression(&mut self) -> Expr {
        let expr = self.maybe_where(|p| p.maybe_call(Self::maybe_binary));
        self.parsed.push(expr.clone());
        expr
    }

    pub fn maybe_where<F: FnMut(&mut Self) -> Expr>(
        &mut self,
        mut parse: F,
    ) -> Expr {
        let expr = parse(self);
        if self.match_literal("where") {
            self.where_binding(expr)
        } else {
            expr
        }
    }

    pub fn top_level(&mut self) -> Expr {
        self.maybe_call(Self::maybe_binary)
    }

    pub fn atom(&mut self) -> Expr {
        self.maybe_call(Self::nonterminal)
    }

    pub fn maybe_binary(&mut self) -> Expr {
        self.binary(0, &mut Self::atom)
    }

    #[inline]
    fn cobinary(
        &mut self,
        token: Token,
        mut left: Option<Expr>,
        (this_prec, that_prec): (usize, usize),
    ) -> Expr {
        if let Some(mut expr) = left {
            self.binary(this_prec, |p: &mut Parser| {
                Expr::from_operator(&token)(
                    token.clone(),
                    Box::new(expr.clone()),
                    Box::new(p.binary(that_prec, Self::atom)),
                )
            })
        } else {
            Expr::Unary(token, Box::new(self.atom()))
        }
    }

    pub fn binary<F: FnMut(&mut Self) -> Expr>(
        &mut self,
        this_prec: usize,
        mut parse: F,
    ) -> Expr {
        let mut left = parse(self);
        let mut token = self.peek();
        if let Some((that_prec, assoc)) = token.operator_info() {
            if (this_prec < that_prec
                || (this_prec == that_prec && matches!(assoc, Assoc::Right)))
            {
                self.next();
                left = self.cobinary(
                    token,
                    Some(left.clone()),
                    (this_prec, that_prec),
                );
            };
        };
        left
    }

    // for parsing binary expressions restricted set of binary operator
    // expressions exclusively left associative
    pub fn bip_op_left<F: FnMut(&mut Self) -> Expr>(
        &mut self,
        mut parse: F,
        ops: &[&str],
    ) -> Expr {
        let mut left = parse(self);
        let mut token = self.peek();
        while token.match_any_of(ops) && !self.done() {
            self.next();
            left = Expr::from_operator(&token)(
                token,
                Box::new(left),
                Box::new(parse(self)),
            );
            token = self.peek();
        }
        left
    }

    pub fn maybe_call<F: FnMut(&mut Self) -> Expr>(
        &mut self,
        mut parse: F,
    ) -> Expr {
        let expr = parse(self);
        let mut token = self.peek();
        match token.literal().as_str() {
            "(" => self.call(expr),
            "." => self.maybe_access(expr),
            "[" => self.maybe_index(expr),
            "::" => self.maybe_path(expr),
            "|>" => self.piped(expr),
            _ => expr,
        }
    }

    pub fn maybe_path(&mut self, root: Expr) -> Expr {
        self.eat_match("::", &mut |parser, b| {
            if b {
                let path = parser.next();
                if let Token::Identifier(..) | Token::Meta(..) = path {
                    let expr2 = Expr::Path(Box::new(root.to_owned()), path);
                    parser.maybe_call(&mut |p: &mut Parser| expr2.to_owned())
                } else {
                    Expr::Error(
                        Halt::InvalidInput(format!(
                            "Invalid path token {} for body {}",
                            path, root
                        )),
                        parser.peek(),
                    )
                }
            } else {
                root.to_owned()
            }
        })
    }

    pub fn maybe_index(&mut self, body: Expr) -> Expr {
        self.eat_match("[", &mut |parser, b| {
            if b {
                let idx = parser.expression();
                parser.eat("]");
                let expr =
                    Expr::Index(Box::new(body.to_owned()), Box::new(idx));
                parser.maybe_call(&mut |p2: &mut Parser| expr.to_owned())
            } else {
                body.to_owned()
            }
        })
    }

    pub fn maybe_access(&mut self, expr: Expr) -> Expr {
        self.eat_match(".", &mut |p: &mut Parser, b| {
            if b {
                let id = p.next();
                if let Token::Identifier(..) | Token::Number(..) = id {
                    let expr2 = Expr::Member(Box::new(expr.to_owned()), id);
                    p.maybe_call(&mut |p: &mut Parser| expr2.to_owned())
                } else {
                    Expr::Error(
                        Halt::InvalidInput(format!(
              "Invalid access token {} may not be used in lookups for {}",
              id, expr
            )),
                        p.peek(),
                    )
                }
            } else {
                expr.to_owned()
            }
        })
    }

    pub fn call(&mut self, expr: Expr) -> Expr {
        Expr::Call(
            Box::new(expr.clone()),
            self.delimited(["(", ",", ")"], &mut Self::expression),
            match expr {
                Expr::Literal(id) => Some(id),
                Expr::Lambda(n, ..) => n,
                Expr::Call(.., n) => n,
                Expr::Member(_, n) => Some(n),
                _ => None,
            },
        )
    }

    pub fn where_binding(&mut self, body: Expr) -> Expr {
        self.eat("where");
        let mut pats = vec![];
        pats.push(self.binding("="));
        while let Token::Punct(',', _) = self.next() {
            if self.done() {
                return Expr::Error(
                    Halt::Unexpected(format!(
                        "Collected pats in where clause: {:#?}\n\nUnexpected EOF in incomplete `where` clause due to trailing comma"
                    , pats)),
                    self.peek(),
                );
            };
            pats.push(self.binding("="));
        }
        Expr::Variable(pats, Box::new(body))
    }

    pub fn let_binding(&mut self) -> Expr {
        let args =
            self.delimited(["let", ",", "in"], &mut |p: &mut Parser| {
                if matches!(p.peek(), Token::Punct(..) | Token::Identifier(..))
                {
                    p.binding("=")
                } else {
                    panic!(
          "Expecting a variable name, but instead got {:?} at {:?}",
          p.peek(),
          p.get_pos()
        )
                }
            });
        Expr::Variable(args, Box::new(self.expression()))
    }

    fn binding(&mut self, op_literal: &str) -> Binding {
        let pram = self.parameter();
        let expr = match self.peek().literal().as_str() {
            op_literal => self.eat_match(op_literal, &mut |p, b| {
                if b {
                    p.expression()
                } else {
                    Expr::Nil
                }
            }),
            "," => Expr::Nil,
        };
        Binding { pram, expr }
    }

    fn annotation(&mut self, sep: &str) -> Morpheme {
        if self.match_literal(sep) {
            self.eat(sep);
            self.pattern()
        } else {
            Morpheme::Empty
        }
    }

    fn pattern(&mut self) -> Morpheme {
        let token = self.peek();
        if token.is_right_punct() {
            Morpheme::Empty
        } else if !token.is_left_punct() {
            let morpheme = (if token.match_literal("...") {
                self.next();
                if self.peek().is_identifier() {
                    Morpheme::Rest
                } else if self.peek().is_meta() {
                    Morpheme::Meta
                } else {
                    |t| Morpheme::Empty
                }
                // Morpheme::Rest
            } else {
                Morpheme::Atom
            })(self.next());
            if self.match_literal("(") {
                let args = self.delimited(["(", ",", ")"], &mut Self::pattern);
                Morpheme::Call(token, args)
            } else {
                morpheme
            }
        } else {
            let prefix = token.literal();
            let pats = self.delimited(
                [&prefix, ",", twin_of(&prefix)],
                &mut Self::pattern,
            );
            match prefix.as_str() {
                "[" => Morpheme::Vector(pats),
                "(" => {
                    if pats.is_empty() {
                        Morpheme::Empty
                    } else {
                        Morpheme::Tuple(pats)
                    }
                }
                "{" => Morpheme::Record(pats),
                "<" => Morpheme::Iter(pats),
                _ => Morpheme::Empty,
            }
        }
    }

    fn parameter(&mut self) -> Parameter {
        let token = self.peek();
        let shape = Shape::from(token.clone());
        let name = if token.clone().is_left_punct() {
            Token::Empty()
        } else if token.clone().is_identifier() {
            self.peek()
        } else {
            token.clone()
        };
        Parameter {
            name: name.clone(),
            pattern: self.pattern(),
            shape: if name == token && shape != Shape::Atom {
                Shape::Empty
            } else {
                shape
            },
            kind: self.annotation(":"),
        }
    }

    pub fn unary(&mut self) -> Expr {
        if self.peek().is_unary() {
            let mut op = self.next();
            let right = self.unary();
            Expr::Unary(op, Box::new(right))
        } else {
            self.atom()
        }
    }

    pub fn conditional(&mut self) -> Expr {
        Expr::Conditional(
            Box::new({
                self.eat("if");
                self.expression()
            }),
            Box::new({
                self.eat("then");
                self.expression()
            }),
            self.eat_match("else", &mut |p, b| {
                if b {
                    Some(Box::new(p.expression()))
                } else {
                    None
                }
            }),
        )
    }

    pub fn function(&mut self) -> Expr {
        self.eat("fn");
        let name = self.next();
        if !name.is_identifier() {
            Expr::Error(
                Halt::InvalidInput(
                    "Identifier names must follow the `fn` keyword!"
                        .to_string(),
                ),
                name,
            )
        } else {
            match (if self.match_any_of(&["|", "||"]) {
                self.lambda()
            } else {
                self.expression()
            }) {
                Expr::Lambda(_, b, c) => Expr::Lambda(Some(name), b, c),
                lambda => lambda,
            }
        }
    }

    pub fn lambda(&mut self) -> Expr {
        let name = if self.peek().is_identifier() {
            Some(self.next())
        } else {
            None
        };
        let prams = if self.match_literal("||") {
            self.next();
            vec![]
        } else {
            self.delimited(["|", ",", "|"], &mut Self::parameter)
        };
        let body = self.maybe_block();
        Expr::Lambda(name, prams, Box::new(body))
    }

    // Haskell style lambdas are curried and inherently anonymous
    pub fn lambda_alt(&mut self) -> Expr {
        self.eat("\\");
        let mut prams = vec![];
        while !self.match_literal("->") && !self.done() {
            prams.push(self.parameter());
            if self.match_literal(",") {
                self.next();
            };
        }
        self.eat("->");
        prams.iter().fold(self.expression(), |a, c| {
            Expr::Lambda(None, vec![c.clone()], Box::new(a))
        })
        // ;
        // Expr::Lambda(None, prams, Box::new(self.expression()))
    }

    fn maybe_block(&mut self) -> Expr {
        if self.match_literal("{") {
            self.block()
        } else {
            self.expression()
        }
    }

    pub fn block(&mut self) -> Expr {
        let is_do = self.eat_match("do", &mut |p, b| b);
        let body = self.delimited(["{", ";", "}"], &mut Self::expression);
        match body.len() {
            0 => Expr::Nil,
            1 => match body.get(0) {
                Some(b) => b.to_owned(),
                None => Expr::Nil,
            },
            _ => Expr::Block(is_do, body.to_owned()),
        }
    }

    pub fn record(&mut self) -> Expr {
        self.ignore("#");
        let name = if matches!(
            self.peek(),
            Token::Identifier(..) | Token::Meta(..)
        ) {
            Kind(self.next())
        } else {
            Kind(Token::Empty())
        };
        let body = self
            .delimited(["{", ",", "}"], &mut |p: &mut Parser| p.binding("="));
        Expr::Record(name, body)
    }

    pub fn case(&mut self) -> Expr {
        self.eat("case");
        let test = self.expression();
        self.eat("of");
        self.eat("{");
        let mut deft: Expr = Expr::Literal(Token::Empty());
        let mut conds = vec![];
        let mut token = self.peek();
        while !(self.done() || token.match_literal("}")) {
            let pattern = self.bip_op_left(
                &mut |p: &mut Self| {
                    p.bip_op_left(
                        |p2: &mut Parser| p2.binary(3, Self::atom),
                        &["|"],
                    )
                },
                &["->", "@"],
            );
            self.eat("=>");
            let branch = self.expression();
            self.ignore(",");
            if token.match_literal("_") {
                deft = branch.clone();
            } else {
                conds.push((pattern, branch));
            };
            token = self.peek();
        }
        self.eat("}");
        Expr::Case(Box::new(test), conds, Box::new(deft))
    }

    pub fn looping(&mut self) -> Expr {
        self.eat("loop");
        let cond = self.maybe_block();
        self.ignore(",");
        let body = self.maybe_block();
        Expr::Loop(Box::new(cond), Box::new(body))
    }

    pub fn maybe_tuple(&mut self) -> Expr {
        let expr = match self.peek() {
            Token::Punct(')', _) => {
                self.next();
                Expr::Nil
            }
            Token::Punct(',', _) => {
                self.next();
                if self.match_literal(")") {
                    self.next();
                    Expr::Nil
                } else {
                    Expr::Error(Halt::Unexpected(format!(
                        "Expected `)` after `(,` since `(,` is only a valid fragment in `(,)`")), self.peek())
                }
            }
            Token::Punct(':', _) => {
                self.next();
                let func = self.expression();
                let mut args = vec![];
                while !self.peek().match_literal(")") {
                    args.push(self.expression());
                }
                self.next();
                return Expr::Call(Box::new(func), args, None);
            }
            _ => self.expression(),
        };
        if matches!(&expr, Expr::Nil) {
            return expr;
        } else {
            match self.peek() {
                Token::Punct(',', _) => {
                    let mut rest =
                        self.delimited([",", ",", ")"], &mut Self::expression);
                    rest.insert(0, expr);
                    Expr::Tuple(rest)
                }
                Token::Punct(')', _) => {
                    self.eat(")");
                    expr
                }
                k => panic!("Parentheses not closed! Token: {:?}", k),
            }
        }
    }

    pub fn list_comprehension(&mut self, head: Expr) -> Expr {
        let mut ranges = vec![];
        let mut fixed = vec![];
        let mut conds = vec![];
        let rhs = self.delimited(["|", ",", "]"], &mut |p| {
      let expr = p.expression();
      if let Expr::Assign(op, b, c) = expr.to_owned() {
        match op.literal().as_str() {
          "<-" => ranges.push((*b, *c)),
          "=" | ":=" => fixed.push((*b, *c)),
          _ =>
          /* TODO */
          {
            log_do!(
                "Invalid expression in list comprehension!" => &expr,
                "Op" => &op,
                "Token" => p.peek(),
                "fixed" => &fixed,
                "ranges" => &ranges,
                "conds" => &conds
            )
          }
        };
      } else {
        conds.push(expr)
      }
    });
        Expr::List(Box::new(Definition::new(head, ranges, fixed, conds)))
    }

    pub fn maybe_list(&mut self) -> Expr {
        self.eat("[");
        self.eat_match("]", &mut |p, is_rt_brack| {
            if is_rt_brack {
                Expr::Vector(vec![])
            } else {
                let item = if p.match_literal("[") {
                    p.maybe_list()
                } else {
                    p.atom()
                };
                match p.peek().literal().as_str() {
                    "|" => p.list_comprehension(item),
                    "," => {
                        let mut expr = p
                            .delimited([",", ",", "]"], &mut Self::expression);
                        expr.insert(0, item);
                        Expr::Vector(expr)
                    }
                    "]" => {
                        p.eat("]");
                        Expr::Vector(vec![item])
                    }
                    _ => {
                        let pos = p.get_pos();
                        p.unknown(
                            format!(
                                "Unable to find closing square bracket {:?}",
                                pos
                            ),
                            Some("]"),
                        )
                    }
                }
            }
        })
    }

    pub fn symbol(&mut self, prefix: String, p: Pos) -> Expr {
        self.next();
        match prefix.as_str() {
            "_" => Expr::Literal(Token::Symbol(prefix, p)),
            "#" => self.record(),
            "`" | "@" => {
                Expr::Literal(Token::Symbol(self.next().literal(), p))
            }
            _ => todo!(),
        }
    }

    pub fn range(&mut self, expr: Option<Expr>) -> Expr {
        let start = if let Some(x) = expr {
            x
        } else {
            Expr::Literal(Token::Number("0".to_string(), 0, self.get_pos()))
        };

        self.eat_match("..", &mut |p0, b0| {
            if b0 {
                let limit = match p0.peek() {
                    Token::Number(..) | Token::Identifier(..) => {
                        Some(Box::new(Expr::Literal(p0.next())))
                    }
                    Token::Punct(']', ref p) => None,
                    Token::Punct('(', ref p) => {
                        p0.eat_match(")", &mut |p1, b1| {
                            if b1 {
                                p1.next();
                                None
                            } else {
                                Some(Box::new(p1.expression()))
                            }
                        })
                    }
                    _ => None,
                };
                Expr::Iter(Box::new(start.to_owned()), limit)
            } else {
                Expr::Error(
                    Halt::InvalidInput(
                        "Expected range operator `..`".to_string(),
                    ),
                    p0.peek(),
                )
            }
        })
    }

    pub fn nonterminal(&mut self) -> Expr {
        let token = self.peek();
        let pos = self.get_pos();
        match token {
            Token::Punct('(', _) => {
                self.next();
                self.maybe_tuple()
            }
            x if x.is_unary() => self.unary(),
            Token::Punct('{', _) => self.block(),
            x if x.match_any_of(&["|", "||"]) => return self.lambda(),

            Token::Operator(x, _) => match x.as_str() {
                "#" => self.record(),
                ".." => self.range(None),
                "\\" => self.lambda_alt(),
                _ => self.unknown(
                    format!("Unable to parse Operator {:?} at {:?}", x, pos),
                    Some(";"),
                ),
            },
            Token::Keyword(k, _) => match k.as_str() {
                "if" => self.conditional(),
                "let" => self.let_binding(),
                "do" => self.block(),
                "case" => self.case(),
                "loop" => self.looping(),
                "fn" => self.function(),
                "data" => self.data(),
                "const" => self.constant(),
                _ => self.unknown(
                    format!("Unable to parse Keyword {} at {:?}", k, pos),
                    Some(";"),
                ),
            },
            Token::Punct('[', _) => self.maybe_list(),
            Token::String(..) | Token::Identifier(..) => {
                self.next();
                Expr::Literal(token)
            }
            Token::Number(..) => {
                self.next();
                if self.match_literal("..") {
                    self.range(Some(Expr::Literal(token)))
                } else {
                    Expr::Literal(token.to_owned())
                }
            }
            Token::Symbol(s, p) => self.symbol(s, p),
            Token::Char(..) | Token::Symbol(..) | Token::Bool(..) => {
                self.next();
                Expr::Literal(token)
            }
            Token::Meta(s, _) => self.named(s),
            _ => {
                let msg = format!("Unable to parse {:?} at {:?}", token, pos);
                self.unknown(
                    format!("Unable to parse {:?} at {:?}", token, pos),
                    Some(";"),
                )
            }
        }
    }

    pub fn constant(&mut self) -> Expr {
        self.eat("const");
        let mut constants = vec![];
        constants.push(self.binding("="));
        while self.next().match_literal(",") {
            constants.push(self.binding("="));
        }
        Expr::Constant(constants)
    }

    pub fn piped(&mut self, arg: Expr) -> Expr {
        let mut pipes = vec![];
        while self.next().match_literal("|>") {
            pipes.push(self.expression());
        }
        Expr::Pipe(Box::new(arg), pipes)
    }

    pub fn named(&mut self, ty: String) -> Expr {
        Expr::Named(
            Kind(self.next()),
            Box::new({
                let tok = self.peek();
                if self.lexer.has_meta(&format!("{} {}", ty, tok.literal())) {
                    Expr::Named(Kind(tok), Box::new(self.expression()))
                } else {
                    self.expression()
                }
            }),
        )
    }

    pub fn data(&mut self) -> Expr {
        self.eat("data");
        // let name = self.next();
        if let Some(n) = self.lexer.id_as_meta() {
            let name = n;
            self.next();
            self.eat("::");
            let variants = self.delimited(["{", "|", "}"], &mut Self::variant);
            Expr::Data(Kind(name), variants)
        } else {
            let tok = self.peek();
            Expr::Error(
                Halt::Unexpected(format!(
                    "Expected data type identifier, but found `{}` instead",
                    tok
                )),
                tok,
            )
        }
    }

    fn variant(&mut self) -> DataVariant {
        let name = self.next();
        if self.match_literal("|") {
            DataVariant {
                ident: Kind(name),
                items: vec![],
            }
        } else {
            let pattern = self.pattern();
            DataVariant {
                ident: Kind(name),
                items: vec![pattern],
            }
        }
    }

    // TO BE (RE)DONE!
    pub fn unknown(&mut self, message: String, flag: Option<&str>) -> Expr {
        let here = (self.peek(), self.get_pos());
        self.state = StreamState::Halted(Token::Invalid(
            format!("{}. Token read: {}", message, self.peek()),
            self.get_pos(),
        ));
        let _ = flag.and_then(|word| {
            while !self.match_literal(word) && !self.lexer.done() {
                if self.get_pos().diff_line {
                    self.state = StreamState::Pending(self.next());
                    break;
                }
                Some(self.next());
                if self.match_literal(word) {
                    self.state = StreamState::Halted(Token::Invalid(
                        format!("{}. Token read: {}", message, self.peek()),
                        self.get_pos(),
                    ));
                };
            }
            return Some(self.peek());
        });
        if self.done()
            || self.lexer.done()
            || self.state.is_complete()
            || self.state.is_halted()
        {
            Expr::Error(
                Halt::Unexpected(format!(
          "Unexpected end of input!\n Unable to recover {:?} from token {}.",
          here,
          self.peek().literal()
        )),
                self.peek(),
            )
        } else {
            panic!("{:#?}", self)
        }
    }
}

pub fn parse_input(src: &str) -> Expr {
    let mut p = Parser::new(src);
    p.parse()
}

#[cfg(test)]
mod tests {
    use crate::{core::rygtype::RygType, log_do};

    use super::*;
    fn inspect_tokens(src: &str) {
        let mut parser = Parser::new(src);
        let mut i = 0;
        println!("source: {:?}", src);
        while !parser.done() {
            i += 1;
            println!("[{}] {:?}", i, parser.next());
        }
        println!("[{}] {:?}", i + 1, parser.next())
    }
    fn parse(src: &str) {
        let mut parser = Parser::new(src);
        println!("source: {:?}", src);
        let ast = parser.parse();
        println!("{:#?}", ast)
    }

    fn inspect_expr(src: &str) {
        let mut parser = Parser::new(src);
        let mut i = 0;
        println!("source: {:?}", src);
        while !parser.done() && !parser.lexer.done() {
            i += 1;
            println!("[{}] {:#?}", i, parser.expression());
        }
        println!("[{}] {:?}", i + 1, parser.next())
    }

    #[test]
    fn inspect_decimal() {
        let src = "3.146";
        let mut parser = Parser::new(src);
        // println!("{:#?}", parser.parse());
        println!("{:?}", parser.next());
        println!("{:?}", parser.next());
    }

    #[test]
    fn inspect_arithmetic() {
        let src = "a <- (b <- c)"; //"1 + 2 / 3 * (4 ** 5 / 6)";
        log_do!(
          // "bin_right" => Parser::new(src).bin_right(0, &mut Parser::expression),
          "default (left)" => Parser::new(src).parse()
          // "(both) bin_direct" => Parser::new(src).bin_direct(0, &mut Parser::expression)
        );
        // inspect_expr(src)
    }

    #[test]
    fn inspect_new_line() {
        let src = "x = 1
        y = 2";
        parse(src);
    }

    #[test]
    fn inspect_vector() {
        let src = "[2, 3];";
        parse(src);
    }

    #[test]
    fn inspect_lambda() {
        let src = "(|a: A, [b, c]: [C, B]| a + 19)()";
        inspect_tokens(src);
        parse(src);
    }

    #[test]
    fn inspect_tuple() {
        let src = "(-i, @, 5, 3 + 5/6)"; //"let x = 4 in x + 1;";
        parse(src);
    }

    #[test]
    fn inspect_record() {
        let src = "#{a: A = b + 1, c: C = d};";
        inspect_tokens(src);
        parse(src);
    }

    #[test]
    fn inspect_loop() {
        let src = "loop a {b; c; d} + 3"; //"a =< b =< |c| a ** b";
        let src = "loop || let t = i += 1 in t { print'ln(t) };";
        parse(src);
        inspect_expr(src);
    }

    #[test]
    fn inspect_let() {
        let src = "let a = 1, b = 2 in a + b == 3;";
        inspect_expr(src);
        parse("let [a, b] = [1, 2] in a + b")
    }

    #[test]
    fn inspect_case() {
        let src = "case 3 + 5 of { 1 => true, 8 => \"womp\"};";
        inspect_tokens(src.clone());
        inspect_expr(src);
    }

    #[test]
    fn inspect_list() {
        let src = "[[(a, b, c)] | a <- [[1], [t]], a + t > 0, c = 3]";
        inspect_tokens(src.clone());
        inspect_expr(src);
    }

    #[test]
    fn inspect_unary() {
        let src = "x = y = -z - 5"; //"-(a) + b - c";
        let mut parser = Parser::new(src);
        log_do!(
          "tok to string" => Token::Identifier(String::from("s34"), Pos::faux()).to_string(),
         "parser" => &parser,
        //  "next" => &parser.next(),
        //  "expression" => &parser.maybe_unary(&mut |p: &mut Parser| p.unary()),
        //   "next" => &parser.next(),
        //   "next" => &parser.next(),
          "expression" => &parser.expression()
        )
    }

    #[test]
    fn member_access() {
        let src = "-foo.bar.baz(x, y)";
        inspect_expr(src);
    }

    #[test]
    fn ranges() {
        let src = "0..3";
        [
            "0..3",
            "[1, 2, 3, 4][0]",
            "[1, [2, [3]]][0][1][0]",
            "[1, 2, 3, 4, 5][0..3]",
            "\"cat\"[0]",
            "\"a cat is not blue\"[..]",
        ]
        .iter()
        .for_each(|s| inspect_expr(s))
        // inspect_expr(src);
    }

    // #[test]
    // fn ten_k_sums() {
    //     let src = (0..)
    //         .take_while(|n| *n < 10 * 57)
    //         .map(|n| n.to_string())
    //         .collect::<Vec<_>>()
    //         .join(" + ");
    //     // println!("{}", src);
    //     log_do!(
    //         "length" => &src.len()
    //     );
    //     let ast = parse_input(&src);
    //     // println!("{:#?}", &ast)
    // }

    #[test]
    fn inspect_data() {
        let src = "data Womp :: { 
            A a 
            | B (b, b') 
            | C  
            | D (Int, Float) 
            | E {a, b} 
        }";
        inspect_expr(src)
    }

    #[test]
    fn expr_ty() {
        let src = "3 + 4";
        let ast = parse_input(src);
        let ty = RygType::from(ast.clone());
        log_do!(
            "3 + 4" => ast,
            "type'of Expr(3 + 4)" => ty
        )
    }
}
