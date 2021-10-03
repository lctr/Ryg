use std::{
  fmt::{Debug, Display},
  ptr::NonNull,
  rc::Rc,
};

use crate::{
  lexing::{
    lexer::Lexer,
    token::{Assoc, Token},
  },
  log_do,
  parsing::expression::{Binding, Definition},
  util::{
    state::{Halt, Pos, StreamState, Streaming},
    types::{Either, Kind, Maybe},
  },
};

use super::expression::{Expr, Parameter, Shape, TokPattern};

pub struct Parsed<'a, T>(&'a [T]);
impl<'a, T: Debug> std::fmt::Debug for Parsed<'a, T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{:#?}", &self.0)
  }
}

impl<'a> Parsed<'a, Expr> {
  pub fn as_block(&self) -> Expr {
    Expr::Block((&(self.0)).to_vec())
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

#[derive(Debug)]
pub struct Parser<'a> {
  lexer: Lexer<'a>,
  state: StreamState<Token>,
  parsed: Vec<Expr>,
}

impl<'a> Streaming<Token> for Parser<'a> {
  fn peek(&mut self) -> Token {
    self.lexer.peek()
  }

  fn next(&mut self) -> Token {
    if let StreamState::Halted(a) = &self.state {
      return a.clone();
    };
    let next = self.lexer.next();
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
  pub fn new(src: &str) -> Parser {
    Parser {
      lexer: Lexer::new(src),
      state: StreamState::new(None),
      parsed: vec![],
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

  pub fn maybe_delimited<K, F: FnMut(&mut Self) -> K>(
    &mut self,
    sep: Option<&str>,
    mut parser: F,
  ) -> Either<K, Vec<K>> {
    let token = self.peek();
    let infix = sep.unwrap_or(",");
    if let Token::Punct(c @ ('(' | '[' | '{' | '<'), _) = token {
      let c = &c.to_string();
      Either::Right(self.delimited((c, infix, twin_of(c)), &mut parser))
    } else {
      Either::Left(parser(self))
    }
  }

  pub fn delimited<K, F: FnMut(&mut Self) -> K>(
    &mut self,
    (prefix, infix, suffix): (&str, &str, &str),
    parser: &mut F,
  ) -> Vec<K> {
    let mut nodes: Vec<K> = vec![];
    self.eat(prefix);
    let mut first = true;
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
      nodes.push(parser(self));
    }
    self.eat(suffix);
    nodes
  }

  pub fn parse(&mut self) -> Expr {
    // Expr {
    let mut body: Vec<Expr> = vec![];
    while !self.done() {
      let expr = self.expression();
      if let Expr::Error(h, t) = expr {
        panic!("{}; token: {}", h, t);
      } else {
        body.push(expr);
        if !self.done() {
          self.eat(";");
          // replace once error handling is finalized
          // self.ignore(";");
        }
      };
    }
    Expr::Block(body)
  }

  pub fn expression(&mut self) -> Expr {
    let expr = self.maybe_call(Self::maybe_binary);
    self.parsed.push(expr.clone());
    expr
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
      _ => expr,
    }
  }

  pub fn maybe_index(&mut self, body: Expr) -> Expr {
    self.eat_match("[", &mut |p1, b| {
      if b {
        let idx = p1.expression();
        p1.eat("]");
        let expr = Expr::Index(Box::new(body.to_owned()), Box::new(idx));
        p1.maybe_call(&mut |p2: &mut Parser| expr.to_owned())
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
          let expr2 = Expr::Index(
            Box::new(expr.to_owned()),
            Box::new(Expr::Literal(id)),
          );
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
      self.delimited(("(", ",", ")"), &mut Self::expression),
      match expr {
        Expr::Literal(id) => Some(id.literal()),
        Expr::Lambda(n, ..) => n,
        Expr::Call(.., n) => n,
        _ => None,
      },
    )
  }

  pub fn atom(&mut self) -> Expr {
    self.maybe_call(Self::terminal)
  }

  pub fn maybe_binary(&mut self) -> Expr {
    self.binary(0, &mut Self::atom)
  }

  fn maybe_unary(
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
      self.bin_direct(this_prec, &mut |p: &mut Parser| {
        let left = p.atom();
        p.maybe_unary(token.clone(), Some(left), (this_prec, that_prec))
      })
      // Expr::Unary(token, Box::new(self.atom()))
    }
  }

  pub fn binary<F: FnMut(&mut Self) -> Expr>(
    &mut self,
    this_prec: usize,
    mut parse: F,
  ) -> Expr {
    let mut left = parse(self);
    let mut token = self.peek();
    match token.clone() {
      x if x.as_operator().is_none() => left,
      Token::Operator(..) => {
        if let Some((that_prec, assoc)) = token.clone().operator_info() {
          if (this_prec < that_prec
            || (this_prec == that_prec && matches!(assoc, Assoc::Right)))
          {
            self.next();
            left = self.maybe_unary(
              token.clone(),
              Some(left.clone()),
              (this_prec, that_prec),
            )
          };
          token = self.peek();
        };
        left
      }
      _ => left,
    }
  }

  // for parsing binary expressions 1 set of operators at a time
  pub fn bip_op_left<F: FnMut(&mut Self) -> Expr>(
    &mut self,
    mut parse: F,
    ops: &[&str],
  ) -> Expr {
    let mut left = parse(self);
    let mut token = self.peek();
    while token.match_any_of(ops) {
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

  pub fn bin_direct<F: FnMut(&mut Self) -> Expr>(
    &mut self,
    this_prec: usize,
    mut parse: &mut F,
  ) -> Expr {
    let mut expr = self.atom();
    let mut token = self.peek();
    while let Some((that_prec, assoc2)) = token.operator_info() {
      let other = match assoc2 {
        Assoc::Left => self.bin_direct(this_prec + 1, parse),
        Assoc::Right | _ => self.bin_direct(this_prec, parse),
      };
      expr = Expr::from_operator(&token)(
        token.clone(),
        Box::new(expr),
        Box::new(other),
      );
      token = self.next();
    }
    expr
  }

  pub fn unary(&mut self) -> Expr {
    if self.match_any_of(&["!", "-"]) {
      let mut op = self.next();
      let right = self.unary();
      Expr::Unary(op, Box::new(right))
    } else {
      self.atom()
    }
  }

  pub fn conditional(&mut self) -> Expr {
    self.eat("if");
    let cond = self.expression();
    self.eat("then");
    let then = self.expression();
    let deft = if self.match_literal("else") {
      self.eat("else");
      Some(Box::new(self.expression()))
    } else {
      None
    };
    Expr::Conditional(Box::new(cond), Box::new(then), deft)
  }

  pub fn function(&mut self) -> Expr {
    self.eat("fn");
    let name = if let Token::Identifier(n, _) = self.next() {
      n
    } else {
      let token = self.peek();
      self.unknown(
        format!(
          "Expected a name for the function definition, but instead found {}",
          token
        ),
        Some("|"),
      );
      return self.lambda();
    };
    match if self.match_any_of(&["|", "||"]) {
      self.lambda()
    } else {
      self.expression()
    } {
      Expr::Lambda(_, b, c) => Expr::Lambda(Some(name), b, c),
      lambda => lambda,
    }
  }

  pub fn lambda(&mut self) -> Expr {
    let name = if let Token::Identifier(t, _) = self.peek() {
      self.next();
      Some(t)
    } else {
      None
    };
    let prams = if self.match_literal("||") {
      self.next();
      vec![]
    } else {
      self.delimited(("|", ",", "|"), &mut Self::parameter)
    };
    let body = self.maybe_block();
    Expr::Lambda(name, prams, Box::new(body))
  }

  fn maybe_block(&mut self) -> Expr {
    if self.match_literal("{") {
      self.block()
    } else {
      self.expression()
    }
  }

  pub fn block(&mut self) -> Expr {
    let body = self.delimited(("{", ";", "}"), &mut Self::expression);
    match body.len() {
      0 => Expr::Nil,
      1 => match body.get(0) {
        Some(b) => b.to_owned(),
        None => Expr::Nil,
      },
      _ => Expr::Block(body.to_owned()),
    }
  }

  pub fn record(&mut self) -> Expr {
    let name =
      if matches!(self.peek(), Token::Identifier(..) | Token::Meta(..)) {
        Kind(self.next())
      } else {
        Kind(Token::Empty())
      };
    let body =
      self.delimited(("{", ",", "}"), &mut |p: &mut Parser| p.binding("="));
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
          p.bip_op_left(|p2: &mut Parser| p2.binary(3, Self::atom), &["|"])
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
    let body = self.maybe_block();
    Expr::Loop(Box::new(cond), Box::new(body))
  }

  pub fn maybe_tuple(&mut self) -> Expr {
    let expr = if self.match_literal(")") {
      self.next();
      return Expr::Nil;
    } else {
      self.expression()
    };
    match self.peek() {
      Token::Punct(',', _) => {
        let mut rest =
          self.delimited((",", ",", ")"), &mut Parser::expression);
        rest.insert(0, expr);
        Expr::Tuple(rest)
      }
      Token::Punct(')', _) => {
        self.eat(")");
        expr
      }
      Token::Operator(x, _) if matches!(x.as_str(), "!" | "-") => {
        Expr::Unary(self.next(), Box::new(self.unary()))
      }
      k => panic!("Idk how to handle {:?}", k),
    }
  }

  pub fn list_comprehension(&mut self, head: Expr) -> Expr {
    let mut ranges = vec![];
    let mut fixed = vec![];
    let mut conds = vec![];
    let rhs = self.delimited(("|", ",", "]"), &mut |p| {
      let expr = p.expression();
      if let Expr::Assign(op, b, c) = expr.to_owned() {
        match op.literal().as_str() {
          "<-" => ranges.push((*b, *c)),
          "=" => fixed.push((*b, *c)),
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
            let mut expr = p.delimited((",", ",", "]"), &mut Self::expression);
            expr.insert(0, item);
            Expr::Vector(expr.to_owned())
            // p.maybe_index(|_| Expr::Vector(expr.to_owned()))
          }
          "]" => {
            p.eat("]");
            Expr::Vector(vec![item.to_owned()])
            // p.maybe_index(|_| Expr::Vector(vec![item.to_owned()]))
          }
          _ => {
            let pos = p.get_pos();
            panic!("Unable to find closing square bracket {:?}", pos);
          }
        }
      }
    })
  }

  pub fn symbol(&mut self, prefix: String, p: Pos) -> Expr {
    self.next();
    match prefix.as_str() {
      "_" => Expr::Literal(Token::Symbol(prefix, p)),
      "#" => {
        let sym = self.next();
        match sym {
          Token::Number(..)
          | Token::Identifier(..)
          | Token::Boolean(..)
          | Token::Operator(..)
          | Token::Keyword(..)
          | Token::Meta(..) => Expr::Literal(Token::Symbol(sym.literal(), p)),
          Token::Punct(st, _) => {
            let st = st.to_string();
            let body = self.delimited((&st, ",", twin_of(&st)), &mut |pr| {
              pr.next().literal()
            });
            Expr::Literal(Token::Symbol(
              [
                prefix.clone(),
                st,
                body.join(">"),
                twin_of(&prefix).to_string(),
              ]
              .concat(),
              p,
            ))
          }
          Token::Meta(_, _) => todo!(),
          _ => {
            let curr = self.peek();
            self.unknown(
              format!(
                "Unable to finish processing symbol {} and symbol body {}",
                sym, curr
              ),
              None,
            )
          }
        }
      }
      // "`" => todo!(),
      _ => todo!(),
    }
  }

  pub fn range(&mut self, expr: Option<Expr>) -> Expr {
    // let zero =
    // |p: &Pos| Expr::Literal(Token::Number("0".to_string(), 0, p.clone()));
    // let token = self.eat("..");

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
          Token::Punct('(', ref p) => p0.eat_match(")", &mut |p1, b1| {
            if b1 {
              p1.next();
              None
            } else {
              Some(Box::new(p1.expression()))
            }
          }),
          _ => None,
        };
        Expr::Iter(Box::new(start.to_owned()), limit)
      } else {
        Expr::Error(
          Halt::InvalidInput("Expected range operator `..`".to_string()),
          p0.peek(),
        )
      }
    })
    // if let Ok(token) = token {
    //   let limit = match self.peek() {
    //     t @ (Token::Number(..) | Token::Identifier(..)) => {
    //       self.next();
    //       Some(Box::new(Expr::Literal(t)))
    //     }
    //     Token::Punct(']', ref p) => {
    //       Some(Box::new(Expr::Iter(Box::new(zero(p)), None)))
    //     }
    //     Token::Punct('(', ref p) => {
    //       self.next();
    //       self.eat_match(")", &mut |p1, b| {
    //         if b {
    //           Some(Box::new(zero(p)))
    //         } else {
    //           Some(Box::new(p1.expression()))
    //         }
    //       })
    //     }
    //     _ => None,
    //   };
    //   Expr::Iter(Box::new(Expr::Literal(token.clone())), limit)
    // } else {
    //   Expr::Error(
    //     Halt::InvalidInput("Expected range operator `..`".to_string()),
    //     self.peek(),
    //   )
    // }
  }

  pub fn terminal(&mut self) -> Expr {
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
        _ => self.unknown(
          format!("Unable to parse Operator {:?} at {:?}", x, pos),
          Some(";"),
        ),
      },
      Token::Keyword(k, _) => match k.as_str() {
        "if" => self.conditional(),
        "let" => self.let_binding(),
        "do" => {
          self.next();
          self.block()
        }
        "case" => self.case(),
        "loop" => self.looping(),
        "fn" => self.function(),
        _ => self.unknown(
          format!("Unable to parse Keyword {} at {:?}", k, pos),
          Some(";"),
        ),
      },
      Token::Punct('[', _) => self.maybe_list(),
      Token::String(..) | Token::Identifier(..) => {
        self.next();
        Expr::Literal(token)
        // self.maybe_index(|_| Expr::Literal(token.to_owned()))
      }
      Token::Number(..) => {
        self.next();
        if self.match_literal("..") {
          self.range(Some(Expr::Literal(token)))
          // self.next();
          // let limit = match self.peek() {
          //   Token::Number(..) | Token::Identifier(..) => {
          //     Some(Box::new(Expr::Literal(self.next())))
          //   }
          //   _ => None,
          // };
          // Expr::Iter(Box::new(Expr::Literal(token.clone())), limit)
        } else {
          Expr::Literal(token.to_owned())
        }
      }
      Token::Symbol(s, p) => self.symbol(s, p),
      Token::Char(..)
      | Token::Meta(..)
      | Token::Symbol(..)
      | Token::Boolean(..) => {
        self.next();
        Expr::Literal(token)
      }
      _ => {
        let msg = format!("Unable to parse {:?} at {:?}", token, pos);
        self.unknown(
          format!("Unable to parse {:?} at {:?}", token, pos),
          Some(";"),
        );
        panic!(msg)
      }
    }
  }

  pub fn let_binding(&mut self) -> Expr {
    let args = self.delimited(("let", ",", "in"), &mut |p: &mut Parser| {
      if matches!(p.peek(), Token::Punct(..) | Token::Identifier(..)) {
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
    Binding {
      pram: self.parameter(),
      expr: self.eat_match(op_literal, &mut |p, b| {
        if b {
          p.expression()
        } else {
          Expr::Nil
        }
      }),
    }
  }

  fn annotation(&mut self) -> Vec<Token> {
    self.eat_match(":", &mut |p1, b| {
      if b {
        match p1.maybe_delimited(Some(","), &mut |p2: &mut Parser| p2.next()) {
          Either::Left(x) => vec![x],
          Either::Right(x) => x,
        }
      } else {
        vec![]
      }
    })
  }

  fn pattern(&mut self) -> TokPattern {
    let token = self.peek();
    if !token.is_left_punct() {
      TokPattern::Atom(self.next())
    } else {
      let prefix = token.literal();
      let pats =
        self.delimited((&prefix, ",", twin_of(&prefix)), &mut Self::pattern);
      match prefix.as_str() {
        "[" => TokPattern::Vector(pats),
        "(" => TokPattern::Tuple(pats),
        "{" => TokPattern::Record(pats),
        "<" => TokPattern::Iter(pats),
        _ => TokPattern::Empty,
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
      kind: self.annotation(),
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
        if self.get_pos().new_line {
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
      self.expression()
    }
  }
}

fn twin_of(s: &str) -> &str {
  match s {
    "(" => ")",
    ")" => "(",
    "[" => "]",
    "]" => "[",
    "{" => "}",
    "}" => "{",
    "|" => "|",
    "<" => ">",
    ">" => "<",
    "let" => "in",
    "var" => "in",
    "case" => "of",
    _ => s.chars().rfold(&"", |a, c| {
      a.to_string().push(c);
      a
    }),
  }
}

pub fn parse_input(src: &str) -> Expr {
  let mut p = Parser::new(src);
  p.parse()
}

#[cfg(test)]
mod tests {
  use crate::log_do;

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
      "default (left)" => Parser::new(src).parse(),
      // "(both) bin_direct" => Parser::new(src).bin_direct(0, &mut Parser::expression)
    );
    // inspect_expr(src)
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
    let src = "#{a = b + 1, c = d};";
    inspect_tokens(src);
    parse(src);
  }

  #[test]
  fn inspect_loop() {
    let src = "loop a {b; c; d} + 3"; //"a =< b =< |c| a ** b";
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
    let src = "x = y = -z"; //"-(a) + b - c";
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
    let src = "foo.bar.baz(x, y);";
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
}
