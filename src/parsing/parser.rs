use std::{ptr::NonNull, rc::Rc};

use crate::{
  lexing::{lexer::Lexer, token::Token},
  parsing::expression::{Binding, Definition},
  util::{
    state::{Halt, Pos, StreamState, Streaming},
    types::{Either, Kind, Maybe},
  },
};

use super::expression::{Expr, Parameter, Shape, TokPattern};

pub struct Parsed<'a, T>(&'a T);

#[derive(Debug)]
pub struct Parser<'a> {
  lexer: Lexer<'a>,
  state: StreamState<Token>,
  parsed: Vec<Expr>,
}

impl<'a> Streaming<Token> for Parser<'a> {
  fn peek(&mut self) -> Token {
    // println!("peek: {:?}", &self.lexer.peek());
    self.lexer.peek()
  }

  fn next(&mut self) -> Token {
    if let StreamState::Halted(a) = &self.state {
      return a.clone();
    };
    let next = self.lexer.next();
    // println!("before next: {:?}", &next);
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
    if !self.state.is_halted() && self.word() == literal {
      Ok(self.next())
    } else {
      let p = self.get_pos();
      let halt = Halt::Unexpected(format!(
        "Expected '{}', but instead found '{}' {:?}",
        literal,
        self.word(),
        p
      ));
      panic!("{}", halt)
    }
  }

  pub fn ignore(&mut self, literal: &str) {
    if self.match_literal(literal) {
      self.eat(literal);
    }
  }

  pub fn word(&mut self) -> String {
    self.peek().literal()
  }

  pub fn match_literal(&mut self, s: &str) -> bool {
    self.peek().match_literal(s)
  }

  pub fn match_any_of(&mut self, literals: &[&str]) -> bool {
    literals.contains(&self.word().as_str())
  }

  pub fn maybe_delimited<K, F: FnMut(&mut Self) -> K>(
    &mut self,
    sep: Option<&str>,
    mut parser: F,
  ) -> Either<K, Vec<K>> {
    let token = self.peek();
    let infix = sep.unwrap_or(",");
    if let Token::Punct(c @ ('(' | '[' | '{'), _) = token {
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
    let mut body: Vec<Expr> = vec![];
    while !self.done() {
      let expr = self.expression();
      if let Expr::Error(h, t) = expr {
        panic!("{}; token: {}", h, t);
      } else {
        body.push(expr);
        self.ignore(";");
      };
    }
    Expr::Block(body)
  }

  pub fn expression(&mut self) -> Expr {
    let expr = self.maybe_call(Parser::maybe_binary);
    self.parsed.push(expr.clone());
    expr
  }

  pub fn maybe_call<F: FnMut(&mut Self) -> Expr>(
    &mut self,
    mut parser: F,
  ) -> Expr {
    let expr = parser(self);
    let mut token = self.peek();
    if token.match_literal("(") {
      self.call(expr)
    } else {
      expr
    }
  }

  pub fn call(&mut self, expr: Expr) -> Expr {
    Expr::Call(
      Box::new(expr.clone()),
      self.delimited(("(", ",", ")"), &mut Parser::expression),
      None,
    )
  }

  pub fn atom(&mut self) -> Expr {
    self.maybe_call(Parser::terminal)
  }

  pub fn maybe_binary(&mut self) -> Expr {
    // self.group(&mut Parser::atom)
    self.operator_assoc_l(Self::atom, 0)
  }

  fn maybe_unary(
    &mut self,
    token: Token,
    mut left: Option<Expr>,
    (this_prec, that_prec): (usize, usize),
  ) -> Expr {
    if let Some(expr) = left {
      self.operator_assoc_l(
        |p: &mut Parser| {
          (if token.match_any_of(&["=", "<-", "=<"]) {
            Expr::Assign
          } else {
            Expr::Binary
          })(
            token.clone(),
            Box::new(expr.clone()),
            Box::new(p.operator_assoc_l(Self::atom, that_prec)),
          )
        },
        this_prec,
      )
    } else {
      Expr::Unary(token, Box::new(self.atom()))
    }
  }

  pub fn operator_assoc_l<F: FnMut(&mut Self) -> Expr>(
    &mut self,
    mut parselet: F,
    this_prec: usize,
  ) -> Expr {
    let mut left = parselet(self);
    let mut token = self.peek();
    match token.clone() {
      x if x.as_operator().is_none() => left,
      Token::Operator(..) => {
        if let Some(that_prec) = token.clone().precedence() {
          if (this_prec < that_prec) {
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
    match if self.peek().match_literal("|") {
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
    self.ignore("do");
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
    self.eat("#");
    let name =
      if matches!(self.peek(), Token::Identifier(..) | Token::Meta(..)) {
        Kind(self.next())
      } else {
        Kind(Token::Empty())
      };
    let body =
      self.delimited(("{", ",", "}"), &mut |p: &mut Parser| p.binding("->"));
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
      let pattern = self.binary(
        &mut |p: &mut Self| p.binary(Self::expression, &["|"]),
        &["->", "@"],
      );
      println!("{:?}", &pattern);
      self.eat("=>");
      let branch = self.maybe_block();
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

  pub fn binary<F: FnMut(&mut Self) -> Expr>(
    &mut self,
    mut parselet: F,
    ops: &[&str],
  ) -> Expr {
    let mut left = parselet(self);
    let mut token = self.peek();
    let eqs = ["=", "<-"];
    while token.match_any_of(ops) {
      self.next();
      left = if eqs.contains(&token.literal().as_str()) {
        Expr::Assign
      } else {
        Expr::Binary
      }(token, Box::new(left), Box::new(parselet(self)));
      token = self.peek();
    }
    left
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

  pub fn indexish<F: FnMut(&mut Self) -> Expr>(
    &mut self,
    mut parser: F,
  ) -> Expr {
    let body = parser(self);
    if self.match_literal("[") {
      self.index(body)
    } else {
      body
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

  pub fn index(&mut self, body: Expr) -> Expr {
    self.eat("[");
    if self.match_literal("..") {
      self.eat("..");
      self.eat("]");
      Expr::Iter(Box::new(body), None)
    } else {
      self.indexish(|p| {
        let idx = p.expression();
        if p.match_literal("..") {
          p.eat("..");
          if p.match_literal("]") {
            p.eat("]");
            return Expr::Range(
              Box::new(body.to_owned()),
              Box::new(idx),
              None,
            );
          } else {
            let end = p.expression();
            p.eat("]");
            return Expr::Range(
              Box::new(body.to_owned()),
              Box::new(idx),
              Some(Box::new(end)),
            );
          }
        }
        p.eat("]");
        Expr::Index(Box::new(body.to_owned()), Box::new(idx))
      })
    }
  }

  fn twice<E, F: FnMut(&mut Self) -> E>(
    &mut self,
    ref mut parser: F,
  ) -> (E, E) {
    (parser(self), parser(self))
  }

  pub fn looping(&mut self) -> Expr {
    self.eat("loop");
    let (cond, body) = self.twice(|p| Box::new(p.maybe_block()));
    Expr::Loop(cond, body)
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
      if let Expr::Assign(op, b, c) = expr {
        match op.literal().as_str() {
          "<-" => ranges.push((*b, *c)),
          "=" => fixed.push((*b, *c)),
          _ =>
          /* TODO */
          {
            panic!("{:?}", p.peek())
          }
        };
      } else {
        conds.push(expr)
      }
    });
    return Expr::List(Box::new(Definition {
      item: head,
      ranges,
      fixed,
      conds,
    }));
  }

  pub fn maybe_list(&mut self) -> Expr {
    self.eat("[");
    let item = if self.match_literal("[") {
      self.maybe_list()
    } else {
      self.atom()
    };
    if self.match_literal("|") {
      self.list_comprehension(item)
    } else if self.match_literal(",") {
      let mut expr = self.delimited((",", ",", "]"), &mut Self::expression);
      expr.insert(0, item);
      self.indexish(|_| Expr::Vector(expr.to_owned()))
    } else if self.match_literal("]") {
      self.eat("]");
      self.indexish(|_| Expr::Vector(vec![item.to_owned()]))
    } else {
      // it is an error to get here! list brackets not closed out
      let pos = self.get_pos();
      self.unknown(
        format!("Unable to find closing square bracket {:?}", pos),
        Some("]"),
      )
    }
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
        self.indexish(|_| Expr::Literal(token.to_owned()))
      }
      Token::Char(..)
      | Token::Meta(..)
      | Token::Number(..)
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
      let list =
        self.delimited((&prefix, ",", twin_of(&prefix)), &mut Self::pattern);
      match prefix.as_str() {
        "[" => TokPattern::Vector(list),
        "(" => TokPattern::Tuple(list),
        "{" => TokPattern::Record(list),
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

    // match shape {
    //   Shape::Atom => Parameter {
    //     name: self.next(),
    //     kind: self.annotation(),
    //     shape,
    //     inner: vec![],
    //     reqs: vec![],
    //     pattern: None,
    //   },
    //   Shape::Tuple => todo!(),
    //   Shape::Vector => {
    //     let prefix = token.literal();
    //     self.delimited((&prefix, ",", twin_of(&prefix)), parser)
    //   }
    //   Shape::List => todo!(),
    //   Shape::Record => todo!(),
    //   Shape::Closure => todo!(),
    //   Shape::Holder => todo!(),
    //   Shape::Empty => todo!(),
    //   Shape::Unknown | _ => todo!(),
    // };

    // let (name, shape, inner, deep) = if token.is_left_punct() {
    //   let mut pram = None;
    //   let wrapped = self.delimited(
    //     (&token.to_string(), ",", twin_of(&token.to_string())),
    //     &mut |p: &mut Parser| {
    //       let tok = p.peek();
    //       if tok.is_left_punct() {
    //         let mut pr = p.parameter();
    //         pr.pattern = pram.clone();
    //         pram = Some(Rc::new(pr.clone()));
    //         pr.name
    //       } else {
    //         p.next()
    //         // tok
    //       }
    //     },
    //   );

    //   (
    //     Token::Empty(),
    //     Shape::from(token.clone()),
    //     vec![],
    //     Some(Rc::new(wrapped)),
    //   )
    // } else if token.is_identifier() {
    //   (self.next(), Shape::from(token), vec![], None)
    // } else {
    //   (token, Shape::Empty, vec![], None)
    // };
    // Parameter {
    //   name,
    //   shape,
    //   inner,
    //   kind: self.annotation(),
    //   reqs: vec![],
    //   pattern: None,
    // }
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
          self.word()
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
    let src = "1 + 2 / 3 * (4 ** 5 / 6)";
    inspect_expr(src)
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
    let src = "#{a -> b, c -> d};";
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
    let src = "[[(a, b, c)] | a <- [1, b], a + b > 0, c = 3]";
    inspect_tokens(src.clone());
    inspect_expr(src);
  }

  #[test]
  fn inspect_unary() {
    let src = "|| 3"; //"-(a) + b - c";
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
}
