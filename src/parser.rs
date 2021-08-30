use super::lexer::*;
use crate::ast::*;
use crate::token::{Pos, Streaming, Token};
use std::fmt;

// use super::halt::Halt;

// enum ParserError {
//   Syntax(String, Pos),
//   Unexpected(String, Pos),
// }

// impl fmt::Display for ParserError {
//   fn fmt(&mut self, f: &mut fmt::Formatter) -> fmt::Formatter {
//     match self {}
//   }
// }

#[derive(Clone, Debug, PartialEq)]

pub enum Expr {
  Literal(Token),
  Unary(Token, Box<Expr>),
  Binary(Token, Box<Expr>, Box<Expr>),
  Assign(Token, Box<Expr>, Box<Expr>),
  Block(Vec<Expr>),
  Vector(Vec<Expr>),
  Lambda(Option<String>, Vec<String>, Box<Expr>),
  Conditional(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
  Let(Vec<Expr>, Box<Expr>),
  Call(Box<Expr>, Vec<Expr>),
  Case(Box<Expr>, Vec<Expr>, Box<Expr>),
  Tuple(Vec<Expr>),
}

// impl fmt::Debug for Expr {
//   fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//     match self {
//       Expr::Literal(t) => write!(f, "{:?}", t)
//       Expr::Unary(t, e) => {}
//       Expr::Binary(a, b, c) => {}
//       Expr::Assign(a, b, c) => {}
//       Expr::Block(a) => {}
//       Expr::Call(a, b) => {}
//       Expr::Case(a, b, c) => {}
//       Expr::Conditional(a, b, c) => {}
//       Expr::Lambda(a, b, c) => {}
//       Expr::Let(a, b) => {}
//       Expr::Vector(a) => {}
//       Expr::Tuple(_) => {}
//     }
//     // write!(f, "{:?}", self.)
//   }
// }

impl Expr {
  pub fn new() {}
  pub fn from(expr: Expr) {
    match expr {
      Expr::Literal(t) => {}
      Expr::Unary(t, e) => {}
      Expr::Binary(a, b, c) => {}
      Expr::Assign(a, b, c) => {}
      Expr::Block(a) => {}
      Expr::Call(a, b) => {}
      Expr::Case(a, b, c) => {}
      Expr::Conditional(a, b, c) => {}
      Expr::Lambda(a, b, c) => {}
      Expr::Let(a, b) => {}
      Expr::Vector(a) => {}
      Expr::Tuple(_) => {}
    }
  }
}

pub struct Parser<'a> {
  lexer: Lexer<'a>,
}

impl<'a> Streaming<Token> for Parser<'a> {
  fn peek(&mut self) -> Token {
    self.lexer.peek()
  }

  fn next(&mut self) -> Token {
    self.lexer.next()
  }

  fn eof(&mut self) -> bool {
    self.peek().match_literal("\0") || self.lexer.eof()
  }
}

impl<'a> Parser<'a> {
  pub fn new(src: &str) -> Parser {
    Parser {
      lexer: Lexer::new(src),
    }
  }

  pub fn pos(&mut self) -> Pos {
    self.lexer.get_pos()
  }
  pub fn eat(&mut self, literal: &str) {
    if self.peek().literal() == literal {
      self.next();
    }
  }
  pub fn ignore(&mut self, literal: &str) {
    if self.match_literal(literal) {
      self.eat(literal)
    }
  }
  pub fn word(&mut self) -> String {
    self.peek().literal()
  }

  pub fn match_literal(&mut self, s: &str) -> bool {
    self.peek().match_literal(s)
  }

  pub fn delimited<K, F: FnMut(&mut Self) -> K>(
    &mut self,
    (prefix, infix, suffix): (&str, &str, &str),
    run: &mut F,
  ) -> Vec<K> {
    let mut nodes: Vec<K> = vec![];
    self.eat(prefix);
    let mut first = true;
    nodes.push(run(self));
    while !self.eof() {
      if self.peek().match_literal(&suffix) {
        break;
      };
      if first {
        first = false;
      } else {
        self.eat(infix);
      };
      if self.peek().match_literal(&suffix) {
        break;
      };
      nodes.push(run(self));
    }
    self.eat(suffix);
    nodes
  }

  pub fn parse(&mut self) -> Expr {
    let mut body: Vec<Expr> = vec![];
    while !self.eof() {
      body.push(self.expression());
      if !self.lexer.eof() {
        self.eat(";");
      }
    }
    Expr::Block(body)
  }

  pub fn expression(&mut self) -> Expr {
    self.callish(|p| p.group(|q| q.atom()))
  }

  pub fn atom(&mut self) -> Expr {
    self.callish(|p| terminal(p))
  }

  pub fn conditional(&mut self) -> Expr {
    self.eat("if");
    let cond = self.expression();
    self.eat("then");
    let then = self.expression();
    let mut deft = None;
    if self.match_literal("else") {
      self.eat("else");
      deft = Some(Box::new(self.expression()))
    } else {
      deft = None
    };
    Expr::Conditional(Box::new(cond), Box::new(then), deft)
  }

  pub fn lambda(&mut self) -> Expr {
    let token = self.peek();
    let mut name = None;
    match token {
      Token::Variable(v) => {
        name = Some(v);
      }
      _ => {}
    }

    let prams = self.delimited(("|", ",", "|"), &mut |p| {
      let name = p.word();
      p.next();
      name
    });

    let body = if self.match_literal("{") {
      self.block()
    } else {
      self.expression()
    };

    Expr::Lambda(name, prams, Box::new(body))
  }

  pub fn block(&mut self) -> Expr {
    let mut body = self.delimited(("{", ";", "}"), &mut |p| p.expression());

    match body.len() {
      0 => Expr::Literal(Token::Boolean("false".to_string())),
      1 => match body.get(0) {
        Some(b) => body.pop().unwrap(),
        None => panic!("what"),
      },
      _ => Expr::Block(body),
    }
  }

  pub fn group<F: FnMut(&mut Self) -> Expr>(&mut self, mut parser: F) -> Expr {
    let token = self.peek();
    match token {
      Token::Operator(_) => parser(self),
      _ => self.callish(|p| p.assign()),
    }
  }

  pub fn callish<F: FnMut(&mut Self) -> Expr>(&mut self, mut parser: F) -> Expr {
    let expr = parser(self);
    let mut token = self.peek();
    if token.match_literal("(") {
      self.call(expr)
    } else {
      expr
    }
  }

  pub fn call(&mut self, expr: Expr) -> Expr {
    let args = self.delimited(("(", ",", ")"), &mut |p| p.expression());
    Expr::Call(Box::new(expr), args)
  }

  pub fn binary<F: FnMut(&mut Self) -> Expr>(&mut self, mut parser: F, ops: Vec<&str>) -> Expr {
    let mut left = parser(self);
    let mut token = self.peek();
    while ops.contains(&token.literal().as_str()) {
      self.next();
      left = Expr::Binary(token, Box::new(left), Box::new(parser(self)));
      token = self.peek();
    }
    left
  }

  pub fn assign(&mut self) -> Expr {
    // self.bin_op()
    self.binary(|p| p.bind(), vec!["=", "<-"])
  }
  pub fn bind(&mut self) -> Expr {
    self.binary(|p| p.or(), vec!["=<"])
  }
  pub fn or(&mut self) -> Expr {
    self.binary(|p| p.and(), vec!["||"])
  }
  pub fn and(&mut self) -> Expr {
    self.binary(|p| p.bit_or(), vec!["&&"])
  }
  pub fn bit_or(&mut self) -> Expr {
    self.binary(|p| p.xor(), vec!["|"])
  }
  pub fn xor(&mut self) -> Expr {
    self.binary(|p| p.bit_and(), vec!["^"])
  }
  pub fn bit_and(&mut self) -> Expr {
    self.binary(|p| p.equality(), vec!["&"])
  }
  pub fn equality(&mut self) -> Expr {
    self.binary(|p| p.comparison(), vec!["==", "!="])
  }
  pub fn comparison(&mut self) -> Expr {
    self.binary(|p| p.conc(), vec!["<", ">", "<=", ">="])
  }
  pub fn conc(&mut self) -> Expr {
    self.binary(|p| p.term(), vec!["<>", "++"])
  }
  pub fn term(&mut self) -> Expr {
    self.binary(|p| p.factor(), vec!["+", "-"])
  }
  pub fn factor(&mut self) -> Expr {
    self.binary(|p| p.unary(), vec!["*", "/", "%"])
  }
  pub fn unary(&mut self) -> Expr {
    let mut token = self.peek();
    if token.match_literal("!") || token.match_literal("-") {
      self.next();
      let right = self.atom();
      Expr::Unary(token, Box::new(right))
    } else {
      self.binary(|p| p.exponent(), vec!["**"])
    }
  }
  pub fn exponent(&mut self) -> Expr {
    self.binary(|p| p.atom(), vec!["**"])
  }

  //   pub fn bin_op(&mut self) -> Expr {
  //     vec![
  //       vec!["=", "<-"],
  //       vec!["||"],
  //       vec!["&&"],
  //       vec!["|"],
  //       vec!["^"],
  //       vec!["&"],
  //       vec!["==", "!="],
  //       vec!["<", ">", "<=", ">="],
  //       vec!["<>", "++"],
  //       vec!["+", "-"],
  //       vec!["*", "/", "%"],
  //     ]
  //     .iter()
  //     .rev()
  //     .fold(self.binary(|p| p.atom(), vec!["**"]), &mut |acc: Expr,
  //                                                        x: &Vec<
  //       &str,
  //     >| {
  //       self.binary(|_| acc.clone(), x.to_vec())
  //     })
  //   }
}

fn terminal<'a>(ref mut parser: &mut Parser<'a>) -> Expr {
  let token = parser.peek();
  match token {
    Token::Punct('(') => {
      parser.next();
      let expr = parser.expression();
      if parser.match_literal(",") {
        let rest = parser.delimited((",", ",", ")"), &mut |p| p.expression());
        Expr::Tuple(rest)
      } else {
        parser.eat(")");
        expr
      }
    }
    Token::Punct('{') => parser.block(),
    Token::Operator(x) => match x.as_str() {
      "!" | "-" => parser.unary(),
      "|" => parser.lambda(),
      _ => panic!("Unable to parse operator {:?} at {:?}", x, parser.pos()),
    },
    Token::Keyword(k) => match k.as_str() {
      "if" => parser.conditional(),
      _ => {
        panic!("")
      }
    },
    Token::String(_)
    | Token::Number(_, _)
    | Token::Variable(_)
    | Token::Symbol(_)
    | Token::Boolean(_) => {
      parser.next();
      Expr::Literal(token)
    }
    _ => panic!("unexpected EOF!"),
  }
}

#[cfg(test)]

mod tests {

  use crate::parser::terminal;

  // use super::Lexer;
  use super::{Parser, Streaming};

  // use super::Token;
  fn inspect_tokens(src: &str) {
    let mut parser = Parser::new(src);
    let mut i = 0;
    println!("source: {:?}", src);
    while !parser.eof() {
      i += 1;
      println!("[{}] {:?}", i, parser.next());
    }
  }

  #[test]
  fn test_decimal() {
    let src = "3.14 + 6";
    inspect_tokens(&src);
    let mut parser = Parser::new(src);
    println!("{:#?}", parser.assign())
  }

  #[test]
  fn test_vector() {
    let src = "2 + 4 / 0x55";
    let mut parser = Parser::new(src);
    println!("source: {}", src);
    let ast = parser.parse();
    println!("{:#?}", ast)
  }
}
