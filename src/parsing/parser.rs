use crate::{
  lexing::{
    lexer::Lexer,
    token::{Pos, Streaming, Token},
  },
  parsing::expression::Expr,
};

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
    self.peek().match_literal("\0")
      || self.lexer.eof()
      || self.peek() == Token::Eof()
  }
}

#[allow(unused)]
impl<'a> Parser<'a> {
  pub fn new(src: &str) -> Parser {
    Parser {
      lexer: Lexer::new(src),
    }
  }

  pub fn get_pos(&mut self) -> Pos {
    self.lexer.get_pos()
  }
  pub fn eat(&mut self, literal: &str) {
    if self.peek().literal() == literal {
      self.next();
    } else {
      panic!(
        "Expected {:?}, but instead found {:?} at {:?}",
        literal,
        self.peek().literal(),
        self.get_pos()
      )
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

  fn match_literal(&mut self, s: &str) -> bool {
    self.peek().match_literal(s)
  }
  pub fn match_any_of(&mut self, literals: &[&str]) -> bool {
    literals.contains(&self.word().as_str())
  }

  pub fn delimited<K, F: FnMut(&mut Self) -> K>(
    &mut self,
    (prefix, infix, suffix): (&str, &str, &str),
    parser: &mut F,
  ) -> Vec<K> {
    let mut nodes: Vec<K> = vec![];
    self.eat(prefix);
    let mut first = true;
    while !self.eof() {
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
    while !self.eof() {
      body.push(self.expression());
      if !self.lexer.eof() {
        self.eat(";");
      }
    }
    Expr::Block(body)
  }

  pub fn expression(&mut self) -> Expr {
    self.callish(Parser::groupish)
  }
  pub fn atom(&mut self) -> Expr {
    self.callish(Parser::terminal)
  }
  pub fn groupish(&mut self) -> Expr {
    self.group(Parser::atom)
  }
  pub fn group<F: FnMut(&mut Self) -> Expr>(&mut self, mut parser: F) -> Expr {
    let token = self.peek();
    match token {
      Token::Operator(_) => parser(self),
      _ => self.callish(Parser::assign),
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

  pub fn lambda(&mut self) -> Expr {
    let token = self.peek();
    let name = if let Token::Identifier(t) = token {
      Some(t)
    } else {
      None
    };

    let prams = self.delimited(("|", ",", "|"), &mut |p| {
      let wrd = p.word();
      p.next();
      wrd
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
        Some(b) => b.to_owned(), //body.pop().unwrap(),
        None => panic!("what"),
      },
      _ => Expr::Block(body),
    }
  }

  pub fn callish<F: FnMut(&mut Self) -> Expr>(
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
    let args = self.delimited(("(", ",", ")"), &mut |p| p.expression());
    Expr::Call(Box::new(expr), args)
  }

  pub fn case(&mut self) -> Expr {
    self.eat("case");
    let test = self.expression();
    self.eat("of");
    self.eat("{");
    let mut deft: Expr = Expr::Literal(Token::Empty());
    let mut conds = vec![];
    let mut token = self.peek();
    while !token.match_literal("}") {
      let pattern = self.expression();
      self.eat("=>");
      let branch = if self.match_literal("{") {
        self.block()
      } else {
        self.expression()
      };
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
    mut parser: F,
    ops: &[&str],
  ) -> Expr {
    let mut left = parser(self);
    let mut token = self.peek();
    let eqs = ["=", ":=", "<-", "=:", "=<"];
    while ops.contains(&token.literal().as_str()) {
      self.next();
      left = if eqs.contains(&token.literal().as_str()) {
        Expr::Assign(token, Box::new(left), Box::new(parser(self)))
      } else {
        Expr::Binary(token, Box::new(left), Box::new(parser(self)))
      };
      token = self.peek();
    }
    left
  }

  pub fn assign(&mut self) -> Expr {
    // self.bin_op()
    self.binary(Parser::bind, &["=", "<-"])
  }
  pub fn bind(&mut self) -> Expr {
    self.binary(Parser::or, &["=<"])
  }
  pub fn or(&mut self) -> Expr {
    self.binary(Parser::and, &["||"])
  }
  pub fn and(&mut self) -> Expr {
    self.binary(Parser::bit_or, &["&&"])
  }
  pub fn bit_or(&mut self) -> Expr {
    self.binary(|p| p.xor(), &["|"])
  }
  pub fn xor(&mut self) -> Expr {
    self.binary(Parser::bit_and, &["^"])
  }
  pub fn bit_and(&mut self) -> Expr {
    self.binary(Parser::equality, &["&"])
  }
  pub fn equality(&mut self) -> Expr {
    self.binary(Parser::comparison, &["==", "!="])
  }
  pub fn comparison(&mut self) -> Expr {
    self.binary(Parser::conc, &["<", ">", "<=", ">="])
  }
  pub fn conc(&mut self) -> Expr {
    self.binary(Parser::term, &["<>", "++"])
  }
  pub fn term(&mut self) -> Expr {
    self.binary(Parser::factor, &["+", "-"])
  }
  pub fn factor(&mut self) -> Expr {
    self.binary(Parser::unary, &["*", "/", "%"])
  }
  pub fn unary(&mut self) -> Expr {
    let mut token = self.peek();
    if token.match_literal("!") || token.match_literal("-") {
      self.next();
      let right = self.atom();
      Expr::Unary(token, Box::new(right))
    } else {
      self.binary(|p| p.exponent(), &["**"])
    }
  }
  pub fn exponent(&mut self) -> Expr {
    self.binary(|p| p.atom(), &["**"])
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
  pub fn index(&mut self, body: Expr) -> Expr {
    self.eat("[");
    if self.match_literal("..") {
      self.eat("..");
      self.eat("]");
      Expr::Iter(Box::new(body))
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
          }
          let end = p.expression();
          p.eat("]");
          return Expr::Range(
            Box::new(body.to_owned()),
            Box::new(idx),
            Some(Box::new(end)),
          );
        }
        p.eat("]");
        Expr::Index(Box::new(body.to_owned()), Box::new(idx))
      })
    }
  }
  pub fn terminal(&mut self) -> Expr {
    let token = self.peek();
    match token {
      Token::Punct('(') => {
        self.next();
        let expr = self.expression();
        if let Token::Punct(',') = self.peek() {
          let mut rest =
            self.delimited((",", ",", ")"), &mut Parser::expression);
          rest.insert(0, expr);
          Expr::Tuple(rest)
        } else {
          self.eat(")");
          expr
        }
      }
      Token::Punct('{') => self.block(),
      Token::Punct('|') => self.lambda(),
      Token::Operator(x) => match x.as_str() {
        "!" | "-" => self.unary(),
        "|" => self.lambda(),
        _ => {
          panic!("Unable to parse Operator {:?} at {:?}", x, self.get_pos())
        }
      },
      Token::Keyword(k) => match k.as_str() {
        "if" => self.conditional(),
        "let" => self.locals(),
        "do" => {
          self.next();
          self.block()
        }
        "case" => self.case(),
        _ => {
          panic!("Unable to parse Keyword {} at {:?}", k, self.get_pos())
        }
      },
      Token::Punct('[') => {
        debug_assert_eq!(self.word(), "[");
        let expr = self.delimited(("[", ",", "]"), &mut Parser::expression);
        Expr::Vector(expr)
      }
      Token::String(_) | Token::Identifier(_) => {
        self.next();
        self.indexish(|_| Expr::Literal(token.to_owned()))
      }
      Token::Number(_, _) | Token::Symbol(_) | Token::Boolean(_) => {
        self.next();
        Expr::Literal(token)
      }
      _ => {
        panic!("Unable to parse {:?} at {:?}", token, self.get_pos());
      }
    }
  }

  pub fn variable_expr(ref mut p: &mut Parser, args: Vec<Defn>) {}
  pub fn locals(&mut self) -> Expr {
    self.eat("let");
    self.ignore("{");
    let mut args = vec![];
    let stop =
      &mut |p: &mut Parser| p.match_literal("in") || p.match_literal("}");
    let mut token = self.peek();
    while !stop(self) {
      if let Token::Identifier(_) = token {
        args.push(self.binding());
        self.ignore(",");
        if stop(self) {
          break;
        }
        token = self.peek();
      } else {
        panic!(
          "Expecting a variable name, but instead got {:?} at {:?}",
          token,
          self.get_pos()
        )
      }
    }
    self.ignore("}");
    self.eat("in");
    Expr::Variable(args, Box::new(self.expression()))
  }
  fn binding(&mut self) -> Defn {
    let mut def = None;
    let mut name = String::new();
    let mut kind = None;
    if let Defn::Parameter(n, k, _) = self.parameter() {
      name = n;
      kind = k;
    }
    if self.match_literal("=") {
      self.next();
      def = Some(self.expression());
    }
    Defn::Binding(name, def, kind)
  }
  fn parameter(&mut self) -> Defn {
    let token = self.peek();
    let w = token.literal();
    let mut shape = None;
    if ["(", "[", "{"].contains(&w.as_str()) {
      shape = Some((
        ["_", &w].join(""),
        self.delimited((&w, ",", complement(&w)), &mut |p| p.parameter()),
      ))
    }
    let name = self.word();
    let mut kind = String::new();
    self.next();
    if [":", "::"].contains(&self.peek().literal().as_str()) {
      self.next();
      if let Token::Identifier(v) = self.peek() {
        kind = v;
        self.next();
      } else {
        panic!(
          "Expected a type for this type annotation, but instead got {:?} at {:?}",
          self.peek(),
          self.get_pos()
        )
      }
    }
    // (name, kind, shape)
    Defn::Parameter(name, Some(kind), shape)
  }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Defn {
  Def(Option<Expr>),
  Name(String),
  // type
  Kind(Option<String>),
  // for destructuring
  Shape(Option<Vec<Defn>>),
  Argmnts(Expr, Option<Vec<Defn>>),
  Binding(String, Option<Expr>, Option<String>),
  Parameter(String, Option<String>, Option<(String, Vec<Defn>)>),
}

fn complement(s: &str) -> &str {
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

/* POTENTIAL REFACTORING OF BIN_OP CHAIN
 * DOES NOT TAKE UNARIES INTO ACCOUNT!!
 *   pub fn bin_op(&mut self) -> Expr {
 *     vec![
 *       vec!["=", "<-"],
 *       vec!["||"],
 *       vec!["&&"],
 *       vec!["|"],
 *       vec!["^"],
 *       vec!["&"],
 *       vec!["==", "!="],
 *       vec!["<", ">", "<=", ">="],
 *       vec!["<>", "++"],
 *       vec!["+", "-"],
 *       vec!["*", "/", "%"],
 *     ]
 *     .iter()
 *     .rev()
 *     .fold(self.binary(|p| p.atom(), vec!["**"]), &mut
 * |acc: Expr,
 * x: &Vec<       &str,
 *     >| {
 *       self.binary(|_| acc.clone(), x.to_vec())
 *     })
 *   }
 */

pub fn get_ast(src: &str) -> Expr {
  let mut p = Parser::new(src);
  p.parse()
}

#[cfg(test)]
mod tests {
  use super::*;
  fn inspect_tokens(src: &str) {
    let mut parser = Parser::new(src);
    let mut i = 0;
    println!("source: {:?}", src);
    while !parser.eof() {
      i += 1;
      println!("[{}] {:?}", i, parser.next());
    }
    println!("[{}] {:?}", i + 1, parser.next())
  }
  fn parse(src: &str) {
    let mut parser = Parser::new(src);
    println!("source: {}", src);
    let ast = parser.parse();
    println!("{:#?}", ast)
  }

  #[test]
  fn inspect_decimal() {
    let src = "3.14 + 6";
    let mut parser = Parser::new(src);
    println!("{:#?}", parser.assign())
  }

  #[test]
  fn inspect_vector() {
    let src = "[2, 3];";
    parse(src);
  }

  #[test]
  fn inspect_lambda() {
    let src = "(|a, b| a + 19)()";
    inspect_tokens(src);
    parse(src);
  }

  #[test]
  fn test_tuple() {
    let src = "(-i,o, 5, 3 + 5/6)"; //"let x = 4 in x + 1;";
    parse(src);
  }

  #[test]
  fn inspect_let() {
    let src = "let a = 1, b = 2 in a + b == 3;";
    parse(src)
  }
}
