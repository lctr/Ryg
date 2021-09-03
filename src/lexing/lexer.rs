use super::token::{Pos, Stream, Streaming, Token};
use std::i64;

#[derive(Debug)]
pub struct Lexer<'a> {
  src: Stream<'a>,
  current: Option<Token>,
}

impl<'a> Streaming<Token> for Lexer<'a> {
  fn peek(&mut self) -> Token {
    if let Some(t) = self.current.clone() {
      t
    } else {
      let tok = self.token();
      self.current = Some(tok.clone());
      tok
    }
  }
  fn next(&mut self) -> Token {
    let token = self.current.clone();
    self.current = None;
    if let Some(t) = token {
      t
    } else {
      self.token()
    }
  }
  fn eof(&mut self) -> bool {
    self.src.peek() == '\0'
      || self.src.peek_iter().is_none()
      || self.current == Some(Token::Eof())
  }
}

// #[allow(dead_code)]
impl<'a> Lexer<'a> {
  pub fn new(src: &str) -> Lexer {
    Lexer {
      src: Stream::new(src),
      current: None,
    }
  }

  pub fn get_pos(&mut self) -> Pos {
    self.src.pos.clone()
  }

  pub fn token(&mut self) -> Token {
    self.eat_while(char::is_whitespace);
    if self.src.eof() {
      return Token::Eof();
    };
    let ch = self.src.peek();
    match &ch {
      '~' => self.comment(),
      '"' => Token::String(self.escaped(&ch)),
      '@' | '_' => Token::Symbol(self.src.next().to_string()),
      c if is_digit(*c) => self.number(),
      c if starts_name(*c) => self.variable(),
      c if is_punct(*c) => Token::Punct(self.src.next()),
      c if is_op_char(*c) => Token::Operator(self.eat_while(is_op_char)),
      _ => {
        panic!(
          "Unable to recognize character {:?} at {:?}",
          &ch,
          self.get_pos()
        )
      }
    }
  }

  pub fn eat_while<F: FnMut(char) -> bool>(&mut self, mut pred: F) -> String {
    let mut word = String::new();
    while !self.src.eof() && pred(self.src.peek()) {
      word.push(self.src.next());
    }
    word
  }

  fn variable(&mut self) -> Token {
    let mut word = String::new();
    word.push_str(&self.eat_while(|c| is_name(&c.to_string())));
    // if is_kw(word.as_str()) {
    //   Token::Keyword(word)
    // } else {
    match word.as_str() {
      "true" | "false" => Token::Boolean(word),
      w if is_kw(w) => Token::Keyword(w.to_string()),
      _ => Token::Identifier(word),
    }
    // }
  }

  fn escaped(&mut self, end: &char) -> String {
    let mut escaped = false;
    let mut word = String::new();
    self.src.next();
    while !self.eof() {
      let c = self.src.next();
      if escaped {
        escaped = false;
        // word.push(c);
        match c {
          'b' | 't' | 'n' | 'f' | 'r' => {
            word.push_str(format!("\\\\{}", c).as_str());
            // word.push(c);
          }
          '\\' => {
            word.push('\\');
          }
          _ => {
            word.push(c);
          }
        };
      } else if &c == end {
        break;
      } else if &c == &'\\' {
        escaped = true;
      } else {
        word.push(c);
      }
    }
    word
  }

  fn comment(&mut self) -> Token {
    self.eat_while(|c| c.ne(&'\n'));
    self.token()
  }

  fn number(&mut self) -> Token {
    let mut infixed = false;
    let mut float = false;
    let mut base: u8 = 0;
    let mut setting: fn(char) -> bool = is_digit;
    let number = self.eat_while(|c| -> bool {
      match c {
        '.' => {
          if infixed {
            return false;
          } else {
            infixed = true;
            base = 10;
            return true;
          }
        }
        'b' | 'o' | 'x' => {
          if infixed {
            return false;
          } else {
            infixed = true;
            match c {
              'b' => {
                setting = is_bin;
                base = 2;
              }
              'o' => {
                setting = is_oct;
                base = 8;
              }
              'x' => {
                setting = is_hex;
                base = 16;
              }
              // this won't get reached anyway based on parent branch
              // conditions, but compiler complains
              _ => {}
            }
            return true;
          }
        }
        'e' => {
          if infixed {
            return false;
          } else {
            infixed = true;
            float = true;
            base = 10;
            return true;
          }
        }
        '+' | '-' => {
          if !float && infixed {
            return false;
          } else {
            return true;
          }
        }
        _ => setting(c),
      }
    });
    Token::Number(number, base)
  }
}

pub fn integer(word: &str, base: u32) -> i32 {
  match i64::from_str_radix(word, base) {
    Ok(n) => n as i32,
    Err(_) => 0,
  }
}

pub fn starts_name(c: char) -> bool {
  c.is_alphabetic()
}

pub fn is_name(word: &String) -> bool {
  let all = word.chars().fold(true, |a, c| {
    a && (c.is_alphanumeric() || matches!(c, '\'' | '_'))
  });
  word.starts_with(char::is_alphabetic) || all
}

pub fn is_digit(c: char) -> bool {
  matches!(c, '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0')
  // c.is_digit(36)
}

pub fn is_punct(c: char) -> bool {
  matches!(c, '(' | ')' | '[' | ']' | '|' | ';' | ',')
}

pub fn is_op_char(c: char) -> bool {
  matches!(
    c,
    '='
      | '+'
      | '-'
      | '*'
      | '/'
      | '%'
      | '^'
      | '<'
      | '>'
      | '|'
      | '&'
      | '@'
      | '!'
      | '~'
      | '?'
      | ':'
      | '\\'
      | '$'
      | '#'
  )
}

fn unicode(lexer: &mut Lexer) -> (String, i32) {
  let mut len = if lexer.src.peek() == 'u' { 4 } else { 6 };
  let w = lexer.eat_while(|_| {
    len -= 1;
    &len > &0
  });
  let n = integer(&w, 16);
  (w, n)
}

pub fn is_bin(c: char) -> bool {
  matches!(c, '0' | '1')
}

pub fn is_oct(c: char) -> bool {
  matches!(c, '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7')
}

pub fn is_hex(c: char) -> bool {
  c.is_ascii_hexdigit()
}

pub fn is_kw(s: &str) -> bool {
  matches!(
    s,
    "do"
      | "let"
      | "var"
      | "if"
      | "then"
      | "else"
      | "true"
      | "false"
      | "at"
      | "with"
      | "in"
      | "case"
      | "of"
      | "fn"
      | "is"
      | "loop"
  )
}

#[cfg(test)]

mod tests {
  use super::*;
  fn inspect_tokens(src: &str) {
    let mut lexer = Lexer::new(src);
    println!("source: {:?}\n", src);
    let mut i = 0;
    while !lexer.eof() {
      i += 1;
      println!("[{}] {:?}", i, lexer.next());
    }
    println!("...");
  }

  #[test]
  fn digits_decimal() {
    let src = "3.14";
    inspect_tokens(src);
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next(), Token::Number(String::from(src), 10));
  }

  #[test]
  fn keywords() {
    let src = "\"\\\"nts ";
    // inspect_tokens(src);
    let mut lexer = Lexer::new(src);
    println!("{:?}", lexer.peek());
    assert_eq!(lexer.next(), Token::Keyword("let".to_owned()))
  }

  #[test]
  fn lambda_call() {
    let src = "he'a (2)";
    inspect_tokens(src);
    let mut lexer = Lexer::new(src);
    [Token::Identifier(String::from("he'a")), Token::Punct('(')]
      .iter()
      .for_each(|t| assert_eq!(*t, lexer.next()));
  }
}
