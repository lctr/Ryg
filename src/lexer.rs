use super::token::{Pos, Stream, Token};
use std::i64;

// TODO
// pub enum LexerError {
//   Unknown(String, Pos),
// }

#[derive(Debug)]
pub struct Lexer<'a> {
  src: Stream<'a>,
  current: Token,
}

#[allow(dead_code)]
impl<'a> Lexer<'a> {
  pub fn new(src: &str) -> Lexer {
    Lexer {
      src: Stream::new(src),
      current: Token::Empty(),
    }
  }

  pub fn get_pos(&mut self) -> Pos {
    self.src.pos.clone()
  }

  pub fn peek(&mut self) -> Token {
    let t = &self.current;
    return if *t == Token::Empty() {
      self.current = self.next_token();
      self.current.to_owned()
    } else {
      t.to_owned()
    };
  }

  pub fn eof(&mut self) -> bool {
    self.src.peek() == &'\0'
  }

  pub fn next(&mut self) -> Token {
    let token = self.current.clone();
    self.current = Token::Empty();
    if token == Token::Empty() {
      self.next_token()
    } else {
      token
    }
  }

  pub fn next_token(&mut self) -> Token {
    self.eat_while(char::is_whitespace);
    if self.eof() {
      return self.current.to_owned();
    }
    let ch = *self.src.peek();
    match &ch {
      '\0' => Token::Eof(),
      '~' => {
        self.comment();
        self.next_token()
      }
      '"' => Token::String(self.escaped(&ch)),

      '(' | ')' | '[' | ']' | '{' | '}' | ',' => Token::Punct(self.src.next()),

      '+' | '-' | '*' | '/' | '%' | '^' | '=' | '<' | '>' | '&' | '@' | '!' | '?' | ':' | '\\'
      | '|' | '$' | '#' => Token::Operator(self.eat_while(is_op_char)),

      '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => self.number(),
      _ => {
        if (ch).is_ascii_alphabetic() {
          return self.variable();
        } else {
          panic!(
            "Unable to recognize character {:?} at {:#?}",
            &ch,
            self.get_pos()
          )
        }
      }
    }
  }
  pub fn eat_while<F: FnMut(char) -> bool>(&mut self, mut pred: F) -> String {
    let mut word = String::new();
    while !self.eof() && self.src.peek_iter().map_or(false, |c| pred(*c)) {
      word.push(self.src.next());
    }
    word
  }

  fn variable(&mut self) -> Token {
    let word = self.eat_while(|c| c.is_alphanumeric() || c == '\'');
    // let w = word.as_str();
    match word.as_str() {
      "true" | "false" => Token::Boolean(word),
      "do" | "let" | "case" | "of" | "var" | "if" | "then" | "else" => Token::Keyword(word),
      _ => Token::Variable(word),
    }
  }

  fn escaped(&mut self, end: &char) -> String {
    let mut escaped = false;
    let mut word = String::new();
    self.src.next();

    while !self.eof() {
      let c = self.src.next();
      // println!("{:?}", &c);
      if escaped {
        escaped = false;
        // word.push(c);
        match c {
          'b' | 't' | 'n' | 'f' | 'r' => {
            &word.push('\\');
            &word.push(c);
          }
          _ => {
            &word.push(c);
            continue;
          }
        }
        continue;
      } else if c == *end {
        break;
      } else if c == '\\' {
        escaped = true;
      } else {
        &word.push(c);
      }
    }
    word
  }

  fn unicode(&mut self) -> String {
    let len = if self.src.peek() == &'u' { 4 } else { 6 };
    let w = self.eat_while(|_| --len > 0);
    let n = integer(&w, 16);
    w
  }

  fn comment(&mut self) {
    self.eat_while(|c| c.ne(&'\n'));
  }

  fn number(&mut self) -> Token {
    let mut infixed = false;
    let mut float = false;
    let mut base: u8 = 0;
    let mut digit: fn(char) -> bool = is_digit;
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
                digit = is_bin;
                base = 2;
              }
              'o' => {
                digit = is_oct;
                base = 8;
              }
              'x' => {
                digit = is_hex;
                base = 16;
              }
              _ => {} // this won't get reached anyway, but the compiler is whining
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
        _ => digit(c),
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

pub fn floating(word: &String) -> f64 {
  match word.parse::<f64>() {
    Ok(n) => n,
    Err(_) => 0.0,
  }
}

pub fn is_digit(c: char) -> bool {
  c.is_digit(36)
}

pub fn is_punct(c: char) -> bool {
  match c {
    '(' | ')' | '[' | ']' | ';' | ',' => true,
    _ => false,
  }
}

pub fn is_op_char(c: char) -> bool {
  match c {
    '=' | '+' | '-' | '*' | '/' | '%' | '^' | '<' | '>' | '|' | '&' | '@' | '!' | '~' | '?'
    | ':' | '\\' | '$' | '#' => true,
    _ => false,
  }
}

pub fn is_bin(c: char) -> bool {
  match c {
    '0' | '1' => true,
    _ => false,
  }
}

pub fn is_oct(c: char) -> bool {
  match c {
    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' => true,
    _ => false,
  }
}

pub fn is_hex(c: char) -> bool {
  c.is_ascii_hexdigit()
}

pub fn is_kw(s: &str) -> bool {
  match s {
    "do" | "let" | "var" | "if" | "then" | "else" | "true" | "false" | "at" | "with" | "in"
    | "case" | "of" | "fn" | "is" | "loop" => true,
    _ => false,
  }
}

#[cfg(test)]
mod tests {
  use super::Lexer;
  use super::Token;
  fn inspect_tokens(src: &str) {
    let mut lexer = Lexer::new(src);
    println!("source: {:?}\n", src);
    let mut i = 0;
    while !lexer.eof() {
      i += 1;
      println!("[{}] {:?}", i, lexer.next());
    }
    println!("");
  }
  #[test]
  fn digits_decimal() {
    let src = "3.14";
    inspect_tokens(src);
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next(), Token::Number(String::from(src), 10));
  }
  #[test]
  fn lambda_call() {
    let src = "say'hi(\"hello!\")";
    inspect_tokens(src);
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next(), Token::Variable(String::from("say'hi")))
  }
}
