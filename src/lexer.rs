use super::token::Token;
use std::{fmt, i64, iter::Peekable, str::Chars};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos {
  line: usize,
  col: usize,
}

impl Pos {
  pub fn new(lexer: &Lexer) -> Pos {
    let line = &lexer.line;
    let col = &lexer.col;
    Pos {
      line: *line,
      col: *col,
    }
  }
}

impl fmt::Display for Pos {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}:{}", self.line, self.col)
  }
}

#[derive(Debug)]
pub struct Lexer<'a> {
  pub pos: usize,
  pub line: usize,
  pub col: usize,
  chars: Peekable<Chars<'a>>,
  current: Token,
}

impl<'a> Lexer<'a> {
  pub fn new(src: &str) -> Lexer {
    Lexer {
      chars: src.trim_end().chars().peekable(),
      pos: 0,
      line: 1,
      col: 0,
      current: Token::Empty(),
    }
  }

  pub fn get_pos(&mut self) -> Pos {
    Pos::new(self)
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
    self.chars.peek() == None || Token::Eof().eq(&self.current) && self.pos > 0
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

    let ch = *self.chars.peek().unwrap_or(&'\0');

    match &ch {
      '\0' => Token::Eof(),
      '~' => {
        self.comment();
        self.next_token()
      }
      '"' => Token::String(self.escaped(&ch)),

      '(' | ')' | '[' | ']' | '{' | '}' | '|' | ',' => Token::Punct(self.chars.next().unwrap()),

      '+' | '-' | '*' | '/' | '%' | '^' | '<' | '>' | '&' | '@' | '!' | '?' | ':' | '\\' | '$'
      | '#' => Token::Operator(self.eat_while(is_op_char)),

      '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => self.number(),

      '\0' => Token::Eof(),

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
  // turns out there's a built-in function for this
  // `take_while` ! current impl left for edu purposes
  pub fn eat_while<F>(&mut self, mut pred: F) -> String
  where
    F: FnMut(char) -> bool,
  {
    let mut word = String::new();
    while !self.eof() && self.chars.peek().map_or(false, |c| pred(*c)) {
      word.push(self.chars.next().unwrap());
      self.pos += 1;
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
    self.chars.next();

    while !self.eof() {
      let c = self.chars.next().unwrap();
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

  fn unicode(&mut self) {
    let len = if self.chars.peek().unwrap() == &'u' {
      4
    } else {
      6
    };
    let w = self.eat_while(|_| --len > 0);
    let n = integer(&w, 16);
  }

  fn comment(&mut self) {
    while let Some(&c) = self.chars.peek() {
      if c.ne(&'\n') {
        self.pos += 1;
        self.col += 1;
        self.chars.next();
      } else {
        self.line += 1;
        self.col = 0;
        break;
      }
    }
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

fn parse_number<K>(number: String, base: u8) {
  if base > 0 {
    // integer(&number, base.into())

    // Token::Int(
    //   String::from(&number),
    //   integer(number.trim_start_matches("0x"), base.into()),
    // )
  } else {
    // floating(&number)

    // Token::Number(String::from(&number), floating(&number))
  }
}

fn integer(word: &str, base: u32) -> i32 {
  match i64::from_str_radix(word, base) {
    Ok(n) => n as i32,
    Err(_) => 0,
  }
}

fn floating(word: &String) -> f64 {
  match word.parse::<f64>() {
    Ok(n) => n,
    Err(_) => 0.0,
  }
}

fn is_digit(c: char) -> bool {
  c.is_digit(36)
}

fn is_op_char(c: char) -> bool {
  match c {
    '+' | '-' | '*' | '/' | '%' | '^' | '<' | '>' | '|' | '&' | '@' | '!' | '~' | '?' | ':'
    | '\\' | '$' | '#' => true,
    _ => false,
  }
}

fn is_bin(c: char) -> bool {
  match c {
    '0' | '1' => true,
    _ => false,
  }
}

fn is_oct(c: char) -> bool {
  match c {
    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' => true,
    _ => false,
  }
}

fn is_hex(c: char) -> bool {
  c.is_ascii_hexdigit()
}

fn is_kw(s: &str) -> bool {
  match s {
    "do" | "let" | "var" | "if" | "then" | "else" | "true" | "false" | "at" | "with" | "in"
    | "case" | "of" | "fn" | "is" | "loop" => true,
    _ => false,
  }
}
