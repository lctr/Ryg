use std::i64;

use crate::util::state::{Pos, StreamState, Streaming};

use super::token::{CharStream, Token};

#[derive(Debug)]
pub struct Lexer<'a> {
  src: CharStream<'a>,
  current: Option<Token>,
  queue: Vec<Token>,
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
  fn done(&mut self) -> bool {
    if let Some(t) = self.current.clone() {
      t.is_eof() || self.src.done()
    } else {
      self.src.peek().is_none()
    }
  }
  fn get_pos(&mut self) -> Pos {
    self.src.pos.clone()
  }
}

#[allow(unused)]
impl<'a> Lexer<'a> {
  pub fn new(src: &str) -> Lexer {
    Lexer {
      src: CharStream::new(src),
      current: None,
      queue: vec![],
    }
  }

  pub fn token(&mut self) -> Token {
    if let Some(tok) = self.queue.pop() {
      return tok;
    };
    self.eat_while(char::is_whitespace);
    let ch = self.src.peek();
    if self.src.done() {
      return Token::Eof(self.get_pos());
    };
    let pos = self.get_pos();
    if let Some(c) = &ch {
      match c {
        '~' => self.comment(),
        '"' => Token::String(self.escaped(&c), pos),
        '\'' => self.character(),
        x @ ('@' | '_' | '`') => {
          self.src.next();
          Token::Symbol(x.to_string(), pos)
        }
        x if is_digit(*x) => self.number(),
        x if starts_name(*x) => self.variable(),
        x if is_punct(*x) => self.punct(*c, pos),
        x if is_op_char(*x) => {
          self.operator(*x, pos)
          // Token::Operator(self.eat_while(is_op_char), pos)
        }
        _ => Token::Invalid(
          format!("Unable to recognize character {:?}", &ch),
          self.get_pos(),
        ),
      }
    } else {
      Token::Invalid(format!("No more characters were found"), pos)
    }
  }

  fn punct(&mut self, ch: char, pos: Pos) -> Token {
    self.src.next();
    let p = self.get_pos();
    if let Some('|') = self.src.peek() {
      self.queue.push(Token::Punct('|', p));
      self.src.next();
    };
    Token::Punct(ch, pos)
  }

  fn operator(&mut self, ch: char, pos: Pos) -> Token {
    let op = self.eat_while(is_op_char);
    // let mut vert = false;
    // let mut v_count = 0;
    // self.eat_while(|c| {
    //   (if c == '|' {
    //     vert = true; v_count += 1; v_count < 2
    //   } else { if vert { false } else { true  }}) && is_op_char(c)
    // });
    match op.as_str() {
      "|" => match self.src.peek() {
        Some(pt) if is_punct(pt) => {
          let pos2 = self.get_pos();
          // self.queue.push(Token::Punct(pt, pos2));
          // self.src.next();
          Token::Punct('|', pos)
        }
        _ => Token::Operator(op, pos),
      },
      wrd @ ("||:" | "||.") => {
        // common: || is first
        let rop = wrd.chars().nth(2).unwrap();
        let p2 = self.get_pos();
        self.queue.push(Token::Punct('|', p2.clone()));
        self.queue.push(Token::Operator(rop.to_string(), {
          let mut p = p2;
          p.next(&rop);
          p
        }));
        Token::Punct('|', pos)
      }
      wrd @ ("|:" | "|.") => {
        // common: | is first
        let rop = wrd.chars().nth(wrd.len() - 1).unwrap();
        // fix pos later
        let p2 = self.get_pos();
        self.queue.push(Token::Operator(rop.to_string(), p2));
        Token::Punct('|', pos)
      }
      _ => Token::Operator(op, pos),
    }
  }

  pub fn eat_while<F: FnMut(char) -> bool>(&mut self, mut pred: F) -> String {
    let mut word = String::new();
    while !self.src.done() && pred(self.src.peek().unwrap()) {
      word.push(self.src.next().unwrap());
    }
    word
  }

  fn variable(&mut self) -> Token {
    let mut word = String::new();
    let pos = self.get_pos();
    word.push_str(&self.eat_while(|c| is_name(&c.to_string())));
    // if is_kw(word.as_str()) {
    //   Token::Keyword(word)
    // } else {
    match word.as_str() {
      "true" | "false" => Token::Boolean(word, pos),
      w if is_kw(w) => Token::Keyword(w.to_string(), pos),
      w if is_built_in(w) => Token::Meta(w.to_string(), pos),
      _ => Token::Identifier(word, pos),
    }
    // }
  }

  fn character(&mut self) -> Token {
    let pos = self.get_pos();
    let mut escaped = false;
    let mut chr: char = self.src.peek().map_or_else(|| '\0', |c| c);
    self.src.next();
    let mut e_ct = 0;
    let wrd = self.eat_while(|c| {
      let ctn = match (escaped, c) {
        (false, '\\') => {
          e_ct = e_ct + 1;
          escaped = true;
          true
        }
        (true, es) => {
          chr = match es {
            '\n' | 'n' => '\n',
            '\r' => '\r',
            '\t' => '\t',
            '\\' => '\\',
            '\'' => '\'',
            '\"' => '\"',
            '\0' => '\0',
            _ => '\0',
          };
          false
        }
        (false, e) => {
          if e_ct == 0 {
            chr = e
          };
          false
        }
      };
      ctn
    });
    let cx = self.src.next();
    let pos = self.get_pos();
    if e_ct > 2 || !matches!(self.src.next(), Some('\'')) {
      let mut mischar = chr.to_string();
      if let Some(cr) = cx {
        mischar.push(cr);
      }
      loop {
        match self.src.next() {
          Some(ct @ '\'') => {
            mischar.push(ct);
            break;
          }
          Some(c3) => {
            mischar.push(c3);
          }
          None => break,
        }
      }
      // println!("{} <=> {:?}", &wrd, &wrd);
      Token::Invalid(
        format!("The input {:?}{:?} is not a valid character", chr, mischar),
        pos,
      )
    } else {
      Token::Char(chr, escaped, pos)
    }
  }

  fn escaped(&mut self, end: &char) -> String {
    let mut escaped = false;
    let mut word = String::new();
    self.src.next();
    while !self.done() {
      if let Some(c) = self.src.next() {
        if escaped {
          escaped = false;
          // word.push(c);
          match c {
            'b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' => {
              word.push_str(format!("\\\\{}", c).as_str());
              // word.push(c);
            }
            // TODO: parsing unicode escapes
            // 'u' => unicode(self).1 as ,
            '\\' => {
              word.push('\\');
            }
            '\0' => { /* null grapheme */ }
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
    }
    word
  }

  fn comment(&mut self) -> Token {
    self.eat_while(|c| c.ne(&'\n'));
    self.token()
  }

  fn number(&mut self) -> Token {
    let pos = self.get_pos();
    let mut infixed = false;
    let mut base: u8 = 0;
    let mut setting: fn(char) -> bool = is_digit;
    let mut seps = 0;
    let mut dot_ct = 0;
    let number = self.eat_while(|c| -> bool {
      match c {
        '_' => true,
        '.' => {
          dot_ct = dot_ct + 1;
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
            base = 10;
            return true;
          }
        }
        '+' | '-' => 10 == base || !infixed,
        _ => setting(c),
      }
    });
    if dot_ct == 2 {
      let mut pos2 = pos.clone();
      let num = Token::Number(
        number.replace('_', "").trim_end_matches('.').to_string(),
        0,
        pos,
      );
      let should_be_dot = self.src.peek();
      if let Some('.') = self.src.peek() {
        pos2.next(&'.');
        self.src.next();
        if let Some('.') = self.src.peek() {
          self.src.next();
          self.queue.push(Token::Operator("...".to_string(), pos2));
        } else {
          self.queue.push(Token::Operator("..".to_string(), pos2));
        }
        self.src.pos.next(&'.');
        num
      } else {
        Token::Invalid(format!("Invalid sequence! Expected a second `.`, after '{}', but instead got {:?}", num, should_be_dot), self.get_pos())
      }
    } else {
      Token::Number(number.replace('_', ""), base, pos)
    }
  }
}

pub fn integer(word: &str, base: u32) -> i32 {
  match i64::from_str_radix(word, base) {
    Ok(n) => n as i32,
    Err(_) => 0,
  }
}

pub fn starts_name(c: char) -> bool {
  c.is_alphabetic() && !matches!(c, '_' | '@' | '\'' | '0'..='9')
}

pub fn is_name(word: &String) -> bool {
  let all = word.chars().fold(true, |a, c| {
    a && (c.is_alphanumeric() || matches!(c, '\'' | '_'))
  });
  word.starts_with(char::is_alphabetic) || all
}

pub fn is_digit(c: char) -> bool {
  matches!(c, '0'..='9')
}

pub fn is_punct(c: char) -> bool {
  matches!(c, '(' | ')' | '{' | '}' | '[' | ']' | ';' | ',')
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
      | '.'
  )
}

fn unicode(lexer: &mut Lexer) -> (String, i32) {
  let mut len = if !lexer.done() && lexer.src.peek().unwrap() == 'u' {
    4
  } else {
    6
  };
  let w = lexer.eat_while(|c| {
    len -= 1;
    &len > &0 && is_hex(c)
  });
  let n = integer(&w, 16);
  (w, n)
}

pub fn is_bin(c: char) -> bool {
  matches!(c, '0' | '1')
}

pub fn is_oct(c: char) -> bool {
  matches!(c, '0'..='7')
}

pub fn is_hex(c: char) -> bool {
  c.is_ascii_hexdigit()
}

// not all of these are implemented, but stay reserved
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
      | "isn't"
      | "aren't"
      | "and"
      | "or"
      | "xor"
      | "nor"
      | "not"
      | "mod"
      | "loop"
      | "like"
  )
}

// Types, ..., ? Reserved IDs
pub fn is_built_in(s: &str) -> bool {
  s.starts_with(|c: char| c.is_uppercase())
    && matches!(
      s,
      "Int"
      | "Float"
      | "Fn"      // named function
      | "Closure" // rust closure pointers used in runtime, builtins, etc
      | "_"       // anonymous
      | "Char"    // 
      | "Str"     // intermediary between Char and String
      | "String"  // iter of ssr's <=> iters of chars
      | "Num"     //
      | "Number"  //
      | "Bit"     //
      | "Byte"
      | "Bin"
      | "Hex"
      | "Oct"
      | "Bool"    // true / false (with Nil partially )
      | "Class"   //
      | "Type"    //
      | "Mod"     //
      | "Pub"     // visibility types*
      | "Priv"    // visibility types*
      | "Ok"      // error type
      | "Stop"    // error type
      | "No"      // "falsey"
      | "Yes"     // "truthsy"
      | "Never"   // 
      | "Then"    // monadic
      | "When"    // synchronous
      | "And"     // algebraic, intersection
      | "Or"      // algebraic, union
      | "To"      // From -> To
      | "From"    // From -> To
      | "With"    // async 
      | "Some"    // type of empty
      | "None"    // type of nil
      | "Maybe"   // (A, B) such that one continues, one breaks
      | "Either" // A, B st. A != B but T\A => B && T\B => A
    )
}

pub fn tokenize_input(src: &str) -> Vec<Token> {
  let mut lexer = Lexer::new(src);
  let mut tokens: Vec<Token> = vec![];
  loop {
    if lexer.done() {
      break;
    } else {
      tokens.push(lexer.next())
    }
  }
  tokens
}

#[cfg(test)]

mod tests {
  use super::*;
  fn inspect_tokens(src: &str) {
    let mut lexer = Lexer::new(src);
    println!("source: {:?}\n", src);
    let mut i = 0;
    while !lexer.done() {
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
    assert_eq!(
      lexer.next(),
      Token::Number(String::from(src), 10, Pos::new())
    );
  }

  #[test]
  fn puncts() {
    let src = "|| 3";
    inspect_tokens(src);
  }

  #[test]
  fn from_to() {
    let src = "3..14";
    inspect_tokens(src);
    let mut lexer = Lexer::new(src);
    assert_eq!(
      lexer.next(),
      Token::Number(String::from("3"), 0, Pos::new())
    );
  }

  #[test]
  fn symbol() {
    inspect_tokens("'\\n' 'cat' wow Int Num String")
  }

  #[test]
  fn keywords() {
    let src = "do let if then else";
    // inspect_tokens(src);
    let mut lexer = Lexer::new(src);
    src.split_whitespace().for_each(|kw| {
      println!("{:?}", lexer.peek());
      let pos = lexer.get_pos();
      assert_eq!(lexer.next(), Token::Keyword(kw.to_owned(), pos));
    });
  }

  #[test]
  fn lambda_call() {
    let src = "he'a (2) _ ";
    inspect_tokens(src);
    let mut lexer = Lexer::new(src);
    let tmpos = &mut Pos::new();
    [
      Token::Identifier(
        String::from("he'a"),
        ['h', 'e', '\'', 'a', ' ', '(', '2', ')', ' ', '_', ' ']
          .iter()
          .map(|_| tmpos.next(&lexer.src.peek().unwrap()))
          .last()
          .and_then(|_| return Some(tmpos.clone()))
          .unwrap(),
      ),
      Token::Punct('(', Pos::faux()),
    ]
    .iter()
    .for_each(|t| assert_eq!(*t, lexer.next()));
  }
}
