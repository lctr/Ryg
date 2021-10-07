use std::{fmt::Write, i64};

use crate::util::state::{Pos, StreamState, Streaming};

use super::token::{CharStream, Token};

#[derive(Debug)]
pub struct Lexer<'a> {
  src: CharStream<'a>,
  current: Option<Token>,
  queue: Vec<Token>,
  depth: Vec<char>,
  // TODO: define spans later
  comments: Vec<(Pos, Pos, String)>,
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
      depth: vec![],
      comments: vec![],
    }
  }

  pub fn get_depth(&self) -> usize {
    self.depth.len()
  }

  pub fn eat_while<F: FnMut(char) -> bool>(&mut self, mut pred: F) -> String {
    let mut word = String::new();
    while !self.src.done() && pred(self.src.peek().unwrap()) {
      word.push(self.src.next().unwrap());
    }
    word
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
        x @ ('#' | '_' | '`') => self.symbol(*x, pos),
        x if is_digit(*x) => self.number(),
        x if starts_name(*x) => self.variable(),
        x if is_punct(*x) => self.punct(*c, pos),
        x if is_op_char(*x) => self.operator(*x, pos),
        _ => Token::Invalid(
          format!("Unable to recognize character {:?}", self.src.next()),
          self.get_pos(),
        ),
      }
    } else {
      Token::Eof(pos)
    }
  }

  fn symbol(&mut self, ch: char, pos: Pos) -> Token {
    self.src.next();
    Token::Symbol(
      if ch == '#' && matches!(self.src.peek(), Some('\'')) {
        self.src.next();
        "#'".into()
      } else {
        ch.into()
      },
      pos,
    )
  }

  fn punct(&mut self, ch: char, pos: Pos) -> Token {
    self.src.next();
    let p = self.get_pos();
    if let Some('|') = self.src.peek() {
      self.queue.push(Token::Punct('|', p));
      self.src.next();
    } else {
      match ch {
        // left punct (`)`, `]`, `}`) adds corresponding right punct to depth
        // stack this corresponds to each group surrounded by
        // parens/brackets/braces depth stack should be empty when
        // reaching EOF
        x if Punct::is_left(x) => self.depth.push(Punct::twin_of(x)),
        // right punct to be popped off depth stack when encountered
        x if Punct::is_right(x) => {
          if let Some(x) = self.depth.last() {
            self.depth.pop();
          }
        }
        _ => {}
      };
    }
    Token::Punct(ch, pos)
  }

  fn operator(&mut self, ch: char, pos: Pos) -> Token {
    let op = self.eat_while(is_op_char);
    // we want to prevent `|` and `:` or `.` from being lexed together
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

  fn comment(&mut self) -> Token {
    /// single line comments preceded by `~~`, while multiline comments are
    /// placed between `~*` and `*~`.
    let pos = self.get_pos();
    let outer = self.src.next();
    if let Some(outer) = outer {
      match self.src.next() {
        Some(x @ '~') => {
          self.eat_while(|c| c != '\n');
          self.token()
        }
        Some(x @ '*') => {
          let mut penult = false;
          let comment = self.eat_while(|c| match (penult, c) {
            (true, '~') => false,
            (true, _) => {
              penult = false;
              true
            }
            (false, '*') => {
              penult = true;
              true
            }
            _ => true,
          });
          self.src.next();
          self.token()
        }
        Some(x) => Token::Operator(x.to_string(), pos),
        None => Token::Invalid(format!("Unexpected EOF after {}", outer), pos),
      }
    } else {
      Token::Invalid(format!("Unexpected EOF after {:?}", outer), pos)
    }
  }

  fn variable(&mut self) -> Token {
    let mut word = String::new();
    let pos = self.get_pos();
    word.push_str(&self.eat_while(|c| is_name(&c.to_string())));
    match word.as_str() {
      "true" | "false" => Token::Boolean(word, pos),
      w if is_kw(w) => Token::Keyword(w.to_string(), pos),
      w if is_built_in(w) => Token::Meta(w.to_string(), pos),
      w if is_name(&word)
        && Token::Keyword(word.clone(), pos.clone())
          .as_operator()
          .is_some() =>
      {
        Token::Operator(word, pos)
      }
      _ => Token::Identifier(word, pos),
    }
  }

  fn character(&mut self) -> Token {
    let pos = self.get_pos();
    let mut escaped = false;
    let mut e_ct = 0;
    let mut chr: char = self.src.peek().map_or_else(|| '\0', |c| c);
    let mut key: u8 = chr as u8;
    self.src.next();
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
            '\r' | 'r' => '\r',
            '\t' | 't' => '\t',
            '\\' => '\\',
            '\'' => '\'',
            '\"' | '"' => '\"',
            '\0' | '0' => '\0',
            _ => '\0',
          };
          key = chr as u8;
          false
        }
        (false, e) => {
          if e_ct == 0 {
            chr = e;
            key = chr as u8;
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
      Token::Invalid(
        format!("The input {:?}{:?} is not a valid character", chr, mischar),
        pos,
      )
    } else {
      Token::Char(key as char, pos)
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
            't' | 'n' | 'r' | '"' | '\'' => {
              word.push_str(format!("\\\\{}", c).as_str());
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

  fn number(&mut self) -> Token {
    let pos = self.get_pos();
    let mut infixed = false;
    let mut base: u8 = 0;
    let mut setting: fn(char) -> bool = is_digit;
    let mut seps = 0;
    let mut dot_ct = 0;
    let zero_first = matches!(self.src.peek(), Some('0'));
    let mut number = self.eat_while(|c| -> bool {
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
                base = 2;
                setting = is_bin;
              }
              'o' => {
                base = 8;
                setting = is_oct;
              }
              'x' => {
                base = 16;
                setting = is_hex;
              }
              _ => unreachable!(),
            };
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
      Token::Number(
        if matches!(base, 2 | 8 | 16) && number.len() > 2 {
          number[2..].to_owned()
        } else {
          number
        }
        .replace('_', ""),
        base,
        pos,
      )
    }
  }
}

pub fn integer(word: &str, base: u32) -> i32 {
  match i64::from_str_radix(word, base) {
    Ok(n) => n as i32,
    Err(_) => 0,
  }
}

pub fn is_comment_delim(left: String, right: String) -> bool {
  matches!(format!("{}{}", left, right).as_str(), "~~" | "~*")
}

//
pub fn starts_name(c: char) -> bool {
  c.is_alphabetic() && !matches!(c, '_' | '@' | '\'' | '0'..='9')
}

pub fn is_name(word: &String) -> bool {
  let all = word.chars().fold(true, |a, c| {
    a && (c.is_alphanumeric() || matches!(c, '\'' | '_'))
  });
  word.starts_with(char::is_alphabetic) || all
}

pub fn is_escapable(c: char) -> bool {
  matches!(c, 't' | 'n' | 'r' | '"' | '\'' | '\\')
}

pub fn get_escaped(c: char) -> char {
  match c {
    't' => '\t',
    'n' => '\n',
    'r' => '\r',
    '"' => '\"',
    '\'' => '\'',
    '\\' => '\\',
    _ => c,
  }
}

pub fn is_digit(c: char) -> bool {
  matches!(c, '0'..='9')
}

/// Characters that begin subcontent, such as `(`, `[`, and `{`, are
/// considered to be `left` puncts, with their corresponding mirror image
/// characters as `right` puncts.
///
/// Note: `|` is equivalent under left and right in lambda argument syntax. The
/// corresponding right form of a left punct is added to the depth stack, to be
/// popped off when encountered in the right order. This helps to measure
/// balanced groupings.
// TODO: lex `|` as corresponding PUNCTs (even in case of ||) based on state
// TODO: differentiate punct '<' from op '<'
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Punct {
  Left(char),
  // end subcontent
  Right(char),
  // partition subcontent
  Sep(char),
  // chars incorrectly lexed (or not universally used as) puncts
  Other(char),
}

impl Punct {
  pub fn get_char(&self) -> char {
    match self {
      Self::Left(c) | Self::Right(c) | Self::Sep(c) | Self::Other(c) => {
        c.to_owned()
      }
    }
  }
  pub fn twin_punct(&self) -> Self {
    match self {
      Self::Left(_) => Self::Right(Self::twin_of(self.get_char())),
      Self::Right(_) => Self::Left(Self::twin_of(self.get_char())),
      _ => self.clone(),
    }
  }
  pub fn is_left(c: char) -> bool {
    matches!(c, '(' | '[' | '{')
  }
  pub fn is_right(c: char) -> bool {
    matches!(c, ')' | ']' | '}')
  }

  pub fn is_punct(c: char) -> bool {
    matches!(c, '(' | ')' | '{' | '}' | '[' | ']' | ';' | ',')
  }
  pub fn twin_of(c: char) -> char {
    match c {
      '[' => ']',
      '(' => ')',
      '{' => '}',
      '<' => '>',
      _ => '\0',
    }
  }
}

impl Into<String> for Punct {
  fn into(self) -> String {
    self.get_char().to_string()
  }
}

impl From<char> for Punct {
  fn from(c: char) -> Self {
    match c {
      '[' | '(' | '{' | '<' => Self::Left(c),
      ')' | ']' | '}' | '<' => Self::Right(c),
      ';' | ',' | ':' => Self::Sep(c),
      '|' | _ => Self::Other(c),
    }
  }
}

impl std::fmt::Display for Punct {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_char(self.get_char())
  }
}

pub fn is_left_punct(c: char) -> bool {
  matches!(c, '(' | '[' | '{')
}

pub fn is_right_punct(c: char) -> bool {
  matches!(c, ')' | ']' | '}')
}

pub fn is_punct(c: char) -> bool {
  matches!(c, '(' | ')' | '{' | '}' | '[' | ']' | ';' | ',')
}

pub fn twin_of(s: &str) -> &str {
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
      // | '#'
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
      "Int" // i32 typeclass
      | "Float"
      | "Fn"      // named function
      | "Closure" // rust closure pointers used in runtime, builtins, etc
      | "_"       // anonymous
      | "Char"    // 
      | "Str"     // 
      | "String"  // 
      | "Num"     //
      | "Number"  //
      | "Bit"     // 
      | "Byte"    // 
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
    || matches!(s, |"type'of"| "len'of" | "print" | "print'ln")
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
  fn run(src: &str) -> Lexer {
    let mut lexer = Lexer::new(src);
    while !lexer.done() {
      lexer.next();
    }
    lexer
  }
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
  fn comments() {
    let src = "hello ~* world *~ World";
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
  fn token_4_should_be_invalid() {
    inspect_tokens("#'\\n' 'cat' wow Int Num String")
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
    println!("INSPECT\n {:#?}", &lexer)
  }

  #[test]
  fn test_base_2() {
    let src = "0b11";
    inspect_tokens(src)
  }

  #[test]
  fn lambda_call() {
    let src = "it's (2) _ ";
    inspect_tokens(src);
  }

  #[test]
  fn should_fail_unbalanced_paren() {
    let src = "print((4) + 5";
    let lexer = run(src);
    assert_eq!(lexer.get_depth(), 0)
  }

  #[test]
  fn should_pass_balanced_puncts() {
    let src = "(let [a, b] = [1, 2] in {(a, a + {b * 2}, -a)})";
    let lexer = run(src);
    println!("{:#?}", &lexer);
    assert_eq!(lexer.get_depth(), 0)
  }
}
