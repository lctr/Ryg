use std::{collections::HashSet, fmt::Write, i64};

use crate::util::types::Kind;

pub use super::{
    literal::Literal,
    stream::{CharStream, Pos, Stream, Streaming},
    token::Token,
};

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    src: CharStream<'a>,
    current: Option<Token>,
    queue: Vec<Token>,
    depth: Vec<char>,
    // TODO: define spans later
    comments: Vec<(Pos, Pos, String)>,
    known: HashSet<String>,
}

impl<'a> Streaming for Lexer<'a> {
    type T = Token;
    fn peek(&mut self) -> Self::T {
        if let Some(t) = self.current.clone() {
            t
        } else {
            let tok = self.token();
            self.current = Some(tok.clone());
            tok
        }
    }

    fn next(&mut self) -> Self::T {
        let token = self.current.clone();
        self.current = None;
        if let Some(t) = token {
            t
        } else {
            self.token()
        }
    }

    fn done(&mut self) -> bool {
        self.queue.is_empty()
            && (if let Some(t) = self.current.clone() {
                t.is_eof() || self.src.done()
            } else {
                self.src.peek().is_none()
            })
    }
    fn get_pos(&mut self) -> Pos {
        self.src.pos.clone()
    }
}

impl<'a> Lexer<'a> {
    pub fn new(src: &str) -> Lexer {
        Lexer {
            src: CharStream::new(src),
            current: None,
            queue: vec![],
            depth: vec![],
            comments: vec![],
            known: HashSet::new(),
        }
    }

    pub fn extend(&mut self, src: &'a str) {
        let extended = self.src.extend(src);
        self.src = extended;
    }

    pub fn register_meta(&mut self, word: &str) {
        self.known.insert(word.to_owned());
    }

    pub fn has_meta(&mut self, word: &String) -> bool {
        self.known.contains(word)
    }

    pub fn take_meta(&mut self) -> HashSet<String> {
        self.known.to_owned()
    }

    pub fn id_as_meta(&mut self) -> Option<Token> {
        if let Token::Identifier(name, p) = self.peek() {
            self.register_meta(name.as_str());
            Some(Token::Meta(name, p))
        } else {
            None
        }
    }

    pub fn reset_known(&mut self) {
        self.known.clear()
    }

    pub fn get_source(&self) -> &str {
        self.src.get_source()
    }

    pub fn get_depth(&self) -> usize {
        self.depth.len()
    }

    fn eat_while<F>(&mut self, mut pred: F) -> String
    where
        F: FnMut(char) -> bool, {
        let mut word = String::new();
        // Since `self.peek()` only returns `None` when done, and since
        // `self.next()` returns the *same* token as `self.peek()` while
        // advancing the iterator, it follows that we can call unwrap on either
        // methods without panic iff `self.src.done()` returns `false`.
        while !self.src.done() && pred(self.src.peek().unwrap()) {
            word.push(self.src.next().unwrap());
        }
        word
    }

    fn eat_while_counting<F>(&mut self, mut pred: F) -> String
    where
        F: FnMut(char, usize) -> bool, {
        let mut count = 0_usize;
        let mut word = String::new();
        while !self.src.done() && pred(self.src.peek().unwrap(), count) {
            word.push(self.src.next().unwrap());
            count += 1;
        }
        word
    }

    pub fn token(&mut self) -> Token {
        if let Some(tok) = self.queue.pop() {
            return tok;
        };
        self.eat_whitespace();
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
                ':' => self.colon(pos),
                x @ ('#' | '@' | '`') => self.symbol(*x, pos),
                x if is_digit(*x) => self.number(),
                x if starts_identifier(*x) || x == &'_' => self.identifier(),
                x if is_punct(*x) => self.punct(*c, pos),
                x if is_op_char(*x) => self.operator(*x, pos),
                _ => Token::Invalid(
                    format!(
                        "Unable to recognize character {:?}",
                        self.src.next()
                    ),
                    self.get_pos(),
                ),
            }
        } else {
            Token::Eof(pos)
        }
    }

    fn symbol(&mut self, ch: char, pos: Pos) -> Token {
        self.src.next();
        // match (ch == '#', self.src.peek()) {
        //     Some('\'') => {}
        //     Some('!') => {}
        //     Some('[') => {}
        //     _ => {}
        // }
        if ch == '#' && matches!(self.src.peek(), Some('\'')) {
            self.src.next();
            Token::Symbol("#'".into(), pos)
        } else {
            Token::Symbol(ch.into(), pos)
        }
    }

    fn punct(&mut self, ch: char, pos: Pos) -> Token {
        self.src.next();
        let p = self.get_pos();
        if let Some(punct @ '|') = self.src.peek() {
            self.queue.push(Token::Punct(punct, p));
            self.src.next();
        };

        match ch {
            // left punct (`)`, `]`, `}`) adds corresponding right punct to
            // depth stack this corresponds to each group
            // surrounded by parens/brackets/braces depth
            // stack should be empty when reaching EOF
            x if Punct::is_left(x) => {
                self.depth.push(Punct::twin_of(x));
                Token::Open(ch, pos)
            }
            // right punct to be popped off depth stack when encountered
            x if Punct::is_right(x) => {
                if let Some(x) = self.depth.last() {
                    self.depth.pop();
                };
                Token::Closed(ch, pos)
            }
            _ => Token::Punct(ch, pos),
        }
    }

    fn colon(&mut self, pos: Pos) -> Token {
        self.src.next();
        match self.src.peek() {
            Some(':') => {
                self.src.next();
                Token::Operator("::".to_string(), pos)
            }
            Some('=') => {
                self.src.next();
                Token::Operator(":=".to_string(), pos)
            }
            _ => Token::Punct(':', pos),
        }
    }

    fn operator(&mut self, ch: char, pos: Pos) -> Token {
        let op = self.eat_while(is_op_char);
        // we want to prevent `|` and `:` or `.` from being lexed together
        match op.as_str() {
            "." => Token::Punct(ch, pos),
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
        let first = self.src.peek();
        let pos = self.get_pos();
        const TILDE: char = '~';
        const STAR: char = '*';
        if let Some(TILDE) = first {
            self.src.next();
            if let Some(c2) = self.src.peek() {
                match c2 {
                    TILDE => {
                        let comment = self.eat_while(|c| c != '\n');
                        let pos2 = self.get_pos();
                        self.comments.push((pos, pos2, comment));
                        self.token()
                    }
                    STAR => {
                        let mut penult = false;
                        let comment = self.eat_while(|c| match (penult, c) {
                            (true, TILDE) => false,
                            (true, STAR) | (false, TILDE) => true,
                            (false, STAR) => {
                                penult = true;
                                true
                            }
                            (true, _) => {
                                penult = false;
                                true
                            }
                            (false, _) => true,
                        });
                        self.src.next();
                        let pos2 = self.get_pos();
                        self.comments.push((pos, pos2, comment));
                        self.token()
                    }
                    _ => {
                        let tok = self.token();
                        self.queue.push(tok);
                        Token::Operator(TILDE.to_string(), pos)
                    }
                }
            } else {
                Token::Invalid(
                    "Unexpected end of input after `~`".to_string(),
                    pos,
                )
            }
        } else {
            self.token()
        }
    }

    fn identifier(&mut self) -> Token {
        let mut word = String::new();
        let pos = self.get_pos();
        word.push_str(&self.eat_while(|c| is_identifier(&c.to_string())));
        match word.as_str() {
            "true" | "false" => Token::Bool(word == "true", pos),
            "_" => Token::Symbol(word, pos),
            w if is_kw(w) => {
                let tok = Token::Keyword(w.to_string(), pos.clone());
                if tok.as_operator().is_some() {
                    Token::Operator(w.to_string(), pos)
                } else {
                    tok
                }
            }
            w if is_built_in(w)
                || self.known.contains(w)
                || self.has_meta(&w.to_string()) =>
            {
                Token::Meta(w.to_string(), pos)
            }
            // lex as operators certain tokens that would be otherwise
            // considered keywords or identifiers (e.g., `mod`, etc.)
            w if is_identifier(&word)
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
        let mut esc_count = 0;
        let mut chr: char = self.src.peek().map_or_else(|| '\0', |c| c);
        let mut key: u8 = chr as u8;
        let mut first = false;
        let mut quote = false;
        self.src.next();
        let wrd = self.eat_while_counting(|c, i| {
            let ctn = match (escaped, c) {
                (false, '\\') => {
                    esc_count += 1;
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
                    if esc_count == 0 {
                        chr = e;
                        key = chr as u8;
                    };
                    false
                }
            };
            ctn && i < 3
        });
        if quote {
            return Token::Symbol("'".to_string(), pos);
        };
        let cx = self.src.next();
        let pos = self.get_pos();
        if esc_count > 2 || !matches!(self.src.next(), Some('\'')) {
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
                format!("The input `{:?}` is not a valid character", mischar),
                pos,
            )
        } else {
            Token::Char(key as char, pos)
        }
    }

    fn eat_whitespace(&mut self) {
        self.eat_while(char::is_whitespace);
    }

    fn escaped(&mut self, end: &char) -> String {
        let mut escaped = false;
        let mut word = String::new();
        self.src.next();
        let mut pos = self.get_pos();
        while !self.done() {
            if let Some(c) = self.src.next() {
                if escaped {
                    escaped = false;
                    match c {
                        esc if is_escapable(esc) => {
                            word.push(get_escaped(esc))
                        }
                        // TODO: parsing unicode escapes
                        // 'u' => {
                        //     let (s, n) = unicode(self);
                        //     println!("{}",);
                        // }
                        '\0' => { /* null grapheme */ }
                        // preserve indentation/ignore whitespace on new line
                        // e.g. `"a b c\
                        //        d e"` lexes as "a b cd e"
                        '\n' => {
                            self.eat_whitespace();
                        }
                        _ => {
                            word.push(c);
                        }
                    };
                } else if &c == end {
                    break;
                } else if &c == &'\\' {
                    pos = self.get_pos();
                    escaped = true;
                } else {
                    word.push(c);
                }
            }
        }
        word
    }

    pub fn maybe_digits(&mut self) -> Token {
        // digits only, used for tuple field access
        let pos = self.get_pos();
        match self.src.peek() {
            Some(c) if is_digit(c) => {
                let digits = self.eat_while(is_digit);
                Token::Number(digits, 0, pos)
            }
            _ => self.token(),
        }
    }
    fn number(&mut self) -> Token {
        let start = self.get_pos();
        let mut infixed = false;
        let mut radix: u8 = 0;
        let mut setting: fn(char) -> bool = is_digit;
        let mut seps = 0;
        let mut dot_ct = 0;
        let zero_first = matches!(self.src.peek(), Some('0'));
        let mut prefixed = false;
        let mut error_msg = String::new();
        // since this method is triggered by recognizing a digit, the dot_idx
        // should never be 0, hence the safety of using 0 as a default value
        // let mut dot_idx = 0;
        let mut number = self.eat_while_counting(|c, i| -> bool {
            if c == '_' {
                return true;
            };
            match radix {
                2 => is_bin(c),
                8 => is_oct(c),
                16 => is_hex(c),
                10 => match c {
                    '+' | '-' => infixed,
                    '.' if dot_ct == 1 && !infixed => {
                        radix = 0;
                        dot_ct += 1;
                        false
                    }
                    _ => is_digit(c),
                },
                // 0 => {}
                _ => match c {
                    '.' => {
                        dot_ct += 1;
                        if infixed {
                            return false;
                        } else {
                            // infixed = true;
                            radix = 10;
                            return true;
                        }
                    }
                    'b' | 'B' | 'o' | 'O' | 'x' | 'X'
                        if zero_first && i == 1 =>
                    {
                        prefixed = true;
                        match c {
                            'b' | 'B' => {
                                radix = 2;
                            }
                            'o' | 'O' => {
                                radix = 8;
                            }
                            'x' | 'X' => {
                                radix = 16;
                            }
                            _ => unreachable!(),
                        };
                        true
                    }
                    'e' | 'E' if !prefixed => {
                        if infixed {
                            return false;
                        } else {
                            infixed = true;
                            radix = 10;
                            return true;
                        }
                    }
                    _ => is_digit(c),
                },
            }
        });
        if dot_ct == 2 {
            let num = Token::Number(
                number.replace('_', "").trim_end_matches('.').to_string(),
                0,
                start.clone(),
            );
            let curr = self.src.peek();
            let dots = self.eat_while(|c| c == '.');
            let Pos {
                pos,
                row,
                col,
                diff_line,
            } = self.get_pos();
            let dotpos = Pos {
                pos: start.clone().pos + dots.len(),
                row,
                col: start.col + dots.len(),
                diff_line,
            };
            match dots.len() {
                1 | 2 => {
                    self.queue.push(Token::Operator(format!(".{}", dots), dotpos));
                    num
                }
                _ => {
                   Token::Invalid(format!("Invalid sequence! Expected a second `.`, after '{:?}', but instead got {:?}", num, curr), self.get_pos())
                }
            }
        } else {
            Token::Number(
                if matches!(radix, 2 | 8 | 16) && number.len() > 2 {
                    number[2..].to_owned()
                } else {
                    number
                }
                .replace('_', ""),
                radix,
                start,
            )
        }
    }
}

impl<'a> Iterator for Stream<Lexer<'a>, Token> {
    type Item = Token;
    fn next(self: &mut Stream<Lexer<'a>, Token>) -> Option<Self::Item> {
        let stream = &mut self.0;
        let done = Streaming::done(&mut *stream);
        if done {
            None
        } else {
            let next = Streaming::next(&mut *stream);
            self.1.push(next.clone());
            Some(next)
        }
    }
}

impl From<&str> for Token {
    fn from(source: &str) -> Self {
        Lexer::new(source).token()
    }
}

impl From<String> for Token {
    fn from(string: String) -> Self {
        Token::from(string.as_str())
    }
}

#[allow(unused)]
pub fn integer(word: &str, base: u32) -> i32 {
    match i64::from_str_radix(word, base) {
        Ok(n) => n as i32,
        Err(_) => 0,
    }
}

pub fn starts_identifier(c: char) -> bool {
    (c.is_alphabetic() || matches!(c, '$' | '_'))
        && !matches!(c, '@' | '\'' | '0'..='9')
}

pub fn is_identifier(word: &String) -> bool {
    word.starts_with(char::is_alphabetic)
        || word.chars().fold(true, |a, c| {
            a && (c.is_alphanumeric() || matches!(c, '\'' | '_' | '$'))
        })
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
        // '\"' => '\"',
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
    pub fn is_left(c: char) -> bool {
        matches!(c, '(' | '[' | '{')
    }
    pub fn is_right(c: char) -> bool {
        matches!(c, ')' | ']' | '}')
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
            ')' | ']' | '}' | '>' => Self::Right(c),
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
        '=' | '+'
            | '-'
            | '*'
            | '/'
            | '%'
            | '^'
            | '<'
            | '>'
            | '|'
            | '&'
            // | '@'
            | '!'
            | '~'
            | '?'
            | ':'
            | '\\'
            // | '$'
            | '.' // single only => punct
    )
}

#[allow(unused)]
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
    // matches!(c.to_lowercase().next().unwrap(), '0'..='9' | 'a'..='f')
    c.is_digit(16)
    // c.is_ascii_hexdigit()
}

// not all of these are implemented, but stay reserved
pub fn is_kw(s: &str) -> bool {
    matches!(
        s,
        "do" | "let"
             | "if"
            | "then"
            | "else"
            | "in"
            | "case"
            | "of"
            | "fn"
            | "where"
            // | "this"
            // | "self"
            | "data"
            // | "type"
            // | "with"
            // | "is"
            | "and"
            | "or"
            | "xor"
            | "not"
            | "mod"
            | "loop"
            | "struct"
    )
}

// Types, ..., ? Reserved IDs
pub fn is_built_in(s: &str) -> bool {
    s.starts_with(|c: char| c.is_uppercase())
        && matches!(
            s,
            "Int" // i32 
      | "Float"
      | "Fn"      // evaluation => fn call 
      | "_"       // anonymous? or unknown
      | "Char"    // 
      | "Str"     // synonym for String?
      | "String"  // 
      | "Byte"    // 
      | "Bool"    // true / false (with Nil partially )
      | "Nil"
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
mod test {
    use crate::log_do;

    use super::*;

    fn run(src: &str) -> Lexer {
        let mut lexer = Lexer::new(src);
        while !lexer.done() {
            Streaming::next(&mut lexer);
        }
        lexer
    }
    fn inspect_tokens<'a>(src: &'a str) {
        let lexer = Lexer::new(src);
        log_do!(
          "lexer" => &lexer,
          "iterate" => &mut Stream(lexer, vec![]).map(|tok| tok).collect::<Vec<_>>()
        );
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
        let src = "3..14 7_9 34e+3...9";
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
        let src = "it's (2) _ mod 12";
        inspect_tokens(src);
    }

    #[test]
    fn escaped_str() {
        let src = "\"a\nb \
           c d\"";
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

    #[test]
    fn token_from_str() {
        let should_be_num_tok = Token::from("35");
        println!("tok from str: {}", &should_be_num_tok);
        assert_eq!(
            should_be_num_tok,
            Token::Number("35".to_string(), 0, Pos::new())
        )
    }
}
