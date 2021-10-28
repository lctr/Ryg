use std::fmt;

use super::stream::Pos;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Assoc {
    Left,
    Right,
}

#[allow(unused)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Bool(String, Pos),
    Char(char, Pos),
    Empty(),
    Eof(Pos),
    Identifier(String, Pos),
    Invalid(String, Pos),
    Keyword(String, Pos),
    Meta(String, Pos),
    Number(String, u8, Pos),
    Operator(String, Pos),
    Punct(char, Pos),
    String(String, Pos),
    Symbol(String, Pos),
}

#[allow(unused)]
impl Token {
    pub fn literal(&self) -> String {
        match &self {
            Token::Number(b, _, _)
            | Token::String(b, _)
            | Token::Bool(b, _)
            | Token::Identifier(b, _)
            | Token::Symbol(b, _)
            | Token::Operator(b, _)
            | Token::Invalid(b, _)
            | Token::Meta(b, _)
            | Token::Keyword(b, _) => b.into(),
            Token::Char(b, _) | Token::Punct(b, _) => b.to_string(),
            Token::Eof(_) | Token::Empty() => "".into(),
        }
    }
    pub fn operator_info(&self) -> Option<((usize, Assoc))> {
        use Assoc::*;
        let (a, b, c, d, e, f) = (0, 3, 6, 9, 12, 15);
        if let Some(Token::Operator(op, _)) = self.as_operator() {
            let (prec, assoc) = match op.as_ref() {
                // "." => (a, Left),
                "<|" => (a, Right),
                "=" | ":=" | "<-" | "=<" | "+=" | "-=" | "*=" | "/=" => {
                    (a + 1, Right)
                }
                // "=:" | "@:" | "#:" => (a + 1, Left),
                "=>" | "&:" => (a + 2, Left),
                "?>" | "!>" => (a + 2, Right),
                "||" | "or" => (b, Left),
                "&&" | "and" => (b + 1, Left),
                "|" => (b + 2, Left),
                "^" | "xor" => (c, Left),
                "&" => (c + 1, Left),
                // "@" => (c + 2, Right),
                "==" | "!=" => (c + 2, Left),
                "<" | "<=" | ">" | ">=" => (d, Left),
                "<>" | "++" => (e, Right),
                "+" | "-" => (e + 1, Left),
                "*" | "/" | "%" | "mod" => (e + 2, Left),
                "**" | "^^" => (f, Right),
                // "->" => (f + 1, Right),
                "|>" => (f + 2, Right),
                // "::" => (f + b, Right),
                // |> \> /> <\
                _ => (50, Left),
            };
            if prec < 50 {
                Some((prec, assoc))
            } else {
                None
            }
        } else {
            None
        }
    }
    pub fn associativity(&self) -> Option<Assoc> {
        self.operator_info().and_then(|(_, assoc)| Some(assoc))
    }
    pub fn precedence(&self) -> Option<usize> {
        self.operator_info().and_then(|(prec, _)| Some(prec))
    }

    /// Identifies whether a context exists for which a non-operator token can
    /// be relexed as an operator token. Irregular lexicographic semantemes
    /// such as 'mod', 'and', 'or', 'not' are not lexed as operators
    /// without calling this method.
    pub fn as_operator(&self) -> Option<Self> {
        let pos = if let Some(p) = self.get_pos() {
            p.clone()
        } else {
            Pos::faux()
        };
        let accepted =
            ["mod", "and", "or", "xor", "nor", "not"].iter().as_slice();
        match self {
            (Self::Keyword(a, _) | Self::Meta(a, _))
                if accepted.contains(&a.as_ref()) =>
            {
                Self::Operator(a.to_owned(), pos).as_some()
            }
            Self::Symbol(a, _) if a.as_str() == "@" => {
                Self::Operator(a.to_string(), pos).as_some()
            }
            Self::Punct(a @ '|', _) => {
                Self::Operator(a.to_string(), pos).as_some()
            }
            Self::Operator(..) => self.as_some(),
            _ => None,
        }
    }

    pub fn as_punct(&self) -> Option<Self> {
        let pos = if let Some(p) = self.get_pos() {
            p.clone()
        } else {
            Pos::faux()
        };
        match &self {
            Self::Punct(..) => Some(self.clone()),
            Self::Operator(o, _)
                if matches!(o.as_str(), "|" | "<" | ">" | "." | ":") =>
            {
                if o.len() > 1 {
                    None
                } else {
                    o.chars()
                        .next()
                        .and_then(|c| Self::Punct(c, pos).as_some())
                }
            }
            _ => None,
        }
    }
    pub fn as_some(&self) -> Option<Self> {
        Some(self.clone())
    }
    pub fn to_string(&self) -> String {
        self.literal().to_owned()
    }

    pub fn match_literal(&self, word: &str) -> bool {
        self.literal() == word
    }

    pub fn match_any_of(&self, literals: &[&str]) -> bool {
        literals.iter().any(|&word| self.match_literal(word))
    }

    pub fn get_pos(&self) -> Option<&Pos> {
        match self {
            Token::String(_, p)
            | Token::Char(_, p)
            | Token::Number(_, _, p)
            | Token::Identifier(_, p)
            | Token::Symbol(_, p)
            | Token::Bool(_, p)
            | Token::Operator(_, p)
            | Token::Keyword(_, p)
            | Token::Punct(_, p)
            | Token::Meta(_, p)
            | Token::Invalid(_, p)
            | Token::Eof(p) => Some(p),
            Token::Empty() => None,
        }
    }

    pub fn is_left_punct(&self) -> bool {
        matches!(self, Token::Punct('(' | '[' | '{', _))
    }
    pub fn is_right_punct(&self) -> bool {
        matches!(self, Token::Punct(')' | ']' | '}', _))
    }
    pub fn is_unary(&self) -> bool {
        self.match_any_of(&["!", "-", "~", "+", "*", "&"])
    }

    /// Returns `true` if the token is [`Eof`].
    ///
    /// [`Eof`]: Token::Eof
    pub fn is_eof(&self) -> bool {
        matches!(self, Self::Eof(_))
    }

    /// Returns `true` if the token is [`Boolean`], [`Number`], or [`String`].
    ///
    /// [`Boolean`]: Token::Boolean
    /// [`Number`]: Token::Number
    /// [`String`]: Token::String
    pub fn is_atomic(&self) -> bool {
        matches!(self, Self::Bool(..) | Self::Number(..) | Self::String(..))
    }

    /// Returns `true` if the token is [`Number`].
    ///
    /// [`Number`]: Token::Number
    pub fn is_number(&self) -> bool {
        matches!(self, Self::Number(..))
    }

    /// Returns `true` if the token is [`Keyword`].
    ///
    /// [`Keyword`]: Token::Keyword
    pub fn is_keyword(&self) -> bool {
        matches!(self, Self::Keyword(..))
    }

    /// Returns `true` if the token is [`Operator`].
    ///
    /// [`Operator`]: Token::Operator
    pub fn is_operator(&self) -> bool {
        matches!(self, Self::Operator(..)) || self.as_operator().is_some()
    }

    /// Returns `true` if the token is [`Identifier`].
    ///
    /// [`Identifier`]: Token::Identifier
    pub fn is_identifier(&self) -> bool {
        matches!(self, Self::Identifier(..))
    }

    /// Returns `true` if the token is [`Meta`].
    ///
    /// [`Meta`]: Token::Meta
    pub fn is_meta(&self) -> bool {
        matches!(self, Self::Meta(..))
    }

    /// Returns `true` if the token is [`Invalid`].
    ///
    /// [`Invalid`]: Token::Invalid
    pub fn is_invalid(&self) -> bool {
        matches!(self, Self::Invalid(..))
    }

    /// Returns `true` if the token is [`Empty`].
    ///
    /// [`Empty`]: Token::Empty
    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty(..))
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::util::display::Paint;
        let mag = Paint::fg_light_magenta;
        let blue = Paint::fg_blue;
        let cyan = Paint::fg_cyan;
        let grn = Paint::fg_green;
        let red = Paint::fg_red;
        let ylw = Paint::fg_yellow;
        let lt_red = Paint::fg_light_red;
        let lt_ylw = Paint::fg_light_yellow;
        let lt_mag = Paint::fg_light_magenta;
        let lt_blue = Paint::fg_light_blue;
        let label = match self {
            Token::Bool(s, _) => ("Bool", lt_mag(s)),
            Token::Char(c, _) => ("Char", ylw(&c.to_string())),
            Token::Empty() => ("", "".to_string()),
            Token::Eof(_) => ("EOF", Paint::fg_light_black("\0")),
            Token::Identifier(s, _) => ("Identifier", blue(s)),
            Token::Invalid(s, _) => ("Invalid", red(s)),
            Token::Keyword(s, _) => ("Keyword", lt_red(s)),
            Token::Meta(s, _) => ("Meta", lt_blue(s)),
            Token::Number(s, _, _) => ("Number", lt_mag(s)),
            Token::Operator(s, _) => ("Operator", grn(s)),
            Token::Punct(c, _) => ("Punct", mag(&c.to_string())),
            Token::String(s, _) => ("String", cyan(s)),
            Token::Symbol(s, _) => ("Symbol", lt_ylw(s)),
        };
        write!(
            f,
            "({} {} {})",
            label.0,
            label.1,
            self.get_pos().unwrap_or(&Pos::faux())
        )
    }
}

#[cfg(test)]
mod test {
    use crate::tok::stream::{CharStream, Streaming};

    use super::*;
    fn inspect_chars(src: &str) {
        let mut stream = CharStream::new(src);
        while !stream.done() {
            println!("char '{}' {}", stream.next().unwrap(), stream.pos)
        }
    }

    #[test]
    fn all_chars() {
        let src = "12 34 this is a line
    break, ~ éé ʃ";
        let tok = Token::String("5".to_string(), Pos::faux());
        println!("{:?}", tok.match_any_of(&["4", "5", "6"]));

        inspect_chars(src);
        assert_eq!(
            Token::Number("1".to_string(), 0, Pos::new()).is_number(),
            true
        )
    }

    #[test]
    fn test_atomics() {
        let number = Token::Number("10".to_owned(), 0, Pos::new());
        let string = Token::String("hi".to_owned(), Pos::new());
        let boolean = Token::Bool("true".to_owned(), Pos::new());
        for t in [number, string, boolean].iter() {
            assert_eq!(t.is_atomic(), true);
        }
    }
}
