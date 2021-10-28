use std::f64::NAN;

use crate::{
    tok::token::Token,
    util::{state::Halt, types::Either},
};

#[derive(Clone, Debug, PartialEq)]
pub enum Atom {
    Nil,
    // type BOTTOM
    Unit(),
    Bool(bool),
    Byte(u8),
    Int(i32),
    Float(f64),
    Char(char),
    String(String),
    Symbol(String),
}

impl std::fmt::Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Unit() => write!(f, "()"),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Byte(b) => write!(f, "{:b}", b),
            Self::Int(n) => write!(f, "{}", n),
            Self::Float(q) => write!(f, "{}", q),
            Self::Char(c) => write!(f, "{:?}", c),
            Self::String(s) => write!(f, "{}{}{}", '"', s, '"'),
            Self::Symbol(s) => write!(f, "{}", s),
        }
    }
}

impl Default for Atom {
    fn default() -> Self {
        Self::Nil
    }
}

impl From<Token> for Atom {
    fn from(token: Token) -> Self {
        match token {
            Token::Empty() => Atom::Nil,
            Token::Bool(b, _) => Atom::Bool(b == "true"),
            Token::Char(c, _) => Atom::Char(c),
            Token::String(s, _) => Atom::String(s),
            Token::Symbol(s, _) => Atom::Symbol(s),
            Token::Number(w, n, _) => {
                if n == 8 {
                    Atom::Byte(byte(&w))
                } else {
                    if let Ok(parsed) = parse_number(w, n) {
                        match parsed {
                            Either::Left(i) => Atom::Int(i),
                            Either::Right(j) => Atom::Float(j),
                            _ => Atom::Nil,
                        }
                    } else {
                        Atom::Nil
                    }
                }
            }
            nil if matches!(nil.literal().as_str(), "()" | "nil") => Atom::Nil,
            _ => Atom::Unit(),
        }
    }
}

pub fn byte(word: &str) -> u8 {
    match word.parse::<u8>() {
        Ok(b) => b,
        Err(_) => 0,
    }
}

impl From<Token> for u8 {
    fn from(token: Token) -> Self {
        if let Token::Number(word, 8, _) = token {
            match word.parse::<u8>() {
                Ok(n) => n as u8,
                Err(_) => 0,
            }
        } else {
            0
        }
    }
}

impl From<Token> for i32 {
    fn from(token: Token) -> Self {
        if let Token::Number(word, base, _) = token {
            match i32::from_str_radix(&word, base.into()) {
                Ok(n) => n as i32,
                Err(_) => 0,
            }
        } else {
            0
        }
    }
}

impl From<Token> for f64 {
    fn from(token: Token) -> Self {
        if let Token::Number(word, 10, _) = token {
            match word.parse::<f64>() {
                Ok(n) => n as f64,
                Err(_) => NAN,
            }
        } else {
            NAN
        }
    }
}

pub fn integer(word: &str, base: u8) -> i32 {
    match i32::from_str_radix(word, base.into()) {
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

pub fn parse_number(
    number: String,
    base: u8,
) -> Result<Either<i32, f64>, Halt> {
    match base {
        2 | 8 | 16 => Ok(Either::Left(integer(&number, base.clone()))),
        x if x == 0 => Ok(Either::Left(
            number
                .parse::<i32>()
                .unwrap_or_else(|_| integer(&number, 10))
                .to_owned(),
        )),
        x if x == 10 => Ok(Either::Right(floating(&number))),
        _ => Err(Halt::InvalidInput(format!(
            "Unable to parse string {:?} to number of base {}",
            &number, base
        ))),
    }
}
