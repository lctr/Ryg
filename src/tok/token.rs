use std::fmt;

use crate::{color, quick_match};

use super::{literal::Literal, stream::Pos};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Assoc {
    Left,
    Right,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Bool(bool, Pos),
    Char(char, Pos),
    Closed(char, Pos),
    Empty,
    Eof(Pos),
    Generic(String, Pos),
    Identifier(String, Pos),
    Invalid(String, Pos),
    Keyword(String, Pos),
    Meta(String, Pos),
    Number(String, u8, Pos),
    Open(char, Pos),
    Operator(String, Pos),
    Punct(char, Pos),
    String(String, Pos),
    Symbol(String, Pos),
}

impl Default for Token {
    fn default() -> Self {
        Self::Empty
    }
}

impl Literal for Token {
    type Lit = String;
    fn literal(&self) -> Self::Lit {
        match &self {
            Self::Number(b, _, _)
            | Self::String(b, _)
            | Self::Identifier(b, _)
            | Self::Symbol(b, _)
            | Self::Operator(b, _)
            | Self::Invalid(b, _)
            | Self::Meta(b, _)
            | Self::Generic(b, _)
            | Self::Keyword(b, _) => b.into(),
            Self::Bool(b, _) => b.to_string(),
            Self::Char(b, _)
            | Self::Punct(b, _)
            | Self::Open(b, _)
            | Self::Closed(b, _) => b.to_string(),
            Self::Eof(_) | Self::Empty => String::new(),
        }
    }
}

impl Token {
    pub fn operator_info(&self) -> Option<((usize, Assoc))> {
        use Assoc::*;
        let (a, b, c, d, e, f) = (0, 3, 6, 9, 12, 15);
        if let Some(Token::Operator(op, _)) = self.as_operator() {
            let (prec, assoc) = match op.as_ref() {
                // "." => (a, Left),
                "<|" => (a, Right),
                "=" | ":=" | "<-" | "=<" | "+=" | "-=" | "*=" | "/="
                | "%=" | "<<=" | ">>=" | "&=" | "^=" | "|=" => (a + 1, Right),
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
                "<=>" => (d + 1, Left),
                "<<" | ">>" => (d + 2, Left),
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
        let accepted = ["mod", "and", "or", "xor", "not"].iter().as_slice();
        match self {
            (Self::Keyword(a, _) | Self::Meta(a, _))
                if accepted.contains(&a.as_ref()) =>
            {
                Self::Operator(a.to_string(), pos).as_some()
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
            | Token::Generic(_, p)
            | Token::Invalid(_, p)
            | Token::Open(_, p)
            | Token::Closed(_, p)
            | Token::Eof(p) => Some(p),
            Token::Empty => None,
        }
    }
    pub fn is_unary(&self) -> bool {
        self.match_any_of(&["!", "not", "-", "~", "+", "*", "&"])
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
        matches!(self, Self::Empty)
    }

    /// Returns `true` if the token is [`Open`], i.e., left-punct.
    ///
    /// [`Open`]: Token::Open
    pub fn is_open(&self) -> bool {
        matches!(self, Self::Open(..))
    }

    /// Returns `true` if the token is [`Closed`], i.e., right-punct.
    ///
    /// [`Closed`]: Token::Closed
    pub fn is_closed(&self) -> bool {
        matches!(self, Self::Closed(..))
    }

    /// Returns `true` if the token is [`Generic`].
    ///
    /// [`Generic`]: Token::Generic
    pub fn is_generic(&self) -> bool {
        matches!(self, Self::Generic(..))
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let label = match self {
            Token::Bool(s, _) => {
                ("Bool", color!(fg LightMagenta s.to_string().as_str()))
            }
            Token::Char(c, _) => ("Char", color! {fg Yellow &c.to_string() }),
            Token::Empty => ("", "".to_string()),
            Token::Eof(_) => ("EOF", color!(fg LightBlack "\0")),
            Token::Identifier(s, _) => ("Identifier", color!(fg Blue s)),
            Token::Invalid(s, _) => ("Invalid", color!(fg Red s)),
            Token::Keyword(s, _) => ("Keyword", color!(fg LightRed s)),
            Token::Meta(s, _) => ("Meta", color!(fg LightBlue s)),
            Token::Generic(s, _) => ("Generic", color!(fg LightCyan s)),
            Token::Number(s, _, _) => ("Number", color!(fg LightMagenta s)),
            Token::Operator(s, _) => ("Operator", color!(fg Green s)),
            Token::Punct(c, _) => ("Punct", color!(fg Magenta &c.to_string())),
            Token::String(s, _) => ("String", color!(fg Cyan s)),
            Token::Symbol(s, _) => ("Symbol", color!(fg LightGreen s)),
            Token::Open(c, _) => {
                ("Open", color!(fg LightYellow &c.to_string()))
            }
            Token::Closed(c, _) => {
                ("Closed", color!(fg LightYellow &c.to_string()))
            }
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

/// Specific subsets of Token `[Operator]` variant, primarily used to reduce
/// dependence on Strings when matching operator tokens later.
pub trait FromToken {
    type Output;
    fn from_token(tok: &Token) -> Option<Self::Output>;
}

/// Enums corresponding to a literal, but that *don't* hold any data values,
/// instead dynamically converting between literal and enum. Literal values are
/// automatically recorded in doc comments for each enum variant.
macro_rules! def_tok_set {
    (meta $m:meta) => {$(#[$m])*};
    (
        $(#[$meta:meta])*
        $opk:ident :: $(
            $lit:literal $name:ident
        )+
    ) => {
        $(#[$meta])*
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum $opk {
            $(
                #[doc = $lit]
                $name,
            )+
        }

        impl FromToken for $opk {
            type Output = Self;
            fn from_token(tok: &Token) -> Option<Self::Output> {
                quick_match! {
                    Self =<< some tok.literal().as_str();
                    $($lit $name),+
                }
            }
        }

        impl Literal for $opk {
            type Lit = String;
            fn literal(&self) -> Self::Lit {
                let s = match self {
                    $($name => $lit,)+
                    _ => ""
                };
                s.to_string()
            }
        }

        impl std::fmt::Display for $opk {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", &self)
            }
        }
    };
    (L) => {Assoc::Left};
    (R) => {Assoc::Right};
    (ident $t:tt) => {$t};
    ($opk:ident $knd:tt :: $($lit:literal $name:ident)+) => {
        def_tok_set! { $opk :: $($lit $name)+ }

        impl $opk {
            pub fn $knd(word: &str) -> bool {
                match word {
                    $($lit => true,)+
                    _ => false
                }
            }
        }
    };
    (all $t1:literal $t2:tt, $opk:ident $knd:tt :: $($lit:literal $name:ident)+) => {
        def_tok_set! { all $t1 $t2, $opk :: $($lit $name)+ }

        impl $opk {
           pub fn $knd(word: &str) -> bool {
                match word {
                    $($lit => true,)+
                    _ => false
                }
            }

        }
    };
    (all $t1:literal $t2:tt, $opk:ident :: $($ts:tt)+) => {
        def_tok_set! { $opk :: $($ts)+ }

        impl $opk {
            pub fn get_prec(&self) -> usize {
                $t1
            }
            pub fn get_assoc(&self) -> Assoc {
                def_tok_set!($t2)
            }
        }
    };
}

def_tok_set! { BinOp is_binary ::
    // Test
    "=>" Imply "||" Or "or" Or2
    "&&" And "and" And2
    "|" BitOr
    "^" BitXor
    "xor" BitXor2
    "&" BitAnd
    "<<" ShL ">>" ShR
    "!=" NotEq "==" Equal
    "<" Less "<=" LessEq ">" Greater ">=" GreaterEq
    "<>" Conc "++" Comb
    "+" Plus "-" Minus
    "*" Times "/" Div "%" Rem "mod" Mod
    "**" Pow "^^" Raise
}

def_tok_set! { all 1 R, AssignOp is_assign_op ::
    "+=" Plus
    "*=" Times
    "-=" Minus
    "/=" Div
    "=_" Mod
    "%=" Rem
    "**=" Pow
    "||=" Or
    "&&=" And
    // "?=" Nil
    "&=" BitAnd
    "^=" BitXor
    "|=" BitOr
    "~=" BitNot
    "<<=" BitShL
    ">>=" BitShR
    "<>=" Conc
    "++=" Comb
    ":=" Def
    "=" Set
    "<-" Put
    "=<" Bind
}

def_tok_set! { all 1 R, UnOp is_unary ::
    "!" Not
    "not" Not2
    "-" Neg
    "~" BitNot
    "&" Ref
    "*" Deref
}

macro_rules! def_tok_info {
    (assoc L) => {Assoc::Left};
    (assoc R) => {Assoc::Right};
    (ident $id:ident) => {$id};
    (ap $opk:ident ::
        $($prec:literal $assoc:tt $($ids:ident)+),+
    ) =>
        (impl $opk {
            pub fn get_prec(&self) -> usize {
                match self {
                    $($(Self::$ids => $prec,)+)+
                    _ => 100
                }
            }
            pub fn get_assoc(&self) -> Assoc {
                match self {
                    $($(Self::$ids => def_tok_info!(assoc $assoc),)+)+
                    _ => Assoc::Left
                }
            }
        });

}

def_tok_info! { ap BinOp ::
    2 L Or Or2,
    3 L And And2,
    4 L BitOr,
    5 L BitXor BitXor2,
    6 L BitAnd,
    7 L Equal NotEq,
    8 L Less LessEq Greater GreaterEq,
    9 L ShL ShR,
    10 R Conc Comb,
    11 L Plus Minus,
    12 L Times Div Mod Rem,
    13 R Pow
}

def_tok_set! { Kw is_kw ::
    "do" Do
    "let" Let
    "in" In
    "if" If
    "then" Then
    "else" Else
    "loop" Loop
    "case" Case
    "of" Of
    "where" Where
    "data" Data
    "fn" Fn
    "this" This
    "struct" Struct
    "import" Import
    "export" Export
}

#[cfg(test)]
mod test {
    use crate::{
        log_do,
        tok::stream::{CharStream, Streaming},
    };

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
        let boolean = Token::Bool(true, Pos::new());
        for t in [number, string, boolean].iter() {
            assert_eq!(t.is_atomic(), true);
        }
    }

    #[test]
    fn tok_ops() {
        let plus = Token::Operator("+".to_string(), Pos::new());
        let binop = BinOp::from_token(&plus);
        log_do!(
            is_unary => UnOp::is_unary(plus.literal().as_ref()),
            is_binary => BinOp::is_binary(plus.literal().as_ref()),
            bin_op => binop,
            assoc => BinOp::get_prec(binop.as_ref().unwrap()),
            plus_is_binary => BinOp::is_binary("+")
        );
    }
}
