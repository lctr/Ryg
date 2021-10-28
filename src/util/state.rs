use std::{
    collections::HashSet,
    fmt::{self, Debug, Display, Error},
    ops::Deref,
    rc::Rc,
};

use crate::{core::rygval::RygVal, util::display::Paint};

use super::types::Maybe;

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum StreamState<A> {
    Ready(Option<A>),
    Pending(A),
    Halted(A),
    Done(A),
}

#[allow(unused)]
impl<A> StreamState<A> {
    pub fn new(init: Option<A>) -> Self {
        Self::Ready(init)
    }
    /// Returns `true` if the parser status is [`Ready`].
    ///
    /// [`Ready`]: ParserStatus::Ready
    pub fn is_ready(&self) -> bool {
        matches!(self, Self::Ready(_))
    }

    /// Returns `true` if the parser status is [`Pending`].
    ///
    /// [`Pending`]: ParserStatus::Pending
    pub fn is_pending(&self) -> bool {
        matches!(self, Self::Pending(..))
    }

    /// Returns `true` if the parser status is [`Halted`].
    ///
    /// [`Halted`]: ParserStatus::Halted
    pub fn is_halted(&self) -> bool {
        matches!(self, Self::Halted(..))
    }

    /// Returns `true` if the parser status is [`Complete`].
    ///
    /// [`Complete`]: ParserStatus::Complete
    pub fn is_complete(&self) -> bool {
        matches!(self, Self::Done(_))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Halt {
    Evaluating(String),
    UnknownError(String),
    InvalidInput(String),
    Unexpected(String),
    DivisionByZero(String),
    ReplError(String),
    Many(Vec<Halt>),
    RefGetError(String),
    RefSetError(String),
    InvalidType(String),
}

impl Halt {
    // pub fn expected_arg() {}
    // pub fn expected_arg_of_type() {}
    pub fn is_many(&self) -> bool {
        if let Self::Many(_) = self {
            true
        } else {
            false
        }
    }
    pub fn to_vec(&self) -> Vec<Halt> {
        if let Self::Many(v) = self {
            v.to_owned()
        } else {
            vec![self.to_owned()]
        }
    }
    pub fn insert<'h: 'i, 'i: 'h>(&mut self, halt: Halt) {
        if let Self::Many(errs) = self {
            errs.push(halt);
        } else {
            let me = Halt::Many(vec![self.to_owned(), halt]);
            std::mem::replace(self, me);
        }
    }
}

impl Display for Halt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Halt::UnknownError(m)
            | Halt::DivisionByZero(m)
            | Halt::Unexpected(m)
            | Halt::InvalidType(m)
            | Halt::InvalidInput(m) => write!(f, "{}", m),
            Halt::RefGetError(m) | Halt::RefSetError(m) => {
                write!(f, "{} is out of scope!", m)
            }
            Halt::Evaluating(m) => write!(f, "Evaluator error! {}", m),
            Halt::ReplError(e) => {
                write!(f, "{} {}", Paint::fg_red("Error!"), e)
            }
            Halt::Many(v) => f.debug_list().entries(v.iter()).finish(), /* write!(f, "Errors: {:?}", v), */
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_append() {
        println!("{}", "\\");
        let h1 = Halt::Unexpected("Error 1".to_owned());
        let h2 = Halt::Unexpected("Error 2".to_owned());
        let mut h3 = Halt::Many(vec![h1]);
        h3.insert(h2);
        println!("{}", h3)
    }
}
