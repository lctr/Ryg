use std::borrow::Cow;

/// Customizable trait to render an item into a string. Similar to the Display
/// trait, except not buffered and corresponds to the actual String value
/// either initially read or further processed. The `to_string` method may be
/// used to vary string representations and is by default an alias of the
/// `literal` method.
pub trait Literal<S = String> {
    type Lit: Clone + std::fmt::Display + std::cmp::PartialEq + Into<S>;
    fn literal(&self) -> Self::Lit;
    fn string(&self) -> Self::Lit {
        self.literal()
    }
    fn match_literal(&self, other: impl Into<Self::Lit>) -> bool {
        self.literal() == other.into()
    }
    fn match_any_of<T>(&self, others: &[T]) -> bool
    where
        T: Into<Self::Lit> + Clone, {
        others
            .to_vec()
            .iter()
            .any(move |other| self.match_literal(other.to_owned()))
    }
}

impl Literal for String {
    type Lit = String;
    fn literal(&self) -> Self::Lit {
        self.clone()
    }
}

impl Literal for &str {
    type Lit = String;
    fn literal(&self) -> Self::Lit {
        self.to_string()
    }
}
