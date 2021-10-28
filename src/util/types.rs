use std::{
    fmt::{self, Debug, Display},
    rc::Rc,
};

use super::state::Halt;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<L, R> Either<L, R> {
    pub fn is_left(&self) -> bool {
        matches!(self, Self::Left(..))
    }
    pub fn is_right(&self) -> bool {
        matches!(self, Self::Right(..))
    }
    pub fn unwrap_left(self) -> L {
        if let Self::Left(left) = self {
            left
        } else {
            panic!(format!("unwrap_left called on Right variant",))
        }
    }
    pub fn unwrap_right(self) -> R {
        if let Self::Right(right) = self {
            right
        } else {
            panic!(format!("unwrap_right called on Left variant",))
        }
    }
}

pub type Maybe<X> = Result<X, Halt>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Name(String);

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Kind<T>(pub T);

impl<T> Kind<T>
where
    T: Display,
{
    pub fn map<F, U>(self, f: F) -> Kind<U>
    where
        F: Fn(Self) -> U, {
        Kind(f(self))
    }
}

impl<T: Display> std::fmt::Debug for Kind<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{}", &(self.0))
        } else {
            write!(f, "{}", &(self.0))
        }
    }
}

impl<T: Display> std::ops::Add for Kind<T> {
    type Output = String;
    fn add(self, rhs: Self) -> Self::Output {
        format!("{} {}", self.0.to_string(), rhs.0.to_string())
    }
}

#[derive(Debug)]
pub struct List<T>(pub Option<T>, pub Option<Rc<List<T>>>, pub usize, usize);

impl<T: Debug + Clone> From<Vec<T>> for List<T> {
    fn from(vts: Vec<T>) -> Self {
        let len = vts.len();
        let mut list: List<T> = List(None, None, len.clone() - 1, len.clone());
        for (i, vt) in vts.iter().rev().enumerate() {
            list = List(
                Some(vt.clone()),
                Some(Rc::new(list)),
                len - i - 1,
                len.clone(),
            );
        }
        list
    }
}

mod test {
    use super::*;

    #[test]
    fn test_list() {
        let arr = vec!["hi", "hello", "what's up"];
        let list = List::from(arr);
        println!("{:#?}", list)
    }
}
