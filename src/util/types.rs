use std::rc::Rc;

use super::state::Halt;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Either<L, R> {
  Left(L),
  Right(R),
}

pub type Maybe<X> = Result<X, Halt>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Name(String);

#[derive(Clone, Debug, PartialEq)]
pub struct Kind<T>(pub T);

#[derive(Debug)]
pub struct List<T>(pub Option<T>, pub Option<Rc<List<T>>>, pub usize, usize);

impl<T: std::fmt::Debug + Clone> From<Vec<T>> for List<T> {
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
