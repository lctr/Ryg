use std::{cell::RefCell, rc::Rc};

use super::rygtype::RygType;

pub struct This<'this, V: Clone>(Rc<&'this V>);

impl<'this, V> This<'this, V>
where
    V: Clone,
{
    pub fn new(inner: &'this V) -> Self {
        Self(Rc::new(inner))
    }
    pub fn get(&self) -> &'this V {
        self.0.as_ref()
    }
    pub fn get_mut(&'this mut self) -> Option<&mut &'this V> {
        // &mut self.0
        Rc::get_mut(&mut self.0)
    }
    pub fn set<'that: 'this>(&mut self, value: &'that V) -> &'this V {
        let old = self.0.to_owned();
        self.0 = Rc::new(value);
        &old
    }
    pub fn apply<F, X, Y>(&self, mut op: F, args: &[X]) -> Y
    where
        F: FnMut(&'this V, &[X]) -> Y, {
        op(self.get(), args)
    }
}
