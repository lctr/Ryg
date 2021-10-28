use std::{cell::RefCell, rc::Rc};

use crate::evaluating::environment::{self, Envr};
use crate::parsing::expression::{DataDef, DataVariant, Definition};
use crate::util::types::{Either, Kind, Maybe, Name};

use super::function::*;
use super::record::Record;
use super::rygtype::RygType;
use super::rygval::RygVal;

// pub const

#[derive(Clone, Debug)]
pub struct RygEnum {
    pub name: Kind<String>,
    pub which: Kind<String>,
    variants: Vec<DataVariant>,
    envr: Envr,
    inner: Box<RygVal>,
}

impl RygEnum {
    pub fn new(
        name: Kind<String>,
        which: Kind<String>,
        variants: Vec<DataVariant>,
        envr: Envr,
        inner: RygVal,
    ) -> Self {
        Self {
            name,
            which,
            variants,
            envr,
            inner: Box::new(inner),
        }
    }
    pub fn find_variant(&self, variant_name: String) -> Option<DataVariant> {
        self.variants
            .iter()
            .find(|DataVariant { ident, .. }| {
                ident.0.literal() == variant_name
            })
            .cloned()
    }
    pub fn match_variant(&self, variant: Self) -> bool {
        self.name == variant.name && self.which == variant.which
    }
    pub fn unwrap(self) -> RygVal {
        *self.inner
    }
    pub fn map<F>(self, mut closure: F) -> Self
    where
        F: FnMut(&mut Self) -> RygVal, {
        let mut item = self.clone();
        item.inner = Box::new(closure(&mut item));
        item
    }
}

impl PartialEq for RygEnum {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.which == other.which
            && self.variants == other.variants
            && self.envr == other.envr
            && self.inner == other.inner
    }
}

impl From<RygEnum> for RygType {
    fn from(variant: RygEnum) -> Self {
        RygType::Data(
            variant.name,
            variant.which,
            Either::Left(Box::new(RygType::from(*variant.inner))),
        )
    }
}

impl std::fmt::Display for RygEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.name, self.inner.as_ref())
    }
}
