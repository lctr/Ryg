use std::{
    collections::HashMap,
    iter::Map,
    ops::{Add, RangeFrom},
    rc::Rc,
    vec::IntoIter,
};

use crate::{
    evaluating::{environment::Envr, evaluator::walk},
    parsing::expression::{Definition, Expr, Shape},
    tok::token::Token,
    util::{
        state::Halt,
        types::{Maybe, Name},
    },
};

use super::{
    function::RygFn,
    rygtype::{Field, RygType},
    rygval::RygVal,
};

#[derive(Clone, Debug, PartialEq)]
pub struct RygVec {
    body: Vec<RygVal>,
    envr: Envr,
}

impl RygVec {
    pub fn new(body: Vec<RygVal>, envr: Envr) -> Self {
        Self { body, envr }
    }
    pub fn get(&self, index: usize) -> Option<&RygVal> {
        self.body.get(index)
    }
    pub fn get_mut(&mut self, index: usize) -> Option<&mut RygVal> {
        self.body.get_mut(index)
    }
    pub fn clear(&mut self) {
        self.body.clear();
    }
    pub fn push(&mut self, item: RygVal) {
        self.body.push(item)
    }
}

impl Default for RygVec {
    fn default() -> Self {
        Self {
            body: vec![],
            envr: Envr::new(),
        }
    }
}

impl IntoIterator for RygVec {
    type Item = RygVal;

    type IntoIter = IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.body.into_iter()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RygIter<N> {
    pub range: RangeFrom<N>,
}

impl RygIter<i32> {
    pub fn new(range: RangeFrom<i32>) -> Self {
        Self { range }
    }
}

impl Iterator for RygIter<i32> {
    type Item = RygVal;
    fn next(&mut self) -> Option<Self::Item> {
        self.range.next().map(RygVal::from)
    }
}

#[derive(Clone, Debug)]
pub struct Range<R>(pub usize, pub Option<usize>, pub Option<R>);

#[derive(Clone, Debug)]
pub struct RygList {
    vars: Vec<Range<(RygType, Maybe<Vec<RygVal>>)>>,
    ranges: Vec<usize>,
    index: usize,
    current: Option<Rc<RygVal>>,
    limit: Option<usize>,
    scope: Envr,
    item: Expr,
    pub kind: RygType,
    conds: Vec<Expr>,
}

impl RygList {
    pub fn new(
        env: Envr,
        vars: Vec<(RygType, Maybe<Vec<RygVal>>)>,
        fixed: Vec<(Field, RygVal)>,
        conds: Vec<Expr>,
        item: Expr,
    ) -> Self {
        let mut kind = RygType::Unknown;
        // let mut limit = vars.len();
        let iters = vars.clone();
        let vars = vars
            .iter()
            .map(|(a, b)| {
                let start = 0;
                let len = b
                    .clone()
                    .and_then(|v| Ok(v.len()))
                    .map_or(None, |lim| Some(lim));
                let body = Some((a.to_owned(), b.to_owned()));
                Range(start, len, body)
                // Range(0, b.and_then(|o| o.len())), (a.to_owned(),
                // b.to_owned())
            })
            .collect::<Vec<_>>();
        let ranges = iters
            .clone()
            .iter()
            .filter_map(|(rt, rv)| {
                kind = rt.clone();
                rv.to_owned().ok()
            })
            .map(|v| v.len())
            .collect::<Vec<_>>();
        let limit = Some(ranges.clone().iter().product());
        Self {
            vars,
            ranges,
            index: 0,
            limit,
            scope: env,
            current: None,
            conds,
            kind,
            item,
        }
    }
}

// impl Iterator for RygList {
//   type Item = RygVal;
//   fn next(&mut self) -> Option<Self::Item> {
//     self.vars.map()
//   }
// }

fn _walk_list(defn: Definition, env: &mut Envr) -> Maybe<RygVal> {
    let Definition {
        // expression to be called iteratively
        item,
        ranges: range,
        fixed,
        conds,
    } = defn;
    let scope = &mut env.extend();

    println!("{:?}", (item, range, fixed, conds));
    Ok(RygVal::Bool(true))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::log_do;
    use crate::parsing::parser::parse_input;
    use crate::{
        evaluating::{
            environment::Envr,
            evaluator::{walk, Interpreter},
        },
        util::types::Either,
    };

    #[test]
    fn inspect_list_1<'a, 'b: 'a>() {
        let expr = parse_input("[x | x <- [1, 2, 3]]");
        let interpreter = Interpreter::new();
        let env = &mut Envr::new();
        let list_ = walk(expr, env);
        if let Ok(RygVal::List(ref list)) = list_ {
            log_do! {
                "vars" => &list.vars,
                "ranges" => &list.ranges,
                "index" => &list.current,
                "current" => &list.current,
                "limit" => &list.limit,
                "scope" => &list.scope
            }
        };
        println!("{:#?}", list_)
    }
}
