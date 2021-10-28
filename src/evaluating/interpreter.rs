// use std::{
//     cell::{Cell, RefCell},
//     collections::HashMap,
//     fmt::Display,
//     ops::IndexMut,
//     rc::Rc,
// };

// use crate::{
//     ast::expression::{Binding, Definition, Expr, Morpheme, Parameter,
// Shape},     core::{
//         function::Lambda,
//         list::RygList,
//         record::{self, Record},
//         rygtype::{Field, RygType},
//         rygval::{RygVal, FALSE, NIL, TRUE, UNIT},
//     },
//     list_compr,
//     tok::{stream::Pos, token::Token},
//     util::state::Halt,
//     util::types::{Either, Kind, Maybe},
// };

// use super::{
//     environment::{self, Envr},
//     native::{load_core, load_math},
// };

// #[derive(Debug)]
// pub struct Context<'a>(pub String, pub &'a mut Envr, pub Maybe<RygVal>);

// #[derive(Debug)]
// pub struct Interpreter<'a> {
//     root: Rc<RefCell<Envr>>,
//     next_scope: Option<&'a mut Envr>,
//     last: Option<Maybe<RygVal>>,
// }

// impl<'a> Interpreter<'a> {
//     pub fn new() -> Self {
//         let mut root = RefCell::new(Envr::new());
//         load_core(root.get_mut());
//         load_math(root.get_mut());
//         Self {
//             root: Rc::new(root),
//             next_scope: None,
//             last: None,
//         }
//     }

//     pub fn eval_error(expr: Expr) -> Halt {
//         Halt::Evaluating(format!("Unable to evaluate {:?}", expr))
//     }
// }

// pub trait Interpret {}

// pub struct Space {
//     name: String,
//     size: usize,
//     envr: Envr,
// }
