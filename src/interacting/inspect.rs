use std::{
    cell::RefCell,
    collections::HashMap,
    io::{self, Read, Write},
};

use crate::{
    core::rygval::RygVal,
    evaluating::{
        environment::Envr,
        evaluator::{walk, Interpreter},
        native::{load_core, load_math},
    },
    tok::{
        lexer::{is_left_punct, tokenize_input},
        token::Token,
    },
    util::display::{Color, Paint},
};
use crate::{
    parsing::{expression::Expr, parser::parse_input},
    util::state::Halt,
};
