use core::f64::consts;
use std::cell::RefCell;
use std::collections::HashMap;
use std::f64::consts::{
    FRAC_1_PI, FRAC_1_SQRT_2, FRAC_2_PI, FRAC_PI_2, FRAC_PI_3, FRAC_PI_4,
    FRAC_PI_6, FRAC_PI_8,
};
use std::fs::File;
use std::path::PathBuf;
use std::rc::Rc;
use std::time::{Instant, SystemTime, UNIX_EPOCH};

use super::environment::Envr;
use super::evaluator::Interpreter;

use crate::core::function::Lambda;
use crate::core::record::Record;
use crate::core::rygtype::Field;
use crate::core::rygval::UNIT;
use crate::evaluating::evaluator::walk;
use crate::parsing::parser::parse_input;
use crate::{
    core::{
        function::RygFn,
        rygtype::RygType,
        rygval::{RygVal, NIL},
    },
    parsing::expression::Shape,
    util::{
        constant::PI,
        state::Halt,
        types::{Either, Maybe},
    },
};

pub fn id<R>(item: R) -> R {
    item
}
// pub fn curry_lambda(lambda: RygVal) -> Maybe<RygVal> {
//     if let RygVal::Lambda(Lambda {
//         name,
//         pram,
//         body,
//         envr,
//     }) = &lambda
//     {
//         pram.iter().map(|p| )
//     }
// }

fn any_lambda() -> RygType {
    RygType::Lambda(vec![RygType::Any], Some(Box::new(RygType::Any)))
}

pub fn get_type(args: Vec<RygVal>) -> Maybe<RygVal> {
    if args.len() > 0 {
        let res = args
            .clone()
            .iter()
            .map(|arg| RygVal::Symbol(arg.type_string()))
            .collect::<Vec<RygVal>>();
        if let Some(fst) = res.first() {
            Ok(fst.to_owned())
        } else {
            Ok(RygVal::Vector(res.to_owned()))
        }
    } else {
        return Err(Halt::InvalidInput(
            "Expected 1 argument, but was provided none!".to_string(),
        ));
    }
}

macro_rules! duration {
    ($f:expr) => {
        let now = Instant::now();
        let res = $f;
        if let Ok(res) = res.clone() {
            println!("{}", &res);
            // println!("{} ns",
            // now.elapsed().as_nanos());
            println!("{} ms", now.elapsed().as_millis());
        };
        res
    };
}

pub fn load_time(env: &mut Envr) {
    let how_long = RygFn::new(
        "how'long",
        Either::Left(RygType::Union(vec![
            RygType::Function(
                Either::Left(Box::new(RygType::Never)),
                Box::new(RygType::Unknown),
            ),
            any_lambda(),
        ])),
        RygType::Unit,
        |arg| match arg.len() {
            1 => {
                if let Some(action) = arg.get(0) {
                    match action.clone() {
                        RygVal::Function(rf) => {
                            let now = Instant::now();
                            let res = rf.call(vec![]);
                            if let Ok(res) = res.clone() {
                                println!("{}", &res);
                                println!("{} ms", now.elapsed().as_millis());
                            };
                            res
                        }
                        RygVal::Lambda(mut lam) => {
                            let now = Instant::now();
                            let res = walk(*lam.body, &mut lam.envr);
                            if let Ok(ref res) = res {
                                println!("{}", res);
                                println!("{} ms", now.elapsed().as_millis());
                            };
                            res
                        }
                        _ => Err(Halt::InvalidType(format!(
                            "{} is not callable!",
                            action
                        ))),
                    }
                } else {
                    Err(Halt::InvalidInput(format!(
                        "Invalid arguments: {:?}",
                        arg
                    )))
                }
            }
            _ => {
                return Err(Halt::InvalidInput(format!(
                    "Expected a callable argument, but was provided with {:?}",
                    &arg
                )))
            }
        },
    );
}

impl From<std::io::Error> for Halt {
    fn from(er: std::io::Error) -> Self {
        Halt::Evaluating(format!("{}", er))
    }
}

pub fn load_fs(mut env: &mut Envr) {
    use crate::util::fs::read_text_file;
    let get_cwd = RygFn::new(
        "cwd",
        Either::Left(RygType::Unit),
        RygType::String,
        |_| {
            Ok(RygVal::String(if let Ok(path) = std::env::current_dir() {
                if let Some(s) = path.to_str() {
                    String::from(s)
                } else {
                    String::new()
                }
            } else {
                String::new()
            }))
        },
    );

    let read_txt_file = RygFn::new(
        "read'text'file",
        Either::Left(RygType::String),
        RygType::String,
        |arg| {
            if let [RygVal::String(rv)] = &arg[..] {
                /* TODO: check that is valid path */
                let path_str = &*rv;
                if path_str.is_empty() {
                    Err(Halt::InvalidInput(format!(
              "Invalid directory provided! `{}` is not a valid directory.",
              rv
            )))
                } else {
                    let path = PathBuf::from(&path_str);
                    let file = File::open(path);
                    let contents = std::fs::read_to_string(path_str);
                    match contents {
                        Ok(s) => Ok(RygVal::String(s)),
                        Err(e) => {
                            Err(Halt::UnknownError(format!("{:?}\n\n", e)))
                        }
                    }
                }
            } else {
                Err(Halt::InvalidInput(format!(
                    "Invalid arguments provided! Expected 1 String, but was provided with {:?}", &arg
                )))
            }
        },
    );

    let write_file = RygFn::new(
        "write'file",
        Either::Left(RygType::Record(vec![
            Field::new(String::from("path"), &RygType::String),
            Field::new(
                String::from("text"),
                &RygType::Union(vec![
                    RygType::String,
                    RygType::Vector(vec![RygType::Byte]),
                ]),
            ),
        ])),
        RygType::Union(vec![RygType::Unit, RygType::Halt]),
        |arg| {
            if let [RygVal::Dict(rec)] = arg.as_slice() {
                if let (
                    Some(RygVal::String(path)),
                    Some(RygVal::String(text)),
                ) = (
                    rec.get(&"path".to_string()),
                    rec.get(&"text".to_string()),
                ) {
                    use std::fs::File;
                    use std::io::prelude::*;
                    let mut f = File::create(path.as_str())?;
                    f.write_all(text.as_bytes())?;
                    f.sync_all().map_or_else(
                        |e| Err(Halt::Evaluating(format!("{}", e))),
                        |_| Ok(NIL),
                    )
                } else {
                    Err(Halt::InvalidType(format!("String")))
                }
            } else {
                Err(Halt::Evaluating(format!("Invalid input! Expected record with String fields `path` and `text`, but was provided {:?}", arg)))
            }
        },
    );

    let mut record = Record::new(HashMap::new());
    [get_cwd, read_txt_file, write_file]
        .into_iter()
        .for_each(|rfn| {
            record.set(&rfn.name, RygVal::Function(rfn.clone()));
        });

    let fs_mod = RygVal::Dict(record);
    env.def(String::from("Fs"), Some(&fs_mod));
}

pub fn load_core(mut env: &mut Envr) {
    let any_lambda: RygType =
        RygType::Lambda(vec![RygType::Any], Some(Box::new(RygType::Any)));

    let throw = RygFn::new(
        "throw",
        Either::Left(RygType::Any),
        RygType::Union(vec![RygType::Halt, RygType::Any]),
        |args| {
            Err(Halt::Evaluating(
                args.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join("\n"),
            ))
        },
    );

    let eval = RygFn::new(
        "eval'",
        Either::Left(RygType::String),
        RygType::Unknown,
        |arg| match arg.len() {
            1 => {
                if let Some(RygVal::String(src)) = arg.get(0) {
                    let mut inter = Interpreter::new();
                    inter.walk(&parse_input(src))
                } else {
                    return Err(Halt::InvalidInput(format!(
            "Expected argument of type String, but was provided with {:?}",
            &arg
          )));
                }
            }
            _ => {
                return Err(Halt::InvalidInput(format!(
                    "Expected 1 String argument, but was provided with {:?}",
                    &arg
                )));
            }
        },
    );

    let type_of = RygFn::new(
        "type'of",
        Either::Left(RygType::Any),
        RygType::Symbol(String::from("Type")),
        get_type,
    );

    let arity = RygFn::new(
        "arity'of",
        Either::Left(RygType::Lambda(
            vec![RygType::Any],
            Some(Box::new(RygType::Any)),
        )),
        RygType::Int,
        |arg| {
            if let [RygVal::Lambda(Lambda { pram, .. })] = &arg[..] {
                Ok(RygVal::from(pram.len()))
            } else {
                Err(Halt::InvalidType("* -> *".to_string()))
            }
        },
    );

    let length = RygFn::new(
        "len'of",
        Either::Left(RygType::Union(vec![
            RygType::String,
            RygType::Vector(vec![RygType::Any]),
            any_lambda.clone(),
            RygType::Record(vec![]),
        ])),
        RygType::Int,
        |args| {
            if let [rv] = args.as_slice() {
                match rv {
                    RygVal::String(st) | RygVal::Symbol(st) => {
                        Ok(RygVal::from(st.len()))
                    }
                    RygVal::Vector(vs) => Ok(RygVal::from(vs.len())),
                    RygVal::Error(h) => Err(h.to_owned()),
                    RygVal::Dict(rc) => Ok(RygVal::from(rc.entries.len())),
                    _ => Err(Halt::InvalidType(format!(
                        "Cannot take the length of non-iterable {}",
                        rv
                    ))),
                }
            } else {
                return Err(Halt::InvalidInput(format!(
                    "Expected 1 argument, but was provided with {}!",
                    args.len()
                )));
            }
        },
    );

    let print_0 = RygFn::new(
        "print",
        Either::Left(RygType::Any),
        RygType::Unit,
        |args| match &args[..] {
            [rv] => {
                print!("{}", rv);
                Ok(rv.to_owned())
            }
            _ => Err(Halt::InvalidInput(format!(
                "Expected 1 argument, but was provided {}!",
                args.len()
            ))),
        },
    );

    let print_ln = RygFn::new(
        "print'ln",
        Either::Left(RygType::Any),
        RygType::Unit,
        |args| {
            args.iter().for_each(|a| println!("{}", &a));
            Ok(RygVal::Nil())
        },
    );

    let to_string = RygFn::new(
        "to'string",
        Either::Left(RygType::Any),
        RygType::String,
        |arg| {
            if arg.len() > 0 {
                if let Some(rv) = arg.get(0) {
                    Ok(RygVal::String(rv.clone().to_string()))
                } else {
                    Ok(RygVal::String(String::new()))
                }
            } else {
                Ok(RygVal::String(String::new()))
            }
        },
    );

    [("nil", NIL)].iter().for_each(|(name, val)| {
        env.def(String::from(*name), Some(val));
    });

    let get_fn_ctx = RygFn::new(
        "inspect'ctx",
        Either::Left(any_lambda.clone()),
        RygType::Record(vec![]),
        |arg| {
            if let [lam @ RygVal::Lambda(Lambda { envr, .. }), ..] = &arg[..] {
                println!("{}", envr);
                Ok(lam.to_owned())
            } else {
                Err(Halt::InvalidType(format!(
                    "Expected a lambda, but instead got {}",
                    if let Some(a) = &arg.first() {
                        a
                    } else {
                        &&UNIT
                    }
                )))
            }
        },
    );

    [
        throw, eval, type_of, length, print_0, print_ln, to_string, get_fn_ctx,
    ]
    .iter()
    .for_each(|f| {
        env.def(f.clone().name, Some(&RygVal::Function(f.to_owned())));
    });
    load_fs(&mut env);
}

macro_rules! math_rs_float {
    ($name:ident()) => {
        RygFn::new(
            format!("{}", stringify!($name)).as_str(),
            Either::Left(RygType::Float), RygType::Float,
            |a| {
                if let Some(n) = a.get(0) {
                    match n {
                        RygVal::Float(q) => Ok(RygVal::Float(f64::$name(*q))),
                        RygVal::Int(q) => Ok(RygVal::Float(f64::$name(q.clone() as f64))),
                        x => Err(RygType::invalid_type(x, "Int or Float"))
                    }
                } else {
                    Err(
                        Halt::InvalidInput(
                            format!(
                                "Function {:?} expects {:?} arguments, but {:?} was/were provided", format!("{}'", stringify!($name)), 1, a.len()
                            )
                        )
                    )
                }
            }
        )
    };
}

/// Thin wrapper of imports from the Rust primitive f64 crate. Basic
/// trigonometric functions along with multiples of `PI` spanning the first
/// quadrant of the unit circle as well as two integer multiples: `PI`, and
/// `PI'2` corresponding to `2 * PI`.
pub fn load_math(env: &mut Envr) {
    let math_cos = math_rs_float!(cos());
    let math_sin = math_rs_float!(sin());
    let math_tan = math_rs_float!(tan());
    let mut math = Record::new(HashMap::new());
    [
        (String::from("PI"), RygVal::Float(PI)),
        (String::from("PI'2"), RygVal::Float(FRAC_PI_2)),
        (String::from("PI'3"), RygVal::Float(FRAC_PI_3)),
        (String::from("PI'4"), RygVal::Float(FRAC_PI_4)),
        (String::from("PI'6"), RygVal::Float(FRAC_PI_6)),
        (String::from("PI'7"), RygVal::Float(FRAC_PI_8)),
        (String::from("inv'PI"), RygVal::Float(FRAC_1_PI)),
        (String::from("inv'PI'2"), RygVal::Float(FRAC_2_PI)),
        (String::from("PI'2"), RygVal::Float(FRAC_1_SQRT_2)),
        (math_sin.name.clone(), RygVal::Function(math_sin)),
        (math_cos.name.clone(), RygVal::Function(math_cos)),
    ]
    .into_iter()
    .for_each(|(name, val)| {
        math.set(name, val.to_owned());
    });
    env.def(String::from("Math"), Some(&RygVal::Dict(math)));
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::evaluating::evaluator::{walk, Interpreter};
    use crate::log_do;
    use crate::parsing::parser::parse_input;

    fn run<'a>(src: &str, ctx: &mut Envr) -> Maybe<RygVal> {
        let res = walk(parse_input(src), ctx);
        println!("src: {}", src);
        println!(
            "> {:?}",
            match res.clone() {
                Ok(v) => v,
                Err(h) => RygVal::Error(h),
            }
        );
        res
    }

    #[test]
    fn test_sin() {
        let mut ctx = &mut Envr::new();
        load_math(&mut ctx);
        let src = "sin'(PI);";
        let res = walk(parse_input(src), ctx);
        println!("{:?}", res.unwrap())
    }

    #[test]
    fn test_type_of() {
        let mut ctx = &mut Envr::new();
        load_core(&mut ctx);
        let src = "type'of(3)";
        let _ = run(src, ctx);
    }

    #[test]
    fn inspect_read_file_text() {
        let pth = "/Users/lictor/Projects/wyg-lang-rs/examples/fibonacci.wg";

        let src = format!("{} ( {}{}{} );", "read'file'text", '"', pth, '"');
        let mut ctx = &mut Envr::new();

        log_do!(
          "fs test" => std::fs::read_to_string(pth),
          "preload" => &ctx,
          "load" => {load_core(ctx); &ctx},
          "source" => format!("{}", &src),
          "result" => match walk(parse_input(&src), ctx) {
            Ok(v) => format!("{}", &v),
            Err(h) => format!("{}", &h)
          },
          "scope" => &ctx
        )
    }
}
