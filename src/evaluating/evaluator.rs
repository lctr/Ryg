use std::{
    borrow::Borrow,
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt::Display,
    ops::IndexMut,
    rc::Rc,
};

use crate::{
    core::{
        function::Lambda,
        list::{RygIter, RygList},
        record::{self, Record},
        rygtype::{Field, RygType},
        rygval::{Container, RygVal, FALSE, NIL, TRUE, UNIT},
        variant::RygEnum,
    },
    list_compr,
    parsing::expression::{
        Binding, DataVariant, Definition, Expr, Morpheme, Parameter, Program,
        Shape, VariantArg,
    },
    tok::{stream::Pos, token::Token},
    util::state::Halt,
    util::types::{Either, Kind, Maybe},
};

use super::{
    environment::{self, Envr},
    native::{load_core, load_math},
};

#[derive(Debug)]
pub struct Context<'a>(
    pub String,
    pub &'a mut Rc<RefCell<Envr>>,
    pub Maybe<RygVal>,
);

#[derive(Debug)]
pub struct Interpreter<'a> {
    global_scope: RefCell<Envr>,
    cache: Vec<&'a mut Context<'a>>,
    last: Option<Maybe<RygVal>>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        let mut global_scope = RefCell::new(Envr::new());
        load_core(global_scope.get_mut());
        load_math(global_scope.get_mut());
        Interpreter {
            global_scope,
            cache: vec![],
            last: None,
        }
    }
    pub fn get_by_name(
        &mut self,
        env: &mut Envr,
        name: String,
    ) -> Maybe<RygVal> {
        if let Some(e) = env.lookup(&name) {
            e.clone().get(&name).ok_or(Halt::RefGetError(name))
        } else {
            Err(Halt::RefGetError(name))
        }
    }

    pub fn get_global(&mut self) -> &mut Envr {
        self.global_scope.get_mut()
    }
    pub fn eval_error(expr: Expr) -> Halt {
        Halt::Evaluating(format!("Unable to evaluate {:?}", expr))
    }
    pub fn walk(&'a mut self, expr: &Expr) -> Maybe<RygVal> {
        let my_scope = &mut self.global_scope;
        let cc = my_scope.get_mut();
        let res = walk(expr.clone(), cc);
        res
    }
}

pub fn walk(expr: Expr, mut env: &mut Envr) -> Maybe<RygVal> {
    match expr {
        Expr::Nil => Ok(NIL),
        Expr::Literal(t) => walk_literal(t, &mut env),
        Expr::Assign(o, l, r) => walk_assign(o, l, r, &mut env),
        Expr::Unary(o, r) => walk_unary(o, r, &mut env),
        Expr::Binary(o, l, r) => walk_binary(o, l, r, &mut env),
        Expr::Lambda(n, p, b) => walk_lambda(n, p, b, &mut env),
        Expr::Conditional(i, t, d) => walk_conditional(i, t, d, &mut env),
        Expr::Block(is_do, p) => walk_block(is_do, p, &mut env),
        Expr::Call(f, a, n) => walk_call(f, a, n, &mut env),
        Expr::Constant(bs) => walk_constant(bs, &mut env),
        Expr::Variable(a, b) => walk_let(a, b, &mut env),
        Expr::Tuple(b) => walk_tuple(b, &mut env),
        Expr::Vector(b) => walk_vector(b, &mut env),
        Expr::Member(b, k) => walk_member(b, k, &mut env),
        Expr::Index(b, i) => walk_index(b, i, &mut env),
        Expr::Iter(h, t) => walk_iter(h, t, &mut env),
        Expr::Range(i, a, b) => walk_range(i, a, b, &mut env),
        Expr::Loop(c, b) => walk_loop(c, b, &mut env),
        Expr::Case(p, c, d) => walk_case(p, c, d, &mut env),
        Expr::Record(k, b) => walk_record(k, b, &mut env),
        Expr::List(defn) => walk_list(*defn, &mut env),
        Expr::Pipe(arg, pipes) => walk_piped(arg, pipes, &mut env),
        Expr::Program(Program {
            name: _,
            body,
            vocab,
        }) => {
            let res = body.into_iter().fold(Ok(NIL), |a, c| walk(c, &mut env));
            res
        }
        Expr::Data(kind, variants) => walk_data(kind, variants, &mut env),
        _ => Err(Halt::UnknownError(format!(
            "Could not evaluate {:#?}",
            expr
        ))),
        Expr::Error(h, _t) => Err(h),
        Expr::Return(_, _) => todo!(),
        Expr::Named(a, b) => walk_named(a, b, &mut env),
    }
}

fn walk_constant(bindings: Vec<Binding>, mut env: &mut Envr) -> Maybe<RygVal> {
    for Binding { pram, expr } in bindings.into_iter() {
        let rhs = walk(expr, &mut env);
        let mut err: Maybe<RygVal> = Err(Halt::Unexpected(String::new()));
        match rhs {
            Ok(rv) => {
                pat_match(pram.pattern, rv, &mut env, |scope, name, value| {
                    let def = scope.def_const(&name, value);
                    if def.is_err() {
                        err = Err(def.unwrap_err());
                    };
                });
            }
            Err(h) => err = Err(h),
        }
    }
    Ok(NIL)
}

fn walk_named(
    kind: Kind<Token>,
    body: Box<Expr>,
    mut env: &mut Envr,
) -> Maybe<RygVal> {
    Ok(RygVal::Holder(Container(
        kind.map(|t| t.0.literal()),
        Box::new(walk(*body, env)?),
    )))
}

fn walk_literal(t: Token, env: &mut Envr) -> Maybe<RygVal> {
    if t.is_identifier() || t.is_meta() {
        let scope = env.lookup(&t.clone().literal());
        match scope {
            Some(ctx) => ctx
                .get(&t.to_string())
                .ok_or(Halt::RefGetError(t.to_string())),
            None => Err(Halt::RefGetError(t.to_string())),
        }
    } else {
        Ok(RygVal::from_token(t))
    }
}

fn walk_data(
    kind: Kind<Token>,
    variants: Vec<DataVariant>,
    mut env: &mut Envr,
) -> Maybe<RygVal> {
    println!("WIP! parsed:\nkind: {}\nvariants: {:#?}", &kind, &variants);
    Ok(NIL)
}

fn walk_piped(
    arg: Box<Expr>,
    pipes: Vec<Expr>,
    mut env: &mut Envr,
) -> Maybe<RygVal> {
    pipes
        .iter()
        .rev()
        .fold(walk(*arg, &mut env), |a, c| match a {
            Ok(res) => match walk(c.clone(), &mut env) {
                Ok(val) => match val {
                    RygVal::Lambda(mut lam) => {
                        Ok(lam.apply(res.to_vec(), &mut env)?)
                    }
                    RygVal::Function(rf) => Ok(rf.call(res.to_vec())?),
                    _ => Err(Halt::InvalidType(format!("Callable"))),
                },
                Err(h) => Err(h),
            },
            Err(h) => Err(h),
        })
}

fn walk_bind(
    left: Box<Expr>,
    right: Box<Expr>,
    mut env: &mut Envr,
) -> Maybe<RygVal> {
    match *left.clone() {
        Expr::Literal(tok) if tok.is_identifier() => {
            let key = tok.literal();
            let new_left = env.get(&key);
            let mut res = Err(Halt::Evaluating("".to_string()));
            env.def(key.clone(), None);
            env.walk(&mut |mut scope: &mut Envr| {
                let val = walk(*right.clone(), scope);
                res = val.clone();
                if let Ok(rv) = val {
                    scope.set(&key, rv);
                };
            });
            Ok(if let Some(result) = env.get(&key) {
                result
            } else {
                NIL
            })
        }
        Expr::Index(a, b) => {
            if let Expr::Literal(Token::Identifier(k, _)) = *a.clone() {
                let rt = walk_assign(
                    Token::Operator("<-".to_string(), Pos::faux()),
                    a.clone(),
                    right.clone(),
                    &mut env,
                );
                if let Ok(rv) = rt.clone() {
                    env.set(&k, rv);
                    rt
                } else {
                    Ok(NIL)
                }
            } else {
                walk_index(left, right, &mut env)
            }
        }
        left => {
            // let mut block = vec![];
            walk(Expr::Block(true, vec![left, *right]), &mut env)
        }
    }
}

pub fn get_member_path(expr: &Expr) -> Either<&Token, Vec<&Token>> {
    let mut path = vec![];
    match expr {
        Expr::Index(a, id) => get_member_path(&**a),
        Expr::Member(root, prop) => {
            let tok = get_member_path(root);
            match tok {
                Either::Left(t) => path.push(t),
                Either::Right(ts) => ts.iter().for_each(|t| path.push(*t)),
            };
            path.push(prop);
            if path.len() == 1 {
                Either::Left(prop)
            } else {
                Either::Right(path)
            }
        }

        Expr::Literal(tok) => {
            if tok.is_identifier() || tok.is_meta() {
                Either::Left(tok)
            } else {
                Either::Right(vec![])
            }
        }
        Expr::Block(_, exps) if exps.len() == 1 => {
            if let Some(n) =
                &exps.first().and_then(|a| Some(get_member_path(a)))
            {
                n.to_owned()
            } else {
                Either::Right(path)
            }
        }
        Expr::Call(fun, args, name) => get_member_path(fun.as_ref()),
        Expr::Block(_, xs) | Expr::Program(Program { body: xs, .. }) => {
            if let Some(x) = xs.last() {
                get_member_path(x)
            } else {
                Either::Right(path)
            }
        }

        _ => Either::Right(path),
    }
}

fn walk_assign(
    op: Token,
    l: Box<Expr>,
    r: Box<Expr>,
    mut env: &mut Envr,
) -> Maybe<RygVal> {
    let err = || -> Maybe<RygVal> {
        Err(Halt::RefSetError(format!(
            "Unable to evaluate (assignment) operator {} on:
    left: {},
    right: {}",
            &op, &l, &r
        )))
    };
    if op.match_literal("=<") {
        return walk_bind(l, r, &mut env);
    };
    let mut rhs = walk(*r.clone(), env)?;
    let mut var_name = String::new();
    match *l.clone() {
        Expr::Member(body, index) => {
            let _body = *body.clone();
            let member_path = get_member_path(&_body);
            let mut obj = walk(*body, &mut env)?;
            if let RygVal::Dict(rec) = obj {
                let mut record = rec;
                let key = index.literal();
                if record.contains_key(&key) {
                    record.set(&key, rhs.clone());
                    if let Either::Left(tok) = member_path {
                        obj = RygVal::Dict(record);
                        env.set(&tok.literal(), obj.to_owned());
                    }
                    Ok(rhs)
                } else {
                    err()
                }
            } else {
                err()
            }
        }
        Expr::Index(body, index) => {
            let _body = *body.clone();
            let idx = walk(*index.clone(), &mut env)?;
            let mut obj = walk(*body, &mut env)?;
            match obj.clone() {
          RygVal::String(s) => err(),
          RygVal::Vector(mut vs) => {
            if obj.accepts_index(&idx) {
              if let Some(w) = idx.as_usize() {
                *vs.index_mut(w) = rhs.to_owned();
                obj = RygVal::Vector(vs.clone());
                if let Expr::Literal(tok) = _body {
                    env.set(&tok.literal(), obj.to_owned());
                }
                Ok(RygVal::Vector(vs))
              } else {
                Err(Halt::InvalidInput(format!("Index {} is out of bounds for {}", idx, &obj)))
              }
            } else { err() }
          }
          RygVal::Tuple(_, vs) => {
            Err(Halt::Evaluating(format!("Tuples are immutable and cannot have their components assigned to! Unable to change index {} of tuple {}", idx, &obj)))
          }
          _ => err(),
        }
        }
        Expr::Literal(Token::Identifier(id, _)) => {
            let val = walk(*(r.clone()), &mut env)?;

            match op.literal().as_str() {
                "<-" => {
                    match env.lookup(&id) {
                        Some(v) => {
                            v.set(&id, val.clone());
                        }
                        None => {
                            if env.is_single() {
                                env.def(id, Some(&val));
                            } else {
                                let mut scope = env.extend();
                                scope.def(id, Some(&val));
                                *env = scope;
                            }
                        }
                    };
                    return Ok(val);
                }
                ":=" => {
                    if env.has(&id) {
                        Err(Halt::Evaluating(format!("Unable to define existing binding {}. Perhaps you meant to modify a variable binding (using `=`, `+=`, `-=`, etc.) if not constant, or `<-` or `=<`", &id)))
                    } else {
                        env.def(id, Some(&val));
                        Ok(val)
                    }
                }
                "=" => {
                    // println!("{:#?}", &env);
                    // if env.is_orphan() {
                    let res = env.set(&id, val);
                    if let Some(RygVal::Error(err)) = res {
                        Err(err)
                    } else {
                        env.get(&id).ok_or(Halt::RefSetError(id))
                    }
                    // } else {
                    //     let mut ret = err();
                    //     if let Some(sc) = env.lookup(&id) {
                    //         sc.modify(
                    //             &id,
                    //             &mut |found: bool, it: &mut RygVal| {
                    //                 if found {
                    //                     ret = Ok(val.clone());
                    //                     *it = val.to_owned();
                    //                 }
                    //             },
                    //         );
                    //     }
                    //     ret
                    // }
                }

                "+=" | "-=" | "*=" | "/=" => {
                    let before = env.get(&id);
                    if let Some(bv) = before {
                        let after = apply_op(
                            Token::Operator(
                                op.literal()
                                    .chars()
                                    .nth(0)
                                    .unwrap()
                                    .to_string(),
                                op.get_pos().unwrap().to_owned(),
                            ),
                            bv,
                            val,
                        );
                        if let Ok(aft1) = after {
                            env.set(&id, aft1.to_owned());
                            Ok(aft1)
                        } else {
                            err()
                        }
                    } else {
                        err()
                    }
                }
                "<|" => err(),
                _ => err(),
            }
        }
        _ => err(),
    }
}

fn walk_record(
    kind: Kind<Token>,
    bindings: Vec<Binding>,
    mut env: &mut Envr,
) -> Maybe<RygVal> {
    let mut map = HashMap::new();
    let mut pairs: Vec<(String, RygVal)> = vec![];
    bindings.iter().for_each(|Binding { pram, expr }| {
        let id = pram.clone().name;
        let key = id.literal();
        // property shorthand -- if variable name matches field name and is in
        // scope, value is inferred
        if matches!(&expr, &Expr::Nil) {
            if let Some(val) = env.get(&key) {
                map.insert(key, val);
            } else {
                map.insert(key, NIL);
            }
        } else {
            let val = if let Ok(rygval) = walk(expr.clone(), &mut env) {
                rygval
            } else {
                RygVal::Error(Halt::Evaluating(format!(
                    "Unable to evaluate {:?}",
                    &expr
                )))
            };
            map.insert(key, val);
        }
    });
    Ok(RygVal::Dict(Record::new(map)))
    // let record = RygVal::Dict(record);
    // match kind.0 {
    //     Token::Identifier(n, ..) | Token::Meta(n, ..) => {
    //         env.def(n, Some(&record));
    //         Ok(record)
    //     }
    //     _ => Ok(record),
    // }
}

fn walk_loop(
    cond: Box<Expr>,
    body: Box<Expr>,
    mut env: &mut Envr,
) -> Maybe<RygVal> {
    let mut scope = RefCell::new(env.clone());
    let mut value = Ok(NIL);
    let mut err: Maybe<RygVal>;
    if let Expr::Iter(a, b) = *cond.clone() {
        if let Ok(RygVal::Vector(vs)) = walk_iter(a, b, scope.get_mut()) {
            value = vs
                .iter()
                .fold(value, move |a, c| walk(*body.clone(), scope.get_mut()));
        }
    } else {
        env.walk(|mut ctx| {
            ctx.inject(scope.get_mut());
            let mut test = &mut walk(*cond.clone(), scope.get_mut());
            while let Ok(RygVal::Bool(true)) = *test {
                match walk(*body.clone(), &mut ctx) {
                    Ok(res) => value = Ok(res),
                    Err(h) => {
                        value = Err(h);
                        break;
                    }
                };
                *test = walk(*cond.clone(), &mut ctx);
            }
        });
    }
    value
}

fn walk_range(
    iter: Box<Expr>,
    start: Box<Expr>,
    end: Option<Box<Expr>>,
    env: &mut Envr,
) -> Maybe<RygVal> {
    todo!()
}

// bodyless ranges -- numerical ranges
fn walk_iter(
    x: Box<Expr>,
    t: Option<Box<Expr>>,
    mut env: &mut Envr,
) -> Maybe<RygVal> {
    fn err<K: Display>(point: K, label: &str) -> Maybe<K> {
        Err(Halt::InvalidType(format!(
            "{} is not a valid range {} position!",
            point, label
        )))
    }
    let lower_val = walk(*x.clone(), &mut env);
    match (lower_val.clone(), t.clone()) {
        (Ok(low_val), Some(hi_val)) => {
            let rval = walk(*hi_val, &mut env);
            let (i0, j0) = match RygVal::coerce_nums(low_val, rval?)? {
                Either::Left(x) => x,
                Either::Right(y) => (y.0.floor() as i32, y.1.floor() as i32),
            };
            let rng = (i0..j0)
                .into_iter()
                .map(|n| RygVal::from(n))
                .collect::<Vec<_>>();
            return Ok(RygVal::Vector(rng));
        }
        (Ok(low_val), None) => {
            let start: Maybe<i32> = match low_val {
                RygVal::Int(n) => Ok(n),
                RygVal::Float(n) => Ok(n.floor() as i32),
                _ => return err(low_val, "start"),
            };
            if let Ok(idx) = start {
                return Ok(RygVal::Iter(Rc::new(RygIter::new(
                    (idx..).into_iter(), /* .map(|i| RygVal::Int(i))
                                          * .collect::<Vec<_>>() */
                ))));
            } else {
                return err(low_val, "start");
            }
        }
        (_, Some(hi_val)) => {
            let rval = walk(*hi_val.clone(), &mut env);
            let end: Maybe<i32> = match rval {
                Ok(RygVal::Int(i)) => Ok(i),
                Ok(RygVal::Float(j)) => Ok(j.floor() as i32),
                Err(h) => return Err(h),
                _ => return err(rval.unwrap(), "end"),
            };
            if let Ok(fin) = end {
                let rng = (0..)
                    .into_iter()
                    .take(fin as usize)
                    .map(|n| RygVal::Int(n))
                    .collect::<Vec<_>>();
                return Ok(RygVal::Vector(rng));
            } else {
                return err(rval.unwrap(), "end");
            }
        }
        (_, None) => err(walk(*x, &mut env).unwrap(), "end"),
    }
}

fn walk_member(
    body: Box<Expr>,
    key: Token,
    mut env: &mut Envr,
) -> Maybe<RygVal> {
    let mut object = walk(*body, &mut env)?;
    if let RygVal::Dict(record) = &mut object {
        if !key.is_identifier() {
            Err(Halt::Evaluating(format!(
                "Invalid lookup error! {:?} is not a valid field!",
                key
            )))
        } else if let Some(val) = record.get(&key.literal()) {
            return Ok(val.to_owned());
        } else {
            Err(Halt::Evaluating(format!(
                "The record {} does not have a field with the name `{}`!",
                object,
                key.literal()
            )))
        }
    } else {
        Err(Halt::Evaluating(format!(
            "Invalid lookup error! {:?} is not record!",
            object
        )))
    }
}

fn walk_index(
    body: Box<Expr>,
    index: Box<Expr>,
    mut env: &mut Envr,
) -> Maybe<RygVal> {
    let err = |index: RygVal, whole: RygVal| {
        Err(Halt::Evaluating(format!(
            "Invalid lookup error! {:?} is not a valid index/field for {:?}",
            &index, &whole
        )))
    };
    let body = walk(*body.clone(), &mut env)?;
    match body.clone() {
        RygVal::String(v) => {
            if let Expr::Iter(ia, ib) = *index {
                if let Some(bx) = ib {
                    let rng = walk_iter(ia, Some(bx), &mut env)?;
                    let slice = rng
                        .to_vec()
                        .into_iter()
                        .zip(v.chars())
                        .map(|(rv, c)| RygVal::Char(c))
                        .collect::<Vec<RygVal>>();
                    Ok(RygVal::Vector(slice))
                } else {
                    Ok(RygVal::Vector(
                        v.chars().map(|c| RygVal::Char(c)).collect::<Vec<_>>(),
                    ))
                }
            } else {
                let len = v.len();
                let idx = walk(*index, &mut env)?;
                if let Some(i) = idx.as_usize() {
                    if i < len {
                        Ok(match v.chars().nth(i) {
                            Some(c) => RygVal::Char(c),
                            None => RygVal::Char('\0'),
                        })
                    } else {
                        Err(Halt::Evaluating(format!(
                            "Index {} is out of bounds for {}",
                            idx, body,
                        )))
                    }
                } else {
                    err(idx, body)
                }
            }
        }
        RygVal::Vector(v) | RygVal::Tuple(_, v) => {
            if let Expr::Iter(ia, ib) = *index {
                let rng = walk_iter(ia, ib, &mut env)?;
                let slice = rng
                    .to_vec()
                    .into_iter()
                    .zip(v.into_iter())
                    .map(|(l, r)| r)
                    .collect::<Vec<RygVal>>();
                Ok(RygVal::Vector(slice))
            } else {
                let len = v.len();
                let idx = walk(*index.clone(), &mut env)?;
                let m = match idx.clone() {
                    RygVal::Float(q) => q.floor() as usize,
                    RygVal::Int(m) => m as usize,
                    _ => return err(idx, body),
                };
                if m >= 0 && m < len {
                    if let Some(res) = v.get(m as usize) {
                        Ok(res.clone())
                    } else {
                        err(idx, body)
                    }
                } else if m < 0 && m - len > 0 {
                    if let Some(res) = v.get((len - m) as usize) {
                        Ok(res.clone())
                    } else {
                        err(idx, body)
                    }
                } else {
                    err(idx, body)
                }
            }
        }
        RygVal::Dict(mut rec) => {
            if let Expr::Literal(tok) = *index {
                let key = tok.literal();
                if let Some(res) = rec.get(&key) {
                    return Ok(res.to_owned());
                } else {
                    Err(Halt::UnknownError(format!(
                        "The unable to find key {} in record {:#?}",
                        key, rec
                    )))
                }
            } else {
                err(RygVal::Nil(), RygVal::Dict(rec))
            }
        }
        _ => err(walk(*index.clone(), &mut env)?, body),
    }
}

fn walk_case(
    subj: Box<Expr>,
    conds: Vec<(Expr, Expr)>,
    deft: Box<Expr>,
    mut env: &mut Envr,
) -> Maybe<RygVal> {
    let mut scope = &mut env.extend();
    let test = walk(*subj, &mut scope);
    for (pat, branch) in conds.iter() {
        if walk(pat.to_owned(), &mut scope) == test {
            return walk(branch.to_owned(), &mut scope);
        }
    }
    walk(*deft, &mut scope)
}

fn walk_vector(b: Vec<Expr>, mut env: &mut Envr) -> Maybe<RygVal> {
    let mut vector: Vec<RygVal> = vec![];
    for v in b.iter() {
        match walk(v.clone(), &mut env) {
            Ok(rv) => vector.push(rv),
            Err(e) => return Err(e),
        };
    }
    Ok(RygVal::Vector(vector))
}

fn walk_tuple(b: Vec<Expr>, mut env: &mut Envr) -> Maybe<RygVal> {
    let mut scope = env;
    let mut tuple: Vec<RygVal> = vec![];
    for v in b.iter() {
        match walk(v.clone(), &mut scope) {
            Ok(rv) => tuple.push(rv),
            Err(e) => return Err(e),
        };
    }
    Ok(RygVal::Tuple(tuple.len(), tuple))
}

fn walk_list(defn: Definition, mut env: &mut Envr) -> Maybe<RygVal> {
    let mut scope = env.extend();
    let mut starts = vec![];
    let vars = defn.ranges.iter().map(|(var, rng)| {
    if let Expr::Literal(Token::Identifier(ident, pos)) = var {
      if let Expr::Vector(ref els) = rng {
        els.iter().enumerate().fold((RygType::Any, Ok(Vec::new())), |a, c| {
          let res = walk(c.1.clone(), &mut env);
          if c.0 == 0 {
            if let Ok(val) = res.clone() {
              let kind = RygType::from(val.clone());
              let defined = scope.def(ident.to_owned(), Some(&val));
              if defined.is_none() {
                starts.push((kind, val))
              };
            };
          };
          match (a.clone(), res) {
            ((a_t, Ok(mut a_vs)), Ok(rv)) => {
              if RygType::from(rv.clone()) == a_t.clone() || matches!(a_t, RygType::Any | RygType::Unknown) {
                a_vs.push(rv);
                return (a_t.clone(), Ok(a_vs));
              } else {
                (a_t.clone(), Err(Halt::InvalidType(format!("List requires all types to be the same! Expected type {}, but found {} {:?}", a_t, rv, pos))))
              }
            }
            (_, Ok(v)) => (RygType::from(v.clone()), a.clone().1.and_then(|mut op| {op.push(v); Ok(op)})),
            (_, Err(e)) => (a.clone().0, Err(e)),
          }
        })
      } else {
        (RygType::Halt, Err(Halt::InvalidInput(format!("The provided token {} is not a valid generator identifier!", var))))
      }
    } else {
      (RygType::Halt, Err(Halt::InvalidInput(format!("The provided token {} is not a valid generator identifier!", var))))
    }
  }).collect::<Vec<_>>();
    let err: Maybe<RygVal> = Err(Halt::InvalidInput(format!(
        "Unable to assign non-variables as constant! {:?}",
        defn
    )));
    let mut fixed = vec![];
    for (cnst, base) in defn.fixed.iter() {
        let evaled = walk(base.to_owned(), &mut scope);
        if let Ok(rv) = evaled {
            if let Expr::Literal(Token::Identifier(n, _)) = cnst {
                let field = Field {
                    name: n.to_string(),
                    kind: RygType::from(rv.clone()),
                };
                if let Some(x) = scope.def(n.to_owned(), Some(&rv)) {
                    fixed.push((field, x));
                };
                // fixed.push(field);
            } else {
                return err;
            }
        }
    }
    Ok(RygVal::List(RygList::new(
        scope, vars, fixed, defn.conds, defn.item,
    )))
}

pub fn pattern_match_rest(
    ref morpheme: Vec<Morpheme>,
    ref rygval: Vec<RygVal>,
    mut scope: &mut Envr,
) {
}

pub fn pat_match<F>(
    pattern: Morpheme,
    rygval: RygVal,
    mut scope: &mut Envr,
    mut action: F,
) where
    F: FnMut(&mut Envr, String, RygVal), {
    match (&pattern, &rygval) {
        (Morpheme::Empty, _) => {}
        (Morpheme::Atom(tok), _) => {
            action(scope, tok.literal(), rygval);
        }
        _ => {}
    }
}

pub fn pattern_match(pattern: Morpheme, rygval: RygVal, mut scope: &mut Envr) {
    match (pattern.clone(), rygval.clone()) {
        (Morpheme::Empty, _) => {
            // RygVal::Nil(); // throw error?
        }
        (Morpheme::Atom(tok), _) => {
            scope.def(tok.literal(), Some(&rygval));
        }
        (Morpheme::Tuple(vt), RygVal::Tuple(_, rhs))
        | (Morpheme::Vector(vt), RygVal::Vector(rhs)) => {
            if let Some(idx) =
                vt.iter().position(|mf| matches!(mf, Morpheme::Rest(..)))
            {
                let (args_l, rest_l) = vt.split_at(idx);
                let (args_r, rest_r) = rhs.split_at(idx);
                pattern_match(
                    rest_l.first().unwrap().clone(),
                    RygVal::Vector(rest_r.to_vec()),
                    &mut scope,
                );
                args_l.iter().zip(args_r.iter()).for_each(|(a, b)| {
                    match a {
                        Morpheme::Atom(tt) => {
                            scope.def(tt.literal(), Some(&b));
                        }
                        _ => {
                            pattern_match(
                                a.to_owned(),
                                b.to_owned(),
                                &mut scope,
                            );
                        }
                    };
                });
            } else {
                vt.iter().zip(rhs.iter()).enumerate().for_each(
                    |(i, (pat, val))| match (pat, val) {
                        (Morpheme::Atom(x), _) => {
                            &scope.def(x.literal(), Some(&val));
                        }
                        (Morpheme::Tuple(tu), RygVal::Tuple(_, rh))
                        | (Morpheme::Vector(tu), RygVal::Vector(rh)) => {
                            if let Some(index) = tu.iter().position(|mf| {
                                if let Morpheme::Atom(_) = mf {
                                    true
                                } else {
                                    false
                                }
                            }) {
                                let (args_l, rest_l) = tu.split_at(index);
                                let (args_r, rest_r) = rh.split_at(index);
                                pattern_match(
                                    rest_l.first().unwrap().clone(),
                                    RygVal::Vector(rest_r.to_vec()),
                                    &mut scope,
                                );
                                args_l.iter().zip(args_r.iter()).for_each(
                                    |(a, b)| {
                                        match a {
                                            Morpheme::Atom(tt) => {
                                                scope.def(
                                                    tt.literal(),
                                                    Some(&b),
                                                );
                                            }
                                            _ => {
                                                pattern_match(
                                                    a.to_owned(),
                                                    b.to_owned(),
                                                    &mut scope,
                                                );
                                            }
                                        };
                                    },
                                );
                            } else {
                                tu.iter().zip(rh.iter()).for_each(|(a, b)| {
                                    match a {
                                        Morpheme::Atom(tt) => {
                                            scope.def(tt.literal(), Some(&b));
                                        }
                                        _ => {
                                            pattern_match(
                                                a.to_owned(),
                                                b.to_owned(),
                                                &mut scope,
                                            );
                                        }
                                    };
                                });
                            }
                        }
                        (Morpheme::Record(obj), RygVal::Dict(record)) => {
                            obj.iter().for_each(|tp| {
                                if let Morpheme::Atom(tt) = tp {
                                    if record.contains_key(&tt.literal()) {
                                        scope.def(
                                            tt.literal(),
                                            record.clone().get(&tt.literal()),
                                        );
                                    };
                                } else {
                                    pattern_match(
                                        tp.to_owned(),
                                        RygVal::Dict(record.to_owned()),
                                        &mut scope,
                                    )
                                }
                            });
                        }
                        (
                            Morpheme::Vector(ts) | Morpheme::Tuple(ts),
                            rv @ (RygVal::Nil() | RygVal::Unit()),
                        ) => {
                            ts.iter().for_each(|tp| {
                                pattern_match(
                                    tp.to_owned(),
                                    RygVal::Nil(),
                                    &mut scope,
                                )
                            });
                        }
                        (lp, rp) => {
                            pattern_match(
                                lp.to_owned(),
                                rp.to_owned(),
                                &mut scope,
                            );
                        }
                    },
                )
            };
        }
        (Morpheme::Record(vt), RygVal::Dict(mut rhs)) => {
            for pat in vt {
                if let Morpheme::Atom(tk) = pat {
                    let key = tk.literal();
                    scope.def(tk.literal(), rhs.get(&key));
                }
            }
        }
        (
            Morpheme::Vector(ts) | Morpheme::Tuple(ts),
            rv @ (RygVal::Nil() | RygVal::Unit()),
        ) => {
            ts.iter().for_each(|tp| {
                pattern_match(tp.to_owned(), RygVal::Nil(), &mut scope)
            });
        }
        (Morpheme::Rest(t), rv @ _) => {
            scope.def(t.literal(), Some(&rygval));
        }
        _ => {
            // println!("pat: {:?}, val: {}", pattern, rygval)
        }
    };
}

fn walk_let<'a, 'b: 'a>(
    defs: Vec<Binding>,
    body: Box<Expr>,
    mut env: &mut Envr,
) -> Maybe<RygVal> {
    let mut scope = &mut env.extend();
    for arg in defs {
        let Parameter {
            name,
            shape,
            kind,
            pattern,
        } = arg.pram.clone();
        let binding = walk(arg.expr, &mut scope);
        // let mut insertions = vec![];
        if let Ok(rval) = binding {
            pattern_match(pattern, rval, &mut scope)
        }
    }
    walk(*body, &mut scope)
}

fn walk_call(
    fnc: Box<Expr>,
    args: Vec<Expr>,
    _name: Option<Token>,
    mut env: &mut Envr,
) -> Maybe<RygVal> {
    let not_callable = |fun: &RygVal| -> Maybe<RygVal> {
        Err(RygType::invalid_type(fun, "Function or Lambda"))
    };
    let fun = walk(*fnc, &mut env)?;
    match fun.clone() {
        RygVal::Lambda(mut lam) => Ok(lam.call(args, &mut env)?),
        RygVal::Function(fnc) => (fnc.this)(
            args.into_iter()
                .map(|x| match walk(x, &mut env) {
                    Ok(rv) => rv,
                    Err(h) => RygVal::Error(h),
                })
                .collect::<Vec<_>>(),
        ),
        _ => not_callable(&fun),
    }
}

fn walk_block(
    is_do: bool,
    body: Vec<Expr>,
    mut env: &mut Envr,
) -> Maybe<RygVal> {
    /// `Do`-blocks are executed under the **same scope** containing them. Any
    /// changes in pre-existing variable bindings made via (re)assignments
    /// within a `do` block persist after the whole block has been evaluated.
    /// Note: Declarations (i.e., assignment expressions using `:=` operator)
    /// **always** persist
    if is_do {
        Ok(body.into_iter().fold(UNIT, |_, c| match walk(c, &mut env) {
            Ok(result) => result,
            Err(h) => RygVal::Error(h),
        }))
    } else {
        /// Non-`do`-blocks run in a new scope extended from the scope
        /// containing them. Any new bindings
        let mut scope = &mut env.extend();
        Ok(body
            .into_iter()
            .fold(UNIT, |_, c| match walk(c, &mut scope) {
                Ok(result) => result,
                Err(h) => RygVal::Error(h),
            }))
    }
}

fn walk_conditional(
    cond: Box<Expr>,
    then: Box<Expr>,
    deft: Option<Box<Expr>>,
    mut env: &mut Envr,
) -> Maybe<RygVal> {
    let test = &mut || walk(*cond.clone(), &mut env);
    let pass;
    if let Ok(t) = test() {
        match t {
            RygVal::Bool(b) => pass = b,
            RygVal::Nil() => pass = false,
            _ => {
                return Err(RygType::invalid_type(
                    &walk(*cond, &mut env)?,
                    "Bool or Nil",
                ))
            }
        };
    } else {
        return Err(Halt::UnknownError(format!(
            "Unknown error for {:#?}, {:#?}, {:#?}",
            cond, then, deft
        )));
    };
    if pass {
        walk(*then, &mut env)
    } else {
        match deft {
            Some(x) => walk(*x, &mut env),
            None => Ok(RygVal::Nil()),
        }
    }
}

fn walk_lambda(
    name: Option<Token>,
    prams: Vec<Parameter>,
    body: Box<Expr>,
    mut env: &mut Envr,
) -> Maybe<RygVal> {
    let mut lam_scope = env.extend();
    prams.iter().for_each(|p| {
        pattern_match(p.clone().pattern, RygVal::Unit(), &mut lam_scope);
    });

    let mut lam = Lambda::new(
        name.clone().map(|tok| tok.literal()),
        prams,
        *body,
        lam_scope,
    );

    let lambda = RygVal::Lambda(lam.to_owned());
    lam.envr.def("self".to_string(), Some(&lambda.clone()));
    if let Some(name_) = &name {
        env.def(name_.literal(), Some(&lambda));
        [&mut lam.envr, &mut env].iter_mut().for_each(|ctx| {
            ctx.def(name_.literal(), Some(&lambda));
        });
    }

    Ok(lambda)
}

fn walk_binary(
    op: Token,
    left: Box<Expr>,
    right: Box<Expr>,
    mut env: &mut Envr,
) -> Maybe<RygVal> {
    match op.literal().as_str() {
        "=>" => match walk(*left, &mut env) {
            Ok(RygVal::Bool(true)) => walk(*right, env),
            _ => Ok(RygVal::Unit()),
        },
        "<>" | "++" => {
            let rhs = walk(*right, env)?;
            match apply_op(op, walk(*left, env)?, rhs) {
                Ok(v) => Ok(v),
                Err(h) => Err(h),
            }
        }
        "**" => {
            let rhs = walk(*right, &mut env)?;
            match (walk(*left, &mut env)?, rhs) {
                (RygVal::Int(a), RygVal::Int(b)) => {
                    if a == 0 && b == 0 {
                        Err(Halt::DivisionByZero(format!("0^0 is undefined!")))
                    } else {
                        // pow is for u32 only, so we split up the work into
                        // n < 0 && n >= 0
                        if b == 0 {
                            Ok(RygVal::Int(1))
                        } else if b > 0 {
                            Ok(RygVal::Int(a.pow(b as u32)))
                        } else {
                            Ok(RygVal::Float(
                                (1 / a.pow(b.abs() as u32)).into(),
                            ))
                        }
                    }
                }
                (RygVal::Int(a), RygVal::Float(b)) => {
                    Ok(RygVal::Float((a as f64).powf(b)))
                }
                (RygVal::Float(a), RygVal::Int(b)) => {
                    Ok(RygVal::Float(a.powi(b)))
                }
                (RygVal::Float(a), RygVal::Float(b)) => {
                    Ok(RygVal::Float(a.powf(b)))
                }
                _ => Err(Halt::InvalidType("Numeric".to_string())),
            }
        }
        _ => apply_op(op, walk(*left, env)?, walk(*right, env)?),
    }
}

fn apply_op(operator: Token, left: RygVal, right: RygVal) -> Maybe<RygVal> {
    let invalid = &mut || -> Maybe<RygVal> {
        Err(Halt::InvalidInput(format!(
            "{:?} is not a valid binary operator for operands {:?} and {:?}",
            operator.clone(),
            left.clone(),
            right.clone()
        )))
    };
    if let Some(Token::Operator(ref op, _)) = operator.as_operator() {
        match op.as_str() {
            "+" => left + right,
            "-" => left - right,
            "*" => left * right,
            "%" | "mod" => left % right,
            "/" => left / right,
            "||" | "or" => {
                if let Ok(v) = left.get_bool() {
                    if v {
                        Ok(TRUE)
                    } else {
                        Ok(RygVal::Bool(right.get_bool()? == true))
                    }
                } else {
                    Err(Halt::InvalidType("Bool".to_string()))
                }
            }
            "&&" | "and" => {
                Ok(RygVal::Bool(left.get_bool()? && right.get_bool()?))
            }
            "<" => Ok(RygVal::Bool(left < right)),
            ">" => Ok(RygVal::Bool(left > right)),
            "==" => Ok(RygVal::Bool(left == right)),
            ">=" => Ok(RygVal::Bool(left >= right)),
            "<=" => Ok(RygVal::Bool(left <= right)),
            "&" => left & right,
            "|" => left | right,
            "^" | "xor" => left ^ right,
            "<>" => Ok(RygVal::Vector(vec![left, right])),
            "++" => Ok(RygVal::Vector({
                let mut v = left.to_vec();
                v.into_iter()
                    .chain(right.to_vec().into_iter())
                    .collect::<Vec<_>>()
            })),
            _ => invalid(),
        }
    } else {
        invalid()
    }
}

fn walk_unary(o: Token, r: Box<Expr>, env: &mut Envr) -> Maybe<RygVal> {
    let right = &mut || walk(*r.clone(), env);
    match &o {
        Token::Operator(op, pos) => match op.as_str() {
            "-" => apply_op(
                Token::Operator(String::from("-"), pos.clone()),
                RygVal::Float(0.0),
                right()?,
            ),
            "!" => apply_op(
                Token::Operator(String::from("=="), pos.clone()),
                FALSE,
                right()?,
            ),
            _ => Err(Halt::InvalidInput(format!(
                "Unable to handle unary operator {:?}",
                o
            ))),
        },
        _ => Err(Halt::Evaluating(format!(
            "Unable to evaluate unary expression with {:?} and {:?}",
            o, *r
        ))),
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use crate::{
        core::{
            rygtype::{Field, RygType},
            rygval::RygVal,
        },
        evaluating::{environment::Envr, evaluator::walk},
        log_do,
        parsing::{expression::Expr, parser::parse_input},
        tok::{stream::Pos, token::Token},
        util::types::{Either, Maybe},
    };

    use super::*;
    pub trait TestWalk {
        fn test_walk(&mut self, expr: Expr);
    }

    impl<'a> TestWalk for Interpreter<'a> {
        fn test_walk(&mut self, expr: Expr) {
            let my_scope = &mut self.global_scope;
            let cc = my_scope.get_mut();
            let res = walk(expr.clone(), cc);
            match res {
                Ok(rv) => {
                    println!(
                        "Expr: {:#?}\n\nScopes: {:#?}\n\nResult: {:?}",
                        expr, cc, rv
                    )
                }
                Err(e) => println!("{:?}", e),
            }
        }
    }

    fn run(src: &str) -> Maybe<RygVal> {
        println!("src: \n> {}\n", src.clone());
        let ast = parse_input(src);
        let mut env = Envr::new();
        let val = walk(ast, &mut env);
        println!(
            "> {:?}\n\nenv: {:?}\n",
            match val.clone() {
                Ok(v) => v,
                Err(e) => RygVal::Error(e),
            },
            env
        );
        val
    }

    #[test]
    fn basic_arithmetic() {
        let src = "3 + 4";
        let result = run(src);
        [("3 + 4", "7"), ("1 + 4 / 2 - 3 * 5", "-12")]
            .map(|(x, y)| assert_eq!(run(x), run(y)));

        let _ = run("100 / 50");
    }

    #[test]
    fn test_conditional() {
        let src = "if case 3 * 3 + 4 of {
      _ => false
    } then 1 else 2;";
        let result = run(src);
        assert_eq!(result.unwrap(), RygVal::Int(2))
    }

    #[test]
    fn test_assignment() {
        let src = "x = 4; x; y = x";
        let result = run(src);
        assert_eq!(result.unwrap(), RygVal::Float(4.0))
    }

    #[test]
    fn test_lambda() {
        let src = "(fn summation |sum| sum + 1)(3.4)";
        let result = run(src);
        println!("{}", result.clone().unwrap());
        assert_eq!(result.unwrap(), RygVal::Float(4.4))
    }

    #[test]
    fn test_cases() {
        let src = "case 1 + 2 of {
      30 => -5,
      4 => 6,
      _ => 7
    };";
        run(src);
    }

    #[test]
    fn test_assign() {
        let src = "
    let x' = 2, 
      y = 3.0,
      z = #{ name: \"cat\" }
    in {
      x' =< 2 * y;
      x';
      z.name =< \"boo\";
      z
    }; "; //fn sum |a, b| a + b;";
        let ast = parse_input(src);
        // run(src);
        let mut interpreter = Interpreter::new();
        interpreter.test_walk(ast)
    }

    #[test]
    fn test_let_pats() {
        let src = "let [a, b] = [0, 1] in a + b;";
        let ast = parse_input(src);
        let mut interpreter = Interpreter::new();
        interpreter.test_walk(ast)
    }

    #[test]
    fn test_index() {
        let src = "[[1], 2, 3][0][0]";
        run(src);
        let mut env = Envr::new();
        let key = String::from("foo");
        env.def(key.clone(), Some(&RygVal::String(String::from("bar"))));
        let mut scope = env.extend();
        println!("{:?}, {:?}", scope.clone(), scope.get(&String::from("foo")));

        let res = walk(parse_input("foo = \"hi\"; foo"), &mut scope);
        println!("{:?}      ... {:#?}", &res, scope);
        let val = Envr::set(&mut scope, &key, res.clone().expect("Asd"));
        println!("{:?}      ... {:#?}", res, scope);
        println!("{:?}", val)
    }

    #[test]
    fn test_conc() {
        println!("{:?}", run("1 <> 2"));
        let src = "
      let a = 2, b = 3, c = 4
      in a <> b <> c";
        run(src);
    }

    #[test]
    fn test_record() {
        let src = "#{a = \"cat\", is = \"cute\"};";
        println!("{:?}", run(src));
    }

    #[test]
    fn inspect_pattern_lambda() {
        let src = "let sum = |[a, b]| a + b in (sum, sum([1, 2]))";
        let mut env = Envr::new();
        log_do!(
          "result" => run(src),
          "env" => &env
        );
    }

    #[test]
    fn test_interpreter() {
        let intr = Interpreter::new();
        println!("{:?}", intr);
        let ctx = intr.global_scope;
        let ast = parse_input("[a | a <- [1, 2, 3], a > 0]");
        let aa = &mut *ctx.borrow_mut();
        println!("{:#?}", walk(ast, aa).unwrap());
        use std::mem::size_of;
        println!("the size of intr is {}", size_of::<Interpreter>())

        // println!("{:?}", aa)
    }

    #[test]
    fn test_tokpat_toks() {
        let intr = Interpreter::new();
        println!("{:?}\n\n", intr);
        let ctx = intr.global_scope;
        let ast = parse_input("|[a, [b], c]| a + b + c");
        if let Expr::Block(_, vs) = ast.clone() {
            let fst = vs.first().unwrap();
            if let Expr::Lambda(a, b, c) = fst.to_owned() {
                b.into_iter().enumerate().for_each(|(i, p)| {
                    let pp = p.clone();
                    let toks = pp.pattern.get_tokens();
                    println!("tokpat[{}] :: {:?}", i, toks)
                })
            }
        };
        let aa = &mut *ctx.borrow_mut();
        // println!("{:#?}", walk(ast, aa).unwrap());

        // println!("{:?}", aa)
    }

    #[test]
    fn test_read_file() {
        let mut intr = Interpreter::new();
        let res = intr.walk(&parse_input(
            "read'file'text'(\"./examples/fibonacci.wg\")",
        ));
        match res {
            Ok(a) => println!("{}", a),
            Err(b) => println!("{}\n\nsample: {:#?}", b, Interpreter::new()),
        };
    }

    #[test]
    fn inspect_path() {
        let member_expr = Expr::Member(
            Box::new(Expr::Member(
                Box::new(Expr::Literal(Token::Identifier(
                    "a".to_string(),
                    Pos::faux(),
                ))),
                Token::Identifier("b".to_string(), Pos::faux()),
            )),
            Token::Identifier("c".to_string(), Pos::faux()),
        );
        println!("{:?}", get_member_path(&member_expr));
        let ast = parse_input("a.b().c");
        println!("{:#?}", &ast);
        println!("{:#?}", get_member_path(&ast));
    }

    #[test]
    fn inspect_lambdas() {
        use crate::parsing::parser::parse_input;
        let src = "(|a| |b| (a, b))(1)(2)";
        let mut interpreter = Interpreter::new();
        run(src);
    }

    #[test]
    fn inspect_problem_block() {
        let src = "let i = 0 in { i += 1; { i += 1; }; print'ln(i); }";
        let mut intr = Interpreter::new();
        let ast = parse_input(src);
        let res = intr.walk(&ast);
        match res {
            Ok(v) => println!("{}", v),
            Err(h) => println!("{}", h),
        }
    }
}
