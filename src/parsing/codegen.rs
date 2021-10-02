/* FOR A MORE IN-DEPTH ANALYSIS
 * implement Expressions as iterables, with node walkers as the iterators?
 */

use std::{borrow::Cow, rc::Rc};

use crate::parsing::expression::{Binding, Definition, Parameter};

use super::{
  expression::{Expr, TokPattern},
  parser::{parse_input, Parser},
};

pub trait Ast {
  fn reduce();
}

pub fn transcribe_pat(tokpat: &TokPattern) -> String {
  match tokpat {
    TokPattern::Empty => String::new(),
    TokPattern::Atom(tok) => tok.literal(),
    TokPattern::Tuple(ts) => format!(
      "({})",
      ts.into_iter()
        .map(|t| transcribe_pat(t))
        .collect::<Vec<_>>()
        .join(", ")
    ),
    TokPattern::Vector(ts) => format!(
      "[{}]",
      ts.into_iter()
        .map(|t| transcribe_pat(t))
        .collect::<Vec<_>>()
        .join(", ")
    ),
    TokPattern::Record(ts) => format!(
      "{{{}}}",
      ts.into_iter()
        .map(|t| transcribe_pat(t))
        .collect::<Vec<_>>()
        .join(", ")
    ),
    TokPattern::Call(a, b) => format!(
      "{}({})",
      transcribe_pat(&*a),
      b.into_iter()
        .map(|t| transcribe_pat(t))
        .collect::<Vec<_>>()
        .join(", ")
    ),
    _ => String::new(),
    /* TokPattern::Unary(_, _) => todo!(),
     * TokPattern::Binary(_, _, _) => todo!(), */
  }
}

pub fn transcribe_expr(expr: Expr) -> String {
  match expr {
    Expr::Nil => String::new(),

    Expr::Assign(a, b, c) | Expr::Binary(a, b, c) => {
      let left = transcribe_expr(*b);
      let op = a.literal();
      let right = transcribe_expr(*c);
      format!("({} {} {})", left, op, right)
    }
    Expr::Block(a) => {
      if a.len() == 1 {
        let xpr = a.first().unwrap();
        transcribe_expr(xpr.to_owned())
      } else {
        format!(
          "(do {{\n  {}}})",
          a.into_iter()
            .map(|x| transcribe_expr(x))
            .collect::<Vec<_>>()
            .join(";\n  ")
        )
      }
    }
    Expr::Call(a, b, c) => format!(
      "{}({})",
      transcribe_expr(*a),
      b.into_iter()
        .map(|x0| transcribe_expr(x0))
        .collect::<Vec<_>>()
        .join(", ")
    ),
    Expr::Case(a, b, c) => format!(
      "case {} of {{\n    {}\n}}",
      transcribe_expr(*a),
      b.into_iter()
        .map(|(x1, x2)| format!(
          "{} => {}",
          transcribe_expr(x1),
          transcribe_expr(x2)
        ))
        .collect::<Vec<_>>()
        .join(",\n    ")
    ),
    Expr::Conditional(a, b, c) => format!(
      "(if ({}) then ({}) else ({}))",
      transcribe_expr(*a),
      transcribe_expr(*b),
      if let Some(cc) = c {
        transcribe_expr(*cc)
      } else {
        String::new()
      }
    ),
    Expr::Index(a, b) => {
      format!("({}[{}])", transcribe_expr(*a), transcribe_expr(*b))
    }
    Expr::Iter(a, b) => format!(
      "({}..{})",
      transcribe_expr(*a),
      if let Some(bb) = b {
        transcribe_expr(*bb)
      } else {
        String::new()
      }
    ),
    Expr::List(x) => {
      let Definition {
        item,
        ranges,
        fixed,
        conds,
      } = *x;
      format!(
        "[{} | {}]",
        transcribe_expr(item),
        ranges
          .into_iter()
          .map(|(x1, x2)| {
            format!("{} <- {}", transcribe_expr(x1), transcribe_expr(x2))
          })
          .chain(fixed.into_iter().map(|(x1, x2)| {
            format!("{} = {}", transcribe_expr(x1), transcribe_expr(x2))
          }))
          .chain(conds.into_iter().map(|cond| transcribe_expr(cond)))
          .collect::<Vec<_>>()
          .join(", ")
      )
    }
    Expr::Range(a, b, c) => format!(
      "({}[{}..{}])",
      transcribe_expr(*a),
      transcribe_expr(*b),
      if let Some(cc) = c {
        transcribe_expr(*cc)
      } else {
        String::new()
      }
    ),
    Expr::Lambda(a, b, c) => {
      let prams = b
        .iter()
        .map(
          |Parameter {
             name: _,
             pattern,
             shape: _,
             kind: _,
           }| { transcribe_pat(pattern) },
        )
        .collect::<Vec<_>>()
        .join(", ");
      if let Some(name) = a {
        format!("(fn {} |{}| {})", name, prams, transcribe_expr(*c))
      } else {
        format!("(|{}| {})", prams, transcribe_expr(*c))
      }
    }
    Expr::Literal(a) => a.literal(),
    Expr::Loop(a, b) => {
      format!("(loop {} {})", transcribe_expr(*a), transcribe_expr(*b))
    }
    Expr::Tuple(a) => format!(
      "({},)",
      a.into_iter()
        .map(|x| transcribe_expr(x))
        .collect::<Vec<_>>()
        .join(", ")
    ),
    Expr::Unary(a, b) => format!("({}({}))", a.literal(), &*b),
    Expr::Variable(a, b) => format!(
      "(let {}\n in {})",
      a.into_iter()
        .map(
          |Binding {
             pram:
               Parameter {
                 name: _,
                 pattern,
                 shape: _,
                 kind: _,
               },
             expr: ex,
           }| {
            format!("{} = {}", transcribe_pat(&pattern), transcribe_expr(ex))
          }
        )
        .collect::<Vec<_>>()
        .join(",\n     "),
      transcribe_expr(*b)
    ),
    Expr::Vector(a) => format!(
      "[{}]",
      a.into_iter()
        .map(|x| transcribe_expr(x))
        .collect::<Vec<_>>()
        .join(", ")
    ),
    Expr::Error(a, b) => String::from("_ERROR_"),
    Expr::Return(a, b) => todo!(),
    Expr::Named(a, b) => todo!(),
    Expr::Record(a, b) => format!(
      "{{ {} }}",
      b.into_iter()
        .map(
          |Binding {
             pram:
               Parameter {
                 name: _,
                 pattern,
                 shape: _,
                 kind: _,
               },
             expr: ex,
           }| {
            format!("{} = {}", transcribe_pat(&pattern), transcribe_expr(ex))
          }
        )
        .collect::<Vec<_>>()
        .join(", ")
    ),
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn gen_let_case_expr() {
    let src = "
    let a = 1, 
        b = 2, 
        c = 3, 
        [d, e] = [9, 9] 
    in case a + b + c of { 
      1 => f(a, b), 
      2 => f(b, a),
      3 | 4 => f(a + b, a + b),
      x | x + 1 @ 5 => f(5, 5),
      _ => f(0, 0)
    };";
    let ast = parse_input(src);
    let code = transcribe_expr(ast);
    println!("{}", code)
  }

  #[test]
  fn gen_list_compr() {
    let src = "[a | a <- [0..4], f(a) > 0]";
    let ast = parse_input(src);
    let code = transcribe_expr(ast);
    println!("{}", code)
  }
}
