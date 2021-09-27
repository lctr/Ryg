use std::{
  cell::RefCell,
  collections::HashMap,
  io::{self, Read, Write},
};

use crate::{
  core::rygval::RygVal,
  evaluating::{
    builtin::{load_core, load_math},
    environment::Envr,
    evaluator::{walk, Interpreter},
  },
  lexing::{lexer::tokenize_input, token::Token},
  util::misc::{Color, Paint},
};
use crate::{
  parsing::{expression::Expr, parser::parse_input},
  util::state::Halt,
};

pub struct Repl<'a> {
  scope: Envr,
  pub exit: bool,
  debug: bool,
  prompts: HashMap<String, String>,
  buffer: ReplBuf,
  interpreter: RefCell<Interpreter<'a>>,
}

pub struct ReplBuf(pub usize);

impl<'a> Repl<'a> {
  fn new<'b: 'a>() -> Self {
    let mut scope = Envr::new();
    let mut prompts = HashMap::new();
    ["(", ")", "Error!", "<)", "(>", "Ast", "Tokens"]
      .iter()
      .enumerate()
      .for_each(|(i, s)| {
        let z = s.to_string();
        let s = z.clone();
        let styled = match i {
          0..=2 => Paint::fg_red(s),
          3 => Paint::fg_blue(s),
          4 => Paint::fg_green(s),
          _ => Paint::fg_yellow(s),
        };
        prompts.insert(z, styled);
      });
    load_core(&mut scope);
    load_math(&mut scope);
    Self {
      scope,
      exit: false,
      debug: false,
      prompts,
      buffer: ReplBuf(0 as usize),
      interpreter: RefCell::new(Interpreter::new()),
    }
  }

  fn get_prompt(&self, key: &str) -> String {
    if let Some(prompt) = self.prompts.get(key) {
      prompt.clone()
    } else {
      String::from("")
    }
  }

  pub fn run(&'a mut self) {
    println!("{}Ryg{}", self.get_prompt("("), self.get_prompt(")"));
    let prompt_out = self.get_prompt("(>"); // Paint::fg_light_cyan(String::from("(>"));
    loop {
      let ast = &self.input();
      if *(&self.exit) {
        break;
      };
      match &mut self.interpret(ast.clone()) {
        Ok(val) => println!("{} {}", prompt_out, val),
        Err(e) => println!("{}", Halt::ReplError(format!("{}", e))),
      };
    }
  }

  pub fn input(&mut self) -> Expr {
    self.std_prompt(&self.get_prompt("<)"), &mut Self::read_next)
  }

  pub fn std_prompt<K, F: FnMut(&mut Self) -> K>(
    &mut self,
    prompt: &str,
    action: &mut F,
  ) -> K {
    print!("{} ", prompt.trim());
    match io::stdout().flush() {
      Ok(_) => action(self),
      Err(e) => {
        println!("{} {}", self.get_prompt("Error!"), e);
        action(self)
      }
    }
  }

  fn read_lines(&mut self) -> String {
    let mut lines = String::new();
    'read: loop {
      let input = self.read_line();
      lines.push_str(input.trim_end_matches("~"));
      lines.push('\n');

      if input.starts_with(|c| matches!(c, ':' | '.'))
        || input.ends_with(|c| c == ';' || c != '~')
      {
        break 'read;
      } else {
        print!(".. ");
        match io::stdout().flush() {
          Ok(_) => continue 'read,
          Err(e) => {
            println!("{} {}", self.get_prompt("Error!"), e);
            break 'read;
          }
        }
      }
    }
    lines
  }

  fn read_line(&mut self) -> String {
    let mut input = String::new();
    io::stdin()
      .read_line(&mut input)
      .and_then(|pos| {
        self.buffer.0 = pos;
        Ok(pos)
      })
      .expect("Failed to read line!");
    input.trim().to_owned()
  }

  pub fn read_next(&mut self) -> Expr {
    let input = self.read_lines();
    if input.trim().starts_with(|c| matches!(c, ':')) {
      self.options(&input)
    } else {
      parse_input(&input.trim())
    }
  }

  pub fn command_lex(&mut self, input: Option<&str>) -> Expr {
    let source = if let Some(src) = input {
      src.to_owned()
    } else {
      self.read_lines()
    };
    let mut tokens = tokenize_input(&source);
    let space = "    ";
    println!("lexing: {}", source);
    if tokens.len() + 1 < 2 {
      println!("token: {}", tokens.pop().unwrap_or(Token::Empty()))
    } else {
      println!(
        "tokens: {}",
        tokens
          .iter()
          .enumerate()
          .map(|(i, t)| { format!("{}{}. {}", space, i + 1, t) })
          .collect::<Vec<_>>()
          .join("\n")
      );
    }
    self.input()
  }

  pub fn command_parse(&mut self, input: Option<&str>) -> Expr {
    if let Some(q) = input {
      let ast = parse_input(q);
      println!("parsing: {}", q);
      println!("ast: {}", ast.clone());
      // Expr::Literal(Token::Empty())
      self.input()
    } else {
      let expr = self.std_prompt("parsing: ", &mut Self::read_next);
      println!("{}: {:?}", self.get_prompt("Ast"), expr.clone());
      expr
    }
  }
  pub fn command_quit(&mut self, input: Option<&str>) -> Expr {
    if let Some(_) = input {
      self.set_exit(true)
    } else {
      let resp = self
        .std_prompt("Are you sure you want to quit?\n", &mut Self::read_line);
      if resp
        .to_lowercase()
        .contains(|c| matches!(c, 'y' | '1' | 'q'))
      {
        self.set_exit(true)
      } else {
        println!("Not quitting.");
        return self.input();
      }
    };
    println!("Quitting. Goodbye!\n> scope: {:#?}", self.scope);
    Expr::Literal(Token::Empty())
  }

  pub fn print_env(&mut self, input: Option<&str>) -> Expr {
    if let Some(cmd) = input {
      // do something specific w/env ?
      match cmd {
        "*" => {
          println!("col: {}\n  {:#?}", self.buffer.0, self.scope)
        }
        "+" => self.debug = true,
        "-" => self.debug = false,
        _ => println!("The print_env:{} command is not implemented", cmd),
      };
      self.input()
    } else {
      println!("{:#?}", &self.scope);
      self.input()
    }
  }

  pub fn options(&mut self, action: &String) -> Expr {
    let input = action.trim().strip_prefix(':');
    match input {
      Some(a) => match a.split_once(' ') {
        Some((p, q)) => match p.to_ascii_lowercase().as_str() {
          "l" | "lex" => self.command_lex(Some(q)),
          "p" | "parse" => self.command_parse(Some(q)),
          "q" | "quit" => self.command_quit(Some(q)),
          "v" | "env" => self.print_env(Some(q)),
          _ => {
            println!("Unable to handle input {:?}", action);
            self.input()
          }
        },
        None => {
          if let Some(p) = input {
            if p.starts_with("Q") {
              return self.command_quit(Some(p));
            };
            match p.to_ascii_lowercase().as_str() {
              "p" | "parse" => self.command_parse(None),
              "q" | "quit" => self.command_quit(None),
              "v" | "env" => self.print_env(None),
              _ => {
                println!("Unable to handle command {:?}", action);
                self.input()
              }
            }
          } else {
            println!("Error reading input {}", action);
            self.input()
          }
        }
      },
      None => {
        println!("Error reading input {}", action);
        self.input()
      }
    }
  }

  pub fn interpret(&mut self, ast: Expr) -> Result<RygVal, Halt> {
    let result = walk(ast, &mut self.scope);
    result
  }

  /// Set the repl's exit.
  pub fn set_exit(&mut self, exit: bool) {
    self.exit = exit;
  }
}

pub fn run() {
  Repl::new().run()
}

pub enum ReplCommand {
  Lex,
  Parse,
  Quit,
  Print,
  DisplayCfg {},
  Load,
}

#[cfg(test)]
mod test {
  use super::*;
  #[test]
  pub fn run() {
    Repl::new().run()
  }
}
