use std::{
    env, error, fmt,
    fs::File,
    io::{self, BufReader, Read, Result, Write},
    path::PathBuf,
    time::Instant,
};

use crate::{
    evaluating::evaluator::Interpreter,
    parsing::{
        expression::{Expr, Program},
        parser::parse_input,
    },
};

#[allow(unused)]
pub fn read_text_file(path: PathBuf) -> Result<String> {
    let file = File::open(path)?;
    let mut buf = BufReader::new(file);
    let mut program = String::new();
    match buf.read_to_string(&mut program) {
        Ok(_) => Ok(program),
        Err(e) => {
            println!("{:?}", e);
            Ok("".to_string())
        }
    }
}

pub fn load_file(path_str: &str) {
    if let Some(file_name) = path_str.split("/").last() {
        if file_name.ends_with(".wg") {
            println!("Loading file `{}`", &file_name);
            if let Ok(src) = read_text_file(PathBuf::from(path_str)) {
                if let mut
                program
                @
                Expr::Program(Program {
                    name: n,
                    body,
                    vocab,
                }) = &parse_input(&src.as_str())
                {
                    let now = Instant::now();
                    let mut interpreter = Interpreter::new();
                    if let Ok(res) =
                        interpreter.walk(&Expr::Program(Program {
                            name: Some(file_name.to_string()),
                            body: body.clone(),
                            vocab: vocab.clone(),
                        }))
                    {
                        println!("{}", res);
                        println!(
                            "({} bytes read, {} ns elapsed)",
                            &src.len(),
                            format!("{}", now.elapsed().as_nanos())
                                .chars()
                                .enumerate()
                                .fold(String::new(), |mut a, (ca, cb)| {
                                    a.push(cb);
                                    if ca >= 0 && ca % 3 == 0 {
                                        a.push(',')
                                    };
                                    a
                                })
                                .trim_end_matches(',')
                        );
                    }
                }
            }
        } else {
            eprintln!(
                "Invalid file extension. Unable to run file `{}`.",
                path_str
            );
        }
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn curr_dir() {
        let dir = std::env::current_dir();
        println!("{:?}", dir)
    }
}
