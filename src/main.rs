mod lexer;
mod token;
use lexer::Lexer;
use token::Token;

fn main() {
  let src = String::from("hello, world!");

  inspect_tokens(&src);
}

fn inspect_tokens(src: &str) {
  let mut lexer = Lexer::new(src);
  let mut token;
  // let mut done = false;
  println!("{:?}", src);
  while !lexer.eof() {
    token = lexer.next();
    println!("{:?}", token);

    // if this prints, then lexer.eof fails somehow
    if token.eq(&Token::Empty()) {
      println!("EOF FAIL");
    }
  }
}
