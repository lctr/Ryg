mod lexer;
use lexer::Lexer;

fn main() {
  let src = String::from("hello, world!");
  let mut lexer = Lexer::new(&src);
  let mut token;
  // let mut done = false;
  println!("{:?}", src);
  while !lexer.eof() {
    token = lexer.next();
    println!("{:?}", token);
    if token.eq(&lexer::Token::Empty()) {
      println!("end...");
    }
  }
}
