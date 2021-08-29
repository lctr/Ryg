mod lexer;
mod token;
use lexer::Lexer;
use token::Token;

fn main() {
  let src = String::from("print \"world\"");
  // let src = String::from("do");
  inspect_tokens(&src);
}

fn inspect_tokens(src: &str) {
  let mut lexer = Lexer::new(src);
  let mut token = lexer.peek();
  // let mut done = false;
  println!("{:?}", src);
  while !lexer.eof() {
    println!("{:?}", lexer.next());
    // if this prints, then lexer.eof fails somehow
    if token.eq(&Token::Empty()) {
      println!("EOF FAIL");
    }
  }
}

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
