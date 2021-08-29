#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
  String(String),
  Number(String, u8),
  Variable(String),
  Symbol(String),
  Boolean(String),
  Operator(String),
  Keyword(String),
  Punct(char),
  Eof(),
  Empty(),
}
