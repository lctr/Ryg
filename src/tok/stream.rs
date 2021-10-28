use std::{fmt, iter::Peekable, str::Chars};

pub trait Streaming {
    type T: Clone;
    fn peek(&mut self) -> Self::T;
    fn next(&mut self) -> Self::T;
    fn done(&mut self) -> bool;
    fn get_pos(&mut self) -> Pos;
    fn run(&mut self) -> Vec<Self::T> {
        let mut past = vec![self.next()];
        while !self.done() {
            past.push(self.next());
        }
        past
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos {
    pub pos: usize,
    pub row: usize,
    pub col: usize,
    pub diff_line: bool,
}

impl Pos {
    pub fn new() -> Pos {
        Pos {
            pos: 0,
            row: 1,
            col: 0,
            diff_line: true,
        }
    }
    // non-existent positions in source file (such as when modifying
    // tokens/token trees during analysis, etc) Pos starts counting rows
    // (=lines) at 1, while position and column are 0 indexed. A nonvalid
    // Pos struct will have a row property of 0.
    pub fn faux() -> Pos {
        Pos {
            pos: 0,
            row: 0,
            col: 0,
            diff_line: true,
        }
    }
    // responsible for keeping line numbers in sync
    pub fn next(&mut self, c: &char) {
        self.diff_line = false;
        match *c {
            // '\0' => {}
            '\n' => {
                self.eol();
            }
            _ => {
                self.pos += 1;
                self.col += 1;
            }
        }
    }

    fn eol(&mut self) {
        self.pos += 1;
        self.col = 0;
        self.row += 1;
        self.diff_line = true;
    }

    #[allow(unused)]
    fn sync<T, S>(&mut self, mut stream: S)
    where
        T: Clone,
        S: Streaming, {
        let Pos {
            pos,
            row,
            col,
            diff_line,
            ..
        } = stream.get_pos();
        self.pos = pos;
        self.row = row;
        self.col = col;
        self.diff_line = diff_line;
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "at {}:{}", self.row, self.col)
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "(:pos {} :row {} :col {} :diff_line? {}",
            self.pos, self.row, self.col, self.diff_line
        )
    }
}

#[derive(Debug, Clone)]
pub struct CharStream<'a> {
    pub pos: Pos,
    chars: Peekable<Chars<'a>>,
    src: &'a str,
}

impl<'a> CharStream<'a> {
    pub fn new(src: &str) -> CharStream {
        CharStream {
            chars: src.trim_end().chars().peekable(),
            pos: Pos::new(),
            src,
        }
    }

    pub fn get_source(&self) -> &str {
        self.src
    }

    pub fn get_line(&self) -> &str {
        let Pos { pos, col, .. } = self.pos;
        let (a, b) = (pos - col, col);
        &self.get_source()[a..b]
    }

    pub fn rest(&self) -> &str {
        &self.src[(self.pos.pos)..]
    }
}

impl<'a> Streaming for CharStream<'a> {
    type T = Option<char>;
    fn peek(&mut self) -> Self::T {
        let c = self.chars.peek();
        c.copied()
    }

    fn next(&mut self) -> Self::T {
        if self.done() {
            None
        } else {
            if let Some(c) = self.peek() {
                self.pos.next(&c);
                self.chars.next();
                Some(c)
            } else {
                None
            }
        }
    }

    fn done(&mut self) -> bool {
        self.peek().is_none()
    }

    fn get_pos(&mut self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug)]
pub struct Stream<S, T>(pub S, pub Vec<T>)
where
    S: Streaming,
    T: Clone;

impl<S, T> Stream<S, T>
where
    S: Streaming,
    T: Clone,
{
}
