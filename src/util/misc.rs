// use std::fmt::{self, Display};

pub fn map_join<S, F>(vector: &Vec<S>, sep: &str, f: F) -> String
where
  S: std::fmt::Display + Clone,
  F: Fn(S) -> String, {
  vector
    .into_iter()
    .map(|x| f(x.clone()))
    .collect::<Vec<_>>()
    .join(sep)
}

#[allow(unused)]
#[derive(Clone)]
#[repr(u16)]
pub enum Color {
  Black = 30,
  Red = 31,
  Green = 32,
  Yellow = 33,
  Blue = 34,
  Magenta = 35,
  Cyan = 36,
  White = 37,
  LightBlack = 90,
  LightRed = 91,
  LightGreen = 92,
  LightYellow = 93,
  LightBlue = 94,
  LightMagenta = 95,
  LightCyan = 96,
  LightWhite = 97,
  Default = 0,
}

#[allow(unused)]
pub struct Paint(pub String, pub Color);

#[allow(unused)]
impl Paint {
  pub fn update_to(&mut self, string: String, color: Color) {
    self.0 = string;
    self.1 = color;
  }
  pub fn reset_color(&mut self) {
    self.1 = Color::Default;
    self.0 = Self::paint_fg_bg(self.0.clone(), Color::Default, Color::Default);
  }
  pub fn paint_fg(string: String, color: Color) -> String {
    format!("\u{1b}[0m\u{1b}[{};15m{}\u{1b}[0m", color as u8, &string)
  }
  pub fn paint_fg_bg(string: String, fg: Color, bg: Color) -> String {
    format!(
      "\u{1b}[0m\u{1b}[{};{};15m{}\u{1b}[0m",
      (bg as u8) + 10,
      fg as u8,
      &string
    )
  }
  pub fn paint_bg(string: String, color: Color) -> String {
    format!(
      "\u{1b}[0m\u{1b}[{};15m{}\u{1b}[0m",
      (color as u8) + 10,
      string
    )
  }

  pub fn fg_rgb(string: String, [r, g, b]: &[u16; 3]) -> String {
    format!("\u{1b}[38;2;{};{};{}m{}\u{1b}[0m", r, g, b, string)
  }

  pub fn set_color(&mut self, color: Color) {
    self.1 = color;
  }
  pub fn fg_light_green(string: String) -> String {
    Paint::paint_fg(string, Color::LightGreen)
  }
  pub fn fg_light_red(string: String) -> String {
    Paint::paint_fg(string, Color::LightRed)
  }
  pub fn fg_light_black(string: String) -> String {
    Paint::paint_fg(string, Color::LightBlack)
  }
  pub fn fg_light_yellow(string: String) -> String {
    Paint::paint_fg(string, Color::LightYellow)
  }
  pub fn fg_light_blue(string: String) -> String {
    Paint::paint_fg(string, Color::LightBlue)
  }
  pub fn fg_light_magenta(string: String) -> String {
    Paint::paint_fg(string, Color::LightMagenta)
  }
  pub fn fg_light_cyan(string: String) -> String {
    Paint::paint_fg(string, Color::LightCyan)
  }
  pub fn fg_green(string: String) -> String {
    Paint::paint_fg(string, Color::Green)
  }
  pub fn fg_red(string: String) -> String {
    Paint::paint_fg(string, Color::Red)
  }
  pub fn fg_black(string: String) -> String {
    Paint::paint_fg(string, Color::Black)
  }
  pub fn fg_yellow(string: String) -> String {
    Paint::paint_fg(string, Color::Yellow)
  }
  pub fn fg_blue(string: String) -> String {
    Paint::paint_fg(string, Color::Blue)
  }
  pub fn fg_magenta(string: String) -> String {
    Paint::paint_fg(string, Color::Magenta)
  }
  pub fn fg_cyan(string: String) -> String {
    Paint::paint_fg(string, Color::Cyan)
  }
  pub fn bg_light_green(string: String) -> String {
    Paint::paint_bg(string, Color::LightGreen)
  }
  pub fn bg_light_red(string: String) -> String {
    Paint::paint_bg(string, Color::LightRed)
  }
  pub fn bg_light_black(string: String) -> String {
    Paint::paint_bg(string, Color::LightBlack)
  }
  pub fn bg_light_yellow(string: String) -> String {
    Paint::paint_bg(string, Color::LightYellow)
  }
  pub fn bg_light_blue(string: String) -> String {
    Paint::paint_bg(string, Color::LightBlue)
  }
  pub fn bg_light_magenta(string: String) -> String {
    Paint::paint_bg(string, Color::LightMagenta)
  }
  pub fn bg_light_cyan(string: String) -> String {
    Paint::paint_bg(string, Color::LightCyan)
  }
  pub fn bg_green(string: String) -> String {
    Paint::paint_bg(string, Color::Green)
  }
  pub fn bg_red(string: String) -> String {
    Paint::paint_bg(string, Color::Red)
  }
  pub fn bg_black(string: String) -> String {
    Paint::paint_bg(string, Color::Black)
  }
  pub fn bg_yellow(string: String) -> String {
    Paint::paint_bg(string, Color::Yellow)
  }
  pub fn bg_blue(string: String) -> String {
    Paint::paint_bg(string, Color::Blue)
  }
  pub fn bg_magenta(string: String) -> String {
    Paint::paint_bg(string, Color::Magenta)
  }
  pub fn bg_cyan(string: String) -> String {
    Paint::paint_bg(string, Color::Cyan)
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use std::process::Command;
  #[test]
  fn testy() {
    let output = Command::new("sh")
      .args(["cargo", "run", "--"])
      .current_dir("~/Projects/wyg-lang-rs/src/")
      .output()
      .expect("Failed to execute");

    let sout = String::from_utf8(output.stdout).expect("Not UTF-8");
    let serr = String::from_utf8(output.stderr).expect("Not UTF-8");

    println!("{}", sout);
    println!("{}", serr);
  }

  #[test]
  fn colors() {
    let _word = "Cargo.lock";
    let _white_on_blue =
      "\u{1b}[0m\u{1b}[44;38;5;15mi love this blue\u{1b}[0m";
    let red_bold_underlined = "\u{1b}[31;22;4mHowdy\u{1b}[0m";
    let make_red = |s: &str| format!("\u{1b}[0m\u{1b}[31;22m{}\u{1b}[0m", s);
    let _light_red_fg = "[31;22m";
    let _dark_blue_fg = "[34;15m";
    let _xterm_green_fg = "[38;2;";
    let white_on_purple = "[48;5;57m";
    let rgb_color = |s: &str, (r, g, b): (&str, &str, &str)| {
      format!("\u{1b}[38;2;{};{};{}m{}\u{1b}[0m", r, g, b, s)
    };
    // let sky_blue = ""
    println!("{}", &red_bold_underlined);
    println!("{}", make_red("hello!"));
    println!("rgb {}", rgb_color("hello in rgb", ("0", "128", "255")));
    println!("rgb {}", rgb_color("hello in rgb", ("0", "128", "255")));
    println!("\u{1b}[0m\u{1b}[44;38;15m{}\u{1b}[0m", "a b c");
    println!("\u{1b}[0m\u{1b}[34;15m{}\u{1b}[0m", "d e f");
    println!("\u{1b}[0m\u{1b}[94;5;15m{}\u{1b}[0m", "g h i");
    println!("\u{1b}[0m\u{1b}[39;5;15m{}\u{1b}[0m", "j k l");

    println!(
      "\u{1b}[0m\u{1b}{}{}\u{1b}[0m",
      white_on_purple, "Cargo.lock"
    );
    for c in 30..=37 {
      for i in 15..=22 {
        println!(
          "\u{1b}[0,\u{1b}[{};5;{}m{}\u{1b}[0m    (c, i) = ({}, {})",
          c, i, "uwu1", c, i
        );

        println!(
          "\u{1b}[0,\u{1b}[{};5;15m{}\u{1b}[0m   (c, i) = ({}, {})",
          c, "uwu2", c, i
        );
      }
    }
  }
  #[test]
  fn write_str() {
    let base = String::from("Hello!");
    [
      Color::Black,
      Color::Red,
      Color::Green,
      Color::Yellow,
      Color::Blue,
      Color::Magenta,
      Color::Cyan,
      Color::White,
      Color::LightBlack,
      Color::LightRed,
      Color::LightGreen,
      Color::LightYellow,
      Color::LightBlue,
      Color::LightMagenta,
      Color::LightCyan,
      Color::LightWhite,
    ]
    .iter()
    .for_each(|c| println!("{}", Paint::paint_fg(base.clone(), c.to_owned())));
    // let text = Paint::light_green(String::from("Hello!"));
    // println!("{}", text)
  }
}
