#[macro_export]
macro_rules! log_do {
    ($key: expr => $val: expr) => {
      println!("{}: {:#?}", $key, $val);
    };
    ($key: expr => $val: expr, $($keys: expr => $vals: expr),+) => {{  log_do!($key => $val);
      log_do!($($keys => $vals),+);
    }};
  }
