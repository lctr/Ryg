use super::display::*;

#[macro_export]
macro_rules! log_do {
    ($key:tt => $val:expr) => {{
        println!("{}:\n  {}", stringify!($key), $val)
    };};
    ($($key:tt => $val:expr),*) => {{
        $(println!("{}:\n  {}", stringify!($key), format!("{:#?}", $val).lines().map(|l|
        format!("  {}", l)).collect::<Vec<_>>().join("\n"));)*
    }};
}

#[macro_export]
macro_rules! list_compr {
    ($id1: ident | $id2: ident <- [$start: expr , $end: expr] , $cond: expr) => {{
        let mut vec = Vec::new();

        for num in $start..$end + 1 {
            if $cond(num) {
                vec.push(num);
            }
        }

        vec
    }};
}

#[macro_export]
macro_rules! simple_pub_struct {
    ($obj:ident :: { $p1:ident : $t1:ty, $($pn:ident : $tn:ty),* }) => {
        pub struct $obj {
            pub $p1: $t1,
            $(pub $pn: $tn),*
        }

        impl $obj {
            pub fn new($p1: $t1, $($pn: $tn),*) -> Self {
                Self {
                    $p1,
                    $($pn),*
                }
            }
        }
    };
     ($obj:ident :: { $p1:ident : $t1:ty, $($pn:ident : $tn:ty),* }
    + $($trs:ident),*) => {
        #[derive($($trs,)*)]
        pub struct $obj {
            pub $p1: $t1,
            $(pub $pn: $tn),*
        }

        impl $obj {
            pub fn new($p1: $t1, $($pn: $tn),*) -> Self {
                Self {
                    $p1,
                    $($pn),*
                }
            }
        }
    };
}

#[cfg(test)]
mod test {}
