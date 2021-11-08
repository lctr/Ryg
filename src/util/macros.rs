use super::display::*;
/// Print a sequence of expressions to stdout, each labeled
#[macro_export]
macro_rules! log_do {
    ($key:tt => $val:expr) => {{
        println!("{}:\n  {:?}", stringify!($key), $val)
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

/// Generate a struct and accompanying implementation of `new` method. The
/// syntax is `struct_name :: { field_entries, } + derived_trait_attrs`,
/// where `field_entries` has the same syntax as field declarations, e.g.,
/// `name; String`.
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

#[macro_export]
macro_rules! cond {
    ($a:expr => $b:expr; $(else)? $c:expr) => {
        if $a {
            $b
        } else {
            $c
        }
    };
}

#[macro_export]
macro_rules! how_long {
    ($ex:expr) => {{
        use std::time::Instant;
        let now = Instant::now();
        let res = $ex();
        let elapsed = now.elapsed();
        (elapsed, res)
    }};
}

#[macro_export]
macro_rules! quick_match {
    (with $self:ident, $(ts:tt)+) => {{
        submatch!($self =<< $(ts)+)
    }};
    ($bind:ident =<< $base:expr;
        $left:ident <| $($p1:pat = $x1:ident,)+
        | $right:ident <| $($p2:pat = $x2:expr)+
    ) => {{
        match $base {
             $($p1 => $left($bind::$x1),)+
            $($p2 => $right($x2),)+
            _ => $right($x2)
        }
    }};
    ($bind:ident =<< $base:expr
        ; $left:ident $($p1:literal $x1:ident)+
        | $right:ident $($p2:literal $x2:expr)+) => {{
            match $base {
                $($p1 => $left($bind::$x1),)+
                $($p2 => $right($x2),)+
                _ => $right($x2)
            }
    }};
    ($bind:ident =<< some $base:expr; $($p:literal $x:ident),+) => {{
        match $base {
            $($p => Some($bind::$x),)+
            _ => None
        }
    }};
}

#[cfg(test)]
mod test {
    macro_rules! rs_tokens {
        ($t:tt) => {{
            let t = stringify!($t);
            println!("{}", t);
            vec![t]
        }};
        ($t:tt $($ts:tt)+) => {{
            let mut toks = rs_tokens!($t);
            toks.extend(rs_tokens!($($ts)+));
            toks

        }};
        ([] <- $($tt:tt)*) => {{
            let mut toks = vec![];
            toks.push($(stringify!($tt))+);
            toks
        }};
    }
    #[test]
    fn tok_tree() {
        let tokens = rs_tokens!([] <- hello world!);
    }
}
