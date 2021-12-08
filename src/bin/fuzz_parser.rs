#[cfg(fuzzing)]
use honggfuzz::fuzz;
use mil::{
    compiler::{BinCode, Compile},
    types::{MelExpr, Value},
};

#[cfg(fuzzing)]
fn main() {
    // Here you can parse `std::env::args and
    // setup / initialize your project

    // You have full control over the loop but
    // you're supposed to call `fuzz` ad vitam aeternam
    loop {
        // The fuzz macro gives an arbitrary object (see `arbitrary crate`)
        // to a closure-like block of code.
        // For performance reasons, it is recommended that you use the native type
        // `&[u8]` when possible.
        // Here, this slice will contain a "random" quantity of "random" data.
        fuzz!(|data: &[u8]| { test_once(data) });
    }
}

fn test_once(data: &[u8]) {
    if let Ok(data) = String::from_utf8(data.to_vec()) {
        eprintln!("{}", data);
        if let Ok(parsed) = mil::parser::parse(&data) {
            if let MelExpr::Value(Value::Bytes(_)) = parsed {
                return;
            }
            eprintln!("{:?}", parsed);
            let _ = parsed.compile_onto(BinCode::default());
        }
    }
}

#[cfg(not(fuzzing))]
fn main() {}
