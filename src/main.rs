use mil::parser;
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::open("./examples/tmp.mil")?;
    let mut code = String::new();
    file.read_to_string(&mut code)?;

    println!("{:?}", parser::parse_sexp(&code[..]));
    Ok(())
}
