use mil::parser;
use mil::compiler::{Compile, BinCode};
use std::fs::File;
use std::io::prelude::*;
use blkstructs::melvm::Covenant;

fn main() -> std::io::Result<()> {
    let mut file = File::open("./examples/tmp.mil")?;
    let mut code = String::new();
    file.read_to_string(&mut code)?;

    // Parse to abstract syntax tree
    let (_, ast) = parser::expr(&code[..])
        .expect("Failed to parse");

    // Compile to binary
    let empty = BinCode(Vec::new());
    let bincode = ast.compile_onto(empty);

    // Wrap in a covenant
    let script = Covenant(bincode.0);

    // Make sure the compiled binary can be disassembled
    println!("{:?}", script.to_ops());
    //println!("{:?}", parser::parse_sexp(&code[..]));
    Ok(())
}
