use mil::{
    parser, executor,
    compiler::{Compile, BinCode}};
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
    println!("AST\n----\n{:?}", ast);

    // Compile to binary
    let empty = BinCode(Vec::new());
    let bincode = ast.compile_onto(empty);
    // Write to file
    std::fs::write("script.mvm", &bincode.0[..])?;
    print!("Binary: 0x");
    for b in bincode.0.iter() {
        print!("{:02x?}", b);
    }
    println!("");
    //println!("Binary code: {:#04x?}", bincode.0);

    // Wrap in a covenant
    let script = Covenant(bincode.0.clone());
    // Disassemble compiled binary
    print!("Disassembly: ");
    if let Some(ops) = script.to_ops() {
        println!("{:?}", ops);
    } else {
        println!("FAILED");
    }

    // Execute and print return value
    let v = executor::execute(bincode.clone());
    println!("Execution evaluated -> {}", v);
    Ok(())
}
