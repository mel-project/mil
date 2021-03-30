use mil::{
    parser, executor,
    parser::mel_expr::MemoryMap,
    parser::semantics::Evaluator,
    compiler::{Compile, BinCode}};
use std::fs::File;
use std::io::prelude::*;
use blkstructs::melvm::Covenant;

fn main() -> std::io::Result<()> {
    let mut file = File::open("./examples/tmp.mil")?;
    let mut code = String::new();
    file.read_to_string(&mut code)?;

    // Parse to MelExpr ops
    let mel_ops = parser::tokens::root(&code[..])
        .map(|(_, (fn_defs, ast))| {
            // First pass AST
            println!("Ast\n----\n{:?}", (fn_defs.clone(), ast.clone()));
            let env = parser::semantics::Env::new(fn_defs);

            // Expand AST
            let expanded = env.expand_fns(&ast);
            println!("Expanded\n-----\n{:?}", expanded);

            // Low-level MelExpr
            let mut mem  = MemoryMap::new();
            let mel_expr = mem.to_mel_expr(expanded.unwrap());
            println!("MelVM\n-----\n{:?}", mel_expr);
            mel_expr
        })
        .map_err(|e| match e {
            nom::Err::Failure(e) | nom::Err::Error(e) => println!("{}", nom::error::convert_error(&code[..], e)),
            _ => unreachable!(),
        }).unwrap();

    // Compile to binary
    let empty = BinCode(Vec::new());
    let bincode = mel_ops.compile_onto(empty);
    // Write to file
    std::fs::write("script.mvm", &bincode.0[..])?;
    println!("Binary: b{}", bincode);

    // Wrap in a covenant
    let script = Covenant(bincode.0.clone());
    // Disassemble compiled binary
    println!("Disassembly: ");

    // Manual disassmebly
    let mut reversed = script.0.clone();
    reversed.reverse();
    let mut output = Vec::new();
    while !reversed.is_empty() {
        match Covenant::disassemble_one(&mut reversed, &mut output, 0) {
            Some(_) => println!("op success"),
            None => println!("broke"),
        }
    }

    /*
    if let Some(ops) = script.to_ops() {
        println!("{:?}", ops);
    } else {
        println!("FAILED");
    }
    */

    // Execute and print return value
    let v = executor::execute(bincode.clone());
    println!("Execution evaluated -> {}", v);
    Ok(())
}
