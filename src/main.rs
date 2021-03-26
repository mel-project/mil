use mil::{
    parser, executor,
    parser::semantics::Evaluator};
    //compiler::{Compile, BinCode}};
use std::fs::File;
use std::io::prelude::*;
use blkstructs::melvm::Covenant;

fn main() -> std::io::Result<()> {
    let mut file = File::open("./examples/tmp.mil")?;
    let mut code = String::new();
    file.read_to_string(&mut code)?;

    // Parse to abstract syntax tree
    //let (_, expr) = parser::tokens::expr(&code[..])
    parser::tokens::root(&code[..])
        .map(|(_, expr)| {
            println!("Ast\n----\n{:?}", expr);
            let env = parser::semantics::Env::new(expr.0);
            println!("Expanded\n-----\n{:?}", env.expand_fns(&expr.1));
        })
        .map_err(|e| match e {
            nom::Err::Failure(e) | nom::Err::Error(e) => println!("{}", nom::error::convert_error(&code[..], e)),
            _ => unreachable!(),
        });
    /*
    match parser::tokens::root(&code[..]) {
        Ok(expr) => println!("Ast\n----\n{:?}", expr),
        Err(e) => match e {
            nom::Err::Failure(e) | nom::Err::Error(e) => println!("{}", nom::error::convert_error(&code[..], e)),
            _ => unreachable!(),
        },
    }
    */
        //.map_err(|e| format!("{:?}", e))?;
        //.expect("Failed to parse");
    //println!("Ast\n----\n{:?}", expr);
    //let ast = parser::syntax::expr(base_expr)
    //    .expect("Failed to parse");
    //println!("Ast\n----\n{:?}", ast);

    /*
    // Compile to binary
    let empty = BinCode(Vec::new());
    let bincode = ast.compile_onto(empty);
    // Write to file
    std::fs::write("script.mvm", &bincode.0[..])?;
    println!("Binary: b{}", bincode);

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
    */
    Ok(())
}
