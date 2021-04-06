use mil::{
    parser, executor,
    cmdline::BuildCmd,
    parser::mel_expr::MemoryMap,
    parser::expansion::Evaluator,
    compiler::{Compile, BinCode}};
use std::fs::File;
use std::io::prelude::*;
use blkstructs::melvm::{Transaction, Covenant};
use structopt::StructOpt;
use tmelcrypt::ed25519_keygen;

fn main() -> std::io::Result<()> {
    // Command line arguments
    let cmd: BuildCmd = StructOpt::from_args();

    //let mut file = File::open("./examples/tmp.mil")?;
    let mut file = File::open(cmd.in_file)?;
    let mut code = String::new();
    file.read_to_string(&mut code)?;

    // Parse to MelExpr ops
    let mel_ops = parser::syntax::root(&code[..])
        .map(|(_, (fn_defs, ast))| {
            // First pass AST
            println!("Ast\n----\n{:?}\n", (fn_defs.clone(), ast.clone()));
            let env = parser::expansion::Env::new(fn_defs);

            // Expand AST
            let expanded = env.expand_fns(&ast);
            println!("Expanded\n-----\n{:?}\n", expanded);

            // Low-level MelExpr
            let mut mem  = MemoryMap::new();
            let mel_expr = mem.to_mel_expr(expanded.unwrap());
            println!("MelVM\n-----\n{:?}\n", mel_expr);
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
    println!("\nDisassembly: ");
    if let Some(ops) = script.to_ops() {
        println!("{:?}\n", ops);

        // Dummy spender transaction calls the covenant
        let (pk, sk) = ed25519_keygen();
        let tx = Transaction::empty_test().sign_ed25519(sk);

        let mut env = executor::ExecutionEnv::new(&tx, &ops);
        if let Some(final_state) = env.into_iter()
            //.inspect(|(stack,heap)| println!("Stack\n{:?}", stack))
            .last()
        {
            println!("Final stack\n--------\n{:?}", final_state.0);
        }
    } else {
        println!("FAILED");
    }

    Ok(())
}
