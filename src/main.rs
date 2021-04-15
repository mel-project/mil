use mil::{
    parser, executor,
    cmdline::BuildCmd,
    parser::mel_expr::MemoryMap,
    parser::expansion::Evaluator,
    compiler::{Compile, BinCode}};
use std::fs::File;
use std::path::PathBuf;
use std::io::prelude::*;
use structopt::StructOpt;
use tmelcrypt::ed25519_keygen;
use blkstructs::melvm::{Transaction, Covenant};

/*
#[derive(Deserialize)]
struct TestTxs {
    pub txs: Vec<Transaction>,
}
*/

/// Read a list of transactions from a JSON file.
fn read_txs(fp: PathBuf) -> anyhow::Result<Vec<Transaction>> {
    let mut file = File::open(fp)?;
    let mut str_txs = String::new();
    file.read_to_string(&mut str_txs)?;

    // TODO: Don't expect here
    Ok(serde_json::from_str(&str_txs)
        .expect("Failed to parse transactions as json."))
}

/*
fn execute_on_txs(txs: TestTxs, bin: &BinCode) {
    let cov_hash = &tmelcrypt::hash_single(&bincode.0);
    // Disassemble compiled binary
    if let Some(ops) = executor::disassemble(bincode) {
        //println!("Disassembly:\n{:?}\n", ops);

        //if let Some(fp) = cmd.test_txs {
            //let l = read_txs(fp)?;
            let execs = txs.iter()
                .map(|tx| executor::execute( executor::ExecutionEnv::new(&tx, &ops, cov_hash) ));

            execs.for_each(|res| match res {
                Some(final_state) => {
                    println!("Successful execution.\n");
                    println!("Final stack\n--------\n{:?}", final_state.0);
                },
                None => {
                    println!("Execution failed.");
                },
            });
        }
    }
}
*/

//fn main() -> std::io::Result<()> {
fn main() -> anyhow::Result<()> {
    // Command line arguments
    let cmd: BuildCmd = StructOpt::from_args();

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
    println!("Binary: 0x{}\n", bincode);

    let cov_hash = &tmelcrypt::hash_single(&bincode.0);
    // Disassemble compiled binary
    if let Some(ops) = executor::disassemble(bincode) {
        println!("Disassembly:\n{:?}\n", ops);

        // Execute script on provided transactions
        // ---------------------------------------

        if let Some(fp) = cmd.test_txs.clone() {
            let l = read_txs(fp)?;

            if cmd.debug {
                l.iter().enumerate().for_each(|(i,tx)| {
                    println!("Debug execution log for tx#{}", i);
                    println!("{:?}", serde_json::to_string(&tx));

                    let mut env = executor::ExecutionEnv::new(&tx, &ops, cov_hash);
                    // Display every step in debug mode
                    env.into_iter()
                        .take_while(|r| r.is_some())
                        .inspect(|res| match res {
                            Some((stack,heap,pc)) =>
                                println!("-----\n\
                                    Executed instruction: {:?}\n\
                                    Next instruction: {:?}\n\n\
                                        Stack\n{:?}\n\n\
                                        Heap\n{:?}\n",
                                    ops[*pc-1], ops.get(*pc), stack, heap),
                            None => (),
                        })
                        .last();
                });
            } else {
                let execs = l.iter()
                    .map(|tx| executor::execute( executor::ExecutionEnv::new(&tx, &ops, cov_hash) ));

                execs.for_each(|res| match res {
                    Some(final_state) => {
                        println!("Successful execution.\n");
                        println!("Final stack\n--------\n{:?}", final_state.0);
                    },
                    None => {
                        println!("Execution failed.");
                    },
                });
            }
        }

        // Execute on a dummy transaction if none provided
        // -----------------------------------------------

        if cmd.test_txs.is_none() {
            let (_, sk) = ed25519_keygen();
            let tx = Transaction::empty_test().sign_ed25519(sk);
            //println!("{:?}", serde_json::to_string(&tx));
            let mut env = executor::ExecutionEnv::new(&tx, &ops, cov_hash);

            if cmd.debug {
                // Display every step in debug mode
                env.into_iter()
                    .take_while(|r| r.is_some())
                    .inspect(|res| match res {
                        Some((stack,heap,pc)) =>
                            println!("-----\n\
                                Executed instruction: {:?}\n\
                                Next instruction: {:?}\n\n\
                                    Stack\n{:?}\n\n\
                                    Heap\n{:?}\n",
                                ops[*pc-1], ops.get(*pc), stack, heap),
                        None => (),
                    })
                    .last();
            } else {
                // Just display the final state
                if let Some(final_state) = executor::execute(env) {
                    println!("Successful execution.\n");
                    println!("Final stack\n--------\n{:?}", final_state.0);
                } else {
                    println!("Execution failed.");
                }
            }
        }
    } else {
        println!("Disassembly failed!");
    }

    Ok(())
}
