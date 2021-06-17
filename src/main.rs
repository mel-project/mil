use mil::{
    parser, executor,
    cmdline::BuildCmd,
    parser::ParseError,
    executor::{ExecutionEnv, CovEnv},
    compiler::{Compile, BinCode}};
use std::fs::File;
use std::path::PathBuf;
use std::io::prelude::*;
use structopt::StructOpt;
use tmelcrypt::ed25519_keygen;
use themelio_stf::Transaction;
use anyhow::anyhow;

/// List of transactions and coin inputs to execute a script on.
type TestTxs = Vec<(CovEnv, Transaction)>;

/// Read a list of transactions from a JSON file.
fn read_txs(fp: PathBuf) -> anyhow::Result<TestTxs> {
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
/*
fn parse(code: &str) -> Result<MelExpr, ()> {
    let mel_ops = parser::parse(&code[..])
        .map_err(|e| match e {
            ParseError::Syntax(e) => match e {
                nom::Err::Failure(e) | nom::Err::Error(e) => format!("{}", nom::error::convert_error(&code[..], e)),
                _ => unreachable!(),
            },
            ParseError::Expansion(msg) => format!("{}", msg.0),
        })?;
*/

//fn main() -> std::io::Result<()> {
fn main() -> anyhow::Result<()> {
    // Command line arguments
    let cmd: BuildCmd = StructOpt::from_args();

    let mut file = File::open(cmd.in_file)?;
    let mut code = String::new();
    file.read_to_string(&mut code)?;

    // Parse to MelExpr ops
    let mel_ops = parser::parse(&code[..])
        .map_err(|e| match e {
            ParseError::Syntax(e) => match e {
                nom::Err::Failure(e) | nom::Err::Error(e) =>
                    anyhow!(format!("{}", nom::error::convert_error(&code[..], e))),
                _ => unreachable!(),
            },
            ParseError::Expansion(msg) => anyhow!(format!("{}", msg.0)),
        })?;

    // Compile to binary
    let empty = BinCode(Vec::new());
    let bincode = mel_ops.compile_onto(empty);

    // Write to file
    if let Some(out) = cmd.out_file {
        //println!("Binary as hex\n-------------\n{}\n", bincode);
        std::fs::write(out, &bincode.0[..])?;
    }

    // Generate hash of the script
    let cov_hash = &tmelcrypt::hash_single(&bincode.0);
    // This is the only thing to print by default
    println!("{}", cov_hash);

    // Disassemble compiled binary
    let ops = executor::disassemble(bincode)
        .ok_or("Failed to disassemble binary.").unwrap();

    // Show disassembly of binary if asked to
    if cmd.show_disassembly {
        println!("Disassembly:\n{:?}\n", ops);
    }

    // Execute script on provided transactions
    // ---------------------------------------
    if let Some(fp) = cmd.test_txs.clone() {
        let l = read_txs(fp)?;

        if cmd.debug {
            l.into_iter().enumerate().for_each(|(i, (cov_env, tx))| {
                println!("Debug execution log for tx#{}", i);
                //println!("{:?}", serde_json::to_string(&tx));

                let mut env = ExecutionEnv::new(tx, cov_env, &ops);
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
            let execs = l.into_iter()
                .map(|(cov_env, tx)|
                    executor::execute(
                        ExecutionEnv::new(tx, cov_env, &ops)));

            execs.enumerate().for_each(|(i,res)| {
                print!("tx#{} - ", i);
                match res {
                    Some(final_state) => {
                        println!("Successful execution.\n");
                        println!("Final stack\n--------\n{:?}", final_state.0);
                    },
                    None => {
                        println!("Execution failed.");
                    },
                }
            });
        }
    }

    Ok(())
}
