use anyhow::anyhow;
use mil::{
    cmdline::BuildCmd,
    compiler::{BinCode, Compile},
    executor::CovEnv,
    parser,
    parser::ParseError,
};
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;
use structopt::StructOpt;
use themelio_stf::{melvm::Covenant, Transaction};

/// List of transactions and coin inputs to execute a script on.
type TestTxs = Vec<(CovEnv, Transaction)>;

/// Read a list of transactions from a JSON file.
fn read_txs(fp: PathBuf) -> anyhow::Result<TestTxs> {
    let mut file = File::open(fp)?;
    let mut str_txs = String::new();
    file.read_to_string(&mut str_txs)?;

    // TODO: Don't expect here
    Ok(serde_json::from_str(&str_txs).expect("Failed to parse transactions as json."))
}

fn main() -> anyhow::Result<()> {
    env_logger::Builder::from_env("RUST_LOG")
        .parse_filters("mil=debug,warn")
        .init();
    // Command line arguments
    let cmd = BuildCmd::from_args();

    let mut file = File::open(cmd.in_file)?;
    let mut code = String::new();
    file.read_to_string(&mut code)?;

    // Parse to MelExpr ops
    let mel_ops = parser::parse(&code[..]).map_err(|e| match e {
        ParseError::Syntax(e) => match e {
            nom::Err::Failure(e) | nom::Err::Error(e) => {
                anyhow!(nom::error::convert_error(&code[..], e))
            }
            _ => unreachable!(),
        },
        ParseError::Expansion(msg) => anyhow!(msg.0),
    })?;

    // Compile to binary
    let empty = BinCode(Vec::new());
    let bincode = mel_ops.compile_onto(empty);
    log::debug!("final output: {:?}", bincode.0);
    let covenant = Covenant::from_ops(&bincode.0).unwrap();
    println!("{}", hex::encode(&covenant.0));
    // // Write to file
    // if let Some(out) = cmd.out_file {
    //     //println!("Binary as hex\n-------------\n{}\n", bincode);
    //     std::fs::write(out, &bincode.0[..])?;
    // }

    // // Generate hash of the script
    // let address = &tmelcrypt::hash_single(&bincode.0).to_addr();
    // // This is the only thing to print by default
    // println!("{}", address);

    // // Disassemble compiled binary
    // let ops = executor::disassemble(bincode).expect("Failed to disassemble binary.");

    // // Show disassembly of binary if asked to
    // if cmd.show_disassembly {
    //     println!("Disassembly:\n{:?}\n", ops);
    // }

    // // Execute script on provided transactions
    // // ---------------------------------------
    // if let Some(fp) = cmd.test_txs.clone() {
    //     let l = read_txs(fp)?;

    //     if cmd.debug {
    //         l.into_iter().enumerate().for_each(|(i, (cov_env, tx))| {
    //             println!("Debug execution log for tx#{}", i);
    //             //println!("{:?}", serde_json::to_string(&tx));

    //             let env = ExecutionEnv::new(tx, cov_env, ops.clone());
    //             // Display every step in debug mode
    //             env.iterate()
    //                 .take_while(|r| r.is_some())
    //                 .inspect(|res| match res {
    //                     Some((stack, heap, pc)) => println!(
    //                         "-----\n\
    //                             Executed instruction: {:?}\n\
    //                             Next instruction: {:?}\n\n\
    //                                 Stack\n{:?}\n\n\
    //                                 Heap\n{:?}\n",
    //                         ops[*pc - 1],
    //                         ops.get(*pc),
    //                         stack,
    //                         heap
    //                     ),
    //                     None => (),
    //                 })
    //                 .last();
    //         });
    //     } else {
    //         let weights: Vec<u128> = l.iter().map(|(_, tx)| tx.weight()).collect();
    //         let execs = l.into_iter().map(|(cov_env, tx)| {
    //             executor::execute(ExecutionEnv::new(tx, cov_env, ops.clone()))
    //         });

    //         execs.enumerate().for_each(|(i, res)| {
    //             // Show weight of the transaction
    //             println!("Transaction weight: {}", weights[i]);

    //             print!("tx#{} - ", i);
    //             match res {
    //                 Some(final_state) => {
    //                     println!("Successful execution.\n");
    //                     println!("Final stack\n--------\n{:?}", final_state.0);
    //                 }
    //                 None => {
    //                     println!("Execution failed.");
    //                 }
    //             }
    //         });
    //     }
    // }

    Ok(())
}
