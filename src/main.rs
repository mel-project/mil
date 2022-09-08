use anyhow::anyhow;
use mil::{
    cmdline::BuildCmd,
    compiler::{BinCode, Compile},
    parser,
    parser::ParseError,
};
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;
use structopt::StructOpt;
use themelio_stf::melvm::Covenant;
use themelio_structs::Transaction;

/*
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
*/
nom_packrat::storage!(String);

fn main() -> anyhow::Result<()> {
    env_logger::Builder::from_env("RUST_LOG")
        .parse_filters("mil=debug,warn")
        .init();

    // Command line arguments
    let cmd = BuildCmd::from_args();

    let mut file = File::open(cmd.in_file)?;
    let mut code = String::new();
    file.read_to_string(&mut code)?;
    log::debug!("read everything");

    // Parse to MelExpr ops
    let mel_ops = parser::parse_no_optimize(&code[..]).map_err(|e| match e {
        ParseError::Syntax(e) => match e {
            nom::Err::Failure(e) | nom::Err::Error(e) => {
                anyhow!(nom::error::convert_error(&code[..], e))
            }
            _ => unreachable!(),
        },
        ParseError::Expansion(msg) => anyhow!(msg.0),
    })?;
    log::debug!("parsing done");

    // Compile to binary
    let empty = BinCode(Vec::new());
    let bincode = mel_ops.compile_onto(empty);
    log::debug!("final output: {:?}", bincode.0);
    let covenant = Covenant::from_ops(&bincode.0).unwrap();
    println!("{}", hex::encode(&covenant.0));

    Ok(())
}
