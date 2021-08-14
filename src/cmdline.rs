use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "Mil", about = "Mil compiler")]
pub struct BuildCmd {
    /// Display VM state at each step of execution.
    #[structopt(short, long)]
    pub debug: bool,
    /// File containing the .mil program to compile.
    pub in_file: PathBuf,
    /// Where to write the compiled binary.
    #[structopt(long = "out")]
    pub out_file: Option<PathBuf>,
    /// File containing a list of transactions as json to test the compiled script.
    #[structopt(long)]
    pub test_txs: Option<PathBuf>,
    /// Show disassembly of the binary. Useful for verifying the binary will be interpreted
    /// correctly.
    #[structopt(long = "disassembly")]
    pub show_disassembly: bool,
}
