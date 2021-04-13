use structopt::StructOpt;
use std::path::PathBuf;

#[derive(Debug, StructOpt)]
#[structopt(name = "Mil", about="Mil compiler")]
pub struct BuildCmd {
    /// File containing the .mil program to compile.
    pub in_file: PathBuf,
    /// Where to write the compiled binary.
    pub out_file: Option<PathBuf>,
    /// File containing a list of transactions as json to test the compiled script on.
    pub test_txs: Option<PathBuf>,
}
