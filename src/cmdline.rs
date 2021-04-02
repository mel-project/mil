use structopt::StructOpt;
use std::path::PathBuf;

#[derive(Debug, StructOpt)]
#[structopt(name = "Mil", about="Mil compiler")]
pub struct BuildCmd {
    pub in_file: PathBuf,
    pub out_file: Option<PathBuf>,
}
