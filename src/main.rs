use mil::parser;
use std::fs::File;
use std::io::prelude::*;
use blkstructs::melvm::Covenant;

fn main() -> std::io::Result<()> {
    let mut file = File::open("./examples/tmp.mil")?;
    let mut code = String::new();
    file.read_to_string(&mut code)?;

    // Compile to a covenant
    //let script = Covenant{ bincode };
    // Make sure the compiled binary can be disassembled
    //println!("{:?}", script.to_ops());
    //println!("{:?}", parser::parse_sexp(&code[..]));
    Ok(())
}
