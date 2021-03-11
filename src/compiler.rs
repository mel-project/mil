use primitive_types::U256;
use crate::types::{BuiltIn, Atom, Expr};

#[derive(Clone)]
pub struct BinCode(pub Vec<u8>);

pub trait Compile {
    /// Produce MelVM interpretable binary from a data type. Consumes a binary struct
    /// and mutates for efficient allocation.
    fn compile_onto(&self, b: BinCode) -> BinCode;

    // Map on the output of the 
    // fn map(&self, other: impl Fn(BinCode) -> BinCode) -> BinCode
}

impl Compile for BuiltIn {
    fn compile_onto(&self, mut b: BinCode) -> BinCode {
        match self {
            Add => b.0.push(0x10),
            Sub => b.0.push(0x11),
        }

        b
    }
}

// Convenience fn for allocating and appending an op code and number to bincode
fn append_pushi(b: &mut Vec<u8>, op: u8, n: &U256) {
    // Write op + n to bincode
    b.push(op);

    let idx = b.len();

    // Extend vec by 32 bytes to effeciently add U256
    let B = 32;
    b.reserve(B);
    unsafe { b.set_len(idx + B); }

    n.to_big_endian(&mut b[idx..]);
}

impl Compile for Atom {
    fn compile_onto(&self, mut b: BinCode) -> BinCode {
        match self {
            // PushI
            Atom::Int(n) => append_pushi(&mut b.0, 0xf1, n),
        }

        b
    }
}

impl Compile for Expr {
    fn compile_onto(&self, mut b: BinCode) -> BinCode {
        match self {
            // Evaluate the args, then append the op
            Expr::App(op, args) => op.compile_onto(
                                args.iter().fold(b, |b_acc, arg|
                                    arg.compile_onto(b_acc))),
            // Push atoms
            Expr::Atom(v) => v.compile_onto(b),
        }
    }
}
