use primitive_types::U256;
use crate::types::{PushI, BuiltIn, Atom, Expr};

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
        b.0.push(self.into());
        b
    }
}

// Convenience fn for allocating and appending an op code and number to bincode
fn write_pushi(mut b: BinCode, n: &U256) -> BinCode {
    // Write op + n to bincode
    b.0.push(PushI.into());

    let idx = b.0.len();

    // Extend vec by 32 bytes to effeciently add U256
    let B = 32;
    b.0.reserve(B);
    unsafe { b.0.set_len(idx + B); }

    n.to_big_endian(&mut b.0[idx..]);

    b
}

fn write_vec(mut bin: BinCode, v: &Vec<Atom>) -> BinCode {
    // Write v.len() vpush opcodes
    let vpush: u8 = (&BuiltIn::Vpush).into();
    bin.0.extend( (1..v.len()).map(|_| vpush) );

    // Followed by vempty
    bin.0.push( (&BuiltIn::Vempty).into() );

    // Then the elements in reverse
    v.iter().rev().fold(bin, |acc, e| e.compile_onto(acc))
}

impl Compile for Atom {
    fn compile_onto(&self, b: BinCode) -> BinCode {
        match self {
            // PushI
            Atom::Int(n) => write_pushi(b, n),
            Atom::Vec{members, ..} => write_vec(b, members),
        }
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

/// Opcode mapping
impl From<&BuiltIn> for u8 {
    fn from(b: &BuiltIn) -> u8 {
        match b {
            BuiltIn::Add => 0x10,
            BuiltIn::Sub => 0x11,
            BuiltIn::Mul => 0x12,
            BuiltIn::Div => 0x13,
            BuiltIn::Rem => 0x14,
            BuiltIn::Oflo => 0x15,
            BuiltIn::And => 0x20,
            BuiltIn::Or => 0x21,
            BuiltIn::Xor => 0x22,
            BuiltIn::Not => 0x23,
            BuiltIn::Vpush => 0x54,
            BuiltIn::Vempty => 0x52,
            BuiltIn::Load => 0x40,
            BuiltIn::Store => 0x41,
        }
    }
}

/// Opcode mapping
impl From<PushI> for u8 {
    fn from(p: PushI) -> u8 {
        0xf1
    }
}
