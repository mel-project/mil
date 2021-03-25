use std::fmt;
use primitive_types::U256;
use crate::types::{PushI, BuiltIn, MelExpr};

/*
#[derive(Clone)]
pub struct BinCode(pub Vec<u8>);

impl fmt::Display for BinCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.iter()
           .fold(String::from(""), |acc, bit|
               acc + &format!("{:02x?}", bit)))
    }
}

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

/*
fn write_vec(mut bin: BinCode, v: &Vec<Atom>) -> BinCode {
    // Write v.len() vpush opcodes
    let vpush: u8 = (&BuiltIn::Vpush).into();
    bin.0.extend( (1..v.len()).map(|_| vpush) );

    // Followed by vempty
    bin.0.push( (&BuiltIn::Vempty).into() );

    // Then the elements in reverse
    v.iter().rev().fold(bin, |acc, e| e.compile_onto(acc))
}
*/

/*
impl Compile for Atom {
    fn compile_onto(&self, b: BinCode) -> BinCode {
        match self {
            // PushI
            Atom::Int(n) => write_pushi(b, n),
            //Atom::Vec{members, ..} => write_vec(b, members),
        }
    }
}
*/

impl Compile for MelExpr {
    fn compile_onto(&self, mut b: BinCode) -> BinCode {
        match self {
            // Integers evaluate to themselves (push onto stack)
            MelExpr::Int(n) => write_pushi(b, n),
            // Evaluate the args, then append the op (postfix)
            MelExpr::App(op, args) => op.compile_onto(
                                args.iter().fold(b, |b_acc, arg|
                                    arg.compile_onto(b_acc))),
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
            //BuiltIn::Oflo => 0x15,
            BuiltIn::And => 0x20,
            BuiltIn::Or => 0x21,
            BuiltIn::Xor => 0x22,
            BuiltIn::Not => 0x23,
            BuiltIn::Vpush => 0x54,
            BuiltIn::Vempty => 0x52,
            BuiltIn::Vref => 0x50,
            BuiltIn::Vlen => 0x53,
            BuiltIn::Vappend => 0x51,
            BuiltIn::Vslice => 0x55,
            BuiltIn::Load => 0x40,
            BuiltIn::Store => 0x41,
        }
    }
}

/*
match expr {
    Expr::Defn(name, args, body) => new_fn(name, args, body),
}
*/

/// Opcode mapping
impl From<PushI> for u8 {
    fn from(p: PushI) -> u8 {
        0xf1
    }
}
*/


#[cfg(test)]
mod test {
    use super::*;
    use crate::parser;
    use blkstructs::melvm::{Covenant, OpCode::{self, *}};

    /*
    fn compile(code: &str) -> Result<BinCode, ()> {
        // Parse
        let (_, ast) = parser::expr(&code[..])
            .map_err(|_| ())?;

        let empty = BinCode(Vec::new());
        Ok( ast.compile_onto(empty) )
    }

    /// Compile a str of code and disassemble back into ops using melVM.
    fn to_ops(code: &str) -> Result<Vec<OpCode>, ()> {
        let bin = compile(code)?;
        Covenant(bin.0).to_ops().ok_or(())
    }
    */

    fn assert_veq<T: PartialEq + std::fmt::Debug>(v1: Vec<T>, v2: Vec<T>) {
        v1.iter().zip(v2.iter())
          .for_each(|(x,y)| assert_eq!(x, y))
    }

    /*
    #[test]
    fn cons_a_vec() {
        let bin = compile("(cons 1 (nil))")
            .unwrap();

        assert_eq!(
            format!("{}", bin),
            "f100000000000000000000000000000000000000000000000000000000000000015254");
    }

    #[test]
    fn vec_len() {
        let ops = to_ops("(len (cons 2 (cons 1 (nil))))").unwrap();
        let target = vec![PUSHI(U256::from(2)), PUSHI(U256::from(1)), VEMPTY, VPUSH, VPUSH, VLENGTH];

        assert_veq(ops, target);
    }

    #[test]
    fn vec_append() {
        let ops = to_ops("(concat (cons 2 (cons 1 (nil))) (cons 4 (cons 3 (nil))))").unwrap();
        let target = vec![PUSHI(U256::from(2)), PUSHI(U256::from(1)), VEMPTY, VPUSH, VPUSH,
                          PUSHI(U256::from(4)), PUSHI(U256::from(3)), VEMPTY, VPUSH, VPUSH, VLENGTH];

        assert_veq(ops, target);
    }
    */
}
