pub mod tokens;
pub mod syntax;

use primitive_types::U256;
use crate::types::{SpecialOp, BuiltIn, Operator};

// TODO: hide underlying parse fns and provide a unified parser interface here.
//pub fn parse(input: &str) -> Result<Expr, ?>

/// First-pass expression type. Does not distinguish different types of s-expressions like [Expr]
/// does. Useful for tokenizing a string.
// TODO: Implement Display
#[derive(Debug)]
pub enum BaseExpr {
    /// Fundamental integer data type.
    Int(U256),
    /// A symbol representing some expression.
    Symbol(String),
    /// A built in operator.
    BuiltIn(BuiltIn),
    /// Operators with special evaluation cases.
    Special(SpecialOp),
    /// Recursive list of expressions.
    List(Vec<BaseExpr>),
}

impl BuiltIn {
    fn from_token(s: &str) -> Option<BuiltIn> {
        match s {
            "+" => Some(BuiltIn::Add),
            "-" => Some(BuiltIn::Sub),
            "*" => Some(BuiltIn::Mul),
            "/" => Some(BuiltIn::Div),
            "%" => Some(BuiltIn::Rem),
            "oflo?" => Some(BuiltIn::Oflo),
            "and" => Some(BuiltIn::And),
            "or" => Some(BuiltIn::Or),
            "xor" => Some(BuiltIn::Xor),
            "not" => Some(BuiltIn::Not),
            "nil" => Some(BuiltIn::Vempty),
            "cons" => Some(BuiltIn::Vpush),
            "get" => Some(BuiltIn::Vref),
            "len" => Some(BuiltIn::Vlen),
            "concat" => Some(BuiltIn::Vappend),
            "slice" => Some(BuiltIn::Vslice),
            "load" => Some(BuiltIn::Load),
            "store" => Some(BuiltIn::Store),
            _ => None,
        }
    }
}

impl SpecialOp {
    fn from_token(s: &str) -> Option<SpecialOp> {
        match s {
            "fn" => Some(SpecialOp::Defn),
            "set!" => Some(SpecialOp::Set),
            _ => None,
        }
    }
}
