pub mod tokens;
pub mod syntax;

use primitive_types::U256;
use crate::types::{BuiltIn, Operator};

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
    Special(SpecialForm),
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
            "fn" => Some(BuiltIn::Defn),
            _ => None,
        }
    }
}
