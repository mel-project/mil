pub mod tokens;
pub mod syntax;

use crate::types::{Symbol, BuiltIn, Expr};

// TODO: hide underlying parse fns and provide a unified parser interface here.
//pub fn parse(input: &str) -> Result<Expr, ?>

/// Macros are not part of an [Expr] because they are only defined at the beginning of a program,
/// and cannot be nested.
pub type Defn = (Symbol, (Vec<Symbol>, Expr));

/*
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
*/

impl BuiltIn {
    fn from_bin_token(s: &str, e1: Expr, e2: Expr) -> Option<BuiltIn> {
        match s {
            "+" => Some(BuiltIn::Add(e1, e2)),
            "-" => Some(BuiltIn::Sub(e1, e2)),
            "*" => Some(BuiltIn::Mul(e1, e2)),
            "/" => Some(BuiltIn::Div(e1, e2)),
            "%" => Some(BuiltIn::Rem(e1, e2)),
            "and" => Some(BuiltIn::And(e1, e2)),
            "or" => Some(BuiltIn::Or(e1, e2)),
            "xor" => Some(BuiltIn::Xor(e1, e2)),
            "cons" => Some(BuiltIn::Vpush(e1, e2)),
            "get" => Some(BuiltIn::Vref(e1, e2)),
            "concat" => Some(BuiltIn::Vappend(e1, e2)),
            _ => None,
        }
    }
    fn from_tri_token(s: &str, e1: Expr, e2: Expr, e3: Expr) -> Option<BuiltIn> {
        match s {
            "slice" => Some(BuiltIn::Vslice(e1, e2, e3)),
            _ => None,
        }
    }

    fn from_empty_token(s: &str) -> Option<BuiltIn> {
        match s {
            "nil" => Some(BuiltIn::Vempty),
            _ => None,
        }
    }

    fn from_uni_token(s: &str, e: Expr) -> Option<BuiltIn> {
        match s {
            "load" => Some(BuiltIn::Load(e)),
            "not" => Some(BuiltIn::Not(e)),
            "len" => Some(BuiltIn::Vlen(e)),
            _ => None,
        }
    }
    /*
    fn from_token(s: &str) -> Option<BuiltIn> {
        match s {
            "+" => Some(BuiltIn::Add),
            "-" => Some(BuiltIn::Sub),
            "*" => Some(BuiltIn::Mul),
            "/" => Some(BuiltIn::Div),
            "%" => Some(BuiltIn::Rem),
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
    */
}

/*
impl SpecialOp {
    fn from_token(s: &str) -> Option<SpecialOp> {
        match s {
            //"fn" => Some(SpecialOp::Defn),
            "set!" => Some(SpecialOp::Set),
            _ => None,
        }
    }
}
*/
