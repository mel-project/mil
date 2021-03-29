pub mod tokens;
pub mod syntax;
pub mod semantics;
pub mod mel_expr;

use crate::types::{Symbol, BuiltIn, Expr};

// TODO: hide underlying parse fns and provide a unified parser interface here.
//pub fn parse(input: &str) -> Result<Expr, ?>

/// Syntax parser error type. May become may intricate later.
#[derive(Debug, PartialEq, Eq)]
pub struct ParseErr(String);

/// Short hand for a Result<_, ParseErr> type given the error string and args.
#[macro_export]
macro_rules! PErr {
    ($msg:expr) => {
        Err(ParseErr($msg.to_string()))
    };
    ($msg:expr, $($var:expr),+) => {
        Err(ParseErr(format!($msg, $($var),+)))
    }
}

/// Macros are not part of an [Expr] because they are only defined at the beginning of a program,
/// and cannot be nested.
pub type Defn = (Symbol, (Vec<Symbol>, Expr));

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
            //"load" => Some(BuiltIn::Load(e)),
            "not" => Some(BuiltIn::Not(e)),
            "len" => Some(BuiltIn::Vlen(e)),
            _ => None,
        }
    }
}
