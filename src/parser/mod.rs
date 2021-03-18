pub mod tokens;
pub mod syntax;

use crate::types::{BuiltIn, Operator};

/// Generic S-expression function application. Every expression has this form.
/// Since Operator is not a standalone expression, we uniquely identify it as the start of an
/// S-expression.
type App = (Operator, Vec<BaseExpr>);

/// First-pass expression type. Does not distinguish different types of s-expressions like [Expr]
/// does. Useful for tokenizing a string.
pub enum BaseExpr {
    /// Fundamental integer data type.
    Int(U256),
    /// A symbol representing some expression.
    Symbol(String),
    /// Recursive list of expressions.
    List(Vec<BaseExpr>),
    /// Application of an op to some arguments.
    //App(Operator, Vec<BaseExpr>),
}

// Use the syntax parsers to find the most appropriate corresponding Expr.
// [Expr] is a superset of [BaseExpr]; a BaseExpr can always convert into an Expr as an App
// or Int, so the Result of a parser is ignored.
// TODO: Maybe don't just unwrap if syntax parser analyzes symbol table or semantics
impl From<BaseExpr> for Expr {
    fn from(e: BaseExpr) -> Expr {
        syntax::expr(e).unwrap()
    }
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
