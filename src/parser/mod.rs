mod expansion;
mod mel_expr;
mod syntax;

/// Count the number of instructions in a [MelExpr].
pub use mel_expr::count_insts;

use crate::{
    optimize,
    types::{Expr, MelExpr, Symbol},
};
use expansion::Evaluator;
use mel_expr::MemoryMap;

/// Module-level aggregate error type. Unifies all parser-type errors.
#[derive(Debug)]
pub enum ParseError<E> {
    /// Error deriving from syntax parsing.
    Syntax(nom::Err<E>),
    /// Error during macro expansion of the AST.
    Expansion(ParseErr),
}

/// A macro definition type.
/// Macros are not part of an [Expr] because they are only defined at the beginning of a program,
/// and cannot be nested.
type Defn = (Symbol, (Vec<Symbol>, Expr));

/// Number of reserved locations on an execution heap, enumerated from 0.
pub const NUM_RESERVED: i32 = 32;

/// Parse a string into the low-level abstract syntax tree, [MelExpr],
/// which can be directly compiled to bytes.
pub fn parse(input: &str) -> Result<MelExpr, ParseError<nom::error::VerboseError<&str>>> {
    // First pass AST
    syntax::root(input)
        .map_err(ParseError::Syntax)
        // Expand AST
        .and_then(|(_, (fn_defs, ast))| {
            //println!("{:?}\n\n{:?}\n", fn_defs, ast);
            let env = expansion::Env::new(fn_defs);
            env.expand_fns(&ast).map_err(ParseError::Expansion)
        })
        // Low-level MelExpr
        .map(|expanded| {
            let expanded = optimize::let_useonce(expanded);
            let mut mem = MemoryMap::new();
            mem.unrolled_to_mel(expanded)
        })
}

/// Syntax parser error type. May become may intricate later.
#[derive(Debug, PartialEq, Eq)]
pub struct ParseErr(pub String);

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

/// Try to extract values from results in vector. Short circuit on the first failure. Note this
/// does not return an iterator (because it folds).
// TODO: For efficiency: fold_results(v: Vec<O>, f: Fn(O) -> Result<O,E>), map and fold in one pass
fn fold_results<O, E>(v: Vec<Result<O, E>>) -> Result<Vec<O>, E> {
    v.into_iter().try_fold(vec![], |mut inner_vec, r| {
        //inner_vec.push(v)))
        let v = r?;
        inner_vec.push(v);
        Ok(inner_vec)
    })
    //.map(|mut v| { v.reverse(); v })
}
