pub mod syntax;
pub mod expansion;
pub mod mel_expr;

use mel_expr::MemoryMap;
use expansion::Evaluator;
use crate::types::{MelExpr, Symbol, BuiltIn, Expr};

/// Module-level aggregate error type. Unifies all parser-type errors.
#[derive(Debug)]
pub enum ParseError<E> {
    Syntax(nom::Err<E>),
    Expansion(ParseErr),
}

/// Number of reserved locations on an execution heap, enumerated from 0.
const NUM_RESERVED: i32 = 8;

// TODO: hide underlying parse fns and provide a unified parser interface here.
pub fn parse(input: &str) -> Result<MelExpr, ParseError<nom::error::VerboseError<&str>>> {
    // First pass AST
    syntax::root(input)
        .map_err(|verbose_err| ParseError::Syntax(verbose_err))
        // Expand AST
        .and_then(|(_, (fn_defs, ast))| {
            let env = expansion::Env::new(fn_defs);
            env.expand_fns(&ast)
               .map_err(|e| ParseError::Expansion(e))
        })
        // Low-level MelExpr
        .map(|expanded| {
            let mut mem = MemoryMap::new();
            mem.to_mel_expr(expanded)})
}

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

/// Try to extract values from results in vector. Short circuit on the first failure. Note this
/// does not return an iterator (because it folds).
// TODO: For efficiency: fold_results(v: Vec<O>, f: Fn(O) -> Result<O,E>), map and fold in one pass
fn fold_results<O,E>(v: Vec<Result<O, E>>) -> Result<Vec<O>, E> {
    v.into_iter()
     .try_fold(vec![], |mut inner_vec, r| { //inner_vec.push(v)))
         let v = r?;
         inner_vec.push(v);
         Ok(inner_vec)
     })
    //.map(|mut v| { v.reverse(); v })
}
