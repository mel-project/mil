//use crate::parser::BaseExpr;
//use crate::types::{SpecialOp, Symbol, BuiltIn, Expr, Operator};

/*
/// Syntax parser result type.
type ParseRes = Result<Expr, ParseErr>;
/// Syntax parser error type. May become may intricate later.
#[derive(Debug, PartialEq, Eq)]
pub struct ParseErr(String);

/// Top-level converter from any [BaseExpr] into an [Expr].
/// This is called after the first pass of parsing, and analyzes
/// syntactic details of the input program.
pub fn expr(input: BaseExpr) -> ParseRes {
    match input {
        BaseExpr::Int(n) => Ok( Expr::Int(n) ),
        BaseExpr::Symbol(s) =>
            Err( ParseErr(format!("Standalone operator '{:?}' is not a valid expression. Consider wrapping in parenthesis.", s)) ),
        BaseExpr::Special(s) =>
            Err( ParseErr(format!("Standalone operator '{:?}' is not a valid expression. Consider wrapping in parenthesis.", s)) ),
        BaseExpr::BuiltIn(s) =>
            Err( ParseErr(format!("Standalone operator '{:?}' is not a valid expression. Consider wrapping in parenthesis.", s)) ),
        BaseExpr::List(mut elems) => {
            // Reverse list to parse in prefix
            elems.reverse();

            let op = elems.pop()
                // Should never happen since parser uses 'separated_list1'
                .ok_or( ParseErr("Expected an operator in empty list expression.".to_string()) )?;

            // Determine the operator-type of the list and handle accordingly
            match op {
                //BaseExpr::Symbol(op) => app(from(Operator::Symbol(op), elems)),
                BaseExpr::Symbol(ref op) => app(to_op(op), elems),
                BaseExpr::BuiltIn(op) => app(Operator::BuiltIn(op), elems),
                BaseExpr::Special(op) => match op {
                    //SpecialOp::Defn => defn(elems),
                    SpecialOp::Set => set(elems),
                },
                BaseExpr::Int(_) | BaseExpr::List(_) =>
                    Err(ParseErr(format!("First element of list, {:?}, should be an operator.", op))),
            }
        },
    }
}

/*
/// Parses a function definition if the input is well formed. Otherwise returns an error.
fn defn(mut elems: Vec<BaseExpr>) -> ParseRes {
    // TODO: Check for symbol conflicts here? Set symbol into env
    if elems.len() != 3 {
        return Err( ParseErr(format!("Expected 3 elements in a function definition, found {}.", elems.len())));
    }

    let fn_name = symbol(  elems.pop().unwrap() )?;
    let params  = fn_args( elems.pop().unwrap() )?;
    let body    = expr(    elems.pop().unwrap() )?;

    Ok( Expr::Defn(fn_name, params, Box::new(body)) )
}
*/

/// Parse a set! expression.
fn set(mut elems: Vec<BaseExpr>) -> ParseRes {
    if elems.len() != 2 {
        return Err( ParseErr(format!("Expected 2 elements to 'set!', found {}.", elems.len())));
    }

    let name = symbol(  elems.pop().unwrap() )?;
    let value = expr(   elems.pop().unwrap() )?;

    Ok( Expr::Set(name, Box::new(value)) )
}

/// Potentially convert a BaseExpr::Symbol to some kind of [Operator].
fn to_op(s: &str) -> Operator {
    BuiltIn::from_token(s)
            .map(Operator::BuiltIn)
        .or(SpecialOp::from_token(s)
            .map(Operator::Special))
        .or(Some(Operator::Symbol(s.to_string())))
        .expect("Base case is a symbol, this failure is unreachable.")
}

/// Interpret a BaseExpr as an Operator::Symbol or fail.
fn symbol(e: BaseExpr) -> Result<Symbol, ParseErr> {
    if let BaseExpr::Symbol(s) = &e {
        // Check that the string doesn't match to a reserved operator
        if let Operator::Symbol(_) = to_op(s) {
            Ok(s.to_string())
        } else {
            Err(ParseErr(format!("{:?} is a reserved operator and cannot be used as a symbol", s)))
        }
    } else {
        Err(ParseErr(format!("{:?} is not a symbol. Only symbols are allowed\
                              in a function parameter list.", e)))
    }
}

/// Convert a BaseExpr::List into a vector of symbols. Returns error otherwise.
fn fn_args(be: BaseExpr) -> Result<Vec<Symbol>, ParseErr> {
    let symbols = match be {
        BaseExpr::List(l) => Ok(l.into_iter().map(symbol).collect()),
        _ => Err(ParseErr(format!("Function arguments must be in a list, not {:?}.", be))),
    }?;

    fold_results(symbols)
}

/// Parses a function application.
fn app(op: Operator, args: Vec<BaseExpr>) -> ParseRes {
    let args = fold_results( args.into_iter().map(expr).collect() )?;
    Ok( Expr::App(op, args) )
}

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
    .map(|mut v| { v.reverse(); v })
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fmt::Debug;
    use primitive_types::U256;

    /// Map a parser over a list of tests and assert equality. Panics when equality fails.
    fn batch_test(
        f: impl Fn(BaseExpr) -> ParseRes,
        tests: Vec<(BaseExpr, ParseRes)>)
    {
        tests.into_iter()
             .for_each(|(i,o)|
                 assert_eq!(f(i), o))
    }

    #[test]
    fn parse_nested_applications() {
        let tests = vec![
            // (+ 1 2)
            (BaseExpr::List(vec![BaseExpr::Symbol("+".to_string()),
                                 BaseExpr::Int(U256::from(1)),
                                 BaseExpr::Int(U256::from(2))]),
             Expr::App(Operator::BuiltIn(BuiltIn::Add),
                           vec![Expr::Int(U256::from(1)),
                                Expr::Int(U256::from(2))])),
            // (+ 1 (- 5 8))
            (BaseExpr::List(vec![
                BaseExpr::Symbol("+".to_string()),
                BaseExpr::Int(U256::from(1)),
                BaseExpr::List(vec![
                    BaseExpr::Symbol("-".to_string()),
                    BaseExpr::Int(U256::from(5)),
                    BaseExpr::Int(U256::from(8))])]),
             Expr::App(Operator::BuiltIn(BuiltIn::Add),
                           vec![Expr::Int(U256::from(1)),
                                Expr::App(Operator::BuiltIn(BuiltIn::Sub),
                                          vec![Expr::Int(U256::from(5)),
                                               Expr::Int(U256::from(8))])]))
        ].into_iter()
         .map(|(i,e)| (i, Ok(e)))
         .collect();

        batch_test(expr, tests)
    }
}
*/
