use crate::parser::BaseExpr;
use crate::types::{Symbol, BuiltIn, Expr, Operator};
use nom::{
    IResult, Parser,
    branch::alt,
};

/// Generic S-expression function application. Every expression has this form.
/// Since Operator is not a standalone expression, we uniquely identify it as the start of an
/// S-expression.
type App = (Operator, Vec<BaseExpr>);

//type ParseRes = IResult<App, Expr, VerboseError<&'a str>>;
type ParseRes = Result<Expr, ParseErr>;
struct ParseErr(String);

fn expr(input: BaseExpr) -> ParseRes {
    match input {
        BaseExpr::Int(n) => Ok( Expr::Int(n) ),
        BaseExpr::Symbol(s) => Ok( Expr::Symbol(s)),
        BaseExpr::List(mut elems) => {
            let op = elems.pop()
                // Should never happen since parser uses 'separated_list1'
                .ok_or( ParseErr("Expected an operator in list expression.".to_string()) )?;

            /*
            let op = match op {
                BaseExpr::Symbol(s) => Ok( Operator::Symbol(s) ),
                BaseExpr::BuiltIn(op) => Ok( Operator::BuiltIn(op) ),
                BaseExpr::Special(op) => Ok(
                _ => Err(ParseErr(format!("First element of list, {:?}, should be an operator.", op))),
            }?;
            */
            match op {
                BaseExpr::Symbol(s) => Ok( Operator::Symbol(s) ),
                BaseExpr::BuiltIn(op) => Ok( Operator::BuiltIn(op) ),
                BaseExpr::Special(op) => match op {
                    SpecialOp::Defn => defn(elems),
                }
                _ => Err(ParseErr(format!("First element of list, {:?}, should be an operator.", op))),
            }?;

            //defn((op, elems))
            //    .or_else(app)
            /*
            alt(defn as fn((Operator, Vec<BaseExpr>)) -> std::result::Result<Expr, ParseErr>,
                app  as fn((Operator, Vec<BaseExpr>)) -> std::result::Result<Expr, ParseErr>)
                ((op, elems))
            */

            //alt((op, elems))
            //alt((defn, app))((op, elems))
        },
    }
}

/// Tries to evaluate f, and if it fails, tries to evaluate g. Discarding the error from f.
fn alt<F>(f: F, g: F) -> F
    where F: Fn(I) -> Result<O,E>
{
    move |i: I| {
        match f(i) {
            Ok(o) => o,
            Err(_) => g(i),
        }
    }
}

/// Parses a function definition if the input is well formed. Otherwise returns an error.
//fn defn((op, args): App) -> ParseRes {
fn defn(elems: Vec<BaseExpr>) -> ParseRes {
    // TODO: Check for symbol conflicts here? Set symbol into env
    if elems != 3 {
        return Err( ParseErr(format!("Incorrect number of elements in a function definition. Expected 3.")) );
    }

    let fn_name = elems.pop().unwrap();
    let params  = elems.pop().unwrap();
    let body    = elems.pop().unwrap();

    if args.len() == 2 {
        let fn_args = fn_args( args.pop().unwrap() )?;
        let body    = expr( args.pop().unwrap() )?;
        Ok( Expr::Defn(fn_name, fn_args, Box::new(body)) )
    } else {
        Err( ParseErr(format!("Expected 2 arguments to 'defn', {:?} provided.", args.len())) )
    }
}

/// Convert a BaseExpr::List into a vector of symbols. Returns error otherwise.
fn fn_args(be: BaseExpr) -> Result<Vec<Symbol>, ParseErr> {
    //let mut symbols = vec![];
    let symbols = match be {
        BaseExpr::List(l) =>
            Ok(l.iter().map(|v| match v {
                BaseExpr::Symbol(s) => Ok(s),
                _ => Err(ParseErr(format!("{:?} is not a symbol. Only symbols are allowed\
                                       in a function parameter list.", v)));
            }).collect()),
        _ => Err(ParseErr(format!("Function arguments must be in a list, not {:?}.", be))),
    }?;

    fold_results(symbols)
}

/// Parses a function application.
fn app((op, args): App) -> ParseRes {
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
}
