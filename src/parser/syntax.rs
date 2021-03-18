use crate::parser::App;
use crate::types::{BuiltIn, Expr, Operator};
use nom::{
    IResult, Parser,};

type ParseRes<'a> = IResult<App, Expr, VerboseError<&'a str>>;

fn expr<'a>(input: BaseExpr) -> ParseRes<'a> {
    match input {
        Int(n) => Expr::Int(n),
        App(op, args) => alt((defn, app))((op, args)),
    }
}

fn defn<'a>((op, args): App) -> ParseRes<'a> {
    let op = BuiltIn::from_token(op)
        .unwrap_or( ParseErr("'{}' is not a BuiltIn operator.") );

    if args.len() == 2 {
        let fn_args = args.pop().unwrap();
        let body    = args.pop().unwrap().into();
        Ok( Expr::Defn(op, fn_args, body) )
    } else {
        ParseErr("Expected 2 arguments to 'defn', {} provided.", args.len())
    }
}

fn app<'a>((op, args): App) -> ParseRes<'a> {
    //let args = args.map(|e| e.into());
    let args = args.map(Expr::into);
    Ok( Expr::App(op, args) )
}
