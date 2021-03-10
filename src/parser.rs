use nom::{
    IResult, Parser,
    error::ParseError,
    branch::alt,
    bytes::complete::is_a,
    combinator::{map_res, map},
    error::{FromExternalError, VerboseError},
    character::complete::{multispace1, one_of, digit1},
    multi::{separated_list1},
    character::complete::char,
    sequence::{preceded, delimited, separated_pair},
};

#[derive(Debug, PartialEq, Eq)]
pub enum BuiltIn {
    Add,
    Sub,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Atom {
    Int(i64), // TODO: Make a i256
    BuiltIn(BuiltIn),
}

pub enum Expr {
    Atom(Atom),
    App(BuiltIn, Vec<Expr>),
}

fn builtin<'a, E: ParseError<&'a str>>(input: &'a str)
-> IResult<&'a str, BuiltIn, E> {
    one_of("+-")(input)
        .map(|(i, o)| {
            let op = match o {
                '+' => BuiltIn::Add,
                '-' => BuiltIn::Sub,
                _ => unreachable!(), // Op codes match list provided to one_of
            };
            (i, op)
        })
}

fn builtin_atom<'a, E: ParseError<&'a str>>(input: &'a str)
-> IResult<&'a str, Atom, E> {
    builtin.map(Atom::BuiltIn).parse(input)
}

/*
fn bind(p: P, f: F) -> impl Parser<I,O,E> {
}
*/

fn int_atom<'a>(input: &'a str)
-> IResult<&'a str, Atom, VerboseError<&'a str>> {
    alt((map_res(digit1, |n_str: &str| n_str.parse::<i64>()
              .map(Atom::Int)),
        map_res(preceded(char('-'), digit1),
                |n_str: &str| n_str.parse::<i64>())
            .map(|n| -1*n)
            .map(Atom::Int)))
        (input)
}

fn atom_expr<'a>(input: &'a str)
-> IResult<&'a str, Expr, VerboseError<&'a str>> {
    alt((int_atom, builtin_atom))
        .map(Expr::Atom).parse(input)
}

/*
fn ws<'a>(input: &'a str)
-> IResult<&'a str, &'a str, VerboseError<&'a str>> {
}
*/

fn app_expr<'a>(input: &'a str)
-> IResult<&'a str, Expr, VerboseError<&'a str>> {
    //let identifier = alpha1;
    let args = separated_list1(multispace1, expr);
    let content = separated_pair(builtin,
                                 multispace1,
                                 args);

    delimited(char('('),
              content,
              char(')'))
        .map(|(op, args)| Expr::App(op, args))
        .parse(input)
}

fn expr<'a>(input: &'a str)
-> IResult<&'a str, Expr, VerboseError<&'a str>> {
    alt((app_expr, atom_expr))(input)
}

/*
#[derive(Debug)]
pub struct Sexp {
    op: char,//Operation,
    args: Vec<Exp>,
}

// TODO: To 256
type U256 = u64;

/// Accepted expression types
#[derive(Debug)]
pub enum Exp {
    Int(U256),
    List(Sexp),
}

//pub fn parse_sexp(i: &str) -> IResult<&str, Sexp> {
pub fn sexp<E>() -> impl Parser<'& str, Exp, E> {
    let delimiter = char(' ');
    let identifier = alpha1;
    let args = separated_list1(delimiter, exp());
    let content = separated_pair(identifier,
                                 delimiter,
                                 args);

    delimited(char('('),
              content,
              char(')'))
        .map(|id, args| Exp::List( Sexp {
            op: id,
            args: args.into(),
         }))
}

pub fn exp() -> impl Parser<'& str, Exp, E> {
    alt((sexp, int))
}
*/

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_int_as_atom() {
        let tests = vec![("10",   Ok(("", Atom::Int(10)))),
                         ("-742", Ok(("", Atom::Int(-742))))];

        tests.into_iter()
             .for_each(|(i,o)|
                 assert_eq!(int_atom(&i), o))
    }
}
