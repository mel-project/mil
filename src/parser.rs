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

type ParseRes<'a, O> = IResult<&'a str, O, VerboseError<&'a str>>;

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

#[derive(Debug, PartialEq, Eq)]
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

#[cfg(test)]
mod test {
    use super::*;
    use std::fmt::Debug;

    /// Map a parser over a list of tests and assert equality. Panics when equality fails.
    fn batch_test<'a, O: Debug + Eq>(
        mut f: impl Parser<&'a str, O, VerboseError<&'a str>>,
        tests: Vec<(&'a str, ParseRes<'a, O>)>)
    {
        tests.into_iter()
             .for_each(|(i,o)|
                 assert_eq!(f.parse(&i), o))
    }

    #[test]
    fn parse_int_as_atom() {
        let tests = vec![("10",   Ok(("", Atom::Int(10)))),
                         ("-742", Ok(("", Atom::Int(-742))))];

        batch_test(int_atom, tests)
    }

    #[test]
    fn parse_app_expr() {
        let tests = vec![
            ("(+ 1 2)",
             Ok(("", Expr::App(BuiltIn::Add,
                               vec![Expr::Atom(Atom::Int(1)),
                                    Expr::Atom(Atom::Int(2))]))))];

        batch_test(app_expr, tests)
    }
}
