use crate::parser::{App, BaseExpr};
use primitive_types::U256;
use crate::types::{BuiltIn, Expr, Operator};
use nom::{
    IResult, Parser,
    InputTake, InputLength, InputTakeAtPosition,
    error::{context, ParseError},
    branch::alt,
    bytes::complete::tag,
    combinator::{success, map_res, map, map_opt, opt},
    error::VerboseError,
    character::complete::{not_line_ending, line_ending, alpha1, multispace1, multispace0, one_of, digit1},
    multi::{separated_list0, many0, fold_many1},
    character::complete::char,
    sequence::{preceded, delimited, separated_pair},
};

type ParseRes<'a, O> = IResult<&'a str, O, VerboseError<&'a str>>;

/*
impl From<&str> for Operator {
    fn from(s: &str) -> Operator {
        match BuiltIn::from_token(s) {
            Some(op) => Operator::BuiltIn(op),
            None     => Operator::Symbol(s.to_string()),
        }
    }
}
*/

fn builtin<'a>(input: &'a str)
-> IResult<&'a str, BuiltIn, VerboseError<&'a str>> {
    context("builtin",
        //map_opt(not_line_ending, BuiltIn::from_token)
        //    .map(Operator::BuiltIn),
        map_opt(alt((tag("+"), tag("-"), alpha1)),
                BuiltIn::from_token))
            .parse(input)
}

fn symbol<'a>(input: &'a str)
-> IResult<&'a str, String, VerboseError<&'a str>> {
    context("symbol", alpha1)(input)

fn int<'a>(input: &'a str)
-> IResult<&'a str, U256, VerboseError<&'a str>> {
    context("int",
        // TODO: Strange parsing behaviour when parsing directly n_str.parse::<U256>
        map_res(digit1, |n_str: &str| n_str.parse::<u64>())
            .map(U256::from))
            .parse(input)
}

/// Effectively tokenizes an input S-expression as a str, into a list of [Expr]s.
fn list<'a>(input: &'a str)
-> IResult<&'a str, Vec<BaseExpr>, VerboseError<&'a str>> {
    //let elements = separated_list1(multispace1, not(multispace);
    //let elements = |s| s.split_whitespace().collect();
    /*
    let args     = separated_list0(multispace1, base_expr);
    let elements = separated_pair(operator,
                                 multispace1,
                                 args);
    */
    let elements = separated_list1(base_expr, multispace1);

    ws(delimited(
        char('(').and(multispace0),
        elements,
        char(')').and(multispace0)))
        .parse(input)
}

fn base_expr<'a>(input: &'a str)
-> IResult<&'a str, BaseExpr, VerboseError<&'a str>> {
    alt((list.map(BaseExpr::List),
         int.map(BaseExpr::Int),
         builtin.map(BaseExpr::BuiltIn),
         symbol.map(BaseExpr::Symbol),
     ))(input)
}

/// Surrounding whitespace parser combinator
fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(inner: F)
-> impl Parser<&'a str, O, E>
  where
  F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
  delimited(
    multispace0,
    inner,
    multispace0
  )
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
                 assert_eq!(f.parse(i), o))
    }

    #[test]
    fn parse_int_as_atom() {
        let tests = vec![("10",   Ok(("", Atom::Int(U256::from(10))))),
                         ("742", Ok(("", Atom::Int(U256::from(742)))))];

        batch_test(int_atom, tests)
    }

    #[test]
    fn parse_app_expr() {
        let tests = vec![
            ("(+ 1 2)",
             Ok(("", Expr::App(BuiltIn::Add,
                               vec![Expr::Atom(Atom::Int(U256::from(1))),
                                    Expr::Atom(Atom::Int(U256::from(2)))])))),
            ("(+ 1 (- 5 8))",
             Ok(("", Expr::App(BuiltIn::Add,
                               vec![Expr::Atom(Atom::Int(U256::from(1))),
                                    Expr::App(BuiltIn::Sub,
                                              vec![Expr::Atom(Atom::Int(U256::from(5))),
                                                   Expr::Atom(Atom::Int(U256::from(8)))])]))))
        ];

        batch_test(app_expr, tests)
    }
}
