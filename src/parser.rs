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
    character::complete::{line_ending, alpha1, multispace1, multispace0, one_of, digit1},
    multi::{separated_list0, many0},
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

fn builtin<'a>(input: &'a str)
-> IResult<&'a str, BuiltIn, VerboseError<&'a str>> {
    context("builtin",
        map_opt(alt((tag("+"), tag("-"),
                    alpha1)),
                |s| BuiltIn::from_token(s)))
            .parse(input)
}

fn int<'a>(input: &'a str)
-> IResult<&'a str, Expr, VerboseError<&'a str>> {
    context("int",
        // TODO: Strange parsing behaviour when parsing directly n_str.parse::<U256>
        map_res(digit1, |n_str: &str| n_str.parse::<u64>())
            .map(U256::from)
            .map(Expr::Int))
            .parse(input)
    /*
    alt((map_res(digit1, |n_str: &str| n_str.parse::<U256>()
              .map(Atom::Int)),
        map_res(preceded(char('-'), digit1),
                |n_str: &str| n_str.parse::<U256>())
            .map(|n| -1*n)
            .map(Atom::Int)))
        (input)
        */
}

/*
fn atom_expr<'a>(input: &'a str)
-> IResult<&'a str, Expr, VerboseError<&'a str>> {
    context("Atom",
        int_atom.map(Expr::Atom))
        .parse(input)
}
*/

/*
fn app_expr<'a>(input: &'a str)
-> IResult<&'a str, Expr, VerboseError<&'a str>> {
    //let identifier = alpha1;
    let args = separated_list1(multispace1, expr);
    let content = alt((
            separated_pair(builtin,
                           multispace1,
                           args),
            builtin
                .map(|op| (op, vec![]))));

    context("App expr",
        ws(delimited(char('('),
                  content,
                  char(')').and(many0(line_ending))))
        .map(|(op, args)| Expr::App(op, args)))
        .parse(input)
}
*/

/*
fn defn<'a>(input: &'a str)
-> IResult<&'a str, Expr, VerboseError<&'a str>> {
}
*/

/// Parse an s-expression, returning a vector of strings. Used as an intermitent step in
/// parsing to an [Expr].
//fn list<'a, O>(parser: impl Parser<&'a str, O, VerboseError<&'a str>>)
//fn list<I, O, E, F>(parser: F)
//-> IResult<&'a str, Vec<&'a str>, VerboseError<&'a str>>
//-> IResult<&'a str, O, VerboseError<&'a str>>
//-> impl Parser<&'a str, O, VerboseError<&'a str>>
//-> impl FnMut(I) -> IResult<I, O, E>
//    where I: Clone + InputTake + InputLength + InputTakeAtPosition,
//          F: Parser<I, O, E>,
//          E: ParseError<I>
    //where P: Fn(Vec<&'a str>) -> IResult<&'a str, O, VerboseError<&'a str>>
/*
fn list<'a>(input: &'a str)
-> IResult<&'a str, Operator, VerboseError<&'a str>> {
    //let elements = separated_list1(multispace1, not(multispace);
    //let elements = |s| s.split_whitespace().collect();

    //move |i: &'a str| {
        context("s-expression",
            ws(delimited(
                char('(').and(multispace0),
                inner,
                char(')').and(multispace0))))
                //char(')').and(many0(line_ending)))))
            .parse(i)
    //}
}
*/

/// Parse out an [Operator].
fn operator<'a>(input: &'a str)
-> IResult<&'a str, Operator, VerboseError<&'a str>> {
    alt((
        // BuiltIn
        map_opt(success, BuiltIn::from_token)
            .map(Operator::BuiltIn),
        // Symbol
        alpha1
            .map(String::from)
            .map(Operator::Symbol),
    ))(input)
}

/*
/// Try to extract values from results in vector. Short circuit on the first failure. Note this
/// does not return an iterator (because it folds).
fn vec_map(v: Vec<Result<Expr, E>>) -> Result<Vec<Expr>, E> {
    v.iter()
     .try_fold(vec![] |inner_vec, r| v.map( inner_vec ++ v))
         //let v = r?;
         //inner_vec ++ v
}
*/

/// Takes whatever the first expression of the list is and treats it as the operator to the
/// arguments in the list following.
/*
fn app(sexpr: Vec<&str>)
-> IResult<&'a str, Expr, VerboseError<&'a str>> {
    // TODO: Panics if sexpr is empty
    let op = operator( sexpr.split_off(1)[0] )?;
    let args = vec_map(sexpr)?;
    /*
    let args = sexpr.iter()
        .map(expr)
        .try_fold(args.get(0), |acc, res| acc.and(res));
    */
    Ok(Expr::App(op, args))
}
*/

pub fn app<'a>(input: &'a str)
-> IResult<&'a str, Expr, VerboseError<&'a str>> {
    let args = separated_list0(multispace1, expr);
    separated_pair(operator, multispace1, args)
        .map(|(op, args)| Expr::App(op, args))
        .parse(input)
}

pub fn expr<'a>(input: &'a str)
-> IResult<&'a str, Expr, VerboseError<&'a str>> {
    context("s-expression",
        ws(delimited(
            char('(').and(multispace0),
            inner,
            char(')').and(multispace0))))
            //char(')').and(many0(line_ending)))))
        .parse(i)
    alt((
        // Int
        int,
        // App
        list(app),
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
