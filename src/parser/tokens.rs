use crate::parser::{Defn, BaseExpr};
use primitive_types::U256;
use crate::types::{SpecialOp, BuiltIn, Expr, Operator};
use nom::{
    IResult, Parser,
    InputTake, InputLength, InputTakeAtPosition,
    error::{context, ParseError},
    branch::alt,
    bytes::complete::tag,
    combinator::{success, map_res, map, map_opt, opt},
    error::VerboseError,
    character::complete::{not_line_ending, line_ending, alpha1, multispace1, multispace0, one_of, digit1},
    multi::{separated_list1, many0, fold_many1},
    character::complete::char,
    sequence::{tuple, preceded, delimited, separated_pair},
};

type ParseRes<'a, O> = IResult<&'a str, O, VerboseError<&'a str>>;

fn defn<'a>(input: &'a str)
-> IResult<&'a str, Defn, VerboseError<&'a str>> {
    context("defn",
        s_expr(
            preceded(ws(tag("fn")),
                     tuple((ws(alpha1),
                            ws(s_expr(many0(symbol))),
                            base_expr)))))
        //.map(|l| (l.pop().unwrap(), (l.pop().unwrap(), l.pop().unwrap())))
        .map(|(name, params, body)| (name.to_string(), (params, body)))
        .parse(input)
}

fn special<'a>(input: &'a str)
-> IResult<&'a str, SpecialOp, VerboseError<&'a str>> {
    context("special operator",
        //map_opt(alt((tag("defn"), tag("let"))),
        map_opt((tag("let")), SpecialOp::from_token))
            .parse(input)
}

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
    context("symbol", alpha1.map(String::from))(input)
}


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
    let elements = separated_list1(multispace1, base_expr);

    context("list",
    delimited(
        ws(char('(')),
        elements,
        char(')').and(many0(line_ending))))
    (input)
}

fn s_expr<'a, O, F>(parser: F)
-> impl FnMut(&'a str) -> IResult<&'a str, O, VerboseError<&'a str>>
where F: Parser<&'a str, O, VerboseError<&'a str>> {
    context("list",
    delimited(
        ws(char('(')),
        parser,
        char(')').and(many0(line_ending))))
}

/// Top level of a program consists of a list of fn definitions and an expression.
pub fn root<'a>(input: &'a str)
-> IResult<&'a str, (Vec<Defn>, BaseExpr), VerboseError<&'a str>> {
    tuple((many0(defn),
           base_expr))
    .parse(input)
}

/// Top level parser returns any valid [BaseExpr].
pub fn base_expr<'a>(input: &'a str)
-> IResult<&'a str, BaseExpr, VerboseError<&'a str>> {
    alt((int.map(BaseExpr::Int),
         builtin.map(BaseExpr::BuiltIn),
         special.map(BaseExpr::Special),
         symbol.map(BaseExpr::Symbol),
         list.map(BaseExpr::List),
     ))(input)
}

/// Surrounding whitespace parser combinator
fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(inner: F)
-> impl Parser<&'a str, O, E>
  where
  F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
  delimited(
    multispace0.and(many0(line_ending)),
    inner,
    multispace0.and(many0(line_ending)),
  )
}
