use crate::parser::{Defn};
use primitive_types::U256;
use crate::types::{BuiltIn, Expr};
use nom::{
    IResult, Parser,
    error::{context, ParseError},
    branch::alt,
    bytes::complete::tag,
    combinator::{map_res, map_opt},
    error::VerboseError,
    character::complete::{line_ending, alpha1, multispace1, multispace0, digit1},
    multi::{separated_list1, many0},
    character::complete::char,
    sequence::{separated_pair, tuple, preceded, delimited},
};

/// Create a parser for an s-expression, where each element of the list is a parser.
/// ```
/// // Parses "(f 1 (* 3 4))"
/// list!(symbol, expr, expr);
/// ```
macro_rules! list {
    ($($parser:expr),+) => (
        s_expr(tuple(($(ws($parser)),+)))
    )
}

type ParseRes<'a, O> = IResult<&'a str, O, VerboseError<&'a str>>;

fn defn<'a>(input: &'a str)
-> IResult<&'a str, Defn, VerboseError<&'a str>> {
    context("defn",
        s_expr(
            preceded(ws(tag("fn")),
                     tuple((ws(alpha1),
                            ws(s_expr(separated_list1(multispace1, symbol))),
                            expr)))))
        .map(|(name, params, body)| (name.to_string(), (params, body)))
        .parse(input)
}

/// Parse a [BuiltIn] expression with three arguments.
fn tri_builtin<'a>(input: &'a str)
-> IResult<&'a str, BuiltIn, VerboseError<&'a str>> {
    context("tri builtin",
        map_opt(
            list!(tag("slice"), expr, expr, expr),
            |(s,e1,e2,e3)| BuiltIn::from_tri_token(s,e1,e2,e3)))
    .parse(input)
}

/// Builtins without arguments aren't wrapped in s-expressions.
fn empty_builtin<'a>(input: &'a str)
-> IResult<&'a str, BuiltIn, VerboseError<&'a str>> {
    context("unary builtin",
        map_opt(ws(tag("nil")),
                BuiltIn::from_empty_token))
    .parse(input)
}

fn unary_builtin<'a>(input: &'a str)
-> IResult<&'a str, BuiltIn, VerboseError<&'a str>> {
    context("unary builtin",
        alt((
            map_opt(
                list!(alt((
                        tag("load"),
                        tag("len"),
                        tag("not"),
                    )),
                    expr),
                |(s,e)| BuiltIn::from_uni_token(s, e)),
                // <tag> <symb>
                // TODO: Probably take these out since theyre very low level and redundant w/ let/set
                alt((
                    list!(
                        tag("store"),
                        symbol)
                    .map(|(_,s)| BuiltIn::Store(s)),
                    list!(
                        tag("load"),
                        symbol)
                    .map(|(_,s)| BuiltIn::Load(s)),
                )))))
    .parse(input)
}

fn binary_builtin<'a>(input: &'a str)
-> IResult<&'a str, BuiltIn, VerboseError<&'a str>> {
    context("binary builtin",
        // <tag> <expr> <expr>
        map_opt(list!(
            alt((
                tag("+"), tag("-"),
                tag("*"), tag("/"),
                tag("%"), tag("and"),
                tag("or"), tag("xor"),
                tag("cons"), tag("get"),
                tag("concat"),
            )),
            expr,
            expr),
            |(s,e1,e2)| BuiltIn::from_bin_token(s, e1, e2)))
    .parse(input)
}

/// Parse a symbol, which is any alphanumeric string.
fn symbol<'a>(input: &'a str)
-> IResult<&'a str, String, VerboseError<&'a str>> {
    context("symbol", alpha1.map(String::from))(input)
}


/// Parse a [U256] integer.
fn int<'a>(input: &'a str)
-> IResult<&'a str, U256, VerboseError<&'a str>> {
    context("int",
        // TODO: Strange parsing behaviour when parsing directly n_str.parse::<U256>
        map_res(digit1, |n_str: &str| n_str.parse::<u64>())
            .map(U256::from))
            .parse(input)
}

/// Wrap a parser in surrounding parenthesis with whitespace.
fn s_expr<'a, O, F>(parser: F)
-> impl FnMut(&'a str) -> IResult<&'a str, O, VerboseError<&'a str>>
where F: Parser<&'a str, O, VerboseError<&'a str>> {
    context("list",
    delimited(
        ws(char('(')),
        parser,
        ws(char(')')).and(many0(line_ending))))
}

/// Top level of a program consists of a list of fn definitions and an expression.
pub fn root<'a>(input: &'a str)
-> IResult<&'a str, (Vec<Defn>, Expr), VerboseError<&'a str>> {
    tuple((many0(defn),
           expr)) // TODO: This should be many expressions since not all return a value (set)
    .parse(input)
}

pub fn set<'a>(input: &'a str)
-> IResult<&'a str, Expr, VerboseError<&'a str>> {
    // <tag> <symb> <expr>
    list!(tag("set"), symbol, expr)
        .map(|(_,s,e)| Expr::Set(s,Box::new(e)))
        .parse(input)
}

/// Top level parser returns any valid [Expr].
pub fn expr<'a>(input: &'a str)
-> IResult<&'a str, Expr, VerboseError<&'a str>> {
    alt((int.map(Expr::Int),
         binary_builtin.map(|b| Expr::BuiltIn(Box::new(b))),
         unary_builtin.map(|b| Expr::BuiltIn(Box::new(b))),
         tri_builtin.map(|b| Expr::BuiltIn(Box::new(b))),
         empty_builtin.map(|b| Expr::BuiltIn(Box::new(b))),
         symbol.map(Expr::Var),
         set,
         //let.map(Expr::Let),
         app,
     )).parse(input)
}

/// Parse a function call (application) to any non-[BuiltIn] function.
pub fn app<'a>(input: &'a str)
-> IResult<&'a str, Expr, VerboseError<&'a str>> {
    let args = separated_list1(multispace1, expr);
    context("Application",
        list!(symbol, args))
            .map(|(s,a)| Expr::App(s,a))
        .parse(input)
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
