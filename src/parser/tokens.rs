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
    sequence::{tuple, preceded, delimited},
};

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

fn tri_builtin<'a>(input: &'a str)
-> IResult<&'a str, BuiltIn, VerboseError<&'a str>> {
    context("tri builtin",
        map_opt(tuple((
              ws(tag("slice")),
              ws(expr),
              ws(expr),
              expr
        )), |(s,e1,e2,e3)| BuiltIn::from_tri_token(s,e1,e2,e3)))
    .parse(input)
}

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
        map_opt(tuple((
            ws(alt((
                tag("load"),
                tag("len"),
                tag("not"),
            ))),
            expr
        )), |(s,e)| BuiltIn::from_uni_token(s, e)))
    .parse(input)
}

fn binary_builtin<'a>(input: &'a str)
-> IResult<&'a str, BuiltIn, VerboseError<&'a str>> {
    context("binary builtin",
        //map_opt(not_line_ending, BuiltIn::from_token)
        //    .map(Operator::BuiltIn),
        alt((
            // <tag> <expr> <expr>
            map_opt(
                tuple((
                    ws(alt((
                        tag("+"), tag("-"),
                        tag("*"), tag("/"),
                        tag("%"), tag("and"),
                        tag("or"), tag("xor"),
                        tag("cons"), tag("get"),
                        tag("concat"),
                    ))),
                    ws(expr),
                    expr
                )),
                |(s,e1,e2)| BuiltIn::from_bin_token(s, e1, e2)
            ),
            // <tag> <symb> <expr>
            tuple((
                ws(tag("store")),
                ws(symbol),
                expr
            )).map(|(_,s,e)| BuiltIn::Store(s,e)),
        )))
        //)), |(s,e1,e2)| BuiltIn::from_bin_token(s, e1, e2)))
    .parse(input)
}

/*
fn special<'a>(input: &'a str)
-> IResult<&'a str, SpecialOp, VerboseError<&'a str>> {
    context("special operator",
        //map_opt(alt((tag("defn"), tag("let"))),
        map_opt((tag("let")), SpecialOp::from_token))
            .parse(input)
}
*/

/*
fn builtin<'a>(input: &'a str)
-> IResult<&'a str, BuiltIn, VerboseError<&'a str>> {
    context("builtin",
        //map_opt(not_line_ending, BuiltIn::from_token)
        //    .map(Operator::BuiltIn),
        //map_opt(alt((tag("+"), tag("-"), alpha1)),
        //        BuiltIn::from_token))
        alt((empty_builtin,
             unary_builtin,
             binary_builtin,
             tri_builtin,
         ))).parse(input)
}
*/

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

/*
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
*/

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
-> IResult<&'a str, (Vec<Defn>, Expr), VerboseError<&'a str>> {
    tuple((many0(defn),
           expr)) // TODO: This should be many expressions since not all return a value (set)
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
         //set.map(Expr::Set),
         //let.map(Expr::Let),
         app,
     )).parse(input)
}

pub fn app<'a>(input: &'a str)
-> IResult<&'a str, Expr, VerboseError<&'a str>> {
    let args = separated_list1(multispace1, expr);
    context("Application",
        tuple((symbol, args)))
        .map(|(s,a)| Expr::App(s,a))
        .parse(input)
}

/*
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
*/

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
