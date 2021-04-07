use crate::PErr;
use crate::parser::{Defn, ParseErr};
use primitive_types::U256;
use crate::types::{Reserved, Symbol, Value, BuiltIn, Expr};
//#[macro_use] use nom_trace::{tr,print_trace, activate_trace};
use nom::{IResult, Parser, branch::alt, bytes::complete::tag,
character::complete::{hex_digit1, line_ending, alpha1, multispace1, multispace0, digit1},
character::complete::char,
combinator::{map_res, map_opt},
error::{context, ParseError},
error::VerboseError,
multi::{separated_list1, many0, many1},
sequence::{separated_pair, tuple, preceded, delimited}};

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

/// The result of a parser on strs with a VerboseError error type.
type ParseRes<'a, O> = IResult<&'a str, O, VerboseError<&'a str>>;

fn sym_binds<'a>(input: &'a str)
-> ParseRes<Vec<(Symbol, Expr)>> {
    s_expr(separated_list1(
        multispace1,
        separated_pair(symbol, multispace1, expr)
        ))(input)
}

fn let_bind<'a>(input: &'a str)
-> ParseRes<(Vec<(Symbol, Expr)>, Vec<Expr>)> {
    context("let binding",
        list!(
            tag("let"),
            sym_binds,
            many1(expr)
            ))
            .map(|(_,a,b)| (a,b))
        .parse(input)
}

fn defn<'a>(input: &'a str)
-> ParseRes<Defn> {
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
-> ParseRes<BuiltIn> {
    context("tri builtin",
        map_opt(
            list!(tag("slice"), expr, expr, expr),
            |(s,e1,e2,e3)| BuiltIn::from_tri_token(s,e1,e2,e3)))
    .parse(input)
}

/// Builtins without arguments aren't wrapped in s-expressions.
fn empty_builtin<'a>(input: &'a str)
-> ParseRes<BuiltIn> {
    context("empty builtin",
        map_opt(ws(tag("nil")),
                BuiltIn::from_empty_token))
    .parse(input)
}

fn unary_builtin<'a>(input: &'a str)
-> ParseRes<BuiltIn> {
    context("unary builtin",
        alt((
            map_opt(
                list!(alt((
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
-> ParseRes<BuiltIn> {
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
-> ParseRes<String> {
    context("symbol", alpha1.map(String::from))(input)
}

// This function assumes an ascii encoding
fn from_hex(s: &str) -> Result<Vec<u8>, ParseErr> {
    if s.len() % 2 != 0 {
        return PErr!("Hex string {} is not an even number of characters.", s);
    }

    let mut bytes = vec![];
    for i in 0..s.len()/2 {
        let idx = i*2;
        let b = u8::from_str_radix(&s[idx..idx+2], 16)
            .map_err(|_| ParseErr("Not a valid hex character.".to_string()))?;
        bytes.push(b);
    }
    Ok(bytes)
    //fold_results( s.chars().map(|c| u8::from_str_radix(c as &str, 16)).collect() )
}

fn bytes<'a>(input: &'a str)
-> ParseRes<Vec<u8>> {
    let res = context("bytes",
        map_res(preceded(tag("0x"), hex_digit1),
                from_hex))
        .parse(input);
    res
}

/// Parse a [U256] integer.
fn int<'a>(input: &'a str)
-> ParseRes<U256> {
    context("int",
        // TODO: Strange parsing behaviour when parsing directly n_str.parse::<U256>
        map_res(digit1, |n_str: &str| n_str.parse::<u64>())
            .map(U256::from))
            .parse(input)
}

/// Wrap a parser in surrounding parenthesis with whitespace.
fn s_expr<'a, O, F>(parser: F)
-> impl FnMut(&'a str) -> IResult<&'a str, O, VerboseError<&'a str>>
//-> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where F: Parser<&'a str, O, VerboseError<&'a str>>,
//where F: Parser<&'a str, O, E>,
//      E: nom::error::ParseError<&'a str> + nom::error::ContextError<&'a str>
{
    context("S expression",
    delimited(
        ws(char('(')),
        parser,
        ws(char(')')).and(many0(line_ending))))
}

/// Top level of a program consists of a list of fn definitions and an expression.
pub fn root<'a>(input: &'a str)
-> ParseRes<(Vec<Defn>, Expr)> {
    tuple((many0(defn),
           expr))
    .parse(input)
}

pub fn set<'a>(input: &'a str)
-> ParseRes<Expr> {
    // <tag> <symb> <expr>
    list!(tag("set!"), symbol, expr)
        .map(|(_,s,e)| Expr::Set(s,Box::new(e)))
        .parse(input)
}

/// Parse a function call (application) to any non-[BuiltIn] function.
pub fn app<'a>(input: &'a str)
-> ParseRes<Expr> {
    let args = separated_list1(multispace1, expr);
    context("Application",
        list!(symbol, args))
            .map(|(s,a)| Expr::App(s,a))
        .parse(input)
}

pub fn if_expr<'a>(input: &'a str)
-> ParseRes<(Expr, Expr, Expr)> {
    context("if expression",
        list!(tag("if"), expr, expr, expr))
        .map(|(_,e1,e2,e3)| (e1,e2,e3))
        .parse(input)
}

pub fn sigeok<'a>(input: &'a str)
-> ParseRes<(u16, Expr, Expr, Expr)> {
    context("sigeok operation",
        list!(tag("sigeok"),
              map_res(digit1, |n_str: &str| n_str.parse::<u16>()),
              expr, expr, expr)
            .map(|(_, n, e1, e2, e3)| (n,e1,e2,e3)))
            .parse(input)
}

pub fn hash<'a>(input: &'a str)
-> ParseRes<(u16, Expr)> {
    context("hash operation",
        list!(tag("hash"),
              map_res(digit1, |n_str: &str| n_str.parse::<u16>()),
              expr)
            .map(|(_, n, e)| (n,e)))
            .parse(input)
}

pub fn loop_expr<'a>(input: &'a str)
-> ParseRes<(u16, Expr)> {
    context("loop expression",
        list!(tag("loop"),
              map_res(digit1, |n_str: &str| n_str.parse::<u16>()),
              expr)
            .map(|(_, n, e)| (n,e)))
            .parse(input)
}

pub fn reserved<'a>(input: &'a str)
-> ParseRes<Reserved> {
    context("reserved identity",
        alt((tag("SpenderTx").map(|_| Reserved::SpenderTx),
             tag("SpenderTxHash").map(|_| Reserved::SpenderTxHash)),
           ))(input)
}

/// Top level parser returns any valid [Expr].
pub fn expr<'a>(input: &'a str)
-> ParseRes<Expr> {
    alt((bytes.map(Value::Bytes).map(Expr::Value),
         int.map(Value::Int).map(Expr::Value),
         binary_builtin.map(|b| Expr::BuiltIn(Box::new(b))),
         unary_builtin.map(|b| Expr::BuiltIn(Box::new(b))),
         tri_builtin.map(|b| Expr::BuiltIn(Box::new(b))),
         empty_builtin.map(|b| Expr::BuiltIn(Box::new(b))),
         symbol.map(Expr::Var),
         set,
         let_bind.map(|(binds, exprs)| Expr::Let(binds, exprs)),
         if_expr.map(|(p,t,f)| Expr::If(Box::new(p), Box::new(t), Box::new(f))),
         loop_expr.map(|(n,e)| Expr::Loop(n, Box::new(e))),
         hash.map(|(n,e)| Expr::Hash(n, Box::new(e))),
         sigeok.map(|(n,e1,e2,e3)| Expr::Sigeok(n, Box::new(e1), Box::new(e2), Box::new(e3))),
         reserved.map(|r| Expr::Reserved(r)),
         app,
     )).parse(input)
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
