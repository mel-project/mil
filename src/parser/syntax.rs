use crate::PErr;
use crate::parser::{Defn, ParseErr};
use primitive_types::U256;
use crate::types::{Reserved, Symbol, Value, BuiltIn, Expr};
//#[macro_use] use nom_trace::{tr,print_trace, activate_trace};
use nom::{IResult, Parser, branch::alt,
bytes::complete::{take_while, is_not, tag},
character::complete::{hex_digit1, line_ending, alpha1, multispace1, multispace0, digit1},
character::complete::{alphanumeric0, char},
combinator::{opt, cut, map_res, map_opt},
error::{context, ParseError},
error::VerboseError, multi::{separated_list1, separated_list0, many0, many1},
sequence::{separated_pair, tuple, preceded, delimited}};

/// Create a parser for an s-expression, where each element of the list is a parser.
/// ```
/// // Parses "(f 1 (* 3 4))"
/// list!(symbol, expr, expr);
/// ```
macro_rules! list {
    (@as_expr $($tuple:expr),+) => {
        s_expr(tuple($($tuple),+))
    };
    (@accum ($($tuple:expr),*) $parser:expr) => {
        list!(@as_expr ($($tuple),*, opt(preceded(multispace0, comment))
                .flat_map(|_| preceded(multispace1, $parser))))
    };
    (@accum ($($tuple:expr),*) $parser:expr, $($tail:tt)*) => {
        list!(@accum ($($tuple),*,
                      opt(preceded(multispace0, comment))
                          .flat_map(|_| preceded(multispace1, $parser)))
                  $($tail)*)
    };
    ($parser:expr, $($tail:tt)*) => {
        list!(@accum (opt(comment)
                          .flat_map(|_| preceded(multispace0, $parser))) $($tail)*)
    };
}

/// The result of a parser on strs with a VerboseError error type.
type ParseRes<'a, O> = IResult<&'a str, O, VerboseError<&'a str>>;

/// Parse a list of symbol->expr binding pairs.
fn sym_binds<'a>(input: &'a str)
-> ParseRes<Vec<(Symbol, Expr)>> {
    context("symbol bindings",
    s_expr(separated_list0(
        multispace1,
        separated_pair(symbol, multispace1, expr)
        )))(input)
}

fn let_bind<'a>(input: &'a str)
-> ParseRes<(Vec<(Symbol, Expr)>, Vec<Expr>)> {
    context("let binding",
        list!(
            tag("let"),
            cut(sym_binds),
            cut(separated_list1(multispace1, expr))
            ))
            .map(|(_,a,b)| (a,b))
        .parse(input)
}

fn defn<'a>(input: &'a str)
-> ParseRes<Defn> {
    context("function definition",
        list!(
            tag("fn"),
            // Fn name
            cut(alpha1),
            // Parameters
            cut(s_expr(separated_list1(multispace1, symbol))),
            // Body
            cut(expr)))
        .map(|(_, name, params, body)| (name.to_string(), (params, body)))
        .parse(input)
}

/// Parse a [BuiltIn] expression with three arguments.
fn tri_builtin<'a>(input: &'a str)
-> ParseRes<BuiltIn> {
    context("tri builtin",
        map_opt(
            list!(alt((tag("vfrom"), tag("slice"))), cut(expr), cut(expr), cut(expr)),
            |(s,e1,e2,e3)| BuiltIn::from_tri_token(s,e1,e2,e3)))
    .parse(input)
}

/// Builtins without arguments aren't wrapped in s-expressions.
fn empty_builtin<'a>(input: &'a str)
-> ParseRes<BuiltIn> {
    context("empty builtin",
        map_opt(alt((tag("nil"), tag("bnil"))),
                BuiltIn::from_empty_token))
    .parse(input)
}

fn unary_builtin<'a>(input: &'a str)
-> ParseRes<BuiltIn> {
    context("unary builtin",
        //alt((
            map_opt(
                list!(alt((
                        tag("len"),
                        tag("not"),
                        tag("bytes->u256"),
                        tag("u256->bytes"),
                    )),
                    cut(expr)),
                |(s,e)| BuiltIn::from_uni_token(s, e)),
            // <tag> <symb>
            // TODO: Probably take these out since theyre very low level and redundant w/ let/set
            /*
            alt((
                list!(
                    tag("store"),
                    symbol)
                .map(|(_,s)| BuiltIn::Store(s)),
                list!(
                    tag("load"),
                    symbol)
                .map(|(_,s)| BuiltIn::Load(s)),
            */
            //)))))
            )
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
                tag("<"), tag(">"),
                tag("%"), tag("and"),
                tag("or"), tag("xor"),
                tag("cons"), tag("get"),
                tag("concat"), tag("="),
            )),
            cut(expr),
            cut(expr)),
            |(s,e1,e2)| BuiltIn::from_bin_token(s, e1, e2)))
    .parse(input)
}

/// Parse a symbol, which is an alphanumeric string with underscores allowed.
fn symbol<'a>(input: &'a str)
-> ParseRes<String> {
    let concat = |(a,b): (&str,&str)| -> Result<String, ParseErr> {
        let mut s = String::from(a);
        s.push_str(b);
        Ok(s)
    };

    context("symbol", map_res(
        tuple((alpha1, take_while(|c|
            match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '?' => true,
                _ => false,
            }))), concat))
        .parse(input)
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
    context("bytes",
        alt((
            // TODO: Support whitespace in strings
            delimited(tag("\""), alphanumeric0.map(|s: &str| s.as_bytes().into()), tag("\"")),
            map_res(preceded(tag("0x"), cut(hex_digit1)),
                from_hex),
           )))
        .parse(input)
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

/// Parse a native vector type.
// TODO: Integrate this into list! macro so that vectors can have comments etc..
fn vector<'a>(input: &'a str)
-> ParseRes<Vec<Expr>> {
    context("vector",
        delimited(
            char('[').and(multispace0),
            cut(separated_list0(multispace1, expr)),
            multispace0.and(char(']'))))
        (input)
}

/// Wrap a parser in surrounding parenthesis with optional internal whitespace.
fn s_expr<'a, O, F>(parser: F)
-> impl FnMut(&'a str) -> IResult<&'a str, O, VerboseError<&'a str>>
//-> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where F: Parser<&'a str, O, VerboseError<&'a str>>,
//where F: Parser<&'a str, O, E>,
//      E: nom::error::ParseError<&'a str> + nom::error::ContextError<&'a str>
{
    context("S expression",
    delimited(
        char('(').and(multispace0),
        parser,
        multispace0.and(char(')'))))
}

/// Parse a comment.
pub fn comment<'a>(input: &'a str)
-> ParseRes<&'a str> {
    context("Comment",
        preceded(tag(";"), cut(is_not("\r\n"))))
        (input)
}

/// Top level of a program consists of a list of fn definitions and an expression.
pub fn root<'a>(input: &'a str)
-> ParseRes<(Vec<Defn>, Expr)> {
    tuple((separated_list0(multispace1, defn),
           preceded(multispace0, expr)))
    .parse(input)
}

pub fn set<'a>(input: &'a str)
-> ParseRes<Expr> {
    // <tag> <symb> <expr>
    list!(tag("set!"), cut(symbol), cut(expr))
        .map(|(_,s,e)| Expr::Set(s,Box::new(e)))
        .parse(input)
}

/// Parse a function call (application) to any non-[BuiltIn] function.
pub fn app<'a>(input: &'a str)
-> ParseRes<Expr> {
    context("Application",
        list!(symbol, separated_list1(multispace1,
              opt(preceded(multispace0, comment))
                  .flat_map(|_| expr))))
            .map(|(s,a)| Expr::App(s,a))
        .parse(input)
}

pub fn if_expr<'a>(input: &'a str)
-> ParseRes<(Expr, Expr, Expr)> {
    context("if expression",
        list!(tag("if"), cut(expr), cut(expr), cut(expr)))
        .map(|(_,e1,e2,e3)| (e1,e2,e3))
        .parse(input)
}

pub fn sigeok<'a>(input: &'a str)
-> ParseRes<(u16, Expr, Expr, Expr)> {
    context("sigeok operation",
        list!(tag("sigeok"),
              cut(map_res(digit1, |n_str: &str| n_str.parse::<u16>())),
              expr, expr, expr)
            .map(|(_, n, e1, e2, e3)| (n,e1,e2,e3)))
            .parse(input)
}

pub fn hash<'a>(input: &'a str)
-> ParseRes<(u16, Expr)> {
    context("hash operation",
        list!(tag("hash"),
              cut(map_res(digit1, |n_str: &str| n_str.parse::<u16>())),
              expr)
            .map(|(_, n, e)| (n,e)))
            .parse(input)
}

pub fn loop_expr<'a>(input: &'a str)
-> ParseRes<(u16, Expr)> {
    context("loop expression",
        list!(tag("loop"),
              cut(map_res(digit1, |n_str: &str| n_str.parse::<u16>())),
              expr)
            .map(|(_, n, e)| (n,e)))
            .parse(input)
}

pub fn reserved<'a>(input: &'a str)
-> ParseRes<Reserved> {
    context("reserved identity",
        alt((tag("SPENDER-TX-HASH").map(|_| Reserved::SpenderTx),
             tag("SPENDER-TX").map(|_| Reserved::SpenderTxHash),
             tag("COV-HASH").map(|_| Reserved::CovHash),
             tag("SELF-INPUT").map(|_| Reserved::SelfInput),
        )))(input)
}

/// Top level parser returns any valid [Expr].
pub fn expr<'a>(input: &'a str)
-> ParseRes<Expr> {
    // The order is important
    alt((bytes.map(Value::Bytes).map(Expr::Value),
         int.map(Value::Int).map(Expr::Value),
         vector.map(Expr::Vector),
         unary_builtin.map(|b| Expr::BuiltIn(Box::new(b))),
         binary_builtin.map(|b| Expr::BuiltIn(Box::new(b))),
         tri_builtin.map(|b| Expr::BuiltIn(Box::new(b))),
         empty_builtin.map(|b| Expr::BuiltIn(Box::new(b))),
         reserved.map(|r| Expr::Reserved(r)),
         symbol.map(Expr::Var),
         set,
         let_bind.map(|(binds, exprs)| Expr::Let(binds, exprs)),
         if_expr.map(|(p,t,f)| Expr::If(Box::new(p), Box::new(t), Box::new(f))),
         loop_expr.map(|(n,e)| Expr::Loop(n, Box::new(e))),
         hash.map(|(n,e)| Expr::Hash(n, Box::new(e))),
         sigeok.map(|(n,e1,e2,e3)| Expr::Sigeok(n, Box::new(e1), Box::new(e2), Box::new(e3))),
         app,
     )).parse(input)
}
