use crate::parser::{Defn, ParseErr};
use crate::types::{BuiltIn, Expr, Reserved, Statement, Symbol, Value};
use crate::PErr;
use ethnum::U256;
//#[macro_use] use nom_trace::{tr,print_trace, activate_trace};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_while, take_while1},
    character::complete::char,
    character::complete::{alpha1, digit1, hex_digit1, multispace0, multispace1},
    combinator::{cut, map_opt, map_res, opt},
    error::context,
    error::VerboseError,
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, preceded, separated_pair, tuple},
    IResult, Parser,
};

use nom_packrat::{packrat_parser, storage};

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
        list!(@as_expr ($($tuple),*, delimited(many1(ws_or_comment), $parser, many0(ws_or_comment))))
    };
    (@accum ($($tuple:expr),*) $parser:expr, $($tail:tt)*) => {
        list!(@accum ($($tuple),*,
                      preceded(many1(ws_or_comment), $parser))
                  $($tail)*)
    };
    ($parser:expr, $($tail:tt)*) => {
        list!(@accum (nom::combinator::flat_map(opt(comment), |_| preceded(multispace0, $parser))) $($tail)*)
    };
}

/// The result of a parser on strs with a VerboseError error type.
type ParseRes<'a, O> = IResult<&'a str, O, VerboseError<&'a str>>;

/// Parse a list of symbol->expr binding pairs.
fn sym_binds(input: &str) -> ParseRes<Vec<(Symbol, Expr)>> {
    context(
        "symbol bindings",
        s_expr(separated_list0(
            many1(ws_or_comment),
            separated_pair(symbol, many1(ws_or_comment), expr),
        )),
    )(input)
}

fn setlet_bind(input: &str) -> ParseRes<(Vec<(Symbol, Expr)>, Vec<Statement>)> {
    context(
        "set-let binding",
        list!(
            tag("set-let"),
            cut(sym_binds),
            cut(separated_list1(many1(ws_or_comment), statement))
        ),
    )
    .map(|(_, a, b)| (a, b))
    .parse(input)
}

fn let_bind(input: &str) -> ParseRes<(Vec<(Symbol, Expr)>, Vec<Statement>, Expr)> {
    context(
        "let binding",
        list!(
            tag("let"),
            cut(sym_binds),
            cut(alt((
                separated_list0(many1(ws_or_comment), statement)
                    .and(preceded(many1(ws_or_comment), expr)),
                expr.map(|e| (vec![], e))
            )))
        ),
    )
    .map(|(_, a, (b, c))| (a, b, c))
    .parse(input)
}

fn defn(input: &str) -> ParseRes<Defn> {
    context(
        "function definition",
        list!(
            tag("fn"),
            // Fn name
            cut(symbol),
            // Parameters
            cut(s_expr(separated_list0(many1(ws_or_comment), symbol))),
            // Body
            cut(expr)
        ),
    )
    .map(|(_, name, params, body)| (name, (params, body)))
    .parse(input)
}

/// Parse a [BuiltIn] expression with three arguments.
fn tri_builtin(input: &str) -> ParseRes<BuiltIn> {
    context(
        "tri builtin",
        map_opt(
            list!(
                take_while1(|x: char| x != ' ' && x != '\t' && x != '\n' && x != '\r'),
                expr,
                expr,
                expr
            ),
            |(s, e1, e2, e3)| match s {
                "v-from" => Some(BuiltIn::Vset(e1, e2, e3)),
                "b-from" => Some(BuiltIn::Bset(e1, e2, e3)),
                "v-slice" => Some(BuiltIn::Vslice(e1, e2, e3)),
                "b-slice" => Some(BuiltIn::Bslice(e1, e2, e3)),
                _ => None,
            },
        ),
    )
    .parse(input)
}

/// Builtins without arguments aren't wrapped in s-expressions.
fn empty_builtin(input: &str) -> ParseRes<BuiltIn> {
    context(
        "empty builtin",
        map_opt(
            // Basically saying that either nil or (nil) is acceptable
            alt((
                s_expr(take_while1(|x: char| {
                    x != ' ' && x != '\t' && x != '\n' && x != '\r'
                })),
                take_while1(|x: char| x != ' ' && x != '\t' && x != '\n' && x != '\r' && x != ')'),
            )),
            |s: &str| match s {
                "v-nil" => Some(BuiltIn::Vempty),
                "b-nil" => Some(BuiltIn::Bempty),
                "fail!" => Some(BuiltIn::Fail),
                _ => None,
            },
        ),
    )(input)
}

fn unary_builtin(input: &str) -> ParseRes<BuiltIn> {
    context(
        "unary builtin",
        map_opt(
            list!(
                take_while1(|x: char| x != ' ' && x != '\t' && x != '\n' && x != '\r'),
                expr
            ),
            |(s, e)| match s {
                "not" => Some(BuiltIn::Not(e)),
                "v-len" => Some(BuiltIn::Vlen(e)),
                "b-len" => Some(BuiltIn::Blen(e)),
                "bytes->u256" => Some(BuiltIn::BtoI(e)),
                "u256->bytes" => Some(BuiltIn::ItoB(e)),
                _ => None,
            },
        ),
    )(input)
}

fn binary_builtin(input: &str) -> ParseRes<BuiltIn> {
    context(
        "binary builtin",
        map_opt(
            list!(
                take_while1(|x: char| x != ' ' && x != '\t' && x != '\n' && x != '\r'),
                expr,
                expr
            ),
            |(s, e1, e2)| match s {
                "=" => Some(BuiltIn::Eql(e1, e2)),
                "+" => Some(BuiltIn::Add(e1, e2)),
                "-" => Some(BuiltIn::Sub(e1, e2)),
                "*" => Some(BuiltIn::Mul(e1, e2)),
                "/" => Some(BuiltIn::Div(e1, e2)),
                "<" => Some(BuiltIn::Lt(e1, e2)),
                ">" => Some(BuiltIn::Gt(e1, e2)),
                "%" => Some(BuiltIn::Rem(e1, e2)),
                "and" => Some(BuiltIn::And(e1, e2)),
                "or" => Some(BuiltIn::Or(e1, e2)),
                "xor" => Some(BuiltIn::Xor(e1, e2)),
                "v-cons" => Some(BuiltIn::Vcons(e1, e2)),
                "v-push" => Some(BuiltIn::Vpush(e1, e2)),
                "v-get" => Some(BuiltIn::Vref(e1, e2)),
                "v-concat" => Some(BuiltIn::Vappend(e1, e2)),
                "b-cons" => Some(BuiltIn::Bcons(e1, e2)),
                "b-push" => Some(BuiltIn::Bpush(e1, e2)),
                "b-get" => Some(BuiltIn::Bref(e1, e2)),
                "b-concat" => Some(BuiltIn::Bappend(e1, e2)),
                "<<" => Some(BuiltIn::Shl(e1, e2)),
                ">>" => Some(BuiltIn::Shr(e1, e2)),
                _ => None,
            },
        ),
    )
    .parse(input)
}

/// Parse a symbol, which is an alphanumeric string with underscores allowed.
fn symbol(input: &str) -> ParseRes<String> {
    let concat = |(a, b): (&str, &str)| -> Result<String, ParseErr> {
        let mut s = String::from(a);
        s.push_str(b);
        Ok(s)
    };

    context(
        "symbol",
        map_res(
            tuple((
                alpha1.or(tag("@")),
                take_while(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '?' | '>' | '!')),
            )),
            concat,
        ),
    )
    .parse(input)
}

// This function assumes an ascii encoding
fn from_hex(s: &str) -> Result<Vec<u8>, ParseErr> {
    if s.len() % 2 != 0 {
        return PErr!("Hex string {} is not an even number of characters.", s);
    }

    let mut bytes = vec![];
    for i in 0..s.len() / 2 {
        let idx = i * 2;
        let b = u8::from_str_radix(&s[idx..idx + 2], 16)
            .map_err(|_| ParseErr("Not a valid hex character.".to_string()))?;
        bytes.push(b);
    }
    Ok(bytes)
    //fold_results( s.chars().map(|c| u8::from_str_radix(c as &str, 16)).collect() )
}

fn bytes(input: &str) -> ParseRes<Vec<u8>> {
    context(
        "bytes",
        alt((
            // TODO: Support whitespace in strings
            delimited(
                tag("\""),
                take_while(|x| x != '"').map(|s: &str| s.as_bytes().into()),
                tag("\""),
            ),
            map_res(preceded(tag("0x"), cut(hex_digit1)), from_hex),
        )),
    )
    .parse(input)
}

/// Parse a noop statement
fn noop(input: &str) -> ParseRes<Statement> {
    context(
        "noop statement",
        s_expr(tag("noop")).map(|_| Statement::Noop),
    )
    .parse(input)
}

/// Parse a [U256] integer.
fn int(input: &str) -> ParseRes<U256> {
    context(
        "int",
        //map_res(digit1, |n_str: &str| U256::from_dec_str(n_str))
        map_res(digit1, |n_str: &str| U256::from_str_radix(n_str, 10)).map(U256::from),
    )
    .parse(input)
}

/// Parse a native vector type.
// TODO: Integrate this into list! macro so that vectors can have comments etc..
fn vector(input: &str) -> ParseRes<Vec<Expr>> {
    context(
        "vector",
        s_expr(
            tag("vector")
                .and(multispace0)
                .and(cut(separated_list0(many1(ws_or_comment), expr))),
        )
        .map(|(_, es)| es),
    )(input)
}

/// Wrap a parser in surrounding parenthesis with optional internal whitespace.
fn s_expr<'a, O, F>(parser: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, VerboseError<&'a str>>
where
    F: Parser<&'a str, O, VerboseError<&'a str>>,
{
    context(
        "S expression",
        delimited(
            char('(').and(multispace0),
            parser,
            multispace0.and(char(')')),
        ),
    )
}

/// Optionally parse comments and atleast one whitespace character
pub fn ws_or_comment<'a>(input: &'a str) -> ParseRes<&'a str> {
    context(
        "Comment/whitespace",
        alt((delimited(multispace0, comment, multispace0), multispace1)),
    )(input)
}

/// Parse a comment.
pub fn comment<'a>(input: &'a str) -> ParseRes<&'a str> {
    context("Comment", preceded(tag(";"), cut(is_not("\r\n"))))(input)
}

/// Top level of a program consists of a list of fn definitions and an expression.
pub fn root(input: &str) -> ParseRes<(Vec<Defn>, Expr)> {
    preceded(
        many0(ws_or_comment),
        tuple((
            separated_list0(many1(ws_or_comment), defn),
            preceded(many0(ws_or_comment), expr),
        )),
    )
    .parse(input)
}

pub fn set(input: &str) -> ParseRes<Statement> {
    // <tag> <symb> <expr>
    list!(tag("set!"), cut(symbol), cut(expr))
        .map(|(_, s, e)| Statement::Set(s, Box::new(e)))
        .parse(input)
}

/// Parse a function call (application) to any non-[BuiltIn] function.
pub fn app(input: &str) -> ParseRes<Expr> {
    context(
        "Application",
        alt((
            s_expr(symbol).map(|s| Expr::App(s, vec![])),
            list!(symbol, separated_list1(many1(ws_or_comment), expr))
                .map(|(s, a)| Expr::App(s, a)),
        )),
    )
    .parse(input)
}

pub fn if_expr(input: &str) -> ParseRes<(Expr, Expr, Expr)> {
    context(
        "if expression",
        list!(tag("if"), cut(expr), cut(expr), cut(expr)),
    )
    .map(|(_, e1, e2, e3)| (e1, e2, e3))
    .parse(input)
}

pub fn if_stmnt(input: &str) -> ParseRes<(Expr, Statement, Statement)> {
    context(
        "if expression",
        list!(tag("set-if"), cut(expr), cut(statement), cut(statement)),
    )
    .map(|(_, e1, s2, s3)| (e1, s2, s3))
    .parse(input)
}

pub fn sigeok(input: &str) -> ParseRes<(u16, Expr, Expr, Expr)> {
    context(
        "sigeok operation",
        list!(
            tag("sigeok"),
            cut(map_res(digit1, |n_str: &str| n_str.parse::<u16>())),
            cut(expr),
            cut(expr),
            cut(expr)
        )
        .map(|(_, n, e1, e2, e3)| (n, e1, e2, e3)),
    )
    .parse(input)
}

pub fn hash(input: &str) -> ParseRes<(u16, Expr)> {
    context(
        "hash operation",
        list!(
            tag("hash"),
            cut(map_res(digit1, |n_str: &str| n_str.parse::<u16>())),
            cut(expr)
        )
        .map(|(_, n, e)| (n, e)),
    )
    .parse(input)
}

pub fn typeof_expr(input: &str) -> ParseRes<BuiltIn> {
    context(
        "typeof expression",
        list!(tag("typeof"), cut(expr)).map(|(_, e)| BuiltIn::TypeQ(e)),
    )
    .parse(input)
}

/*
pub fn dup(input: &str) -> ParseRes<BuiltIn> {
    context(
        "dup expression",
        list!(
            tag("dup!"),
            cut(expr)
        )
        .map(|(_, e)| BuiltIn::Dup(e)),
    )
    .parse(input)
}
*/

pub fn loop_stmnt(input: &str) -> ParseRes<(u16, Statement)> {
    context(
        "loop expression",
        list!(
            tag("loop"),
            cut(map_res(digit1, |n_str: &str| n_str.parse::<u16>())),
            cut(statement)
        )
        .map(|(_, n, e)| (n, e)),
    )
    .parse(input)
}

pub fn reserved(input: &str) -> ParseRes<Reserved> {
    context(
        "reserved identity",
        alt((
            tag("SPENDER-TX-HASH").map(|_| Reserved::SpenderTxHash),
            tag("SPENDER-TX").map(|_| Reserved::SpenderTx),
            tag("PARENT-TX-HASH").map(|_| Reserved::ParentTxHash),
            tag("PARENT-INDEX").map(|_| Reserved::ParentIndex),
            tag("SELF-HASH").map(|_| Reserved::SelfHash),
            tag("PARENT-VALUE").map(|_| Reserved::ParentValue),
            tag("PARENT-DENOM").map(|_| Reserved::ParentDenom),
            tag("PARENT-DATA").map(|_| Reserved::ParentData),
            tag("PARENT-HEIGHT").map(|_| Reserved::ParentHeight),
            tag("SPENDER-INDEX").map(|_| Reserved::SpenderIndex),
            tag("LAST-HEADER").map(|_| Reserved::LastHeader),
        )),
    )(input)
}

/// Parse a mil statement (non-returning expression)
pub fn statement(input: &str) -> ParseRes<Statement> {
    // The order is important
    alt((
        setlet_bind.map(|(binds, stmnts)| Statement::SetLet(binds, stmnts)),
        set,
        noop,
        loop_stmnt.map(|(n, s)| Statement::Loop(n, Box::new(s))),
        if_stmnt.map(|(p, t, f)| Statement::If(Box::new(p), Box::new(t), Box::new(f))),
    ))
    .parse(input)
}

/// Top level parser returns any valid [Expr].
#[packrat_parser]
pub fn expr(input: &str) -> ParseRes<Expr> {
    nom_packrat::init!();
    // The order is important
    alt((
        bytes.map(Value::Bytes).map(Expr::Value),
        int.map(Value::Int).map(Expr::Value),
        vector.map(Expr::Vector),
        let_bind.map(|(binds, stmnts, expr)| Expr::Let(binds, stmnts, Box::new(expr))),
        unary_builtin.map(|b| Expr::BuiltIn(Box::new(b))),
        binary_builtin.map(|b| Expr::BuiltIn(Box::new(b))),
        tri_builtin.map(|b| Expr::BuiltIn(Box::new(b))),
        empty_builtin.map(|b| Expr::BuiltIn(Box::new(b))),
        reserved.map(Expr::Reserved),
        symbol.map(Expr::Var),
        if_expr.map(|(p, t, f)| Expr::If(Box::new(p), Box::new(t), Box::new(f))),
        hash.map(|(n, e)| Expr::Hash(n, Box::new(e))),
        sigeok.map(|(n, e1, e2, e3)| Expr::Sigeok(n, Box::new(e1), Box::new(e2), Box::new(e3))),
        typeof_expr.map(|b| Expr::BuiltIn(Box::new(b))),
        app,
    ))
    .parse(input)
}
