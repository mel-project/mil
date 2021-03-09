use nom::{
    IResult, Parser,
    error::ParseError,
    multi::{separated_list1},
    character::complete::char,
    sequence::{delimited},
};

#[derive(Debug)]
pub struct Sexp {
    op: char,//Operation,
    args: Vec<char>,
}

/// Space between elements of an s-expression
/*
fn sexp_delimiter<I>(i: I) -> IResult<I, O> {
    char(' ')(i)
}
*/

// fn identifier(


pub fn parse_sexp(i: &str) -> IResult<&str, Sexp> {
    let delimiter = char(' ');
    //let identifer = char('.');
    //let args = sepBy(char('.'), delimiter);
    let elems = separated_list1(delimiter, char('x'))
        .map(|l| Sexp {
            op: l[0],
            args: l[1..].into(),
        });
    let mut sexp = delimited(char('('), elems, char(')'));
    sexp(i)
}
