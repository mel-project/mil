use nom::{
    IResult, Parser,
    error::ParseError,
    multi::many1,
    character::complete::char,
    sequence::{tuple, delimited},
    combinator::{opt, iterator, ParserIterator},
};

struct Sexp<'a> {
    op: u8,//Operation,
    args: Vec<&'a str>,
}

/// Space between elements of an s-expression
/*
fn sexp_delimiter<I>(i: I) -> IResult<I, O> {
    char(' ')(i)
}
*/

// fn identifier(

//fn sepBy<I,O,E,F>(content: F, separator: F) -> impl Fn(I) -> IResult<(I,Vec<O>), E> 
fn sepBy<I,O,E,F>(content: F, separator: F)
    -> impl Fn(I) -> IResult<(I, Vec<O>), E>
    //-> impl Fn(I) -> IResult<(I, ParserIterator<I,E,F>), E>
    where F: Parser<I,O,E>,
          E: ParseError<I>,
          I: Clone
{
    let combo = tuple((content, opt(separator)));
    |i| {
        let it = iterator(i, combo);
        let elems = it
            .map(|(c,_)| c)
            .collect();

        it.finish()
          .map(|(i,_)| (i,elems))
        /*
        if elems.is_empty() {
            Err(..)
        } else {
        */
            //Ok(((leftover, elems), ()))
        //}
    }
}

fn parse_sexp<'a>(i: &'a str) -> IResult<&'a str, Sexp<'a>> {
    // TODO: parse identifier, then sepBy on args (exprs)
    // TODO: separate into a sexp parser
    let delimiter = char(' ');
    let args = sepBy(char('.'), delimiter);
    let sexp = delimited(char('('), args, char(')'));
    sexp(i)
}
