use mil::parser;

fn main() {
    println!("{:?}", parser::parse_sexp("(x x x)"));
}
