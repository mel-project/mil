use primitive_types::U256;

#[derive(Debug, PartialEq, Eq)]
pub enum BuiltIn {
    Add,
    Sub,
    PushI,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Atom {
    Int(U256),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Atom(Atom),
    App(BuiltIn, Vec<Expr>),
}
