use primitive_types::U256;

/// Push is inherent in the language and so not a variant of BuiltIn.
pub struct PushI;

#[derive(Debug, PartialEq, Eq)]
pub enum BuiltIn {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Oflo,
    And,
    Or,
    Xor,
    Not,
    Load,
    Store,
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
