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
    Vpush,
    Vempty,
    Vref,
    Vlen,
    Vappend,
    Vslice,
    Load,
    Store,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Atom {
    Int(U256),
    Vec {
        members: Vec<Atom>,
        is_struct: bool
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Atom(Atom),
    App(BuiltIn, Vec<Expr>),
}
