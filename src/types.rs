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

/// Operators with special evaluation cases when parsing. These are distinguished from
/// [BuiltIn]s, which translace directly to MelVM. Special forms are evaluated into BuiltIns.
#[derive(Debug, PartialEq, Eq)]
pub enum SpecialOp {
    //Defn,
    Set,
    //Let,
}

/// Symbolic name for an expression
pub type Symbol = String;

#[derive(Debug, PartialEq, Eq)]
/// Operator of an expression (the first element of an S-expression), can either
/// be a [BuiltIn], or a user defined [Symbol].
pub enum Operator {
    BuiltIn(BuiltIn),
    Symbol(Symbol),
    Special(SpecialOp),
}

/*
#[derive(Debug, PartialEq, Eq)]
/// Lisp evaluator fundamental data types. These are used by the compiler, not by MelVM.
pub enum Atom {
    Int(U256),
    Symbol(Symbol),
    /*
    Vec {
        members: Vec<Atom>,
        is_struct: bool
    },
    */
}
*/

#[derive(Debug, PartialEq, Eq)]
/// The lower level representation of a program that is directly compilable into a binary for the
/// MelVM.
pub enum MelExpr {
    /// Fundamental data type.
    Int(U256),
    /// Application of an op to some arguments.
    App(BuiltIn, Vec<MelExpr>),
}

#[derive(Debug, PartialEq, Eq)]
/// Abstract syntax tree of mil. This is evaluated into a [MelExpr] which can be compiled directly to
/// the MelVM.
pub enum Expr {
    /// Fundamental data type.
    Int(U256),
    // Symbol
    //Symbol(String),
    /// Application of an [Operator] to some arguments.
    App(Operator, Vec<Expr>),
    // Function definition.
    //Defn(Symbol, Vec<Symbol>, Box<Expr>),
    /// Assign a value stored on the heap to a symbol
    Set(Symbol, Box<Expr>),
    /// A variable is a pointer to a location on the heap.
    Var(Symbol),
    // Bind a symbol to a value that is determined at compile-time
    //Let(Symbol, Box<Expr>),
}

/// An expression where all applications are on [BuiltIn] operators.
/// This is the result of applying all defined macros.
pub enum UnrolledExpr {
    /// Fundamental data type.
    Int(U256),
    // Symbol
    //Symbol(String),
    /// Application of an [Operator] to some arguments.
    App(BuiltIn, Vec<UnrolledExpr>),
    /// Assign a value stored on the heap to a symbol
    Set(Symbol, Box<UnrolledExpr>),
    /// A variable is a pointer to a location on the heap.
    Var(Symbol),
}
