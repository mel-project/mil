use primitive_types::U256;

/// Push is inherent in the language and so not a variant of BuiltIn.
pub struct PushI;
pub struct PushB;
/// An index for a location on the MelVM heap.
pub type HeapPos = u16;

/// Primitive operations as the are represented internally in the AST.
/// Most notably, symbols are replaced with locations in memory.
/// ExpandedBuiltins are directly compilable to MelVM opcodes.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpandedBuiltIn<E> {
    // Arithmetic
    Add(E, E),
    Sub(E, E),
    Mul(E, E),
    Div(E, E),
    Rem(E, E),
    // Logical
    Not(E),
    Or(E, E),
    And(E, E),
    Xor(E, E),
    Eql(E, E),
    Lt(E, E),
    Gt(E, E),
    // Bitwise logical
    Shl(E, E),
    Shr(E, E),
    // Vectors
    Vempty,
    Vlen(E),
    Vref(E, E),
    Vpush(E, E),
    Vcons(E, E),
    Vappend(E, E),
    Vslice(E, E, E),
    Vset(E, E, E),
    // Bytes
    Bempty,
    Blen(E),
    Bref(E, E),
    Bpush(E, E),
    Bcons(E, E),
    Bappend(E, E),
    Bslice(E, E, E),
    Bset(E, E, E),
    // Control flow
    Bez(u16),
    Bnz(u16),
    Jmp(u16),
    // Type casts
    ItoB(E),
    BtoI(E),
    // Heap access
    Load(HeapPos),
    Store(HeapPos),
}

/// Primitive operations that are accessible in the mil language front-end.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BuiltIn {
    // Arithmetic
    /// (+ 4 2)
    Add(Expr, Expr),
    /// (- 4 2)
    Sub(Expr, Expr),
    /// (* 2 2)
    Mul(Expr, Expr),
    /// (/ 10 3)
    Div(Expr, Expr),
    /// (% 10 3)
    Rem(Expr, Expr),

    // Logical
    // ---------
    /// (and 1 0)
    And(Expr, Expr),
    /// (or 1 0)
    Or(Expr, Expr),
    /// (xor 0 1)
    Xor(Expr, Expr),
    /// (not 0)
    Not(Expr),
    /// (= 1 1)
    Eql(Expr, Expr),
    /// (< x y) ; x < y
    Lt(Expr, Expr),
    /// (> x y) ; x > y
    Gt(Expr, Expr),

    // Bitwise logical
    // ---------
    /// (<< b 1)
    Shl(Expr, Expr),
    /// (>> b 1)
    Shr(Expr, Expr),

    // Vectors
    // ---------
    /// (v-push v-nil 1) ; construct a vector from a push to the back
    Vpush(Expr, Expr),
    /// (v-cons 1 v-nil) ; construct a vector, prepending an element
    Vcons(Expr, Expr),
    /// v-nil
    Vempty,
    /// (v-get v 0)
    Vref(Expr, Expr),
    /// (v-len v)
    Vlen(Expr),
    /// (v-concat (v-cons 2 v-nil) (v-cons 1 v-nil)) => [2 1]
    Vappend(Expr, Expr),
    /// (v-slice v 32 64) ; [32..64]
    Vslice(Expr, Expr, Expr),
    /// (v-from v 0 2) ; Create a new vector/bytes like v but the 0th element is 2
    Vset(Expr, Expr, Expr),

    // Bytes
    // ---------
    /// bnil
    Bempty,
    /// (b-len v)
    Blen(Expr),
    /// (b-get v 0)
    Bref(Expr, Expr),
    /// (b-push b-nil 1) ; construct a vector from a push to the back
    Bpush(Expr, Expr),
    /// (b-cons 1 b-nil) ; construct a vector, prepending an element
    Bcons(Expr, Expr),
    /// (b-concat (b-cons 2 b-nil) (b-cons 1 b-nil)) => [2 1]
    Bappend(Expr, Expr),
    /// (b-slice v 32 64) ; [32..64]
    Bslice(Expr, Expr, Expr),
    /// (b-from v 0 2) ; Create a new vector/bytes like v but the 0th element is 2
    Bset(Expr, Expr, Expr),

    // Type casts
    // ---------
    /// Integer to bytes
    ItoB(Expr),
    /// First 32 bytes of a Bytes to an integer
    BtoI(Expr),
    // TODO: Remove these
    // Unimplemented
    //Load(Symbol),
    // Unimplemented
    //Store(Symbol),
}

/// Symbolic name for an expression
pub type Symbol = String;
/// Internal data type for tracking variable ids.
pub type VarId = i32;

#[derive(Clone, Debug, PartialEq, Eq)]
/// Lisp evaluator fundamental data types. These are used by the compiler, not by MelVM.
pub enum Value {
    Int(U256),
    Bytes(Vec<u8>),
}

// TODO: Why are SpenderTx and SpenderTxHash reversed??
#[derive(Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
/// Reserved identities for values available in every MelVM script.
/// u8 represents the corresponding location on the heap.
pub enum Reserved {
    /// The transaction spending the covenant script's output, as a value.
    SpenderTx = 1,
    /// Hash of the spender transaction.
    SpenderTxHash = 0,
    /// Hash of the spent coin's transaction.
    SelfTxHash = 2,
    /// Index of the output in spent coin's transaction.
    CoinIndex = 3,
    /// Hash of the covenant script.
    CovHash = 4,
    /// The spent coin's monetary value.
    CoinValue = 5,
    /// Denomination of the currency of the spent coin.
    CoinDenom = 6,
    /// Arbitrary bytes as input to a UTXO.
    SelfData = 7,
    /// Block height of the spent coin.
    CoinHeight = 8,
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// The lower level representation of a program that is directly compilable into a binary for the
/// MelVM.
pub enum MelExpr {
    /// Fundamental data type.
    Value(Value),
    /// Builtin operations.
    BuiltIn(Box<ExpandedBuiltIn<MelExpr>>),
    /// A sequence of instructions.
    Seq(Vec<MelExpr>),
    /// Loop an expression a specified number of  times.
    Loop(u16, Box<MelExpr>),
    /// Hash the return value of an expression.
    Hash(u16, Box<MelExpr>),
    /// Sign a message with a public key and check that it matches a signature.
    Sigeok(u16, Box<MelExpr>, Box<MelExpr>, Box<MelExpr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Abstract syntax tree of mil. This is evaluated into a [MelExpr] which can be compiled directly to
/// the MelVM.
pub enum Expr {
    /// Fundamental data type.
    Value(Value),
    /// Another fundamental data type in MelVM, but not a variant of [Value]
    /// because it is unrolled to vector operations in the AST.
    Vector(Vec<Expr>),
    /// Builtin operations.
    BuiltIn(Box<BuiltIn>),
    /// Application of a user-defined function to some arguments.
    App(Symbol, Vec<Expr>),
    /// Assign a value stored on the heap to a symbol
    Set(Symbol, Box<Expr>),
    /// A variable is a pointer to a location on the heap.
    Var(Symbol),
    /// Reserved identities for values available in every MelVM script.
    Reserved(Reserved),
    /// Bind a symbol to a value within the scope of a given expression.
    Let(Vec<(Symbol, Expr)>, Vec<Expr>),
    // Set a symbol to point to a location.
    //SetTo(Symbol, Box<Expr>),
    /// If expression.
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    /// Loop an expression a specified number of  times.
    Loop(u16, Box<Expr>),
    /// Hash the return value of an expression.
    Hash(u16, Box<Expr>),
    /// Sign a message with a public key and check that it matches a signature.
    Sigeok(u16, Box<Expr>, Box<Expr>, Box<Expr>),
}

/// An expression where all applications are on [BuiltIn] operators.
/// Variables are also mangled to distinguish scope.
/// It is the generated by applying all defined functions to an [Expr].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnrolledExpr {
    /// Fundamental data type.
    Value(Value),
    /// Builtin operations.
    BuiltIn(Box<ExpandedBuiltIn<UnrolledExpr>>),
    /// Assign a value stored on the heap to a symbol
    Set(VarId, Box<UnrolledExpr>),
    /// A variable is a pointer to a location on the heap.
    /// The [VarId] represents a unique-mangled variable id.
    Var(VarId),
    /// Bind a symbol to a value within the scope of a given expression.
    Let(Vec<(VarId, UnrolledExpr)>, Vec<UnrolledExpr>),
    /// If expression.
    If(Box<UnrolledExpr>, Box<UnrolledExpr>, Box<UnrolledExpr>),
    /// Loop an expression a specified number of  times.
    Loop(u16, Box<UnrolledExpr>),
    /// Hash the return value of an expression.
    Hash(u16, Box<UnrolledExpr>),
    /// Sign a message with a public key and check that it matches a signature.
    Sigeok(u16, Box<UnrolledExpr>, Box<UnrolledExpr>, Box<UnrolledExpr>),
}
