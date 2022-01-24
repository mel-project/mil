use ethnum::U256;

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
    Exp(E, E, u8),
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
    TypeQ(E),
    // Stack ops
    Dup(E),
    // Overflow
    //Oflo,
    // Heap access
    Load(HeapPos),
    Store(HeapPos),
}

impl<E> ExpandedBuiltIn<E> {
    /// Gets a vector of arguments
    pub fn arguments(&self) -> Vec<&E> {
        let mut toret = Vec::with_capacity(3);
        match self {
            ExpandedBuiltIn::Add(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Sub(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Mul(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Exp(x, y, _) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Div(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Rem(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Not(x) => toret.push(x),
            ExpandedBuiltIn::Or(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::And(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Xor(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Eql(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Lt(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Gt(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Shl(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Shr(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Vlen(x) => toret.push(x),
            ExpandedBuiltIn::Vref(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Vpush(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Vcons(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Vappend(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Vslice(x, y, z) => toret.extend_from_slice(&[x, y, z]),
            ExpandedBuiltIn::Vset(x, y, z) => toret.extend_from_slice(&[x, y, z]),
            ExpandedBuiltIn::Blen(x) => toret.push(x),
            ExpandedBuiltIn::Bref(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Bpush(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Bcons(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Bappend(x, y) => toret.extend_from_slice(&[x, y]),
            ExpandedBuiltIn::Bslice(x, y, z) => toret.extend_from_slice(&[x, y, z]),
            ExpandedBuiltIn::Bset(x, y, z) => toret.extend_from_slice(&[x, y, z]),
            ExpandedBuiltIn::ItoB(x) => toret.push(x),
            ExpandedBuiltIn::BtoI(x) => toret.push(x),
            ExpandedBuiltIn::TypeQ(x) => toret.push(x),
            ExpandedBuiltIn::Dup(x) => toret.push(x),
            _ => {}
        };
        toret
    }

    /// Structural map.
    pub fn structural_map(self, mut f: impl FnMut(E) -> E) -> Self {
        match self {
            ExpandedBuiltIn::Add(x, y) => ExpandedBuiltIn::Add(f(x), f(y)),
            ExpandedBuiltIn::Sub(x, y) => ExpandedBuiltIn::Sub(f(x), f(y)),
            ExpandedBuiltIn::Mul(x, y) => ExpandedBuiltIn::Mul(f(x), f(y)),
            ExpandedBuiltIn::Exp(x, y, k) => ExpandedBuiltIn::Exp(f(x), f(y), k),
            ExpandedBuiltIn::Div(x, y) => ExpandedBuiltIn::Div(f(x), f(y)),
            ExpandedBuiltIn::Rem(x, y) => ExpandedBuiltIn::Rem(f(x), f(y)),
            ExpandedBuiltIn::Not(x) => ExpandedBuiltIn::Not(f(x)),
            ExpandedBuiltIn::Or(x, y) => ExpandedBuiltIn::Or(f(x), f(y)),
            ExpandedBuiltIn::And(x, y) => ExpandedBuiltIn::And(f(x), f(y)),
            ExpandedBuiltIn::Xor(x, y) => ExpandedBuiltIn::Xor(f(x), f(y)),
            ExpandedBuiltIn::Eql(x, y) => ExpandedBuiltIn::Eql(f(x), f(y)),
            ExpandedBuiltIn::Lt(x, y) => ExpandedBuiltIn::Lt(f(x), f(y)),
            ExpandedBuiltIn::Gt(x, y) => ExpandedBuiltIn::Gt(f(x), f(y)),
            ExpandedBuiltIn::Shl(x, y) => ExpandedBuiltIn::Shl(f(x), f(y)),
            ExpandedBuiltIn::Shr(x, y) => ExpandedBuiltIn::Shr(f(x), f(y)),
            ExpandedBuiltIn::Vlen(x) => ExpandedBuiltIn::Vlen(f(x)),
            ExpandedBuiltIn::Vref(x, y) => ExpandedBuiltIn::Vref(f(x), f(y)),
            ExpandedBuiltIn::Vpush(x, y) => ExpandedBuiltIn::Vpush(f(x), f(y)),
            ExpandedBuiltIn::Vcons(x, y) => ExpandedBuiltIn::Vcons(f(x), f(y)),
            ExpandedBuiltIn::Vappend(x, y) => ExpandedBuiltIn::Vappend(f(x), f(y)),
            ExpandedBuiltIn::Vslice(x, y, z) => ExpandedBuiltIn::Vslice(f(x), f(y), f(z)),
            ExpandedBuiltIn::Vset(x, y, z) => ExpandedBuiltIn::Vset(f(x), f(y), f(z)),
            ExpandedBuiltIn::Blen(x) => ExpandedBuiltIn::Blen(f(x)),
            ExpandedBuiltIn::Bref(x, y) => ExpandedBuiltIn::Bref(f(x), f(y)),
            ExpandedBuiltIn::Bpush(x, y) => ExpandedBuiltIn::Bpush(f(x), f(y)),
            ExpandedBuiltIn::Bcons(x, y) => ExpandedBuiltIn::Bcons(f(x), f(y)),
            ExpandedBuiltIn::Bappend(x, y) => ExpandedBuiltIn::Bappend(f(x), f(y)),
            ExpandedBuiltIn::Bslice(x, y, z) => ExpandedBuiltIn::Bslice(f(x), f(y), f(z)),
            ExpandedBuiltIn::Bset(x, y, z) => ExpandedBuiltIn::Bset(f(x), f(y), f(z)),
            ExpandedBuiltIn::ItoB(x) => ExpandedBuiltIn::ItoB(f(x)),
            ExpandedBuiltIn::BtoI(x) => ExpandedBuiltIn::BtoI(f(x)),
            ExpandedBuiltIn::TypeQ(x) => ExpandedBuiltIn::TypeQ(f(x)),
            ExpandedBuiltIn::Dup(x) => ExpandedBuiltIn::TypeQ(f(x)),
            other => other,
        }
    }
}

/// Primitive operations that are accessible in the mil language front-end.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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
    /// (** 10 3)
    Exp(Expr, Expr, u8),

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
    /// Get type of expression, Int=1 Bytes=2 Vector=3
    TypeQ(Expr),
    /// Duplicate the value of an expression
    Dup(Expr),

    Fail,
    // TODO: Remove these
    // Unimplemented
    //Load(Symbol),
    // Unimplemented
    //Store(Symbol),
}

/// Symbolic name for an expression
pub type Symbol = String;
pub type Symb = str;
/// Internal data type for tracking variable ids.
pub type VarId = i32;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
/// Lisp evaluator fundamental data types. These are used by the compiler, not by MelVM.
pub enum Value {
    Int(U256),
    Bytes(Vec<u8>),
}

// TODO: Why are SpenderTx and SpenderTxHash reversed??
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
/// Reserved identities for values available in every MelVM script.
/// u8 represents the corresponding location on the heap.
pub enum Reserved {
    /// The transaction spending the covenant script's output, as a value.
    SpenderTx = 0,
    /// Hash of the spender transaction.
    SpenderTxHash = 1,
    /// Hash of the spent coin's transaction.
    ParentTxHash = 2,
    /// Index of the output in spent coin's transaction.
    ParentIndex = 3,
    /// Hash of the covenant script.
    SelfHash = 4,
    /// The spent coin's monetary value.
    ParentValue = 5,
    /// Denomination of the currency of the spent coin.
    ParentDenom = 6,
    /// Arbitrary bytes as input to a UTXO.
    ParentData = 7,
    /// Block height of the spent coin.
    ParentHeight = 8,
    /// Which input of the spender is this
    SpenderIndex = 9,
    /// Header of the last block to be added to the chain.
    LastHeader = 10,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
/// Non-value-returning syntax.
pub enum Statement {
    /// Bind a symbol to a value within for the scope of a list of statements.
    SetLet(Vec<(Symbol, Expr)>, Vec<Statement>),
    /// Loop a statement a specified number of  times.
    Loop(u16, Box<Statement>),
    /// An if control flow with bodies as statements.
    If(Box<Expr>, Box<Statement>, Box<Statement>),
    /// Assign a value stored on the heap to a symbol
    Set(Symbol, Box<Expr>),
    /// No-operation
    Noop,
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Non-value-returning syntax.
pub enum UnrolledStatement {
    /// Bind a symbol to a value within for the scope of a list of statements.
    SetLet(Vec<(VarId, UnrolledExpr)>, Vec<UnrolledStatement>),
    /// Loop a statement a specified number of  times.
    Loop(u16, Box<UnrolledStatement>),
    /// An if control flow with bodies as statements.
    If(
        Box<UnrolledExpr>,
        Box<UnrolledStatement>,
        Box<UnrolledStatement>,
    ),
    /// Assign a value stored on the heap to a symbol
    Set(VarId, Box<UnrolledExpr>),
    /// No-operation
    Noop,
}

impl UnrolledStatement {
    pub fn structural_map(
        self,
        stmt_map: &mut impl FnMut(Self) -> Self,
        expr_map: &mut impl FnMut(UnrolledExpr) -> UnrolledExpr,
    ) -> Self {
        let new_self = match self {
            UnrolledStatement::SetLet(binds, stmt) => {
                let binds = binds
                    .into_iter()
                    .map(|(i, j)| (i, j.structural_map(expr_map, stmt_map)))
                    .collect();
                let stmt = stmt
                    .into_iter()
                    .map(|s| s.structural_map(stmt_map, expr_map))
                    .collect();
                UnrolledStatement::SetLet(binds, stmt)
            }
            UnrolledStatement::Loop(n, body) => {
                let body = Box::new(body.structural_map(stmt_map, expr_map));
                UnrolledStatement::Loop(n, body)
            }
            UnrolledStatement::If(x, y, z) => {
                let x = Box::new(x.structural_map(expr_map, stmt_map));
                let y = Box::new(y.structural_map(stmt_map, expr_map));
                let z = Box::new(z.structural_map(stmt_map, expr_map));
                UnrolledStatement::If(x, y, z)
            }
            UnrolledStatement::Set(i, expr) => {
                let expr = Box::new(expr.structural_map(expr_map, stmt_map));
                UnrolledStatement::Set(i, expr)
            }
            UnrolledStatement::Noop => UnrolledStatement::Noop,
        };
        stmt_map(new_self)
    }
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
    /// No-operation
    Noop,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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
    /// A variable is a pointer to a location on the heap.
    Var(Symbol),
    /// Reserved identities for values available in every MelVM script.
    Reserved(Reserved),
    /// Bind a symbol to a value within the scope of a given expression.
    Let(Vec<(Symbol, Expr)>, Vec<Statement>, Box<Expr>),
    /// If expression.
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    /// Hash the return value of an expression.
    Hash(u16, Box<Expr>),
    /// Sign a message with a public key and check that it matches a signature.
    Sigeok(u16, Box<Expr>, Box<Expr>, Box<Expr>),
    // Run the expression and then check overflow, with a default value if true.
    //Checked(Box<Expr>, Box<Expr>),
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
    // Assign a value stored on the heap to a symbol
    //Set(VarId, Box<UnrolledExpr>),
    /// A variable is a pointer to a location on the heap.
    /// The [VarId] represents a unique-mangled variable id.
    Var(VarId),
    /// Bind a symbol to a value within the scope of a given expression.
    Let(
        Vec<(VarId, UnrolledExpr)>,
        Vec<UnrolledStatement>,
        Box<UnrolledExpr>,
    ),
    /// If expression.
    If(Box<UnrolledExpr>, Box<UnrolledExpr>, Box<UnrolledExpr>),
    // Loop an expression a specified number of  times.
    //Loop(u16, Box<UnrolledExpr>),
    /// Hash the return value of an expression.
    Hash(u16, Box<UnrolledExpr>),
    /// Sign a message with a public key and check that it matches a signature.
    Sigeok(u16, Box<UnrolledExpr>, Box<UnrolledExpr>, Box<UnrolledExpr>),
}

impl UnrolledExpr {
    /// Strucuturally recursively "maps" a function to the UnrolledExpr. In particular, given any UnrolledExpr, the given function is first applied to all subexpressions recursively, then to a new UnrolledExpr built from these new subexpressions.
    pub fn structural_map(
        self,
        expr_map: &mut impl FnMut(Self) -> Self,
        stmt_map: &mut impl FnMut(UnrolledStatement) -> UnrolledStatement,
    ) -> Self {
        let new_self = match self {
            UnrolledExpr::BuiltIn(builtin) => UnrolledExpr::BuiltIn(Box::new(
                builtin.structural_map(|f| f.structural_map(expr_map, stmt_map)),
            )),
            UnrolledExpr::Let(x, y, z) => {
                let x: Vec<(i32, UnrolledExpr)> = x
                    .into_iter()
                    .map(|(i, j)| (i, j.structural_map(expr_map, stmt_map)))
                    .collect();
                let y: Vec<UnrolledStatement> = y
                    .into_iter()
                    .map(|i| i.structural_map(stmt_map, expr_map))
                    .collect();
                let z = z.structural_map(expr_map, stmt_map);
                UnrolledExpr::Let(x, y, Box::new(z))
            }
            UnrolledExpr::If(x, y, z) => UnrolledExpr::If(
                Box::new(x.structural_map(expr_map, stmt_map)),
                Box::new(y.structural_map(expr_map, stmt_map)),
                Box::new(z.structural_map(expr_map, stmt_map)),
            ),
            UnrolledExpr::Hash(x, y) => {
                UnrolledExpr::Hash(x, Box::new(y.structural_map(expr_map, stmt_map)))
            }
            UnrolledExpr::Sigeok(n, x, y, z) => UnrolledExpr::Sigeok(
                n,
                Box::new(x.structural_map(expr_map, stmt_map)),
                Box::new(y.structural_map(expr_map, stmt_map)),
                Box::new(z.structural_map(expr_map, stmt_map)),
            ),
            other => other,
        };
        expr_map(new_self)
    }
}
