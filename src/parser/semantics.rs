/// An index for a location on the MelVM heap.
/*
type HeapPos = u16;

/// Evaluate a Mil [Expr], tracking symbols and unrolling macros.
trait Evaluator {
    fn eval(UnrolledExpr) -> MelExpr;
    /// Recursively unroll macro invocations in an [Expr] so that only [BuiltIn]s are left.
    fn unroll_macros(&Expr) -> UnrolledExpr;
    fn new(Vec<Macros>) -> Self;
    //fn unroll_macros(Expr, &HashMap<Macro>) -> UnrolledExpr;
}

struct Eval {
    /// Symbol table
    symbols: HashMap<Symbol, HeapPos>,
    /// Tracking macros. Notice [Macro] bodies are [Expr]s, meaning they can use other macros
    /// (non-builtins).
    macros: HashMap<Symbol, Expr>,
}

impl Evaluator for Eval {
    // fn eval(UnrolledExpr) -> MelExpr;
    pub fn new(self, macros: Vec<Macro>) -> Self {
        // Store macros in a hashmap
        let macros = HashMap<Symbol, Expr> = macros.iter().collect();

        Eval {
            symbols: HashMap::new(),
            macros: macros,
        }
    }

    fn unroll_macros(&self, e: &Expr) -> Result<UnrolledExpr, ParseErr> {
        if let Expr::App(op, args) = e {
            if let Operator::Symbol(sym) = op {
                match self.macros.get(sym) {
                    // Apply the unrolled arguments to the macro body
                    Some((params, body)) => substitute(
                        body, params, args.iter().map(unroll_macros)),
                    // Otherwise recurse unroll, but leave current op as-is
                    None => UnrolledExpr::App(
                        op, args.iter().map(unroll_macros)),
                }
            }
        }
    }
}

fn apply_macro(body: &Expr, params: Vec<Symbol>, args: Vec<UnrolledExpr>)
-> Result<UnrolledExpr, ParseErr> {
    let symbols: HashMap<Symbol, UnrolledExpr>
        = params.iter().zip(args.iter()).collect();

    // Check that params and args length match.
    if params.len() != args.len() {
        return Err(ParseErr(format!("Macro invocation expected {} arguments, {} were supplied",
                            params.len(), args.len())));
    }

    // Substitute args for symbols if application
    // Ignore set!s, bcs macros cannot be "set".
    match body {
        Expr::App(op, args) => {
            // Subst op and args
        },
        Expr::Int(n)   => UnrolledExpr::Int(n),
        Expr::Set(s,b) => UnrolledExpr::Set(s,b),
    }
}
*/

/*
/// If top-level operator is a symbol, returns its value.
fn is_symbol(e: &Expr) -> Option<Symbol> {
    if let Expr::Symbol(s) = &e {
        // Check that the string doesn't match to a reserved operator
        if let Operator::Symbol(_) = to_op(s) {
            Ok(s.to_string())
}
*/
