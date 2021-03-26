use std::collections::HashMap;
use crate::types::{Symbol, Expr, VarId, UnrolledExpr};
use crate::parser::{ParseErr, Defn};

/// An index for a location on the MelVM heap.
type HeapPos = u16;
/// A list of a function's parameters and its body.
type FnInfo = (Vec<Symbol>, Expr);

/// Evaluate a Mil [Expr], tracking symbols and unrolling fns.
trait Evaluator {
    //fn eval(UnrolledExpr) -> MelExpr;
    /// Recursively unroll fn invocations in an [Expr] so that only [BuiltIn]s are left.
    fn expand_fns(&self, &Expr) -> UnrolledExpr;
    fn new(Vec<Defn>) -> Self;
}

/// Short hand for a Result<_, ParseErr> type given the error string and args.
macro_rules! PErr {
    ($msg:expr, $($var:expr),+) => {
        Err(ParseErr(format!($msg, $($var),+)))
    }
}

struct Env {
    // Mapping variables to the location they point to on the heap.
    /// Mapping parameters as defined in a fn definition, to their mangled form.
    mangled: HashMap<Symbol, VarId>,
    /// Tracking fns. Notice [Defn] bodies are [Expr]s, meaning they can use other fns
    /// (non-builtins).
    fns: HashMap<Symbol, FnInfo>,
}

impl Evaluator for Env {
    fn new(fns: Vec<Defn>) -> Self {
        // Store fns in a hashmap
        let fns: HashMap<Symbol, FnInfo> = fns.into_iter().collect();

        Env {
            mangled: HashMap::new(),
            fns,
        }
    }

    /// Mangle variables and substitute all in body.
    /// Prepend set! ops for each variable parameter to the body.
    /// Return the new expression as an [UnrolledExpr].
    fn expand_fns(&self, expr: &Expr) -> Result<UnrolledExpr, ParseErr> {
        match expr {
            Expr::Var(x) => {
                match self.mangled.get(x) {
                    Some(v) => Ok(UnrolledExpr::Var(v)),
                    None => PErr!("Variable {} is not defined.", x),
                }
            },
            Expr::BuiltIn(b) => match *b {
            },
            Expr::Set(s,e) => {
                let var  = vars.get(s)
                    .ok_or(ParseErr(format!("Variable {} is not defined.", s)))?;
                let expr = subst_vars(&**e, vars)?;
                Ok(UnrolledExpr::Set(*var, Box::new(expr)))
            },
            Expr::App(f,es) => {
                // Get the fn definition from the env
                let (params, body) = self.fns.get(f)
                    .ok_or(ParseErr(format!("Function {} was called but is not deifned.", f)))?;

                // Check that args length macthes params to fn
                if params.len() != es.len() {
                    return PErr!("Function invocation expected {} arguments, {} were supplied.",
                        params.len(), es.len());
                }

                // Expand arguments before expanding body
                let args = fold_results(es.iter()
                    .map(|e| self.expand_fns(e)).collect())?;

                // Mangle parameters of fn
                let mangled_vars = params.iter().map(mangled).collect();
                // Map between mangled and original
                let mangled_map: HashMap<Symbol, VarId>
                    = params.clone().into_iter()
                            .zip(mangled_vars.clone().into_iter())
                            .collect();

                // Create a new env to expand the body and replace variables with the mangled version
                let f_env = Env {
                    // TODO: Make sure mangled_map overrides mangled
                    mangled: self.mangled.clone().into_iter().chain(mangled_map).collect(),
                    fns: self.fns,
                };

                // lol
                let mangled_body = expand_fns(f_env, body)?;

                let bindings = mangled_vars.into_iter()
                                           .zip(args.into_iter())
                                           .collect();

                // Wrap our mangled body in let bindings
                Ok(UnrolledExpr::Let(bindings, mangled_body))
            },
            Expr::Let(syms, e) => {
                Ok(UnrolledExpr::Let(var, expr))
            },
            Expr::Int(n) => UnrolledExpr::Int(n),
        }
    }

    /*
    fn expand_fns(&self, e: &Expr) -> Result<UnrolledExpr, ParseErr> {
        if let Expr::App(fn_name, args) = e {
            match self.fns.get(fn_name) {
                // Apply the unrolled arguments to the fn body
                Some((params, body)) => apply_fn(
                    body, params, args.iter().map(expand_fns)),
                None => Err(ParseErr(format!("Undefind fn '{}'.", fn_name))),
            },
                // Replace with location in symbol table
                /*
                Operator::Special(sp) => match sp {
                    Set => set(env),
                },
                */
                // Otherwise recurse unroll and just cast op as-is
                //Operator::Builtin => UnrolledExpr::App(
                    //op, args.iter().map(expand_fns)),
            //}
        }
    }
    */
}

// TODO: This should probably be part of Env so that previously mangled symbols return the same number
fn mangled(s: &Symbol) -> VarId {
    0 // TODO
}

/*
/// If top-level operator is a symbol, returns its value.
fn is_symbol(e: &Expr) -> Option<Symbol> {
    if let Expr::Symbol(s) = &e {
        // Check that the string doesn't match to a reserved operator
        if let Operator::Symbol(_) = to_op(s) {
            Ok(s.to_string())
}
*/

/// Try to extract values from results in vector. Short circuit on the first failure. Note this
/// does not return an iterator (because it folds).
// TODO: For efficiency: fold_results(v: Vec<O>, f: Fn(O) -> Result<O,E>), map and fold in one pass
fn fold_results<O,E>(v: Vec<Result<O, E>>) -> Result<Vec<O>, E> {
    v.into_iter()
     .try_fold(vec![], |mut inner_vec, r| { //inner_vec.push(v)))
         let v = r?;
         inner_vec.push(v);
         Ok(inner_vec)
     })
    .map(|mut v| { v.reverse(); v })
}
