#[macro_use]
use crate::PErr;
use std::collections::HashMap;
use crate::types::{ExpandedBuiltIn, BuiltIn, Symbol, Expr, VarId, UnrolledExpr};
use crate::parser::{ParseErr, Defn};

/// A list of a function's parameters and its body.
type FnInfo = (Vec<Symbol>, Expr);

/// Evaluate a Mil [Expr], tracking symbols and unrolling fns.
pub trait Evaluator {
    //fn eval(UnrolledExpr) -> MelExpr;
    /// Recursively unroll fn invocations in an [Expr] so that only [BuiltIn]s are left.
    fn expand_fns(&self, e: &Expr) -> Result<UnrolledExpr, ParseErr>;
    fn new(fns: Vec<Defn>) -> Self;
}

pub struct Env {
    // Mapping variables to the location they point to on the heap.
    /// Mapping parameters as defined in a fn definition, to their mangled form.
    mangled: HashMap<Symbol, VarId>,
    /// Tracking fns. Notice [Defn] bodies are [Expr]s, meaning they can use other fns
    /// (non-builtins).
    fns: HashMap<Symbol, FnInfo>,
}

/// A simple mangler that just returns i+1 for the next variable id.
struct LinearMangler { idx: VarId }

impl LinearMangler {
    fn next(&mut self) -> VarId {
        self.idx = self.idx + 1;
        self.idx
    }
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
        self.expand_mangle_fns(expr, &mut LinearMangler{ idx:0 })
    }
}

impl Env {
    fn expand_mangle_fns(&self, expr: &Expr, mangler: &mut LinearMangler) -> Result<UnrolledExpr, ParseErr>
    {
        match expr {
            Expr::Var(x) => {
                let v = try_get_var(x, &self.mangled)?;
                Ok(UnrolledExpr::Var(v))
            },
            Expr::BuiltIn(b) => match &**b {
                BuiltIn::Add(e1,e2) => {
                    let e1 = self.expand_mangle_fns(&e1, mangler)?;
                    let e2 = self.expand_mangle_fns(&e2, mangler)?;

                    Ok(UnrolledExpr::BuiltIn( Box::new(ExpandedBuiltIn::Add(e1, e2)) ))
                },
                _ => unreachable!(), // TODO
            },
            Expr::Set(s,e) => {
                let var = try_get_var(s, &self.mangled)?;
                let expr = self.expand_mangle_fns(e, mangler)?;
                Ok(UnrolledExpr::Set(var, Box::new(expr)))
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
                    .map(|e| self.expand_mangle_fns(e, mangler)).collect())?;

                // Mangle parameters of fn
                let mangled_vars: Vec<VarId> = params.iter().map(|_| mangler.next()).collect();
                // Map between mangled and original
                let mangled_map: HashMap<Symbol, VarId>
                    = params.clone().into_iter()
                            .zip(mangled_vars.clone().into_iter())
                            .collect();

                // Create a new env to expand the body and replace variables with the mangled version
                let f_env = Env {
                    // TODO: Make sure mangled_map overrides mangled
                    mangled: self.mangled.clone().into_iter().chain(mangled_map).collect(),
                    fns: self.fns.clone(),
                };

                // lol
                let mangled_body = f_env.expand_mangle_fns(body, mangler)?;

                let bindings = mangled_vars.into_iter()
                                           .zip(args.into_iter())
                                           .collect();

                // Wrap our mangled body in let bindings
                Ok(UnrolledExpr::Let(bindings, Box::new(mangled_body)))
            },
            Expr::Let(binds, e) => {
                let mangled_binds = fold_results(binds.iter()
                    .map(|(sym, expr)| {
                        let m_sym  = try_get_var(sym, &self.mangled)?;
                        let m_expr = self.expand_mangle_fns(expr, mangler)?;
                        Ok((m_sym, m_expr))
                    }).collect())?;
                let u_expr = self.expand_mangle_fns(expr, mangler)?;
                Ok(UnrolledExpr::Let(mangled_binds, Box::new(u_expr)))
            },
            Expr::Int(n) => Ok(UnrolledExpr::Int(n.clone())),
        }
    }
}

fn try_get_var(sym: &Symbol, hm: &HashMap<Symbol, VarId>) -> Result<VarId, ParseErr> {
    hm.get(sym)
        .ok_or(ParseErr(format!("Variable {} is not defined.", sym)))
        .map(|v| v.clone())
}

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
