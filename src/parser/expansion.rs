use crate::types::{
    BuiltIn, ExpandedBuiltIn, Expr, Statement, Symbol, UnrolledExpr, UnrolledStatement, Value,
    VarId,
};
use crate::PErr;
use crate::{
    parser::{fold_results, Defn, ParseErr, NUM_RESERVED},
    types::Symb,
};
use std::collections::HashMap;

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
struct LinearMangler {
    idx: VarId,
}

impl LinearMangler {
    fn next(&mut self) -> VarId {
        self.idx += 1;
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
        // Start from 2 bcs 0 and 1 memory locations are occupied in the VM
        self.expand_mangle_fns(expr, &mut LinearMangler { idx: NUM_RESERVED })
    }
}

impl Env {
    // Convenience abstraction for repetitive code
    fn expand_binop<F>(
        &self,
        e1: &Expr,
        e2: &Expr,
        op: F,
        mangler: &mut LinearMangler,
    ) -> Result<UnrolledExpr, ParseErr>
    where
        F: Fn(UnrolledExpr, UnrolledExpr) -> ExpandedBuiltIn<UnrolledExpr>,
    {
        let e1 = self.expand_mangle_fns(&e1, mangler)?;
        let e2 = self.expand_mangle_fns(&e2, mangler)?;

        Ok(UnrolledExpr::BuiltIn(Box::new(op(e1, e2))))
    }

    fn expand_triop<F>(
        &self,
        e1: &Expr,
        e2: &Expr,
        e3: &Expr,
        op: F,
        mangler: &mut LinearMangler,
    ) -> Result<UnrolledExpr, ParseErr>
    where
        F: Fn(UnrolledExpr, UnrolledExpr, UnrolledExpr) -> ExpandedBuiltIn<UnrolledExpr>,
    {
        let e1 = self.expand_mangle_fns(&e1, mangler)?;
        let e2 = self.expand_mangle_fns(&e2, mangler)?;
        let e3 = self.expand_mangle_fns(&e3, mangler)?;

        Ok(UnrolledExpr::BuiltIn(Box::new(op(e1, e2, e3))))
    }

    fn expand_monop<F>(
        &self,
        e: &Expr,
        op: F,
        mangler: &mut LinearMangler,
    ) -> Result<UnrolledExpr, ParseErr>
    where
        F: Fn(UnrolledExpr) -> ExpandedBuiltIn<UnrolledExpr>,
    {
        let e = self.expand_mangle_fns(&e, mangler)?;
        Ok(UnrolledExpr::BuiltIn(Box::new(op(e))))
    }

    /// Auxillery function to expand and mangle a statement
    fn expand_mangle_stmnt(
        &self,
        stmnt: &Statement,
        mangler: &mut LinearMangler,
    ) -> Result<UnrolledStatement, ParseErr> {
        match stmnt {
            Statement::SetLet(binds, stmnts) => {
                // Generate mangled names for variables
                let mangled_vars: Vec<VarId> = binds.iter().map(|_| mangler.next()).collect();
                // Expand binding expressions
                let expanded_bind_exprs = fold_results(
                    binds
                        .iter()
                        .map(|(_, expr)| self.expand_mangle_fns(expr, mangler))
                        .collect(),
                )?;
                // Zip em together for later
                let mangled_binds = mangled_vars
                    .iter()
                    .cloned()
                    .zip(expanded_bind_exprs.iter().cloned())
                    .collect();

                // Map between mangled and original variable names
                let mangled_map: HashMap<Symbol, VarId> = binds
                    .iter()
                    .map(|(s, _)| s.clone())
                    .zip(mangled_vars.into_iter())
                    .collect();

                // Create a new env to expand the body and replace variables with the mangled version
                let f_env = Env {
                    // TODO: Make sure mangled_map overrides mangled
                    mangled: self
                        .mangled
                        .clone()
                        .into_iter()
                        .chain(mangled_map)
                        .collect(),
                    fns: self.fns.clone(),
                };

                let expanded_stmnts = fold_results(
                    stmnts
                        .iter()
                        .map(|stm| f_env.expand_mangle_stmnt(stm, mangler))
                        .collect(),
                )?;

                Ok(UnrolledStatement::SetLet(mangled_binds, expanded_stmnts))
            }
            // A `set!` must operate on a bound variable; find it and also expand the assignment expression
            Statement::Set(sym, e) => {
                let var = try_get_var(sym, &self.mangled)?;
                let expr = self.expand_mangle_fns(e, mangler)?;
                Ok(UnrolledStatement::Set(var, Box::new(expr)))
            }
            Statement::Loop(n, stmnt) => {
                let u_stmnt = self.expand_mangle_stmnt(stmnt, mangler)?;
                Ok(UnrolledStatement::Loop(*n, Box::new(u_stmnt)))
            }
            Statement::If(pred, on_true, on_false) => {
                let u_pred = self.expand_mangle_fns(pred, mangler)?;
                let on_true = self.expand_mangle_stmnt(on_true, mangler)?;
                let on_false = self.expand_mangle_stmnt(on_false, mangler)?;

                Ok(UnrolledStatement::If(
                    Box::new(u_pred),
                    Box::new(on_true),
                    Box::new(on_false),
                ))
            }
            Statement::Noop => Ok(UnrolledStatement::Noop),
        }
    }

    /// Auxillery function to expand and mangle an expression
    fn expand_mangle_fns(
        &self,
        expr: &Expr,
        mangler: &mut LinearMangler,
    ) -> Result<UnrolledExpr, ParseErr> {
        match expr {
            Expr::Checked(expr, default) => {
                let var = mangler.next();
                Ok(UnrolledExpr::Let(
                        vec![(var, self.expand_mangle_fns(expr, mangler)?)],
                        vec![],
                        Box::new(UnrolledExpr::If(
                            Box::new(UnrolledExpr::BuiltIn(Box::new(ExpandedBuiltIn::Oflo))),
                            Box::new(self.expand_mangle_fns(default, mangler)?),
                            Box::new(UnrolledExpr::Var(var))))))
            }
            // A variable should already be mangled, find its mangled value
            Expr::Var(x) => {
                let v = try_get_var(x, &self.mangled)?;
                Ok(UnrolledExpr::Var(v))
            }
            Expr::Reserved(r) => Ok(UnrolledExpr::Var(r.clone() as i32)),
            Expr::Vector(v) => {
                let exp_v = fold_results(
                    v.iter()
                        .map(|e| self.expand_mangle_fns(e, mangler))
                        .collect(),
                )?;

                fn as_cons(mut v: Vec<UnrolledExpr>) -> UnrolledExpr {
                    if v.is_empty() {
                        UnrolledExpr::BuiltIn(Box::new(ExpandedBuiltIn::<UnrolledExpr>::Vempty))
                    } else if v.len() == 1 {
                        UnrolledExpr::BuiltIn(Box::new(ExpandedBuiltIn::<UnrolledExpr>::Vpush(
                            UnrolledExpr::BuiltIn(Box::new(
                                ExpandedBuiltIn::<UnrolledExpr>::Vempty,
                            )),
                            v.pop().expect("Vector should not be empty"),
                        )))
                    } else {
                        let x = v.pop().expect("Vector should not be empty");
                        UnrolledExpr::BuiltIn(Box::new(ExpandedBuiltIn::<UnrolledExpr>::Vpush(
                            as_cons(v),
                            x,
                        )))
                    }
                }

                Ok(as_cons(exp_v))
            }
            // For a builtin op, expand its arguments and cast into an ExpandedBuiltIn
            Expr::BuiltIn(b) => match &**b {
                BuiltIn::Oflo => Ok(UnrolledExpr::BuiltIn(Box::new(
                    ExpandedBuiltIn::<UnrolledExpr>::Oflo,
                ))),
                BuiltIn::Vempty => Ok(UnrolledExpr::BuiltIn(Box::new(
                    ExpandedBuiltIn::<UnrolledExpr>::Vempty,
                ))),
                BuiltIn::Bempty => Ok(UnrolledExpr::BuiltIn(Box::new(
                    ExpandedBuiltIn::<UnrolledExpr>::Bempty,
                ))),
                BuiltIn::Fail => Ok(UnrolledExpr::BuiltIn(Box::new(
                    ExpandedBuiltIn::<UnrolledExpr>::Load(u16::MAX),
                ))),
                BuiltIn::Not(e) => {
                    self.expand_monop(e, ExpandedBuiltIn::<UnrolledExpr>::Not, mangler)
                }
                BuiltIn::Vlen(e) => {
                    self.expand_monop(e, ExpandedBuiltIn::<UnrolledExpr>::Vlen, mangler)
                }
                BuiltIn::Blen(e) => {
                    self.expand_monop(e, ExpandedBuiltIn::<UnrolledExpr>::Blen, mangler)
                }
                BuiltIn::BtoI(e) => {
                    self.expand_monop(e, ExpandedBuiltIn::<UnrolledExpr>::BtoI, mangler)
                }
                BuiltIn::ItoB(e) => {
                    self.expand_monop(e, ExpandedBuiltIn::<UnrolledExpr>::ItoB, mangler)
                }
                BuiltIn::TypeQ(e) => {
                    self.expand_monop(e, ExpandedBuiltIn::<UnrolledExpr>::TypeQ, mangler)
                }
                BuiltIn::Dup(e) => {
                    self.expand_monop(e, ExpandedBuiltIn::<UnrolledExpr>::Dup, mangler)
                }
                BuiltIn::Add(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Add, mangler)
                }
                BuiltIn::Sub(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Sub, mangler)
                }
                BuiltIn::Mul(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Mul, mangler)
                }
                BuiltIn::Exp(e1, e2, k) => Ok(UnrolledExpr::BuiltIn(Box::new(ExpandedBuiltIn::<UnrolledExpr>::Exp(
                    self.expand_mangle_fns(&e1, mangler)?,
                    self.expand_mangle_fns(&e2, mangler)?,
                    *k,
                )))),
                BuiltIn::Div(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Div, mangler)
                }
                BuiltIn::Rem(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Rem, mangler)
                }
                BuiltIn::And(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::And, mangler)
                }
                BuiltIn::Eql(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Eql, mangler)
                }
                BuiltIn::Lt(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Lt, mangler)
                }
                BuiltIn::Gt(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Gt, mangler)
                }
                BuiltIn::Or(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Or, mangler)
                }
                BuiltIn::Xor(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Xor, mangler)
                }
                BuiltIn::Shl(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Shl, mangler)
                }
                BuiltIn::Shr(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Shr, mangler)
                }
                BuiltIn::Vref(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Vref, mangler)
                }
                BuiltIn::Vappend(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Vappend, mangler)
                }
                BuiltIn::Vpush(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Vpush, mangler)
                }
                BuiltIn::Vcons(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Vcons, mangler)
                }
                BuiltIn::Vslice(e1, e2, e3) => {
                    self.expand_triop(e1, e2, e3, ExpandedBuiltIn::<UnrolledExpr>::Vslice, mangler)
                }
                BuiltIn::Vset(e1, e2, e3) => {
                    self.expand_triop(e1, e2, e3, ExpandedBuiltIn::<UnrolledExpr>::Vset, mangler)
                }
                BuiltIn::Bref(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Bref, mangler)
                }
                BuiltIn::Bappend(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Bappend, mangler)
                }
                BuiltIn::Bpush(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Bpush, mangler)
                }
                BuiltIn::Bcons(e1, e2) => {
                    self.expand_binop(e1, e2, ExpandedBuiltIn::<UnrolledExpr>::Bcons, mangler)
                }
                BuiltIn::Bslice(e1, e2, e3) => {
                    self.expand_triop(e1, e2, e3, ExpandedBuiltIn::<UnrolledExpr>::Bslice, mangler)
                }
                BuiltIn::Bset(e1, e2, e3) => {
                    self.expand_triop(e1, e2, e3, ExpandedBuiltIn::<UnrolledExpr>::Bset, mangler)
                }
                /*
                BuiltIn::Store(e) => {
                    let e = self.expand_mangle_fns(&e, mangler)?;
                    Ok(UnrolledExpr::BuiltIn( Box::new(ExpandedBuiltIn::<UnrolledExpr>::Store(e)) ))
                },
                */
                //_ => todo!("Not all builtins have been implemented"),
            },
            // Expand a fn call to its body, fail if a defn is not found
            Expr::App(f, es) => {
                // Get the fn definition from the env
                let (params, body) = self.fns.get(f).ok_or_else(|| {
                    ParseErr(format!("Function '{}' was called but is not defined.", f))
                })?;

                // Check that args length macthes params to fn
                if params.len() != es.len() {
                    return PErr!(
                        "Function invocation expected {} arguments, {} were supplied.",
                        params.len(),
                        es.len()
                    );
                }

                // Expand arguments before expanding body
                let args = fold_results(
                    es.iter()
                        .map(|e| self.expand_mangle_fns(e, mangler))
                        .collect(),
                )?;

                // Mangle parameters of fn
                let mangled_vars: Vec<VarId> = params.iter().map(|_| mangler.next()).collect();
                // Map between mangled and original
                let mangled_map: HashMap<Symbol, VarId> = params
                    .clone()
                    .into_iter()
                    .zip(mangled_vars.clone().into_iter())
                    .collect();

                // Create a new env to expand the body and replace variables with the mangled version
                let f_env = Env {
                    mangled: mangled_map,
                    fns: self.fns.clone(),
                };

                // lol
                let mangled_body = f_env.expand_mangle_fns(body, mangler)?;

                let bindings = mangled_vars.into_iter().zip(args.into_iter()).collect();

                // Wrap our mangled body in let bindings
                Ok(UnrolledExpr::Let(bindings, vec![], Box::new(mangled_body)))
            }
            // Mangling happens here
            Expr::Let(binds, stmnts, e) => {
                // Generate mangled names for variables
                let mangled_vars: Vec<VarId> = binds.iter().map(|_| mangler.next()).collect();
                // Expand binding expressions
                let expanded_bind_exprs = fold_results(
                    binds
                        .iter()
                        .map(|(_, expr)| self.expand_mangle_fns(expr, mangler))
                        .collect(),
                )?;
                // Zip em together for later
                let mangled_binds = mangled_vars
                    .iter()
                    .cloned()
                    .zip(expanded_bind_exprs.iter().cloned())
                    .collect();

                // Map between mangled and original variable names
                let mangled_map: HashMap<Symbol, VarId> = binds
                    .iter()
                    .map(|(s, _)| s.clone())
                    .zip(mangled_vars.into_iter())
                    .collect();

                // Create a new env to expand the body and replace variables with the mangled version
                let f_env = Env {
                    // TODO: Make sure mangled_map overrides mangled
                    mangled: self
                        .mangled
                        .clone()
                        .into_iter()
                        .chain(mangled_map)
                        .collect(),
                    fns: self.fns.clone(),
                };

                // Expand body statements
                let expanded_stmnts = fold_results(
                    stmnts
                        .iter()
                        .map(|stm| f_env.expand_mangle_stmnt(stm, mangler))
                        .collect(),
                )?;

                // Expand final expression
                let expanded_e = f_env.expand_mangle_fns(e, mangler)?;

                Ok(UnrolledExpr::Let(
                    mangled_binds,
                    expanded_stmnts,
                    Box::new(expanded_e),
                ))
            }
            Expr::If(pred, on_true, on_false) => {
                let u_pred = self.expand_mangle_fns(pred, mangler)?;
                let on_true = self.expand_mangle_fns(on_true, mangler)?;
                let on_false = self.expand_mangle_fns(on_false, mangler)?;

                Ok(UnrolledExpr::If(
                    Box::new(u_pred),
                    Box::new(on_true),
                    Box::new(on_false),
                ))
            }
            Expr::Hash(n, expr) => {
                let u_expr = self.expand_mangle_fns(expr, mangler)?;
                Ok(UnrolledExpr::Hash(*n, Box::new(u_expr)))
            }
            Expr::Sigeok(n, e1, e2, e3) => {
                let u_e1 = self.expand_mangle_fns(e1, mangler)?;
                let u_e2 = self.expand_mangle_fns(e2, mangler)?;
                let u_e3 = self.expand_mangle_fns(e3, mangler)?;
                Ok(UnrolledExpr::Sigeok(
                    *n,
                    Box::new(u_e1),
                    Box::new(u_e2),
                    Box::new(u_e3),
                ))
            }
            Expr::Value(v) => match v {
                Value::Int(n) => Ok(UnrolledExpr::Value(Value::Int(*n))),
                Value::Bytes(b) => Ok(UnrolledExpr::Value(Value::Bytes(b.clone()))),
            },
        }
    }
}

fn try_get_var(sym: &Symb, hm: &HashMap<Symbol, VarId>) -> Result<VarId, ParseErr> {
    hm.get(sym)
        .ok_or_else(|| ParseErr(format!("Variable {} is not defined.", sym)))
        .map(|v| *v)
}
