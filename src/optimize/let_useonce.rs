use std::collections::{BTreeMap, BTreeSet};

use crate::{
    parser::NUM_RESERVED,
    types::{UnrolledExpr, UnrolledStatement, VarId},
};

/// Inlines things until idempotent
pub fn let_useonce(input: UnrolledExpr) -> UnrolledExpr {
    let mut output = let_useonce_once(input);
    for pass in 1.. {
        log::debug!("pass {}...", pass);
        let new_output = let_useonce_once(output.clone());
        if new_output == output {
            break;
        }
        output = new_output;
    }
    output
}

/// Inlines let bindings that are only used once
fn let_useonce_once(input: UnrolledExpr) -> UnrolledExpr {
    const INLINE_THRESHOLD: usize = 1;
    // First, we gather all the varids.
    let mut varid_counts: BTreeMap<VarId, usize> = BTreeMap::new();
    let mut mutated_varids: BTreeSet<VarId> = BTreeSet::new();
    let mut varid_bindings: BTreeMap<VarId, UnrolledExpr> = BTreeMap::new();
    let input = input.structural_map(
        &mut |expr| {
            match &expr {
                UnrolledExpr::Var(varid) => {
                    if varid > &NUM_RESERVED {
                        *varid_counts.entry(*varid).or_default() += 1
                    }
                }
                UnrolledExpr::Let(bindings, _, _) => {
                    for (k, v) in bindings {
                        varid_bindings.insert(*k, v.clone());
                    }
                }
                _ => {}
            }
            expr
        },
        &mut |stmt| {
            if let UnrolledStatement::Set(vid, _) = &stmt {
                mutated_varids.insert(*vid);
            }
            stmt
        },
    );
    log::trace!("varid usages: {:#?}", varid_counts);
    // Then, we delete all the let bindings that are used insufficiently often and inline them instead.
    input.structural_map(
        &mut |expr| match expr {
            UnrolledExpr::Let(mut bindings, stmt, expr) => {
                // Remove any bindings that aren't actually used
                bindings.retain(|(vid, _)|
                    varid_counts.get(vid).copied().unwrap_or_default()
                    + mutated_varids.get(vid).copied().unwrap_or_default() as usize > 0);
                if bindings.is_empty() && stmt.is_empty() {
                    *expr
                } else {
                    UnrolledExpr::Let(bindings, stmt, expr)
                }
            }
            UnrolledExpr::Var(vid) => {
                if varid_counts
                    .get(&vid)
                    .map(|cnt| *cnt <= INLINE_THRESHOLD)
                    .unwrap_or_default()
                    && !mutated_varids.contains(&vid)
                {
                    varid_bindings
                        .get(&vid)
                        .cloned()
                        .unwrap_or(UnrolledExpr::Var(vid))
                } else {
                    UnrolledExpr::Var(vid)
                }
            }
            expr => expr,
        },
        &mut |stmt| stmt,
    )
}

// fn visit_varids_expr(expr: &UnrolledExpr, visit: &mut impl FnMut(VarId, usize)) {
//     match expr {
//         UnrolledExpr::Let(bindings, statements, expr) => {
//             for (varid, _) in bindings.iter() {
//                 visit(*varid, 1);
//             }
//             statements
//                 .iter()
//                 .for_each(|stmt| visit_varids_stmt(stmt, visit));
//             visit_varids_expr(expr, visit);
//         }
//         UnrolledExpr::If(c, x, y) => {
//             visit_varids_expr(c, visit);
//             visit_varids_expr(x, visit);
//             visit_varids_expr(y, visit);
//         }
//         UnrolledExpr::Value(_) => {}
//         UnrolledExpr::BuiltIn(builtin) => {
//             for expr in builtin.arguments() {
//                 visit_varids_expr(expr, visit)
//             }
//         }
//         UnrolledExpr::Var(varid) => visit(*varid, 1),
//         UnrolledExpr::Hash(_, v) => visit_varids_expr(v, visit),
//         UnrolledExpr::Sigeok(_, x, y, z) => {
//             visit_varids_expr(x, visit);
//             visit_varids_expr(y, visit);
//             visit_varids_expr(z, visit)
//         }
//     }
// }

// fn visit_varids_stmt(stmt: &UnrolledStatement, visit: &mut impl FnMut(VarId, usize)) {
//     match stmt {
//         UnrolledStatement::SetLet(bindings, stmt) => {
//             for (varid, _) in bindings {
//                 visit(*varid, 1)
//             }
//             for stmt in stmt {
//                 visit_varids_stmt(stmt, visit)
//             }
//         }
//         UnrolledStatement::Loop(_) => todo!(),
//         UnrolledStatement::If(_, _, _) => todo!(),
//         UnrolledStatement::Set(_, _) => todo!(),
//         UnrolledStatement::Noop => todo!(),
//     }
// }
