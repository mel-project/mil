use std::collections::HashMap;
use crate::types::{UnrolledStatement, Value, ExpandedBuiltIn, UnrolledExpr, MelExpr, VarId, HeapPos};

pub struct MemoryMap {
    memory_store: HashMap<VarId, HeapPos>,
}

impl MemoryMap {
    pub fn new() -> Self {
        let mut hm = HashMap::new();
        // Reserved mappings provided in MelVM
        for i in 0..crate::parser::NUM_RESERVED {
            hm.insert(i as i32, i as u16);
        }

        MemoryMap { memory_store: hm }
    }

    // Abstraction for repetition
    fn binop<F>(&mut self, e1: UnrolledExpr, e2: UnrolledExpr, op: F)
    -> ExpandedBuiltIn<MelExpr>
        where F: Fn(MelExpr, MelExpr) -> ExpandedBuiltIn<MelExpr>
    {
        let mel_e1 = self.to_mel_expr(e1);
        let mel_e2 = self.to_mel_expr(e2);
        op(mel_e1, mel_e2)
    }

    pub fn stmnt_to_mel_expr(&mut self, stmnt: UnrolledStatement) -> MelExpr {
        match stmnt {
            UnrolledStatement::Set(var_id, body) => {
                let mel_body = self.to_mel_expr(*body);
                let loc = self.memory_store.get(&var_id)
                              .expect("Failed to access variable id, there's a bug somewhere.");

                // Evaluate the body, then store the result in memory at `loc`
                MelExpr::Seq(vec![
                    mel_body,
                    MelExpr::BuiltIn(Box::new(ExpandedBuiltIn::Store(loc.clone())))])
            },
            UnrolledStatement::Loop(n, stmnt) => MelExpr::Loop(n, Box::new(self.stmnt_to_mel_expr(*stmnt))),
            UnrolledStatement::If(pred, on_true, on_false) => {
                let mel_true = self.stmnt_to_mel_expr(*on_true);
                let mel_false = self.stmnt_to_mel_expr(*on_false);

                MelExpr::Seq(vec![
                    self.to_mel_expr(*pred),
                    MelExpr::BuiltIn(Box::new(ExpandedBuiltIn::Bez((count_insts(&mel_true) + 1) as u16))),
                    mel_true,
                    MelExpr::BuiltIn(Box::new(ExpandedBuiltIn::Jmp(count_insts(&mel_false) as u16))),
                    mel_false,
                ])
            },
            UnrolledStatement::SetLet(binds, stmnts) => {
                // For each binding, evaluate the expression (to push onto stack) and store in a new
                // memory location.
                // TODO: What happens when the binding expression is a 'set!'?
                let mut mel_binds = vec![];
                binds.into_iter().for_each(|(var_id, expr)| {
                    // Make sure the variable is not somehow already there
                    self.memory_store.get(&var_id)
                        .map(|_| panic!("Variable id in let binding should not already be defined, this is a bug."));

                    // Assign the variable a memory location
                    // TODO: For simplicity, just converting the id into an address. This
                    // should probably be decoupled though.
                    let loc = var_id as HeapPos;
                    self.memory_store.insert(var_id, loc);

                    // Translate expr into mel instructions
                    let mel_expr = self.to_mel_expr(expr);

                    // Evaluate the expression,
                    // then store whatever is popped from the stack at 'loc'
                    mel_binds.push(mel_expr);
                    mel_binds.push( MelExpr::BuiltIn(Box::new(ExpandedBuiltIn::Store(loc))) );
                });

                // Finally, evaluate the body
                let mel_stmnts = stmnts.into_iter().map(|stm| self.stmnt_to_mel_expr(stm));
                mel_binds.extend(mel_stmnts);

                MelExpr::Seq( mel_binds )
            },
        }
    }

    /// Translate an [UnrolledExpr] into a set of low-level [MelExpr] instructions.
    pub fn to_mel_expr(&mut self, expr: UnrolledExpr) -> MelExpr {
        match expr {
            UnrolledExpr::Value(v) => match v {
                Value::Int(n) => MelExpr::Value(Value::Int(n)),
                Value::Bytes(b) => MelExpr::Value(Value::Bytes(b)),
            }
            // A variable by itself is the value of its location in memory
            UnrolledExpr::Var(ref v) => {
                let loc = self.memory_store.get(v)
                              .expect("Expected to find a mapping for variable.");
                MelExpr::BuiltIn(Box::new(ExpandedBuiltIn::Load(*loc)))
            },
            UnrolledExpr::BuiltIn(b) => {
                let mel_b = match *b {
                    ExpandedBuiltIn::Vempty => ExpandedBuiltIn::<MelExpr>::Vempty,
                    ExpandedBuiltIn::Bempty => ExpandedBuiltIn::<MelExpr>::Bempty,
                    ExpandedBuiltIn::Bez(n) => ExpandedBuiltIn::<MelExpr>::Bez(n),
                    ExpandedBuiltIn::Bnz(n) => ExpandedBuiltIn::<MelExpr>::Bnz(n),
                    ExpandedBuiltIn::Jmp(n) => ExpandedBuiltIn::<MelExpr>::Jmp(n),
                    ExpandedBuiltIn::Load(p) => ExpandedBuiltIn::<MelExpr>::Load(p),
                    ExpandedBuiltIn::Store(p) => ExpandedBuiltIn::<MelExpr>::Store(p),
                    ExpandedBuiltIn::Not(e) => ExpandedBuiltIn::<MelExpr>::Not(self.to_mel_expr(e)),
                    ExpandedBuiltIn::Vlen(e) => ExpandedBuiltIn::<MelExpr>::Vlen(self.to_mel_expr(e)),
                    ExpandedBuiltIn::Blen(e) => ExpandedBuiltIn::<MelExpr>::Blen(self.to_mel_expr(e)),
                    ExpandedBuiltIn::BtoI(e) => ExpandedBuiltIn::<MelExpr>::BtoI(self.to_mel_expr(e)),
                    ExpandedBuiltIn::ItoB(e) => ExpandedBuiltIn::<MelExpr>::ItoB(self.to_mel_expr(e)),
                    ExpandedBuiltIn::Add(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Add),
                    ExpandedBuiltIn::Sub(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Sub),
                    ExpandedBuiltIn::Mul(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Mul),
                    ExpandedBuiltIn::Div(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Div),
                    ExpandedBuiltIn::Rem(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Rem),
                    ExpandedBuiltIn::Eql(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Eql),
                    ExpandedBuiltIn::Lt(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Lt),
                    ExpandedBuiltIn::Gt(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Gt),
                    ExpandedBuiltIn::And(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::And),
                    ExpandedBuiltIn::Or(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Or),
                    ExpandedBuiltIn::Xor(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Xor),
                    ExpandedBuiltIn::Shl(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Shl),
                    ExpandedBuiltIn::Shr(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Shr),
                    ExpandedBuiltIn::Vref(e1,e2)  => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Vref),
                    ExpandedBuiltIn::Vpush(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Vpush),
                    ExpandedBuiltIn::Vcons(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Vcons),
                    ExpandedBuiltIn::Vappend(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Vappend),
                    ExpandedBuiltIn::Vslice(e1,e2,e3) =>
                        ExpandedBuiltIn::<MelExpr>::Vslice(self.to_mel_expr(e1),
                                                           self.to_mel_expr(e2),
                                                           self.to_mel_expr(e3)),
                    ExpandedBuiltIn::Vset(e1,e2,e3) =>
                        ExpandedBuiltIn::<MelExpr>::Vset(self.to_mel_expr(e1),
                                                         self.to_mel_expr(e2),
                                                         self.to_mel_expr(e3)),
                    ExpandedBuiltIn::Bref(e1,e2)  => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Bref),
                    ExpandedBuiltIn::Bpush(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Bpush),
                    ExpandedBuiltIn::Bcons(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Bcons),
                    ExpandedBuiltIn::Bappend(e1,e2) => self.binop(e1,e2, ExpandedBuiltIn::<MelExpr>::Bappend),
                    ExpandedBuiltIn::Bslice(e1,e2,e3) =>
                        ExpandedBuiltIn::<MelExpr>::Bslice(self.to_mel_expr(e1),
                                                           self.to_mel_expr(e2),
                                                           self.to_mel_expr(e3)),
                    ExpandedBuiltIn::Bset(e1,e2,e3) =>
                        ExpandedBuiltIn::<MelExpr>::Bset(self.to_mel_expr(e1),
                                                         self.to_mel_expr(e2),
                                                         self.to_mel_expr(e3)),
                };

                MelExpr::BuiltIn(Box::new(mel_b))
            },
            UnrolledExpr::If(pred, on_true, on_false) => {
                let mel_true = self.to_mel_expr(*on_true);
                let mel_false = self.to_mel_expr(*on_false);

                MelExpr::Seq(vec![
                    self.to_mel_expr(*pred),
                    MelExpr::BuiltIn(Box::new(ExpandedBuiltIn::Bez((count_insts(&mel_true) + 1) as u16))),
                    mel_true,
                    MelExpr::BuiltIn(Box::new(ExpandedBuiltIn::Jmp(count_insts(&mel_false) as u16))),
                    mel_false,
                ])
            },
            UnrolledExpr::Let(binds, stmnts, expr) => {
                // For each binding, evaluate the expression (to push onto stack) and store in a new
                // memory location.
                // TODO: What happens when the binding expression is a 'set!'?
                let mut mel_binds = vec![];
                binds.into_iter().for_each(|(var_id, expr)| {
                    // Make sure the variable is not somehow already there
                    self.memory_store.get(&var_id)
                        .map(|_| panic!("Variable id in let binding should not already be defined, this is a bug."));

                    // Assign the variable a memory location
                    // TODO: For simplicity, just converting the id into an address. This
                    // should probably be decoupled though.
                    let loc = var_id as HeapPos;
                    self.memory_store.insert(var_id, loc);

                    // Translate expr into mel instructions
                    let mel_expr = self.to_mel_expr(expr);

                    // Evaluate the expression,
                    // then store whatever is popped from the stack at 'loc'
                    mel_binds.push(mel_expr);
                    mel_binds.push( MelExpr::BuiltIn(Box::new(ExpandedBuiltIn::Store(loc))) );
                });

                // Finally, evaluate the body
                let mel_stmnts = stmnts.into_iter().map(|stm| self.stmnt_to_mel_expr(stm));
                mel_binds.extend(mel_stmnts);

                let mel_expr = self.to_mel_expr(*expr);
                mel_binds.push(mel_expr);

                MelExpr::Seq( mel_binds )
            },
            UnrolledExpr::Hash(n, expr) => MelExpr::Hash(n, Box::new(self.to_mel_expr(*expr))),
            UnrolledExpr::Sigeok(n, e1,e2,e3) =>
                MelExpr::Sigeok(n, Box::new(self.to_mel_expr(*e1)),
                                   Box::new(self.to_mel_expr(*e2)),
                                   Box::new(self.to_mel_expr(*e3))),
        }
    }
}

/// Count the number of primitive instructions recursively from a [MelExpr].
/// Loop-embedded instructions aren't counted (a loop is 1 instruction).
pub fn count_insts(e: &MelExpr) -> u16 {
    match e {
        MelExpr::Seq(v) => v.iter().map(count_insts).reduce(|a,b| a+b).unwrap_or(0),
        // Loop-embedded instructions aren't counted in the VM.
        MelExpr::Loop(_,e) => 1 + count_insts(e),
        MelExpr::Hash(_,e) => 1 + count_insts(e),
        MelExpr::Sigeok(_,e1,e2,e3) => 1 + count_insts(e1) + count_insts(e2) + count_insts(e3),
        MelExpr::Value(val) => match val {
            Value::Int(_) => 1,
            Value::Bytes(_) => 1,
        },
        MelExpr::BuiltIn(b) => match &**b {
            ExpandedBuiltIn::Add(e1,e2) => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Sub(e1,e2) => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Mul(e1,e2) => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Div(e1,e2) => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Rem(e1,e2) => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Eql(e1,e2) => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Lt(e1,e2)  => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Gt(e1,e2)  => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::And(e1,e2) => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Or(e1,e2)  => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Xor(e1,e2) => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Shl(e1,e2) => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Shr(e1,e2) => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Not(e)      => 1 + count_insts(&e),
            ExpandedBuiltIn::BtoI(e)     => 1 + count_insts(&e),
            ExpandedBuiltIn::ItoB(e)     => 1 + count_insts(&e),
            ExpandedBuiltIn::Vempty         => 1,
            ExpandedBuiltIn::Bempty         => 1,
            ExpandedBuiltIn::Vref(e1,e2)    => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Vappend(e1,e2) => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Vlen(e)        => 1 + count_insts(&e),
            ExpandedBuiltIn::Vpush(e1,e2)   => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Vcons(e1,e2)   => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Vslice(e1,e2,e3) => 1 + count_insts(&e1) + count_insts(&e2) + count_insts(&e3),
            ExpandedBuiltIn::Vset(e1,e2,e3)   => 1 + count_insts(&e1) + count_insts(&e2) + count_insts(&e3),
            ExpandedBuiltIn::Bref(e1,e2)    => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Bappend(e1,e2) => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Blen(e)        => 1 + count_insts(&e),
            ExpandedBuiltIn::Bpush(e1,e2)   => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Bcons(e1,e2)   => 1 + count_insts(&e1) + count_insts(&e2),
            ExpandedBuiltIn::Bslice(e1,e2,e3) => 1 + count_insts(&e1) + count_insts(&e2) + count_insts(&e3),
            ExpandedBuiltIn::Bset(e1,e2,e3)   => 1 + count_insts(&e1) + count_insts(&e2) + count_insts(&e3),
            ExpandedBuiltIn::Jmp(_)     => 1,
            ExpandedBuiltIn::Bez(_)     => 1,
            ExpandedBuiltIn::Bnz(_)     => 1,
            ExpandedBuiltIn::Store(_) => 1,
            ExpandedBuiltIn::Load(_)  => 1,
        },
    }
}
