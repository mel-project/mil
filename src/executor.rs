use std::collections::HashMap;
use crate::compiler::BinCode;
use tmelcrypt::ed25519_keygen;
use blkstructs::{
    Transaction,
    melvm::{Value, Executor, Covenant, OpCode}};

pub type ProgramCounter = usize;
type Stack = Vec<Value>;
type Heap  = HashMap<u16, Value>;

pub struct ExecutionEnv<'a> {
    /// A stack and heap environment.
    executor: Executor,
    /// Program instructions to execute.
    ops: &'a [OpCode],
}

impl<'a> ExecutionEnv<'a> {
    /*
    pub fn from_bin(tx: &'a Transaction, bin: BinCode) -> Option<ExecutionEnv<'a>> {
        let cov_hash = &tmelcrypt::hash_single(&bin.0);
        // Disassemble binary
        let ops = disassemble(bin)?;
        Some( ExecutionEnv::new(tx, ops, cov_hash) )
    }
    */

    pub fn new(tx: &Transaction, ops: &'a [OpCode], cov_hash: &[u8]) -> ExecutionEnv<'a> {
        // Pre-load the tx onto the heap
        let mut heap = std::collections::HashMap::new();
        heap.insert(0, Value::from(tx.clone()));
        heap.insert(1, Value::from_bytes(&tx.hash_nosigs()));
        println!("cov hash: {:?}", &tmelcrypt::hash_single(cov_hash));
        heap.insert(2, Value::from_bytes(&tmelcrypt::hash_single(cov_hash)));

        ExecutionEnv {
            executor: Executor::new(heap),
            ops,
        }
    }

    pub fn view(&self) -> (Stack, Heap) {
        (self.executor.stack.clone(), self.executor.heap.clone())
    }
}

impl<'a,'b> IntoIterator for &'b mut ExecutionEnv<'a> {
    type Item = Option<(Stack, Heap, ProgramCounter)>;
    type IntoIter = ExecutorIter<'a,'b>;

    fn into_iter(self) -> ExecutorIter<'a,'b> {
        ExecutorIter {
            env: self,
            pc: 0,
        }
    }
}

pub struct ExecutorIter<'a,'b> {
    env: &'b mut ExecutionEnv<'a>,
    /// Tracks the next instruction to be executed.
    pc: ProgramCounter,
}

/// Iterate over program instructions in an [ExecutionEnv], returning an optional
/// view into the environment state each time. If the inner optional is none, the
/// program failed execution. If the outer optional is none, execution finished
/// successfully. If the inner optional is not checked, this iterator may be
/// non-terminating.
impl<'a,'b> Iterator for ExecutorIter<'a,'b> {
    type Item = Option<(Stack, Heap, ProgramCounter)>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.env.ops.get(self.pc) {
            Some(op_next) => match self.env.executor.do_op(op_next, self.pc as u32) {
                Some(pc_new) => {
                    self.pc = pc_new as usize;
                    let view = self.env.view();
                    Some( Some((view.0, view.1, self.pc)) )
                },
                None => Some(None),
            },
            None => None,
        }
    }
}

/// Disassemble a binary code using the MelVM disassembler.
pub fn disassemble(bin: BinCode) -> Option<Vec<OpCode>> {
    // Wrap in a covenant
    let script = Covenant(bin.0.clone());
    // Disassemble compiled binary
    script.to_ops()
}

/// Execute the given environment to completion or failure.
pub fn execute(mut env: ExecutionEnv) -> Option<(Stack, Heap, ProgramCounter)> {
    let mut final_state = (vec![], HashMap::new(), 0);
    let e = &mut env;
    for x in e.into_iter() {
        match x {
            None => return None,
            Some(state) => final_state = state,
        }
    };

    Some(final_state)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    use crate::types::MelExpr;
    use crate::compiler::{Compile, BinCode};
    use primitive_types::U256;
    use tmelcrypt::{Ed25519PK, Ed25519SK};
    use im::vector;

    fn compile(ops: MelExpr) -> BinCode {
        // Compile to binary
        let empty = BinCode(Vec::new());
        ops.compile_onto(empty)
    }

    fn key_and_empty_tx() -> (Ed25519PK, Ed25519SK, Transaction) {
        let (pk, sk) = ed25519_keygen();
        let tx       = Transaction::empty_test().sign_ed25519(sk);
        (pk, sk, tx)
    }

    fn exec(tx: &Transaction, ops: MelExpr) -> (Stack, Heap, ProgramCounter) {
        let bin = compile(ops);
        let cov_hash = tmelcrypt::hash_single(&bin.0);
        let dis = disassemble(bin).expect("Failed to disassemble");
        execute(ExecutionEnv::new(&tx, &dis, &cov_hash))
            .expect(&format!("Failed to execute: {:?}", dis))
    }

    #[test]
    fn add_numbers() {
        let ops   = parse("(+ 1 2)").unwrap();
        let (pk, sk, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(3))]);
    }

    #[test]
    fn test_eql() {
        let ops   = parse("(= 1 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(1))]);
    }

    #[test]
    fn test_not_eql() {
        let ops   = parse("(= (+ 2 2) 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(0))]);
    }

    #[test]
    fn fn_application() {
        let ops   = parse("(fn f (x y) (* x y)) (f 2 3)").unwrap();
        let (pk, sk, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(6))]);
    }

    #[test]
    fn set_value() {
        let ops   = parse("(let (x 1) (set! x 2) x)").unwrap();
        let (pk, sk, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(2))]);
    }

    #[test]
    fn cons_nil_1() {
        let ops   = parse("(cons 1 nil)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Vector(vector![Value::Int(U256::one())])]);
    }

    #[test]
    fn concat_vectors() {
        let ops   = parse("(concat (cons 2 nil) (cons 1 nil))").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Vector(
                vector![Value::Int(U256::one()), Value::Int(U256::from(2))])]);
    }

    #[test]
    fn ref_vector() {
        let ops   = parse("(get 1 (concat (cons 2 nil) (cons 1 nil)))").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(2))]);
    }

    #[test]
    fn two_gt_one() {
        let ops   = parse("(> 2 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(1))]);
    }

    #[test]
    fn one_lt_two() {
        let ops   = parse("(< 1 2)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(1))]);
    }

    #[test]
    fn btoi_1() {
        let ops   = parse("(btoi 0x0000000000000000000000000000000000000000000000000000000000000001)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(1))]);
    }

    #[test]
    fn btoi_max() {
        let ops   = parse("(btoi 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Int(U256::max_value())]);
    }

    #[test]
    fn itob_1() {
        // TODO: Can't yet parse this
        //let ops = parse(&format!("(itob {})", U256::max_value())).unwrap();
        let ops = parse(&format!("(itob {})", U256::one())).unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Bytes(vector![1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])]);
    }

    #[test]
    fn nested_lets() {
        let ops   = parse("(let (x 3) (let (y 2) (* x y)))").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(6))]);
    }

    #[test]
    fn if_true_branch() {
        let ops   = parse("(if (and 1 1) (* 2 2) 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(4))]);
    }

    #[test]
    fn if_false_branch() {
        let ops   = parse("(if 0 (* 2 2) 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(1))]);
    }

    #[test]
    fn vfrom_bytes() {
        let ops   = parse("(vfrom 2 0 (cons 1 bnil))").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Bytes(vector![2])]);
    }

    #[test]
    fn vset_vector() {
        let ops   = parse("(vfrom 2 0 (cons 1 nil))").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Vector(vector![Value::Int(U256::from(2))])]);
    }

    #[test]
    fn vset_bytes() {
        let ops   = parse("(vfrom 2 0 0x00)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Bytes(vector![2])]);
    }

    #[test]
    fn inlined_comments() {
        let ops   = parse("
        (if ; this is just
            0 ;for tests
            (* 2 2); dont mind me
            1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(1))]);
    }

    #[test]
    fn empty_string_is_empty_bytes() {
        let ops   = parse("(let (x \"\") x)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Bytes(vector![])]);
    }

    #[test]
    fn init_vec_native() {
        let ops   = parse("(let (v [1 2 3]) v)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Vector(vector![
            Value::Int(U256::from(1)),
            Value::Int(U256::from(2)),
            Value::Int(U256::from(3))])]);
    }

    #[test]
    fn loop_add_expr_4_times() {
        let ops   = parse("(loop 4 (+ 1 2))").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, ops);

        assert_eq!(
            state.0,
            vec![Value::Int(U256::from(3)),
                 Value::Int(U256::from(3)),
                 Value::Int(U256::from(3)),
                 Value::Int(U256::from(3))]);
    }

    #[test]
    fn hash_bytes() {
        let ops   = parse("(hash 1 0xF0)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let mut state = exec(&tx, ops);

        if let blkstructs::melvm::Value::Bytes(im_bytes) = state.0.pop().unwrap() {
            assert_eq!(im_bytes.into_iter().collect::<Vec<u8>>(), vec![
                    233, 131, 224, 169, 229, 83, 12, 43, 119, 20, 230,
                    120, 233, 61, 188, 129, 150, 148, 124, 190, 111, 195,
                    63, 163, 212, 106, 36, 240, 111, 251, 98, 193]);
        } else {
            panic!();
        }
    }

    #[test]
    fn sigeok_bytes() {
        let (pk, _, tx) = key_and_empty_tx();
        let ops   = parse(&format!("
                (sigeok 32
                    (get 0 (get 6 SpenderTx))
                    0x{}
                    SpenderTxHash)", hex::encode(&pk.0))).unwrap();

        let state = exec(&tx, ops);

        assert_eq!(state.0, vec![Value::Int(U256::one())]);
    }
}
