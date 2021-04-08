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
    pub fn new(tx: &'a Transaction, ops: &'a [OpCode]) -> ExecutionEnv<'a> {
        // Pre-load the tx onto the heap
        let mut heap = std::collections::HashMap::new();
        heap.insert(0, Value::from(tx));
        heap.insert(1, Value::from_bytes(&tx.hash_nosigs()));

        ExecutionEnv {
            executor: Executor::new(heap),
            ops,
        }
    }

    pub fn view(&self) -> (Stack, Heap) {
        (self.executor.stack.clone(), self.executor.heap.clone())
    }
}

impl<'a> IntoIterator for &'a mut ExecutionEnv<'a> {
    type Item = Option<(Stack, Heap)>;
    type IntoIter = ExecutorIter<'a>;

    fn into_iter(self) -> ExecutorIter<'a> {
        ExecutorIter {
            env: self,
            pc: 0,
        }
    }
}

pub struct ExecutorIter<'a> {
    env: &'a mut ExecutionEnv<'a>,
    /// Tracks the next instruction to be executed.
    pc: ProgramCounter,
}

/// Iterate over program instructions in an [ExecutionEnv], returning an optional
/// view into the environment state each time. If the inner optional is none, the
/// program failed execution. If the outer optional is none, execution finished
/// successfully.
impl<'a> Iterator for ExecutorIter<'a> {
    type Item = Option<(Stack, Heap)>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.env.ops.get(self.pc) {
            Some(op_next) => match self.env.executor.do_op(op_next, self.pc as u32) {
                Some(pc_new) => {
                    self.pc = pc_new as usize;
                    Some( Some(self.env.view()) )
                },
                None => Some(None),
            },
            None => None,
        }
    }
}

/*
pub fn execute(bincode: BinCode) -> bool {
    // Wrap in a covenant
    let script = Covenant(bincode.0);

    // Dummy spender transaction calls the covenant
    let (_, sk) = ed25519_keygen();
    let tx = Transaction::empty_test().sign_ed25519(sk);

    script.check(&tx)
}
*/

pub fn execute(bincode: BinCode) -> Result<(Stack, Heap), ()> {
    // Wrap in a covenant
    let script = Covenant(bincode.0.clone());

    // Disassemble compiled binary
    //if let Some(ops) = script.to_ops() {
    let ops = script.to_ops().ok_or(())?;

    // Dummy spender transaction calls the covenant
    let (pk, sk) = ed25519_keygen();
    let tx = Transaction::empty_test().sign_ed25519(sk);

    // Execute
    let mut env = ExecutionEnv::new(&tx, &ops);
    /*
    let final_state = env.into_iter()
        .take_while(|x| x.is_some())
        .map(|x| x.unwrap());
        //.last();
    */
    let mut final_state = (vec![], HashMap::new());
    for x in env.into_iter() {
        match x {
            None => return Err(()),
            Some(state) => final_state = state,
        }
    };

    Ok(final_state)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    use crate::types::MelExpr;
    use crate::compiler::{Compile, BinCode};
    use primitive_types::U256;

    fn compile(ops: MelExpr) -> BinCode {
        // Compile to binary
        let empty = BinCode(Vec::new());
        ops.compile_onto(empty)
    }

    #[test]
    fn add_numbers() {
        let ops   = parse("(+ 1 2)").unwrap();
        let state = execute( compile(ops) ).unwrap();

        assert_eq!(state.0, vec![Value::Int(U256::from(3))]);
    }

    #[test]
    fn set_value() {
        let ops   = parse("(let (x 1) (set! x 2) x)").unwrap();
        let state = execute( compile(ops) ).unwrap();

        assert_eq!(state.0, vec![Value::Int(U256::from(2))]);
    }

    #[test]
    fn nested_lets() {
        let ops   = parse("(let (x 3) (let (y 2) (* x y)))").unwrap();
        let state = execute( compile(ops) ).unwrap();

        assert_eq!(state.0, vec![Value::Int(U256::from(6))]);
    }

    #[test]
    fn if_true_branch() {
        let ops   = parse("(if (and 1 1) (* 2 2) 1)").unwrap();
        let state = execute( compile(ops) ).unwrap();

        assert_eq!(state.0, vec![Value::Int(U256::from(4))]);
    }

    #[test]
    fn if_false_branch() {
        let ops   = parse("(if 0 (* 2 2) 1)").unwrap();
        let state = execute( compile(ops) ).unwrap();

        assert_eq!(state.0, vec![Value::Int(U256::from(1))]);
    }

    /*
    #[test]
    fn empty_string_is_empty_bytes() {
        let ops   = parse("(let (x \"\") x)").unwrap();
        let state = execute( compile(ops) ).unwrap();

        assert_eq!(state.0, vec![Value::Bytes(vec![])]);
    }
    */

    #[test]
    fn loop_add_expr_4_times() {
        let ops   = parse("(loop 4 (+ 1 2))").unwrap();
        let state = execute( compile(ops) ).unwrap();

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
        let mut state = execute( compile(ops) ).unwrap();
        if let blkstructs::melvm::Value::Bytes(im_bytes) = state.0.pop().unwrap() {
            assert_eq!(im_bytes.into_iter().collect::<Vec<u8>>(), vec![
                    233, 131, 224, 169, 229, 83, 12, 43, 119, 20, 230,
                    120, 233, 61, 188, 129, 150, 148, 124, 190, 111, 195,
                    63, 163, 212, 106, 36, 240, 111, 251, 98, 193]);
        } else {
            panic!();
        }
    }

    /*
    #[test]
    fn sigeok_bytes() {
        let ops   = parse("(sigeok 32 0xF0)").unwrap();
        let state = execute( compile(ops) ).unwrap();

        assert_eq!(state.0, );
    }
    */
}
