use std::collections::HashMap;
use crate::compiler::BinCode;
use tmelcrypt::ed25519_keygen;
use blkstructs::{
    Transaction,
    melvm::{Value, Executor, Covenant, OpCode}};

pub type ProgramCounter = usize;
type Stack = Vec<Value>;
type Heap  = HashMap<u16, Value>;
//pub fn execute_on_tx(tx: &Transaction) -> 

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
        heap.insert(0, Value::from_tx(&tx));
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
    type Item = (Stack, Heap);
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

/// Iterate over program instructions in an [ExecutionEnv], returning a view into the environment state each time.
impl<'a> Iterator for ExecutorIter<'a> {
    type Item = (Stack, Heap);

    fn next(&mut self) -> Option<Self::Item> {
        self.pc = self.env.executor.do_op(self.env.ops.get(self.pc)?, self.pc as u32)? as usize;
        Some( self.env.view() )
    }
}

pub fn execute(bincode: BinCode) -> bool {
    // Wrap in a covenant
    let script = Covenant(bincode.0);

    // Dummy spender transaction calls the covenant
    let (pk, sk) = ed25519_keygen();
    let tx = Transaction::empty_test().sign_ed25519(sk);

    script.check(&tx)
}
