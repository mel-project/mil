use crate::compiler::BinCode;
use tmelcrypt::ed25519_keygen;
use blkstructs::{
    Transaction,
    melvm::{Value, Executor, Covenant, OpCode}};

pub type ProgramCounter = usize;
//pub fn execute_on_tx(tx: &Transaction) -> 

pub struct ExecutionEnv<'a> {
    /// A stack and heap environment.
    pub executor: Executor,
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

    pub fn iter_mut(&'a mut self) -> DbgExecutor<'a> {
        DbgExecutor {
            executor: &mut self.executor,
            ops: self.ops,
            pc: 0,
        }
    }
}

pub struct DbgExecutor<'a> {
    /// A stack and heap environment.
    pub executor: &'a mut Executor,
    /// Program instructions to execute.
    ops: &'a [OpCode],
    /// Tracks the next instruction to be executed.
    pc: ProgramCounter,
}

/*
impl<'a> DbgExecutor<'a> {
    pub fn new(tx: &'a Transaction, ops: &'a [OpCode]) -> DbgExecutor<'a> {
        // Pre-load the tx onto the heap
        let mut heap = std::collections::HashMap::new();
        heap.insert(0, Value::from_tx(&tx));
        heap.insert(1, Value::from_bytes(&tx.hash_nosigs()));

        DbgExecutor {
            executor: Executor::new(heap),
            ops,
            pc: 0,
        }
    }
}
*/

impl<'a> Iterator for DbgExecutor<'a> {
    type Item = ProgramCounter;

    fn next(&mut self) -> Option<Self::Item> {
        self.executor.do_op(self.ops.get(self.pc)?, self.pc as u32)
            .map(|x| x as usize)
    }
}
impl<'a> Iterator for DbgExecutor<'a> {
    type Item = ProgramCounter;

    fn next(&mut self) -> Option<Self::Item> {
        self.executor.do_op(self.ops.get(self.pc)?, self.pc as u32)
            .map(|x| x as usize)
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
