use std::collections::HashMap;
use crate::compiler::BinCode;
use tmelcrypt::{ed25519_keygen, HashVal};
use serde::Deserialize;
use blkstructs::{
    Header, Transaction, CoinDataHeight, CoinID, CoinData,
    melvm::{self, Value, Executor, Covenant, OpCode}};
use genawaiter::{yield_, rc::gen};

/// Points to current instruction of a program in an [ExecutionEnv].
pub type ProgramCounter = usize;
/// A view into an execution environment.
pub type EnvView = (Stack, Heap, ProgramCounter);
type Stack = Vec<Value>;
type Heap  = HashMap<u16, Value>;

/// The execution environment of a covenant.
/// Matches the CovenantEnv struct of blkstructs package.
/// However, fields here are owned not borrowed.
#[derive(Debug, Deserialize)]
pub struct CovEnv {
    pub parent_coinid: CoinID,
    pub parent_cdh: CoinDataHeight,
    pub spender_index: u8,
    pub last_header: Header,
}

impl<'a> From<&'a CovEnv> for melvm::CovenantEnv<'a> {
    fn from(e: &'a CovEnv) -> Self {
        melvm::CovenantEnv {
            parent_coinid: &e.parent_coinid,
            parent_cdh: &e.parent_cdh,
            spender_index: e.spender_index,
            last_header: &e.last_header,
        }
    }
}

/// A wrapper of the MelVM Executor, associated with a list of opcodes to execute.
pub struct ExecutionEnv<'a> {
    /// A stack and heap environment.
    executor: Executor,
    /// Program instructions to execute.
    ops: &'a [OpCode],
}

impl<'a> ExecutionEnv<'a> {
    pub fn new(
        spending_tx: Transaction,
        cov_env: CovEnv,
        ops: &'a [OpCode]) -> ExecutionEnv<'a>
    {
        ExecutionEnv {
            executor: Executor::new_from_env(
                          spending_tx,
                          Some(melvm::CovenantEnv::from(&cov_env))),
            ops,
        }
    }

    pub fn view(&self, pc: ProgramCounter) -> EnvView {
        (self.executor.stack.clone(), self.executor.heap.clone(), pc)
    }

    pub fn into_iter(mut self) -> impl Iterator<Item=Option<EnvView>> + 'a {
        let mut pc = 0;
        gen!({
            while let Some(op_next) = self.ops.get(pc) {
                match self.executor.do_op(op_next, pc as u32) {
                    Some(pc_new) => {
                        pc = pc_new as usize;
                        // Successful ongoing execution
                        yield_!(Some(self.view(pc)))
                    },
                    // Failed execution
                    None => yield_!(None),
                }
            }
        })
        .into_iter()
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
pub fn execute(env: ExecutionEnv) -> Option<(Stack, Heap, ProgramCounter)> {
    let mut final_state = (vec![], HashMap::new(), 0);
    for x in env.into_iter() {
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
        let tx       = Transaction::empty_test().signed_ed25519(sk);
        (pk, sk, tx)
    }

    fn exec(tx: &Transaction, input: &[u8], ops: MelExpr) -> (Stack, Heap, ProgramCounter) {
        let bin = compile(ops);
        let dis = disassemble(bin).expect("Failed to disassemble");
        let empty_ci = CoinID {
            txhash: tmelcrypt::HashVal::default(),
            index: 0,
        };
        let empty_cdh = CoinDataHeight {
            coin_data: CoinData {
                covhash: tmelcrypt::HashVal::default(),
                value: 0,
                denom: vec![],
                additional_data: input.into(),
            },
            height: 0,
        };

        let cov_env = CovEnv {
            parent_coinid: empty_ci,
            parent_cdh: empty_cdh,
            spender_index: 0,
            last_header:
                Header {
                    network: blkstructs::NetID::Testnet,
                    previous: HashVal::default(),
                    height: 0,
                    history_hash: HashVal::default(),
                    coins_hash: HashVal::default(),
                    transactions_hash: HashVal::default(),
                    fee_pool: 0,
                    fee_multiplier: 0,
                    dosc_speed: 0,
                    pools_hash: HashVal::default(),
                    stakes_hash: HashVal::default(),
                },
        };

        execute(ExecutionEnv::new(tx.clone(), cov_env, &dis))
            .expect(&format!("Failed to execute: {:?}", dis))
    }

    #[test]
    fn add_numbers() {
        let ops   = parse("(+ 1 2)").unwrap();
        let (pk, sk, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(3))]);
    }

    #[test]
    fn test_eql() {
        let ops   = parse("(= 1 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(1))]);
    }

    #[test]
    fn test_not_eql() {
        let ops   = parse("(= (+ 2 2) 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(0))]);
    }

    #[test]
    fn fn_application() {
        let ops   = parse("(fn f (x y) (* x y)) (f 2 3)").unwrap();
        let (pk, sk, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(6))]);
    }

    #[test]
    fn set_value() {
        let ops   = parse("(let (x 1) (set! x 2) x)").unwrap();
        let (pk, sk, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(2))]);
    }

    #[test]
    fn push_nil_1() {
        let ops   = parse("(v-push v-nil 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Vector(vector![Value::Int(U256::one())])]);
    }

    #[test]
    fn concat_vectors() {
        let ops   = parse("(v-concat (v-cons 2 v-nil) (v-cons 1 v-nil))").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Vector(
                vector![Value::Int(U256::from(2)), Value::Int(U256::one())])]);
    }

    #[test]
    fn ref_vector() {
        let ops   = parse("(v-get (v-concat (v-cons 2 v-nil) (v-cons 1 v-nil)) 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(1))]);
    }

    #[test]
    fn two_gt_one() {
        let ops   = parse("(> 2 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(1))]);
    }

    #[test]
    fn one_lt_two() {
        let ops   = parse("(< 1 2)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(1))]);
    }

    #[test]
    fn shift_left() {
        let ops   = parse("(<< 2 3)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(16))]);
    }

    #[test]
    fn bitwise_and() {
        let ops   = parse("(and 3 2)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(2))]);
    }

    #[test]
    fn bitwise_or() {
        let ops   = parse("(or 1 2)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(3))]);
    }

    #[test]
    fn btoi_1() {
        let ops   = parse("(bytes->u256 0x0000000000000000000000000000000000000000000000000000000000000001)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(1))]);
    }

    #[test]
    fn btoi_max() {
        let ops   = parse("(bytes->u256 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::max_value())]);
    }

    #[test]
    fn itob_1() {
        // TODO: Can't yet parse this
        //let ops = parse(&format!("(itob {})", U256::max_value())).unwrap();
        let ops = parse(&format!("(u256->bytes {})", U256::one())).unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Bytes(vector![1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])]);
    }

    #[test]
    fn nested_lets() {
        let ops   = parse("(let (x 3) (let (y 2) (* x y)))").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(6))]);
    }

    #[test]
    fn if_true_branch() {
        let ops   = parse("(if (and 1 1) (* 2 2) 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(4))]);
    }

    #[test]
    fn if_false_branch() {
        let ops   = parse("(if 0 (* 2 2) 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(1))]);
    }

    #[test]
    fn from_bytes() {
        let ops   = parse("(b-from (b-cons 1 b-nil) 0 2)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Bytes(vector![2])]);
    }

    #[test]
    fn vset_vector() {
        let ops   = parse("(v-from (v-cons 1 v-nil) 0 2)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Vector(vector![Value::Int(U256::from(2))])]);
    }

    #[test]
    fn set_bytes() {
        let ops   = parse("(b-from 0x00 0 2)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Bytes(vector![2])]);
    }

    #[test]
    fn inlined_comments() {
        let ops   = parse("
        ; aghhh
        ; and a second

        (if ; this is just
            0 ;for tests
            ;everywhere
            (* 2 ;they're everywhere!
            2); dont mind me
            1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(1))]);
    }

    #[test]
    fn empty_string_is_empty_bytes() {
        let ops   = parse("(let (x \"\") x)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Bytes(vector![])]);
    }

    #[test]
    fn rot_from_shifts() {
        let ops = parse("\
            (fn rot (b n) \
                (or (<< b n) (>> b (- 256 n)))) \
            (rot (bytes->u256 0x0100000000000000000000000000000000000000000000000000000000000000) 8)\
        ").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::from(1))]);
    }

    #[test]
    fn init_vec_native() {
        let ops   = parse("(let (v [1 2 3]) v)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Vector(vector![
            Value::Int(U256::from(1)),
            Value::Int(U256::from(2)),
            Value::Int(U256::from(3))])]);
    }

    #[test]
    fn loop_add_expr_4_times() {
        let ops   = parse("(loop 4 (+ 1 2))").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

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
        let mut state = exec(&tx, &[], ops);

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
                    (v-get (v-get SPENDER-TX 6) 0)
                    0x{}
                    SPENDER-TX-HASH)", hex::encode(&pk.0))).unwrap();

        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::one())]);
    }

    /*
    #[test]
    fn fn_no_capture() {
        let ops = parse("\
            (fn f (x) y)\
            (let (y 1) (f 2))\
            ").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

    }
    */
}
