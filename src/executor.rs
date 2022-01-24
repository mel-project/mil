use crate::compiler::BinCode;
use genawaiter::{rc::gen, yield_};
use serde::Deserialize;
use std::collections::HashMap;
use themelio_structs::{CoinDataHeight, CoinID, Header, Transaction};
use themelio_stf::melvm::{
    self,
    opcode::{DecodeError, OpCode},
    Covenant, Executor, Value,
};

/// Points to current instruction of a program in an [ExecutionEnv].
pub type ProgramCounter = usize;
/// A view into an execution environment.
pub type EnvView = (Stack, Heap, ProgramCounter);
type Stack = Vec<Value>;
type Heap = HashMap<u16, Value>;

/// The execution environment of a covenant.
/// Matches the CovenantEnv struct of themelio_stf package.
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
//pub struct ExecutionEnv<'a> {
pub struct ExecutionEnv {
    /// A stack and heap environment.
    executor: Executor,
    /// Program instructions to execute.
    //ops: &'a [OpCode],
    ops: Vec<OpCode>,
}

//impl<'a> ExecutionEnv<'a> {
impl ExecutionEnv {
    //pub fn new(spending_tx: Transaction, cov_env: CovEnv, ops: &'a [OpCode]) -> ExecutionEnv<'a> {
    pub fn new(spending_tx: Transaction, cov_env: CovEnv, ops: Vec<OpCode>) -> ExecutionEnv {
        ExecutionEnv {
            executor: Executor::new_from_env(
                ops.clone(),
                spending_tx,
                Some(melvm::CovenantEnv::from(&cov_env)),
            ),
            ops,
        }
    }

    pub fn view(&self, pc: ProgramCounter) -> EnvView {
        (self.executor.stack.clone(), self.executor.heap.clone(), pc)
    }

    //pub fn iterate(mut self) -> impl Iterator<Item = Option<EnvView>> + 'a {
    pub fn iterate(mut self) -> impl Iterator<Item = Option<EnvView>> {
        gen!({
            while self.executor.pc() < self.ops.len() {
                /*
                let next_op = self
                    .ops
                    .get(self.executor.pc())
                    .expect("ExecutionEnv iterator should never index an op out of range");
                */

                //match self.executor.step() {
                match self.executor.step() {
                    Some(_) => {
                        yield_!(Some(self.view(self.executor.pc())))
                    }
                    // Failed execution
                    None => yield_!(None),
                }
            }
        })
        .into_iter()
    }
}

/// Disassemble a binary code using the MelVM disassembler.
pub fn disassemble(bin: BinCode) -> Result<Vec<OpCode>, DecodeError> {
    // Wrap in a covenant
    let script = Covenant::from_ops(&bin.0).unwrap();
    log::debug!("disassembling covenant of weight {}", script.weight()?);
    // Disassemble compiled binary
    script.to_ops()
}

/// Execute the given environment to completion or failure.
pub fn execute(env: ExecutionEnv) -> Option<(Stack, Heap, ProgramCounter)> {
    let mut final_state = (vec![], HashMap::new(), 0);
    for x in env.iterate() {
        match x {
            None => return None,
            Some(state) => final_state = state,
        }
    }

    Some(final_state)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::{BinCode, Compile};
    use crate::parser::parse;
    use crate::types::MelExpr;
    use ethnum::U256;
    use themelio_structs::{Address, CoinData, TxHash, TxKind, Denom, NetID};
    use tmelcrypt::{ed25519_keygen, Ed25519PK, Ed25519SK, HashVal};

    fn empty_test() -> Transaction {
        Transaction {
            kind: TxKind::Normal,
            inputs: Vec::new(),
            outputs: Vec::new(),
            fee: 0.into(),
            covenants: Vec::new(),
            data: Vec::new(),
            sigs: Vec::new(),
        }
    }

    fn compile(ops: MelExpr) -> BinCode {
        // Compile to binary
        let empty = BinCode(Vec::new());
        ops.compile_onto(empty)
    }

    fn key_and_empty_tx() -> (Ed25519PK, Ed25519SK, Transaction) {
        let (pk, sk) = ed25519_keygen();
        let tx = empty_test().signed_ed25519(sk);
        (pk, sk, tx)
    }

    fn exec(tx: &Transaction, input: &[u8], ops: MelExpr) -> (Stack, Heap, ProgramCounter) {
        let bin = compile(ops);
        let dis = disassemble(bin).expect("Failed to disassemble");
        let empty_ci = CoinID {
            txhash: TxHash(tmelcrypt::HashVal::default()),
            index: 0,
        };
        let empty_cdh = CoinDataHeight {
            coin_data: CoinData {
                covhash: Address::coin_destroy(),
                value: 0.into(),
                denom: Denom::Mel,
                additional_data: input.into(),
            },
            height: 0.into(),
        };

        let cov_env = CovEnv {
            parent_coinid: empty_ci,
            parent_cdh: empty_cdh,
            spender_index: 0,
            last_header: Header {
                network: NetID::Testnet,
                previous: HashVal::default(),
                height: 0.into(),
                history_hash: HashVal::default(),
                coins_hash: HashVal::default(),
                transactions_hash: HashVal::default(),
                fee_pool: 0.into(),
                fee_multiplier: 0,
                dosc_speed: 0,
                pools_hash: HashVal::default(),
                stakes_hash: HashVal::default(),
            },
        };

        execute(ExecutionEnv::new(tx.clone(), cov_env, dis.clone()))
            .unwrap_or_else(|| panic!("Failed to execute: {:?}", dis))
    }

    #[test]
    fn add_numbers() {
        let ops = parse("(+ 1 2)").unwrap();
        let (pk, sk, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(3))]);
    }

    #[test]
    fn exp_numbers() {
        let ops = parse("(** 2 2 4)").unwrap();
        let (pk, sk, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(16))]);
    }

    #[test]
    fn test_eql() {
        let ops = parse("(= 1 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(1))]);
    }

    #[test]
    fn test_not_eql() {
        let ops = parse("(= (+ 2 2) 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(0))]);
    }

    #[test]
    fn fn_application() {
        let ops = parse("(fn f (x y) (* x y)) (f 2 3)").unwrap();
        let (pk, sk, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(6))]);
    }

    #[test]
    fn set_value() {
        let ops = parse("(let (x 1) (set! x 2) x)").unwrap();
        let (pk, sk, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(2))]);
    }

    #[test]
    fn push_nil_1() {
        let ops = parse("(v-push v-nil 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(
            state.0,
            vec![Value::Vector(vec![Value::Int(U256::new(1))].into())]
        );
    }

    #[test]
    fn concat_vectors() {
        let ops = parse("(v-concat (v-cons 2 v-nil) (v-cons 1 v-nil))").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(
            state.0,
            vec![Value::Vector(vec![
                Value::Int(U256::new(2)),
                Value::Int(U256::new(1))
            ].into())]
        );
    }

    #[test]
    fn ref_vector() {
        let ops = parse("(v-get (v-concat (v-cons 2 v-nil) (v-cons 1 v-nil)) 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(1))]);
    }

    #[test]
    fn two_gt_one() {
        let ops = parse("(> 2 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(1))]);
    }

    #[test]
    fn one_lt_two() {
        let ops = parse("(< 1 2)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(1))]);
    }

    #[test]
    fn shift_left() {
        let ops = parse("(<< 2 3)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(16))]);
    }

    #[test]
    fn bitwise_and() {
        let ops = parse("(and 3 2)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(2))]);
    }

    #[test]
    fn bitwise_or() {
        let ops = parse("(or 1 2)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(3))]);
    }

    #[test]
    fn btoi_1() {
        let ops = parse(
            "(bytes->u256 0x0000000000000000000000000000000000000000000000000000000000000001)",
        )
        .unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(1))]);
    }

    #[test]
    fn btoi_max() {
        let ops = parse(
            "(bytes->u256 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)",
        )
        .unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::MAX)]);
    }

    #[test]
    fn itob_1() {
        //let ops = parse(&format!("(itob {})", U256::MAX)).unwrap();
        let ops = parse(&format!("(u256->bytes {})", U256::new(1))).unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(
            state.0,
            vec![Value::Bytes(vec![
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 1
            ].into())]
        );
    }

    #[test]
    fn nested_lets() {
        let ops = parse("(let (x 3) (let (y 2) (* x y)))").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(6))]);
    }

    #[test]
    fn if_true_branch() {
        let ops = parse("(if (and 1 1) (* 2 2) 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(4))]);
    }

    #[test]
    fn if_false_branch() {
        let ops = parse("(if 0 (* 2 2) 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(1))]);
    }

    #[test]
    fn set_if_true_branch() {
        let ops = parse("(let (x 0) (set-if 1 (set! x 1) (set! x 2)) x)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(1))]);
    }

    #[test]
    fn set_if_false_branch() {
        let ops = parse("(let (x 0) (set-if 0 (set! x 1) (set! x 2)) x)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(2))]);
    }

    #[test]
    fn from_bytes() {
        let ops = parse("(b-from (b-cons 1 b-nil) 0 2)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Bytes(vec![2].into())]);
    }

    #[test]
    fn vset_vector() {
        let ops = parse("(v-from (v-cons 1 v-nil) 0 2)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(
            state.0,
            vec![Value::Vector(vec![Value::Int(U256::new(2))].into())]
        );
    }

    #[test]
    fn set_bytes() {
        let ops = parse("(b-from 0x00 0 2)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Bytes(vec![2].into())]);
    }

    #[test]
    fn inlined_comments() {
        let ops = parse(
            "
        ; aghhh
        ; and a second

        (if ; this is just
            0 ;for tests
            ;everywhere
            (* 2 ;they're everywhere!
            2); dont mind me
            1)",
        )
        .unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(1))]);
    }

    #[test]
    fn empty_string_is_empty_bytes() {
        let ops = parse("(let (x \"\") x)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Bytes(vec![].into())]);
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

        assert_eq!(state.0, vec![Value::Int(U256::new(1))]);
    }

    #[test]
    fn init_vec_native() {
        let ops = parse("(let (v (vector 1 2 3)) v)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(
            state.0,
            vec![Value::Vector(vec![
                Value::Int(U256::new(1)),
                Value::Int(U256::new(2)),
                Value::Int(U256::new(3))
            ].into())]
        );
    }

    #[test]
    fn loop_add_expr_4_times() {
        let ops = parse("(let (x 0) (loop 4 (set! x (+ 1 x))) x)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(4))]);
    }

    #[test]
    fn set_let() {
        let ops = parse("(let (x 0) (set-let () (set! x 2)) x)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(2))]);
    }

    #[test]
    fn typeof_int() {
        let ops = parse("(typeof 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(0))]);
    }

    #[test]
    fn typeof_vec() {
        let ops = parse("(typeof (vector 1 2))").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(2))]);
    }

    /*
    #[test]
    fn dup() {
        let ops = parse("(dup! 1)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![
            Value::Int(U256::new(1)),
            Value::Int(U256::new(1)),
        ]);
    }
    */

    #[test]
    fn noop() {
        let ops = parse("(let () (noop) 2)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(2))]);
    }

    #[test]
    fn loop_with_noop() {
        let ops = parse(
            "
        (let (i 0
          sum 0)

          (loop 10 (set-let ()
            (set-if (> i 5)
              (set! sum (+ sum i))
              (noop))
            (set! i (+ i 1))))

          sum)
        ",
        )
        .unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(30))]);
    }

    #[test]
    fn nested_loop() {
        let ops = parse("(let (x 0) (loop 3 (loop 2 (set! x (+ x 1)))) x)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(6))]);
    }

    #[test]
    fn hash_bytes() {
        let ops = parse("(hash 1 0xF0)").unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let mut state = exec(&tx, &[], ops);

        if let themelio_stf::melvm::Value::Bytes(im_bytes) = state.0.pop().unwrap() {
            assert_eq!(
                im_bytes,
                vec![
                    233, 131, 224, 169, 229, 83, 12, 43, 119, 20, 230, 120, 233, 61, 188, 129, 150,
                    148, 124, 190, 111, 195, 63, 163, 212, 106, 36, 240, 111, 251, 98, 193
                ].into()
            );
        } else {
            panic!();
        }
    }

    #[test]
    fn sigeok_bytes() {
        let (pk, _, tx) = key_and_empty_tx();
        let ops = parse(&format!(
            "
                (sigeok 32
                    (v-get (v-get SPENDER-TX 6) 0)
                    0x{}
                    SPENDER-TX-HASH)",
            hex::encode(&pk.0)
        ))
        .unwrap();

        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(1))]);
    }

    #[test]
    fn overflow() {
        let max = U256::MAX;
        let ops = crate::parser::parse_no_optimize(&format!("(let (x (+ {max} 10)) oflo)")).unwrap();
        let (_, _, tx) = key_and_empty_tx();
        let state = exec(&tx, &[], ops);

        assert_eq!(state.0, vec![Value::Int(U256::new(1))]);
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
