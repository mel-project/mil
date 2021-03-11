use blkstructs::{
    transaction::Transaction,
    melvm::Covenant};

fn execute(bincode: &BinCode) -> bool {
    // Wrap in a covenant
    let script = Covenant(bincode.0);

    // Dummy spender transaction calls the covenant
    let (pk, sk) = tmelcrypt::ed25519_keygen();
    let tx = Transaction::empty_test().sign_ed25519(sk);

    script.check(&tx)
}
