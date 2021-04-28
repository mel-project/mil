# Mel Intermediate Lisp
A simple Lisp to write low-level programs for the MelVM.

Some example programs can be seen [here](https://github.com/themeliolabs/mil/blob/1fc29af6cb272ce0b4bcc7192fc2a6418e4d2270/src/executor.rs#L147).

### Build
With stable Rust installed:
```bash
cargo build
```

### Use the Nix shell environment
```bash
# Reads the flake.nix/.lock files to get the specified rust version
nix develop
```

### Compiling a Mil program
Provide the path to a *.mil file to compile.
```
mil examples/hellohash.mil

# Alternatively
cargo run -- examples/hellohash.mil
```
By default, only the hash of the script is printed to stdout upon successful compilation.

To get the binary, tell the compiler to write it to a file.
```
mil examples/hellohash.mil --output hh.mvm
```

### Execution tests
The mil compiler also provides an environment for executing scripts on user-configured scenarios. In the MelVM, a script is associated with a UTXO, and executed only when a transaction attempts to spend it. Therefore, this test environment takes a json file consisting of a list of (UTXO, spender-transaction) values to execute. The json file specifically is of type `[(CoinID, CoinDataHeight, Transaction)]`. See the [mil binary search tree configuration](https://github.com/jaybutera/bst_mil/blob/master/txs.json) for an example.

To execute a script on a test tx file,
```bash
mil bst.mil --test-txs.json
```

The results of each tx pair is printed. Indicating whether execution succeeded, and displaying the final state of the MelVM stack, which if `Int(0)` indicates that the spend is not allowed, and any other value means it is.
```
tx#0 - Successful execution.

Final stack
--------
[Int(1)]

tx#1 - Successful execution.

Final stack
--------
[Int(0)]
```

### Debugging
To see the entire execution, one instruction at a time, and the evolution of the stack and heap, attach the `--debug` flag to compilation.

You can also disassemble a program after its been compiled to see its opcodes as interpreted by the MelVM. Use the `--show-disassembly` flag to get the disassembly on stdout.
