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

Compilation results are vomitted onto stdout.
```
Ast
----
([], Hash(10, Value(Bytes([104, 101, 108, 108, 111, 119, 111, 114, 108, 100]))))

Expanded
-----
Ok(Hash(10, Value(Bytes([104, 101, 108, 108, 111, 119, 111, 114, 108, 100]))))

MelVM
-----
Hash(10, Value(Bytes([104, 101, 108, 108, 111, 119, 111, 114, 108, 100])))

Binary: 0xf00a68656c6c6f776f726c6430000a

Disassembly:
[PUSHB([104, 101, 108, 108, 111, 119, 111, 114, 108, 100]), HASH(10)]

Successful execution.

Final stack
--------
[Bytes([123, 178, 5, 36, 77, 128, 131, 86, 49, 142, 198, 93, 10, 229, 79, 50, 238, 58, 123, 171, 93, 250, 244, 49, 176, 30, 86, 126, 3, 186, 171, 79])]
```
This shows the program structure at each step of compilation: AST, Expanded-AST, MelVM ops, and binary. "Disassembly" is the ops as interpreted by MelVM from the compiled binary. "Final stack" is the final state of the MelVM stack after execution of the script. More on this in execution tests.

Stdout wont always look this verbose. But in these early days of the mil compiler, it may give insight into what the program is doing under the hood.

The binary can also be written to a file
```
mil examples/hellohash.mil --output hh.mvm
```

### Execution tests
The mil compiler also provides an environment for executing and debugging programs.
As of now, the only behaviour is to execute the script on an empty transaction. This is run automatically after compiling succeeds.
