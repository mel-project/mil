## Mel Intermediate Lisp
A simple Lisp to write low-level programs for the MelVM.

### Compiling a Mil program
Provide the path to a *.mil file to compile.
```
mil examples/hellohash.mil
```

### Execution tests
The mil compiler also provides an environment for executing and debugging programs.
As of now, the only behaviour is to execute the script on an empty transaction. This is run automatically after compiling succeeds.

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
