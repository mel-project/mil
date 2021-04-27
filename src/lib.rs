//! This is the mil compiler; a low-level language for the Mel VM.

/// Parse strings into low-level syntax trees which can be compiled to binary.
pub mod parser;
/// Project level types such as the abstract syntax tree and intermediate representations.
pub mod types;
/// Compiles the lowest-level representation, [MelExpr]s, into binary.
pub mod compiler;
/// Execute a set of opcodes in a MelVM environment.
pub mod executor;
/// User-facing command line interface to the compiler.
pub mod cmdline;
