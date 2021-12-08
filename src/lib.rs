//! This is the mil compiler; a low-level language for the Mel VM.

/// User-facing command line interface to the compiler.
pub mod cmdline;
/// Compiles the lowest-level representation, [MelExpr]s, into binary.
pub mod compiler;
/// Execute a set of opcodes in a MelVM environment.
pub mod executor;
/// Parse strings into low-level syntax trees which can be compiled to binary.
pub mod parser;
/// Project level types such as the abstract syntax tree and intermediate representations.
pub mod types;

/// Non-user-facing, optimization functions
mod optimize;

// Declare storage used by packrat_parser
nom_packrat::storage!(types::Expr);
