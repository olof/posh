mod builtin;
mod config;
mod engine;
mod error;
mod history;
mod input;
mod path;
mod repl;

pub use crate::engine::Engine;
pub use crate::error::{Error, Result};

fn main() {
    let mut repl = repl::Repl::new();

    if let Err(e) = repl.run() {
        eprintln!("{e}");
        std::process::exit(1);
    }
}
