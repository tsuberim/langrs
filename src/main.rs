mod typing;
mod term;
mod value;
mod repl;

use anyhow::{Result, Ok};
use crate::{repl::repl};

fn main() -> Result<()> {
    repl()?;

    Ok(())
}
