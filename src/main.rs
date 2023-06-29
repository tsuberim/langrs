mod typing;
mod term;
mod value;
mod repl;

use std::{env::args, fs, hash::Hash};

use anyhow::{Result, Ok, format_err};
use im::HashMap;
use tree_sitter::Parser;
use tree_sitter_fun::language;
use crate::{repl::repl, term::to_ast, value::eval, typing::Infer};

use clap::{Parser as ClapParser, command};

/// Simple program to greet a person
#[derive(ClapParser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    file: Option<String>
}


fn main() -> Result<()> {
    let args = Args::parse();

    if let Some(file) = args.file {
        run(&file)?
    } else {
        repl()?;
    }
    

    Ok(())
}

fn run(file: &String) -> Result<()> {
    let mut parser = Parser::new();
    parser.set_language(language())?;

    let src = fs::read_to_string(file)?;

    let tree = parser.parse(&src, None).ok_or(format_err!("could not parse {}", file))?;

    let term = to_ast(tree.root_node(), &src)?;

    let mut infer = Infer::new();
    let t = infer.infer(&term, &HashMap::new())?;

    let val = eval(&term, &HashMap::new())?;
    
    println!("{} : {}", val, t);

    Ok(())
}
