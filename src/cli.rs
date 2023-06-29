use std::{io::{self, Write}, process};
use im::HashMap;
use anyhow::{Result, Ok};
use rustyline::DefaultEditor;
use tree_sitter::Parser;
use tree_sitter_fun::language;

use crate::{term::to_ast, typing::infer, value::eval};

use clap::{Parser as ClapParser, command};

#[derive(ClapParser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    file: Option<String>
}



pub fn repl() -> Result<()> {
    let mut parser = Parser::new();
    parser.set_language(language())?;

    let mut rl = DefaultEditor::new()?;

    let mut step = || -> Result<()> {
        let src = rl.readline("fun> ")?;

        let ast = parser.parse(&src, Option::None).ok_or(anyhow::format_err!("could not parse"))?;

        let term = to_ast(ast.root_node(), src.as_str())?;

        let t = infer(&term, &HashMap::new())?;

        let val = eval(&term, &HashMap::new())?;

        println!("{} : {}", val, t);

        rl.add_history_entry(src)?;

        Ok(())
    };

    loop {   
        if let Err(err) = step() {
            println!("Error: {}", err);
            io::stdout().flush()?;

            if err.to_string().contains("Interrupted") {
                process::exit(0)
            }
        }
    }
}

