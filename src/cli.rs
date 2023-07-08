use std::{io::{self, Write}, process};
use im::HashMap;
use anyhow::{Result, Ok};
use ropey::Rope;
use rustyline::DefaultEditor;
use tree_sitter::Parser;
use tree_sitter_fun::language;

use crate::{term::to_ast, typing::infer, value::eval, builtins::{get_context, get_type_env, get_value_env}};


pub fn repl() -> Result<()> {
    let mut parser = Parser::new();
    let lang = language();
    parser.set_language(lang)?;

    let mut rl = DefaultEditor::new()?;

    let mut step = || -> Result<()> {
        let src = rl.readline("fun> ")?;

        let ast = parser.parse(&src, Option::None).ok_or(anyhow::format_err!("could not parse"))?;

        let src = src.as_str();
        let src = Rope::from_str(src);

        let context = get_context();
        let type_env = get_type_env(&context);
        let val_env = get_value_env(&context);

        let term = to_ast(ast.root_node(), &src, &mut HashMap::new());

        let (t, _errs) = infer(&term, &type_env, &mut HashMap::new());

        let val = eval(&term, &val_env)?;

        println!("{} : {}", val, t);

        rl.add_history_entry(src.to_string())?;

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

