use std::io::{self, Write};
use im::HashMap;
use anyhow::{Result, Ok};
use tree_sitter::Parser;
use tree_sitter_fun::language;

use crate::{term::to_ast, typing::{Infer, generalize, ForAll}, value::eval};

pub fn repl() -> Result<()> {
    let mut parser = Parser::new();
    parser.set_language(language())?;

    fn step(parser: &mut Parser) -> Result<()> {
        print!("fun> ");
        io::stdout().flush()?;
        let mut line = String::new();
        io::stdin().read_line(&mut line)?;
        
        let src = line;
        let ast = parser.parse(&src, Option::None).ok_or(anyhow::format_err!("could not parse"))?;

        let term = to_ast(ast.root_node(), src.as_str())?;

        let mut infer = Infer::new();
        let t = infer.infer(&term, &HashMap::new())?;
        let t: ForAll = generalize(t);

        let val = eval(&term, &HashMap::new())?;

        println!("{} : {}", val, t);

        Ok(())
    }

    loop {   
        if let Err(err) = step(&mut parser) {
            println!("Error: {}", err);
            io::stdout().flush()?;
        }
    }
}
