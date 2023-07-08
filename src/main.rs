mod typing;
mod term;
mod value;
mod cli;
mod utils;
mod lsp;
mod builtins;
mod module;
mod doc;

use std::{fs, vec};

use anyhow::{Result, Ok, format_err};
use cli::repl;
use im::{HashMap};

use lsp::Backend;
use ropey::Rope;

use tree_sitter::Parser;
use tree_sitter_fun::language;

use crate::{term::to_ast, value::{eval, Value}, typing::{infer, ForAll, Type}, builtins::{get_context, get_value_env, get_type_env}};

use tower_lsp::{LspService, Server};
use clap::{Parser as ClapParser, command};

/// Simple program to greet a person
#[derive(ClapParser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    file: Option<String>,
    #[arg(short, long)]
    stdio: bool
}

#[tokio::main]
async fn main() {
    let args = Args::parse();

    if args.stdio {   
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();

        let (service, socket) = LspService::new(|client| Backend::new(client).unwrap());
        Server::new(stdin, stdout, socket).concurrency_level(1).serve(service).await;
    } else if let Some(file) = args.file {
        run(&file).unwrap()
    } else {
        repl().unwrap()
    }
}


fn run(file: &String) -> Result<()> {
    let mut parser = Parser::new();
    parser.set_language(language())?;

    let src = fs::read_to_string(file)?;
    let src = format!("({})", src); 

    let context = get_context();
    let type_env = get_type_env(&context);
    let val_env = get_value_env(&context);

    let tree = parser.parse(&src, None).ok_or(format_err!("could not parse {}", file))?;

    let rope = Rope::from_str(&src);
    let term = to_ast(tree.root_node(), &rope, &mut HashMap::new());

    let (mut t, _errs) = infer(&term, &type_env, &mut HashMap::new());

    let mut val = eval(&term, &val_env)?;

    match (val.as_ref(), t.clone()) {
        (Value::Task(f), ForAll(vars, Type::Cons(cons, args))) if cons == "Task" => {
            val = f()?;
            let ty = args.get(0).unwrap();
            t = ForAll(vec![],ty.clone());
        }
        _ => {}
    }
    
    println!("{} : {}", val, t);

    Ok(())
}
