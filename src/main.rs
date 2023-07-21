mod builtins;
mod doc;
mod lsp;
mod module;
mod term;
mod typing;
mod utils;
mod value;
mod llvm;

use std::{
    fs,
    io::{self, Write},
    process, vec, sync::Arc,
};

use anyhow::{Ok, Result};
use colored::Colorize;
use inkwell::context::Context;
use lsp::Backend;
use rustyline::DefaultEditor;
use url::Url;

use crate::{
    builtins::{get_context, get_value_env},
    doc::Doc,
    typing::{ForAll, Type},
    value::{eval, Value}
};

use clap::{command, Parser as ClapParser};
use tower_lsp::{LspService, Server};

/// Simple program to greet a person
#[derive(ClapParser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    file: Option<String>,
    #[arg(short, long)]
    stdio: bool,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();

    if args.stdio {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();

        let (service, socket) = LspService::new(|client| Backend::new(client).unwrap());
        Server::new(stdin, stdout, socket)
            .concurrency_level(1)
            .serve(service)
            .await;
    } else if let Some(file) = args.file {
        run(&file)
    } else {
        repl().unwrap()
    }
}

fn print_value(url: Url, src: &str) -> Result<()> {
    let doc = Doc::new(url, &src).unwrap();

    for err in doc.parse_errors() {
        println!(
            "{}",
            doc.format_err(err.node_id, err.message.as_str())
        )
    }

    match doc.module() {
        None => {
            println!("Could not run due to parse errors")
        },
        Some(module) => {
            let context = Context::create();
            let mut main_mod = llvm::CodeGen::compile(&context, Arc::clone(&module.root_term()))?;
            println!("{}", main_mod.to_string());
            let main_function = main_mod.get_function("_main").unwrap();
            let execution = main_mod.create_jit_execution_engine(inkwell::OptimizationLevel::None).map_err(|err| anyhow::format_err!("JIT error: {}", err))?;
            
            let result = unsafe {execution.run_function(main_function, &[])};
            let result = result.as_int(true);

            println!("result = {:?}", result);

            let type_errors = module.type_errors();
            if type_errors.len() > 0 {
                for err in type_errors {
                    println!(
                        "{}",
                        doc.format_err(err.node_id, err.message.as_str())
                    )
                }

                return Err(anyhow::format_err!("TypeErrors"));
            }

            let context = get_context();
            let val_env = get_value_env(&context);

            let term = module.root_term();
            let mut t = module.root_type().clone();

            let mut val = eval(&term, &val_env)?;

            match (val.as_ref(), t.clone()) {
                (Value::Task(f), ForAll(_, Type::Cons(cons, args))) if cons == "Task" => {
                    val = f()?;
                    let ty = args.get(0).unwrap();
                    t = ForAll(vec![], ty.clone());
                }
                _ => {}
            }

            println!("{} : {}", val, t);
        }
    }

    Ok(())
}

fn run(file: &String) {
    let url = Url::parse(format!("file://{}", file).as_str()).unwrap();
    let src = fs::read_to_string(file).unwrap();

    let result = print_value(url, src.as_str());

    match result {
        Result::Ok(_) => {}
        Result::Err(err) => {
            println!("{}", err)
        }
    }
}

fn repl() -> Result<()> {
    let mut rl = DefaultEditor::new()?;

    let mut step = || -> Result<()> {
        let src = rl.readline("fun> ")?;
        rl.add_history_entry(src.to_string())?;
        let url = Url::parse("repl://inline").unwrap();
        print_value(url, src.as_str())?;

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
