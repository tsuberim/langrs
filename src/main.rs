mod builtins;
mod doc;
mod lsp;
mod module;
mod term;
mod typing;
mod utils;
mod value;

use std::{fs, vec, process, io::{self, Write}};

use anyhow::{Ok, Result};
use colored::Colorize;
use lsp::Backend;
use rustyline::DefaultEditor;
use url::Url;

use crate::{
    builtins::{get_context, get_value_env},
    doc::Doc,
    typing::{ForAll, Type},
    value::{eval, Value},
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
    let src = format!("({})", src);

    let doc = Doc::new(url, &src).unwrap();

    let type_errors = doc.module().type_errors();
    if type_errors.len() > 0 {
        for err in type_errors {
            println!("{}", doc.format_err(err.node_id, err.message.as_str()).red())
        }

        return Err(anyhow::format_err!("TypeErrors"))
    }

    let context = get_context();
    let val_env = get_value_env(&context);

    let term = doc.module().root_term();
    let mut t = doc.module().root_type().clone();

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

    Ok(())
}

fn run(file: &String) {
    let url = Url::parse(format!("file://{}", file).as_str()).unwrap();
    let src = fs::read_to_string(file).unwrap();

    let result = print_value(url, src.as_str());

    match result {
        Result::Ok(_) => {},
        Result::Err(err) => {
            println!("{}", err)
        },
    }
}

fn repl() -> Result<()> {
    let mut rl = DefaultEditor::new()?;

    let mut step = || -> Result<()> {
        let src = rl.readline("fun> ")?;
        let url = Url::parse("repl://inline").unwrap();
        print_value(url, src.as_str())?;
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
