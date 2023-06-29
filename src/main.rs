mod typing;
mod term;
mod value;
mod cli;
mod utils;

use std::{fs, hash::Hash, rc::Rc};

use anyhow::{Result, Ok, format_err, bail};
use im::{HashMap, hashmap};
use tree_sitter::Parser;
use tree_sitter_fun::language;
use crate::{cli::repl, term::{to_ast, Lit}, value::{eval, Value, ValueEnv}, typing::{infer, ForAll, Type, TypeEnv}, utils::Namer};

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

fn add(args: Vec<Rc<Value>>) -> Result<Rc<Value>> {
    if args.len() != 2 {
        bail!("add invoked with args length different than 2")
    }

    let x = args.get(0).unwrap();
    let y = args.get(1).unwrap();

    if let (Value::Lit(Lit::Num(x)), Value::Lit(Lit::Num(y))) = (x.as_ref(),y.as_ref()) {
        Ok(Rc::new(Value::Lit(Lit::Num(*x + *y))))
    } else {
        bail!("add invoked with non-nums")
    }
}

fn run(file: &String) -> Result<()> {
    let mut parser = Parser::new();
    parser.set_language(language())?;

    let src = fs::read_to_string(file)?;
    let src = format!("({})", src);

    let num = Type::Cons("Num".to_string(), vec![]);
    let context: HashMap<String, (Rc<Value>, ForAll)> = hashmap! {
        "add".to_string() => (
            Rc::new(Value::Builtin("add", add)), 
            ForAll(vec![], Type::Cons("Fun".to_string(), vec![num.clone(), num.clone(), num.clone()]))
        )
    };

    let val_env: ValueEnv = context.iter().map(|(k, (v, t))| (k.clone(), Rc::clone(v))).collect();
    let type_env: TypeEnv = context.iter().map(|(k, (v, t))| (k.clone(), t.clone())).collect();

    let tree = parser.parse(&src, None).ok_or(format_err!("could not parse {}", file))?;

    let term = to_ast(tree.root_node(), &src)?;

    let t = infer(&term, &type_env)?;

    let val = eval(&term, &val_env)?;
    
    println!("{} : {}", val, t);

    Ok(())
}
