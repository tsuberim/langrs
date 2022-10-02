use std::{
    collections::HashMap,
    io::{Lines, StdinLock, Write},
};

use tree_sitter::Parser;
use tree_sitter_fun::language;
use types::{Scheme, Type};
use value::{Closure, Value};
use anyhow::{Result, anyhow, bail, Context};

use crate::{
    term::parse,
    types::{generalize, Infer},
    value::evaluate,
};

mod term;
mod types;
mod value;

fn repl(
    lines: &mut Lines<StdinLock>,
    env: &mut HashMap<String, (Value, Scheme)>,
) -> Result<String> {
    print!("fun> ");
    std::io::stdout().flush()?;
    let line = lines.next().unwrap()?;
    if line.is_empty() {
        return Ok("".into());
    }

    let term = parse(&line)?;
    println!("{}", &term);

    let type_env = env
        .iter()
        .map(|(k, (_, t))| (k.clone(), t.clone()))
        .collect();
    let (_, t) = Infer::new().infer(&term, &type_env).context("Type error")?;
    let scheme = generalize(&t);

    let value_env = env
        .iter()
        .map(|(k, (v, _))| (k.clone(), v.clone()))
        .collect();
    let value = evaluate(&term, &value_env)?;

    Ok(format!("{} : {}", value, scheme))
}

fn add(args: Vec<Value>, _env: HashMap<String, Value>) -> Result<Value> {
    let lhs = args.get(0).context("Expected lhs")?;
    let rhs = args.get(1).context("Expected rhs")?;

    if args.len() > 2 {
        bail!("More than 2 args");
    }

    match (lhs, rhs) {
        (Value::Num(x), Value::Num(y)) => Ok(Value::Num(x + y)),
        _ => Err(anyhow!("Type mismatch")),
    }
}

pub fn main() {
    let mut lines = std::io::stdin().lines();

    let mut parser = Parser::new();
    parser
        .set_language(language())
        .expect("Must be able to set parser");

    let mut env = HashMap::new();
    env.insert(
        String::from("add"),
        (
            Value::Clo(Closure {
                captures: HashMap::new(),
                args: vec!["lhs".into(), "rhs".into()],
                closure: value::ClosureImpl::Builtin(add),
            }),
            generalize(&Type::App(
                Box::new(Type::Con("Fun".into())),
                vec![
                    Type::Con("Num".into()),
                    Type::Con("Num".into()),
                    Type::Con("Num".into()),
                ],
            )),
        ),
    );

    loop {
        match repl(&mut lines, &mut env) {
            Ok(result) => println!("{}", result),
            Err(err) => println!("{:#?}", err),
        }
    }
}
