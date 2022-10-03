use std::{
    collections::HashMap,
    io::{Lines, StdinLock, Write},
};

use anyhow::{anyhow, bail, Context, Result};
use colored::Colorize;
use tree_sitter::Parser;
use tree_sitter_fun::language;
use types::{Scheme, Type};
use value::{Closure, Value};

use crate::{
    evm::{run, Compiler},
    term::{arb_term, parse, Term},
    types::{generalize, Infer},
    value::evaluate,
};

pub mod evm;
pub mod term;
pub mod types;
pub mod value;

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

    let mut compiler = Compiler::new();
    compiler.compile(&term);
    let smart_contract = compiler.result();

    let gas = run(smart_contract)?;

    Ok(format!("{} : {}   ({} gas)", value, scheme, gas))
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
            Err(err) => println!("{:?}", err),
        }
    }
}

use proptest::prelude::*;

fn type_check(term: &Term) -> Result<Type> {
    Infer::new().infer(&term, &HashMap::new()).map(|(_, t)| t)
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(3000))]

    #[test]
    fn test_display_inverse_parse(term in arb_term()) {
        let str = term.to_string();
        let s = String::from_utf8(strip_ansi_escapes::strip(str).unwrap()).unwrap();
        println!("{}", s);
        if let Ok(parsed) = parse(s.as_str()) {
            assert_eq!(term, parsed);
        } else {
            panic!("Could not parse term")
        }
    }

    #[test]
    fn test_typechecks_implies_evaluates(term in arb_term()) {
        if let Ok(t) = type_check(&term) {
            evaluate(&term, &HashMap::new()).expect("Should evaluate");
        }
    }
}
