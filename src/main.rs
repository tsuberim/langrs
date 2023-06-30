mod typing;
mod term;
mod value;
mod cli;
mod utils;

use std::{fs, rc::Rc, vec, ops::Add};

use anyhow::{Result, Ok, format_err, bail};
use im::{HashMap, hashmap};

use term::Lit;
use tree_sitter::Parser;
use tree_sitter_fun::language;
use value::apply;
use crate::{cli::repl, term::{to_ast}, value::{eval, Value, ValueEnv}, typing::{infer, ForAll, Type, TypeEnv}};

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

fn add(_env: &ValueEnv, args: &Vec<Rc<Value>>) -> Result<Rc<Value>>{
    let x = args.get(0).unwrap();
    let y = args.get(1).unwrap();

    if let (Value::Lit(Lit::Num(x)), Value::Lit(Lit::Num(y))) = (x.as_ref(),y.as_ref()) {
        Ok(Rc::new(Value::Lit(Lit::Num(*x + *y))))
    } else {
        bail!("")
    }
}

fn concat(_env: &ValueEnv, args: &Vec<Rc<Value>>) -> Result<Rc<Value>>{
    let x = args.get(0).unwrap();
    let y = args.get(1).unwrap();

    if let (Value::Lit(Lit::Str(x)), Value::Lit(Lit::Str(y))) = (x.as_ref(),y.as_ref()) {
        let sum = x.clone() + y;
        Ok(Rc::new(Value::Lit(Lit::Str(sum))))
    } else {
        bail!("")
    }
}

fn map(env: &ValueEnv, args: &Vec<Rc<Value>>) -> Result<Rc<Value>>{
    let f = args.get(0).unwrap();
    let list = args.get(1).unwrap();

    if let Value::List(items) = list.as_ref() {
        let mapped: Vec<Rc<Value>> = items
            .iter()
            .map(|val| apply(env, Rc::clone(f), &vec![Rc::clone(val)]))
            .collect::<Result<Vec<Rc<Value>>>>()?;
        Ok(Rc::new(Value::List(mapped)))
    } else {
        bail!("map invoked with non-list")
    }
}

fn fold(env: &ValueEnv, args: &Vec<Rc<Value>>) -> Result<Rc<Value>>{
    let init = args.get(0).unwrap();
    let acc_f = args.get(1).unwrap();
    let list = args.get(2).unwrap();

    if let Value::List(items) = list.as_ref() {
        let folded =items.iter()
            .fold(
                Ok(Rc::clone(init)), 
                |acc, val| -> Result<Rc<Value>> {
                    let acc = acc?;
                    let x = apply(env, Rc::clone(acc_f), &vec![acc, Rc::clone(val)])?;
                    Ok(x)
                }
            )?;

        Ok(folded)
    } else {
        bail!("fold invoked with non-list")
    }
}

fn run(file: &String) -> Result<()> {
    let mut parser = Parser::new();
    parser.set_language(language())?;

    let src = fs::read_to_string(file)?;
    let src = format!("({})", src);

    let num = Type::Cons("Num".to_string(), vec![]);
    let str = Type::Cons("Str".to_string(), vec![]);
    let context: HashMap<String, (Rc<Value>, ForAll)> = hashmap! {
        "+".to_string() => (
            Rc::new(Value::Builtin("+", add)),
            ForAll(vec![], Type::Cons("Fun".to_string(), vec![num.clone(), num.clone(), num.clone()]))
        ),
        "++".to_string() => (
            Rc::new(Value::Builtin("++", concat)),
            ForAll(vec![], Type::Cons("Fun".to_string(), vec![str.clone(), str.clone(), str.clone()]))
        ),
        "map".to_string() => (
            Rc::new(Value::Builtin("map", map)),
            ForAll(vec!["a".to_string(), "b".to_string()], Type::Cons("Fun".to_string(), vec![
                Type::Cons("Fun".to_string(), vec![Type::Var("a".to_string()), Type::Var("b".to_string())]),
                Type::Cons("List".to_string(), vec![Type::Var("a".to_string())]),
                Type::Cons("List".to_string(), vec![Type::Var("b".to_string())])
            ]))
        ),
        "fold".to_string() => (
            Rc::new(Value::Builtin("fold", fold)),
            ForAll(vec!["a".to_string(), "b".to_string()], Type::Cons("Fun".to_string(), vec![
                Type::Var("b".to_string()),
                Type::Cons("Fun".to_string(), vec![Type::Var("b".to_string()), Type::Var("a".to_string()), Type::Var("b".to_string())]),
                Type::Cons("List".to_string(), vec![Type::Var("a".to_string())]),
                Type::Cons("List".to_string(), vec![Type::Var("b".to_string())])
            ]))
        ),
    };

    let val_env: ValueEnv = context.iter().map(|(k, (v, _t))| (k.clone(), Rc::clone(v))).collect();
    let type_env: TypeEnv = context.iter().map(|(k, (_v, t))| (k.clone(), t.clone())).collect();

    let tree = parser.parse(&src, None).ok_or(format_err!("could not parse {}", file))?;

    let term = to_ast(tree.root_node(), &src)?;

    let t = infer(&term, &type_env)?;

    let val = eval(&term, &val_env)?;
    
    println!("{} : {}", val, t);

    Ok(())
}
