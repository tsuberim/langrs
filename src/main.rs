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
use crate::{cli::repl, term::{to_ast}, value::{eval, Value, ValueEnv}, typing::{infer, ForAll, Type, TypeEnv}, utils::Namer};

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

fn print_fn(env: &ValueEnv, args: &Vec<Rc<Value>>) -> Result<Rc<Value>>{
    let str = args.get(0).unwrap();
    if let Value::Lit(Lit::Str(str)) = str.as_ref() {
        let str = str.clone();
        Ok(Rc::new(Value::Task(Rc::new(move || -> Result<Rc<Value>> {
            println!("{}", str);
            Ok(Rc::new(Value::Record(hashmap!{})))
        }))))
    } else {
        bail!("_a")
    }
    
}

fn bind(_: &ValueEnv, args: &Vec<Rc<Value>>) -> Result<Rc<Value>>{
    let task = args.get(0).unwrap();
    let f = Rc::clone(args.get(1).unwrap());

    if let Value::Task(task) = task.as_ref() {
        let task = Rc::clone(task);
        Ok(Rc::new(Value::Task(Rc::new(move || {
            let val = task()?;
            let val = apply(&hashmap!{}, f.clone(), &vec![val])?;
            if let Value::Task(task) = val.as_ref() {
                task()
            } else {
                bail!("asdf")
            }
        }))))
    } else {
        bail!("bind invoked with non-task")
    }
}

fn ret(_: &ValueEnv, args: &Vec<Rc<Value>>) -> Result<Rc<Value>>{
    let val = Rc::clone(args.get(0).unwrap());

    Ok(Rc::new(Value::Task(Rc::new(move || {
        Ok(val.clone())
    }))))
}

fn fail(_: &ValueEnv, args: &Vec<Rc<Value>>) -> Result<Rc<Value>>{
    let val = Rc::clone(args.get(0).unwrap());

    Ok(Rc::new(Value::Task(Rc::new(move || {
        bail!("{}", val.clone())
    }))))
}

fn run(file: &String) -> Result<()> {
    let mut parser = Parser::new();
    parser.set_language(language())?;

    let src = fs::read_to_string(file)?;
    let src = format!("({})", src);

    let num = Type::Cons("Num".to_string(), vec![]);
    let str = Type::Cons("Str".to_string(), vec![]);

    let unit = Type::Record { items: hashmap! {}, union: false, rest: None };
    
    let context: HashMap<String, (Rc<Value>, ForAll)> = hashmap! {
        "+".to_string() => (
            Rc::new(Value::Builtin("+", Box::new(add))),
            ForAll(vec![], Type::Cons("Fun".to_string(), vec![num.clone(), num.clone(), num.clone()]))
        ),
        "++".to_string() => (
            Rc::new(Value::Builtin("++", Box::new(concat))),
            ForAll(vec![], Type::Cons("Fun".to_string(), vec![str.clone(), str.clone(), str.clone()]))
        ),
        "map".to_string() => (
            Rc::new(Value::Builtin("map", Box::new(map))),
            ForAll(vec!["_a".to_string(), "_b".to_string()], Type::Cons("Fun".to_string(), vec![
                Type::Cons("Fun".to_string(), vec![Type::Var("_a".to_string()), Type::Var("_b".to_string())]),
                Type::Cons("List".to_string(), vec![Type::Var("_a".to_string())]),
                Type::Cons("List".to_string(), vec![Type::Var("_b".to_string())])
            ]))
        ),
        "fold".to_string() => (
            Rc::new(Value::Builtin("fold", Box::new(fold))),
            ForAll(vec!["_a".to_string(), "_b".to_string()], Type::Cons("Fun".to_string(), vec![
                Type::Var("_b".to_string()),
                Type::Cons("Fun".to_string(), vec![Type::Var("_b".to_string()), Type::Var("_a".to_string()), Type::Var("_b".to_string())]),
                Type::Cons("List".to_string(), vec![Type::Var("_a".to_string())]),
                Type::Cons("List".to_string(), vec![Type::Var("_b".to_string())])
            ]))
        ),
        "print".to_string() => (
            Rc::new(Value::Builtin("print", Box::new(print_fn))),
            ForAll(vec![], Type::Cons("Fun".to_string(), vec![
                str.clone(),
                Type::Cons("Task".to_string(), vec![unit.clone(), Type::Record { items: hashmap! {}, union: true, rest: Some("void_t".to_string()) }])
            ]))
        ),
        "$".to_string() => (
            Rc::new(Value::Builtin("$", Box::new(bind))),
            ForAll(vec!["_a".to_string(),"_b".to_string(),"c".to_string()], Type::Cons("Fun".to_string(), vec![
                Type::Cons("Task".to_string(), vec![Type::Var("_a".to_string()), Type::Var("_b".to_string())]),
                Type::Cons("Fun".to_string(), vec![
                    Type::Var("_a".to_string()), 
                    Type::Cons("Task".to_string(), vec![Type::Var("c".to_string()), Type::Var("_b".to_string())])
                ]),
                Type::Cons("Task".to_string(), vec![Type::Var("c".to_string()), Type::Var("_b".to_string())]),
            ]))
        ),
        "return".to_string() => (
            Rc::new(Value::Builtin("return", Box::new(ret))),
            ForAll(vec!["_a".to_string(), "_b".to_string()], Type::Cons("Fun".to_string(), vec![
                Type::Var("_a".to_string()),
                Type::Cons("Task".to_string(), vec![Type::Var("_a".to_string()), Type::Var("_b".to_string())]),
            ]))
        ),
        "fail".to_string() => (
            Rc::new(Value::Builtin("fail", Box::new(fail))),
            ForAll(vec!["_a".to_string(), "_b".to_string()], Type::Cons("Fun".to_string(), vec![
                Type::Var("_a".to_string()),
                Type::Cons("Task".to_string(), vec![Type::Var("_b".to_string()), Type::Var("_a".to_string())]),
            ]))
        ),
    };

    let val_env: ValueEnv = context.iter().map(|(k, (v, _t))| (k.clone(), Rc::clone(v))).collect();
    let type_env: TypeEnv = context.iter().map(|(k, (_v, t))| (k.clone(), t.clone())).collect();

    let tree = parser.parse(&src, None).ok_or(format_err!("could not parse {}", file))?;

    let term = to_ast(tree.root_node(), &src)?;

    let mut t = infer(&term, &type_env)?;

    let mut val = eval(&term, &val_env)?;

    match (val.as_ref(), t.clone()) {
        (Value::Task(f), ForAll(vars, Type::Cons(cons, args))) if vars.len() == 0 && cons == "Task" => {
            val = f()?;
            let ty = args.get(0).unwrap();
            t = ForAll(vec![],ty.clone());
        }
        _ => {}
    }
    
    println!("{} : {}", val, t);

    Ok(())
}
