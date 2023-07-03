mod typing;
mod term;
mod value;
mod cli;
mod utils;
mod lsp;

use std::{fs, rc::Rc, vec};

use anyhow::{Result, Ok, format_err, bail};
use im::{HashMap, hashmap};

use lsp::Backend;
use term::Lit;
use tree_sitter::Parser;
use tree_sitter_fun::language;
use value::apply;
use crate::{cli::repl, term::to_ast, value::{eval, Value, ValueEnv}, typing::{infer, ForAll, Type, TypeEnv, generalize}};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use clap::{Parser as ClapParser, command};

/// Simple program to greet a person
#[derive(ClapParser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    file: Option<String>
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend::new(client).unwrap());
    Server::new(stdin, stdout, socket).serve(service).await;
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

fn print_fn(_: &ValueEnv, args: &Vec<Rc<Value>>) -> Result<Rc<Value>>{
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

fn request(_: &ValueEnv, args: &Vec<Rc<Value>>) -> Result<Rc<Value>>{
    let url = args.get(0).unwrap();

    if let Value::Lit(Lit::Str(url)) = url.as_ref() {
        let url = url.clone();
        Ok(Rc::new(Value::Task(Rc::new(move || {
            let response = reqwest::blocking::get(url.clone())?;
            let text = response.text()?;

            let val = Value::Record(hashmap!{
                "body".to_string() => Rc::new(Value::Lit(Lit::Str(text)))
            });

            Ok(Rc::new(val))
        }))))    
    } else {
        bail!("request invoked with non-str")
    }
}

fn pipe(_: &ValueEnv, args: &Vec<Rc<Value>>) -> Result<Rc<Value>>{
    let url = args.get(0).unwrap();

    if let Value::Lit(Lit::Str(url)) = url.as_ref() {
        let url = url.clone();
        Ok(Rc::new(Value::Task(Rc::new(move || {
            let response = reqwest::blocking::get(url.clone())?;
            let text = response.text()?;

            let val = Value::Record(hashmap!{
                "body".to_string() => Rc::new(Value::Lit(Lit::Str(text)))
            });

            Ok(Rc::new(val))
        }))))    
    } else {
        bail!("request invoked with non-str")
    }
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
            generalize(Type::Cons("Fun".to_string(), vec![num.clone(), num.clone(), num.clone()]))
        ),
        "++".to_string() => (
            Rc::new(Value::Builtin("++", Box::new(concat))),
            generalize(Type::Cons("Fun".to_string(), vec![str.clone(), str.clone(), str.clone()]))
        ),
        "map".to_string() => (
            Rc::new(Value::Builtin("map", Box::new(map))),
            generalize(Type::Cons("Fun".to_string(), vec![
                Type::Cons("Fun".to_string(), vec![Type::Var("_a".to_string()), Type::Var("_b".to_string())]),
                Type::Cons("List".to_string(), vec![Type::Var("_a".to_string())]),
                Type::Cons("List".to_string(), vec![Type::Var("_b".to_string())])
            ]))
        ),
        "fold".to_string() => (
            Rc::new(Value::Builtin("fold", Box::new(fold))),
            generalize(Type::Cons("Fun".to_string(), vec![
                Type::Var("_b".to_string()),
                Type::Cons("Fun".to_string(), vec![Type::Var("_b".to_string()), Type::Var("_a".to_string()), Type::Var("_b".to_string())]),
                Type::Cons("List".to_string(), vec![Type::Var("_a".to_string())]),
                Type::Cons("List".to_string(), vec![Type::Var("_b".to_string())])
            ]))
        ),
        "print".to_string() => (
            Rc::new(Value::Builtin("print", Box::new(print_fn))),
            generalize(Type::Cons("Fun".to_string(), vec![
                str.clone(),
                Type::Cons("Task".to_string(), vec![unit.clone(), Type::Record { items: hashmap! {}, union: true, rest: Some("void_t".to_string()) }])
            ]))
        ),
        "$".to_string() => (
            Rc::new(Value::Builtin("$", Box::new(bind))),
            generalize(Type::Cons("Fun".to_string(), vec![
                Type::Cons("Task".to_string(), vec![Type::Var("_a".to_string()), Type::Var("_b".to_string())]),
                Type::Cons("Fun".to_string(), vec![
                    Type::Var("_a".to_string()), 
                    Type::Cons("Task".to_string(), vec![Type::Var("_c".to_string()), Type::Var("_b".to_string())])
                ]),
                Type::Cons("Task".to_string(), vec![Type::Var("_c".to_string()), Type::Var("_b".to_string())]),
            ]))
        ),
        "ok".to_string() => (
            Rc::new(Value::Builtin("ok", Box::new(ret))),
            generalize(Type::Cons("Fun".to_string(), vec![
                Type::Var("_a".to_string()),
                Type::Cons("Task".to_string(), vec![Type::Var("_a".to_string()), Type::Var("_b".to_string())]),
            ]))
        ),
        "err".to_string() => (
            Rc::new(Value::Builtin("err", Box::new(fail))),
            generalize(Type::Cons("Fun".to_string(), vec![
                Type::Var("_a".to_string()),
                Type::Cons("Task".to_string(), vec![Type::Var("_b".to_string()), Type::Var("_a".to_string())]),
            ]))
        ),
        "request".to_string() => (
            Rc::new(Value::Builtin("request", Box::new(request))),
            generalize(Type::Cons("Fun".to_string(), vec![
                Type::Cons("Str".to_string(), vec![]),
                Type::Cons("Task".to_string(), vec![
                    Type::Record { items: hashmap!{"body".to_string() => Type::Cons("Str".to_string(), vec![])}, union: false, rest: None },
                    Type::Record { items: hashmap!{}, union: true, rest: None },
                ]),
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
