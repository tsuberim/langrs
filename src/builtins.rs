use std::rc::Rc;

use anyhow::{bail, Result};
use im::{hashmap, HashMap};

use crate::{value::{Value, ValueEnv, apply}, term::Lit, typing::{ForAll, generalize, Type, TypeEnv}};

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

pub fn get_type_env(context: &Context) -> TypeEnv {
    let type_env: TypeEnv = context.iter().map(|(k, (_v, t))| (k.clone(), t.clone())).collect();
    type_env
}

pub fn get_value_env(context: &Context) -> ValueEnv {
    let val_env: ValueEnv = context.iter().map(|(k, (v, _t))| (k.clone(), Rc::clone(v))).collect();
    val_env
}

pub type Context = HashMap<String, (Rc<Value>, ForAll)>;

pub fn get_context() -> Context {
    let num = Type::Cons("Num".to_string(), vec![]);
    let str = Type::Cons("Str".to_string(), vec![]);

    let unit = Type::Record { items: hashmap! {}, union: false, rest: None };

    hashmap! {
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
    }
}