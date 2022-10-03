use colored::Colorize;
use quickcheck::Arbitrary;

use crate::term::{Term, Pattern};
use std::{collections::HashMap, fmt::Display, rc::Rc};
use anyhow::{Result, anyhow, bail};
use proptest_derive::{Arbitrary};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Str(String),
    Num(u64),
    Rec(HashMap<String, Value>),
    Cons(String, Box<Value>),
    Clo(Closure),
}

#[derive(Debug, Clone)]
pub enum ClosureImpl {
    Builtin(fn(Vec<Value>, HashMap<String, Value>) -> Result<Value>),
    Body(Rc<Term>),
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub captures: HashMap<String, Value>,
    pub args: Vec<String>,
    pub closure: ClosureImpl,
}
impl PartialEq for Closure {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Str(str) => write!(f, "'{}'", str.yellow()),
            Value::Num(num) => write!(f, "{}", num.to_string().yellow()),
            Value::Clo(_) => write!(f, "{}", "<closure>".yellow()),
            Value::Rec(entries) => write!(f, "{{{}}}", entries.iter().map(|(name, term)| format!("{}: {}", name.bright_black(), term)).collect::<Vec<String>>().join(", ")),
            Value::Cons(name, content) => write!(f, "{}({})", name.bright_black(), content),
        }
    }
}

pub type ValueEnv = HashMap<String, Value>;

pub fn evaluate(term: &Term, env: &ValueEnv) -> Result<Value> {
    match term {
        Term::Num(v) => Ok(Value::Num(*v)),
        Term::Str(v) => Ok(Value::Str(v.clone())),
        Term::Var(name) => env
            .get(name)
            .map(|x| x.clone())
            .ok_or(anyhow!("Unbound variable {}", name)),
        Term::App(func, args) => {
            let f = evaluate(func, env)?;
            let mut vals = Vec::new();
            for arg in args {
                let v = evaluate(arg, env)?;
                vals.push(v);
            }
            if let Value::Clo(Closure {
                captures,
                args,
                closure,
            }) = f
            {
                match closure {
                    ClosureImpl::Builtin(f) => f(vals, captures),
                    ClosureImpl::Body(body) => {
                        let mut new_env = captures;
                        for (arg, val) in args.iter().zip(vals.iter()) {
                            new_env.insert(arg.clone(), val.clone());
                        } 
                        evaluate(&body, &new_env)
                    }
                }
            } else {
                Err(anyhow!("{} is not a function", func))
            }
        }
        expr @ Term::Abs(args, body) => {
            let mut captures = HashMap::new();
            for name in expr.free_vars().keys().cloned() {
                let val = env.get(&name).ok_or(anyhow!("Unbound variable {}", name))?;
                captures.insert(name, val.clone());
            }

            Ok(Value::Clo(Closure {
                captures,
                args: args.clone(),
                closure: ClosureImpl::Body(body.clone()),
            }))
        }
        Term::Rec(entries) => {
            let mut values = HashMap::new();
            for (name, term) in entries.iter() {
                let v = evaluate(term, env)?;
                values.insert(name.clone(), v);
            }
            Ok(Value::Rec(values))
        },
        Term::Acc(term, prop) => {
            let val = evaluate(term, env)?;
            if let Value::Rec(entries) = val {
                entries.get(prop).map(|x| x.clone()).ok_or(anyhow!("Key {} not found", prop))
            } else {
                Err(anyhow!("Value is not a record"))
            }
        },
        Term::Cons(name, content) => {
            let val = evaluate(content, env)?;
            Ok(Value::Cons(name.clone(), Box::new(val)))
        },
        Term::Case(term, cases, otherwise) => {
            let val = evaluate(term, env)?;
            if let Value::Cons(name, val) = val {
                for (Pattern::Cons(case, var), consequence) in cases.iter() {
                    if *case == *name {
                        let mut new_env = env.clone();
                        new_env.insert(var.clone(), *val);
                        return evaluate(consequence, &new_env);
                    }
                }

                if let Some(otherwise) = otherwise {
                    return evaluate(&otherwise, env);
                }

                Err(anyhow!("No case matced"))
            } else {
                Err(anyhow!("Not a cons"))
            }
        },
    }
}
