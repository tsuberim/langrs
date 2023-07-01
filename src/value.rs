use std::{rc::Rc, fmt::Display};
use colored::Colorize;
use im::HashMap;
use anyhow::{Result, Ok, bail};

use crate::term::{Lit, Term};

type Id = String;

pub enum Value {
    Lit(Lit),
    Tag(Id, Rc<Value>),
    Record(HashMap<String, Rc<Value>>),
    List(Vec<Rc<Value>>),
    Closure(ValueEnv, Vec<Id>, Rc<Term>),
    Builtin(&'static str, Box<dyn Fn (&ValueEnv, &Vec<Rc<Value>>) -> Result<Rc<Value>>>),
    Task(Rc<dyn Fn () -> Result<Rc<Value>>>),
    
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Lit(lit) => lit.fmt(f),
            Value::Tag(name, payload) => {
                if let Value::Record(data) = payload.as_ref() {
                    if data.len() == 0 {
                        return write!(f, "{}", name.blue())
                    }
                }
                write!(f, "{} {}", name.blue(), payload)
            }
            Value::Record(data) => {
                let pairs: Vec<String> = data.iter().map(|(k ,v)| format!("{}: {}", k, v)).collect();
                write!(f, "{{{}}}", pairs.join(", "))
            },
            Value::List(items) => {
                let pairs: Vec<String> = items.iter().map(|item| format!("{}", item)).collect();
                write!(f, "[{}]", pairs.join(", "))
            }
            Value::Closure(_, params, body) => write!(f, "<closure \\{} -> {}>", params.join(", ").green(), body),
            Value::Builtin(name, _) => write!(f, "<builtin {}>", name),
            Value::Task(_) => write!(f, "<task>"),
        }
    }
}


pub type ValueEnv = HashMap<Id, Rc<Value>>;

pub fn eval(term: &Term, env: &ValueEnv) -> Result<Rc<Value>> {
    match term {
        Term::Lit(lit) => Ok(Rc::new(Value::Lit(lit.clone()))),
        Term::Var(id) => {
            
            if let Some(val) = env.get(id) {
                Ok(Rc::clone(val))
            } else {
                Err(anyhow::format_err!("unbound variable {}", id))
            }
        },
        Term::Tag(id, payload) => {
            let payload = eval(&payload, env)?;
            Ok(Rc::new(Value::Tag(id.clone(), payload)))
        }
        Term::Record(data) => {
            let data = data.into_iter().map(|(k, v)| -> Result<(String, Rc<Value>)> {
                let v = eval(v, env)?;
                Ok((k.clone(), v))
            }).collect::<Result<HashMap<String, Rc<Value>>>>()?;
            Ok(Rc::new(Value::Record(data)))
        },
        Term::List(items) => {
            let vals = items.into_iter().map(|x| eval(x, env)).collect::<Result<Vec<Rc<Value>>>>()?;
            Ok(Rc::new(Value::List(vals)))
        },
        Term::App(f, args) => {
            let f = eval(f, env)?;
            let args = args.iter().map(|term| eval(term, env)).collect::<Result<Vec<Rc<Value>>>>()?;
            apply(env, f, &args)
        },
        Term::Lam(params, body) => {
            let fv = body.free_vars();

            let env: ValueEnv = env
                .iter()
                .filter(|(k, _v)| fv.contains(*k))
                .map(|(k, v)| (k.clone(), Rc::clone(v)))
                .collect();

            Ok(Rc::new(Value::Closure(env.clone(), params.clone(), body.clone())))
        },
        Term::Match(term, cases, default) => {
            let val = eval(term, env)?;
            if let Value::Tag(id, payload) = val.as_ref() {
                if let Some((var, body)) = cases.get(id) {
                    return eval(body, &env.update(var.clone(), Rc::clone(payload)))
                } else if let Some(default) = default {
                    return eval(default, env)
                } else {
                    bail!("no match branch for tag {}", id)
                }
            } else {
                bail!("matching on non tag value {}", val)
            }
        },
        Term::Access(term, property) => {
            let val = eval(term, env)?;
            if let Value::Record(items) = val.as_ref() {
                if let Some(out) = items.get(property) {
                    Ok(Rc::clone(out))
                } else {
                    bail!("record {} does not have property {}", val, property)
                }
            } else {
                bail!("cannot access property {} of a non-record {}", property, val)
            }
        },
        Term::Block(defs, term) => {
            let mut extended_env = env.clone();
            for (id, def) in defs {
                let val = eval(def, &extended_env)?;
                extended_env = extended_env.update(id.clone(), val);
            }
            eval(term, &extended_env)
        },
    }
}

pub fn apply(env: &ValueEnv ,f: Rc<Value>, args: &Vec<Rc<Value>>) -> Result<Rc<Value>> {
    if let Value::Closure(closure_env, params, body) = f.as_ref() {
        let mut extended_env = closure_env.clone();
        for (param, val) in params.iter().zip(args) {
            extended_env = extended_env.update(param.clone(), Rc::clone(val));
        }

        eval(body.as_ref(), &extended_env)
    } else if let Value::Builtin(_, f) = f.as_ref() {
        f(env, args)
    } else {
        Err(anyhow::format_err!("{} is not a function", f))
    }
}