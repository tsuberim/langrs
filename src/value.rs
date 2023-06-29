use std::{rc::Rc, fmt::Display};
use colored::Colorize;
use im::{HashMap};
use anyhow::{Result, Ok, bail};

use crate::term::{Lit, Term};

type Id = String;

#[derive(Debug, Clone)]
pub enum Value {
    Lit(Lit),
    Tag(Id, Rc<Value>),
    Record(HashMap<String, Rc<Value>>),
    List(Vec<Rc<Value>>),
    Closure(ValueEnv, Vec<Id>, Rc<Term>),
    Builtin(&'static str, fn (Vec<Rc<Value>>) -> Result<Rc<Value>>)
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
            if let Value::Closure(closure_env, params, body) = f.as_ref() {

                let mut extended_env = closure_env.clone();
                for (param, arg) in params.iter().zip(args) {
                    let val = eval(arg, env)?;
                    extended_env = extended_env.update(param.clone(), val);
                }

                eval(body.as_ref(), &extended_env)
            } else if let Value::Builtin(_, f) = f.as_ref() {
                let args = args.iter().map(|term| eval(term, env)).collect::<Result<Vec<Rc<Value>>>>()?;
                f(args)
            } else {
                Err(anyhow::format_err!("{} is not a function", f))
            }
            
        },
        Term::Lam(params, body) => {
            let fv = body.free_vars();

            let env: ValueEnv = env
                .iter()
                .filter(|(k, v)| fv.contains(*k))
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
                let val = eval(def, env)?;
                extended_env.insert(id.clone(), val);
            }
            eval(term, &extended_env)
        },
    }
}
