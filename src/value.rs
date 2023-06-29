use std::{rc::Rc, fmt::Display};
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
    Closure(Env, Id, Rc<Term>)
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Lit(lit) => lit.fmt(f),
            Value::Tag(name, payload) => {
                if let Value::Record(data) = payload.as_ref() {
                    if data.len() == 0 {
                        return write!(f, "{}", name)
                    }
                }
                write!(f, "{} {}", name, payload)
            }
            Value::Record(data) => {
                let pairs: Vec<String> = data.iter().map(|(k ,v)| format!("{}: {}", k, v)).collect();
                write!(f, "{{{}}}", pairs.join(", "))
            },
            Value::List(items) => {
                let pairs: Vec<String> = items.iter().map(|item| format!("{}", item)).collect();
                write!(f, "[{}]", pairs.join(", "))
            }
            Value::Closure(_, arg, body) => write!(f, "<closure {} -> {}>", arg, body)
        }
    }
}


type Env = HashMap<Id, Rc<Value>>;

pub fn eval(term: &Term, env: &Env) -> Result<Rc<Value>> {
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
        Term::App(f, arg) => {
            let f = eval(f, env)?;
            if let Value::Closure(closure_env, arg_name, body) = f.as_ref() {
                let arg = eval(arg, env)?;
                eval(body.as_ref(), &closure_env.update(arg_name.clone(), arg))
            } else {
                Err(anyhow::format_err!("{} is not a function", f))
            }
            
        },
        Term::Lam(arg, body) => Ok(Rc::new(Value::Closure(env.clone(), arg.clone(), body.clone()))),
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
    }
}

