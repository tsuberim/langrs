use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use anyhow::{anyhow, bail, Result, Context};
use colored::Colorize;

use crate::{
    term::{Pattern, Term},
    value::Value,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Var(String),
    Con(String),
    Rec {
        entries: HashMap<String, Type>,
        open: bool,
        union: bool,
        rest: String,
    },
    App(Box<Type>, Vec<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scheme(HashSet<String>, Type);

pub type TypeEnv = HashMap<String, Scheme>;
pub type Subst = HashMap<String, Type>;

impl Type {
    fn free_vars(&self) -> HashMap<String, u64> {
        match self {
            Type::Con(_) => HashMap::new(),
            Type::Var(name) => {
                let mut map = HashMap::new();
                map.insert(name.clone(), 1);
                map
            }
            Type::App(f, args) => {
                let mut f = f.free_vars();
                for arg in args {
                    for (k, n) in arg.free_vars().iter() {
                        *f.entry(k.into()).or_insert(0) += n;
                    }
                }
                f
            }
            Type::Rec {
                entries,
                open,
                union: _,
                rest,
            } => {
                let mut map = HashMap::new();
                for (_, term) in entries.iter() {
                    for (k, n) in term.free_vars().iter() {
                        *map.entry(k.into()).or_insert(0) += n;
                    }
                }
                if *open {
                    *map.entry(rest.into()).or_insert(0) += 1;
                }
                map
            }
        }
    }

    fn apply(&self, subst: &Subst) -> Result<Self> {
        match self {
            var @ Type::Var(name) => match subst.get(name) {
                Some(t) => Ok(t.clone()),
                None => Ok(var.clone()),
            },
            con @ Type::Con(_) => Ok(con.clone()),
            Type::App(f, args) => {
                let f = f.apply(subst)?;
                let args: Result<Vec<Type>> = args.iter().map(|t| t.apply(subst)).collect();

                Ok(Type::App(Box::new(f), args?))
            }
            Type::Rec {
                entries,
                open,
                union,
                rest,
            } => {
                let new_entries: Result<Subst> = entries
                    .iter()
                    .map(|(name, t)| Ok((name.clone(), t.apply(subst)?)))
                    .collect();
                let mut new_entries = new_entries?;

                let rest_rec = subst.get(rest);
                if let Some(rest_rec) = rest_rec {
                    if !open {
                        bail!("Cannot substitute rest var for non-open record");
                    }
                    match rest_rec {
                        Type::Rec {
                            entries,
                            open,
                            union,
                            rest,
                        } => {
                            for (name, t) in entries.iter() {
                                new_entries.insert(name.clone(), t.clone());
                            }
                            Ok(Type::Rec {
                                rest: rest.clone(),
                                open: open.clone(),
                                union: union.clone(),
                                entries: new_entries,
                            })
                        }
                        Type::Var(v) => Ok(Type::Rec {
                            rest: v.clone(),
                            open: open.clone(),
                            union: union.clone(),
                            entries: new_entries,
                        }),
                        _ => Err(anyhow!(
                            "Cannot substitute a non-record for rest var of record"
                        )),
                    }
                } else {
                    Ok(Type::Rec {
                        rest: rest.clone(),
                        open: open.clone(),
                        union: union.clone(),
                        entries: new_entries,
                    })
                }
            }
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Var(name) => write!(f, "{}", name.green()),
            Type::Con(name) => write!(f, "{}", name.red()),
            Type::App(func, args) => write!(
                f,
                "{}<{}>",
                func,
                args.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Type::Rec {
                entries,
                open,
                union,
                rest,
            } => {
                if *union {
                    write!(
                        f,
                        "[{}]{}",
                        entries
                            .iter()
                            .map(|(name, term)| format!("{}({})", name.bright_black(), term))
                            .collect::<Vec<String>>()
                            .join(", "),
                        if *open { rest.green() } else { "".green() }
                    )
                } else {
                    write!(
                        f,
                        "{{{}}}{}",
                        entries
                            .iter()
                            .map(|(name, term)| format!("{}: {}", name.bright_black(), term))
                            .collect::<Vec<String>>()
                            .join(", "),
                        if *open { rest.green() } else { "".green() }
                    )
                }
            }
        }
    }
}

impl Scheme {
    fn apply(&self, subst: &Subst) -> Result<Self> {
        let Self(vars, t) = self;
        let mut new_subst = subst.clone();
        for name in vars.iter() {
            new_subst.remove(name);
        }
        let t = t.apply(&new_subst)?;
        Ok(Self(vars.clone(), t))
    }
}

impl Display for Scheme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Scheme(vars, t) = self;
        let free_vars = t.free_vars();
        let mut letters = "abcdefghijklmnop"
            .chars()
            .into_iter()
            .cycle()
            .zip((0..).into_iter());
        let mut get_name = move || {
            let (c, i) = letters.next().unwrap();
            if i > 0 {
                format!("{}{}", c, i)
            } else {
                format!("{}", c)
            }
        };
        let subst: Subst = vars
            .into_iter()
            .map(|s| {
                let t = if free_vars.get(s).unwrap_or(&0) > &1 {
                    Type::Var(get_name())
                } else {
                    Type::Var("*".into())
                };

                (s.clone(), t)
            })
            .collect();

        let t = t.apply(&subst).map_err(|_| std::fmt::Error)?;
        let explicit_vars: Vec<String> = subst
            .values()
            .filter(|v| **v != Type::Var("*".into()))
            .map(|t| t.to_string())
            .collect();

        if explicit_vars.len() > 0 {
            write!(f, "∀{}. {}", explicit_vars.join(", ").green(), t)
        } else {
            t.fmt(f)
        }
    }
}

pub fn generalize(t: &Type) -> Scheme {
    let free_vars = t.free_vars();
    Scheme(free_vars.keys().cloned().collect(), t.clone())
}

pub fn occurs(var: &String, t: &Type) -> bool {
    let free_vars = t.free_vars();
    free_vars.get(var).is_some()
}

pub fn bind(var: &String, t: &Type) -> Result<Subst> {
    if let Type::Var(v) = t {
        if v == var {
            return Ok(HashMap::new());
        }
    }

    if occurs(var, t) {
        bail!("Var {} occurs in {}", var, t);
    }

    let mut map = HashMap::new();
    map.insert(var.clone(), t.clone());
    Ok(map)
}

pub fn compose(s1: &Subst, s2: Subst) -> Result<Subst> {
    let new_subst: Result<Subst> = s1
        .into_iter()
        .map(|(name, t)| Ok((name.clone(), t.apply(&s2)?)))
        .collect();
    let mut new_subst = new_subst?;
    new_subst.extend(s2);
    Ok(new_subst)
}

pub fn apply_env(env: &TypeEnv, subst: &Subst) -> Result<TypeEnv> {
    env.into_iter()
        .map(|(name, scheme)| Ok((name.clone(), scheme.apply(&subst)?)))
        .collect()
}

pub struct Infer {
    var_count: u64,
}

impl Infer {
    pub fn new() -> Infer {
        Infer { var_count: 0 }
    }

    pub fn unify(&mut self, t1: &Type, t2: &Type) -> Result<Subst> {
        match (t1, t2) {
            (Type::App(f1, args1), Type::App(f2, args2)) => {
                let mut subst = self.unify(f1, f2)?;
                if args1.len() != args2.len() {
                    bail!("Arguments have different lengths");
                }
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    let arg1 = &arg1.apply(&subst)?;
                    let arg2 = &arg2.apply(&subst)?;
                    let s = self.unify(arg1, arg2)?;
                    subst = compose(&s, subst)?
                }
                Ok(subst)
            }
            (Type::Con(c1), Type::Con(c2)) if c1 == c2 => Ok(HashMap::new()),
            (
                rec1 @ Type::Rec {
                    open: open1,
                    entries: entries1,
                    rest: rest1,
                    union: union1,
                },
                rec2 @ Type::Rec {
                    open: open2,
                    entries: entries2,
                    rest: rest2,
                    union: union2,
                },
            ) => {
                let mut subst: Subst = HashMap::new();

                if union1 != union2 {
                    bail!("Cannot unify record with union");
                }
                let union = *union1;

                // unify common keys
                for (name, t1) in entries1.iter() {
                    if let Some(t2) = entries2.get(name) {
                        let s = self.unify(t1, t2)?;
                        subst = compose(&s, subst)?;
                    }
                }

                let rec1_minus_rec2: HashMap<String, Type> = entries1
                    .iter()
                    .filter(|(k, _)| !entries2.contains_key(*k))
                    .map(|(k, t)| (k.clone(), t.clone()))
                    .collect();
                let rec2_minus_rec1: HashMap<String, Type> = entries2
                    .iter()
                    .filter(|(k, _)| !entries1.contains_key(*k))
                    .map(|(k, t)| (k.clone(), t.clone()))
                    .collect();

                let rest = self.fresh_var_name();
                let open = *open1 && *open2;

                if !(rec2_minus_rec1.len() > 0) || *open1 {
                    if rec2_minus_rec1.len() > 0 {
                        subst.insert(
                            rest1.clone(),
                            Type::Rec {
                                entries: rec2_minus_rec1,
                                open,
                                union,
                                rest: rest.clone(),
                            },
                        );
                    }
                } else {
                    bail!(
                        "Record {} is not open and does not contain keys: [{}]",
                        rec1,
                        rec2_minus_rec1.keys().cloned().collect::<Vec<String>>().join(", ").bright_black()
                    );
                }
                if !(rec1_minus_rec2.len() > 0) || *open2 {
                    if rec1_minus_rec2.len() > 0 {
                        subst.insert(
                            rest2.clone(),
                            Type::Rec {
                                entries: rec1_minus_rec2,
                                open,
                                union,
                                rest: rest.clone(),
                            },
                        );
                    }
                } else {
                    bail!(
                        "Record {} is not open and does not contain keys: [{}]",
                        rec2,
                        rec1_minus_rec2.keys().cloned().collect::<Vec<String>>().join(", ").bright_black()
                    );
                }

                Ok(subst)
            }
            (Type::Var(v), t) => bind(v, t),
            (t, Type::Var(v)) => bind(v, t),
            (_, _) => Err(anyhow!("Could not unify types {}, {}", t1, t2)),
        }
    }

    pub fn fresh_var(&mut self) -> Type {
        Type::Var(self.fresh_var_name())
    }
    pub fn fresh_var_name(&mut self) -> String {
        self.var_count += 1;
        format!("T{}", self.var_count)
    }

    pub fn instantiate(&mut self, scheme: &Scheme) -> Result<Type> {
        let Scheme(vars, t) = scheme;
        let subst: Subst = vars
            .into_iter()
            .map(|var| (var.clone(), self.fresh_var()))
            .collect();
        t.apply(&subst)
    }

    pub fn infer(&mut self, term: &Term, env: &TypeEnv) -> Result<(Subst, Type)> {
        match term {
            Term::Lit(val) => match val {
                Value::Str(_) => Ok((HashMap::new(), Type::Con("Str".into()))),
                Value::Num(_) => Ok((HashMap::new(), Type::Con("Num".into()))),
                _ => Err(anyhow!("Cannot infer type for non-literal value")),
            },
            Term::Var(id) => {
                let scheme = env
                    .get(id)
                    .ok_or(anyhow!("Unresolved type for variable {}", id))?;
                Ok((HashMap::new(), self.instantiate(scheme)?))
            }
            Term::App(f, args) => {
                let result_type = self.fresh_var();

                let (sf, tf) = self.infer(f, env)?;

                let mut subst: Subst = sf;
                let mut targs = vec![];
                for arg in args.iter() {
                    let (s, targ) = self.infer(arg, &apply_env(&env, &subst)?)?;
                    targs.push(targ.apply(&subst)?);
                    subst = compose(&s, subst)?;
                }

                targs.push(result_type.clone());
                let s = self.unify(
                    &tf.apply(&subst)?,
                    &Type::App(Box::new(Type::Con("Fun".into())), targs),
                )?;
                let subst = compose(&s, subst)?;

                let result_type = result_type.apply(&subst.clone())?;
                Ok((subst, result_type))
            }
            Term::Abs(args, body) => {
                let mut new_env: TypeEnv = env.clone();
                let mut targs = vec![];
                for arg in args.iter() {
                    let targ = self.fresh_var();
                    new_env.insert(arg.clone(), Scheme(HashSet::new(), targ.clone()));
                    targs.push(targ);
                }

                let (s, t) = self.infer(body, &new_env)?;
                targs.push(t);

                let ftype = Type::App(Box::new(Type::Con("Fun".into())), targs).apply(&s)?;
                Ok((s, ftype))
            }
            Term::Rec(entries) => {
                let mut subst = HashMap::new();
                let mut types = HashMap::new();
                for (name, term) in entries.iter() {
                    let (s, t) = self.infer(term, env)?;
                    types.insert(name.clone(), t.apply(&subst)?);
                    subst = compose(&s, subst)?;
                }
                Ok((
                    subst,
                    Type::Rec {
                        entries: types,
                        open: false,
                        union: false,
                        rest: self.fresh_var_name(),
                    },
                ))
            }
            Term::Acc(term, prop) => {
                let out = self.fresh_var();
                let (s, t) = self.infer(term, env)?;

                let mut subst = s;

                let mut entries = HashMap::new();
                entries.insert(prop.clone(), out.clone());
                let rest = self.fresh_var_name();
                let s = self.unify(
                    &t,
                    &Type::Rec {
                        entries,
                        open: true,
                        union: false,
                        rest,
                    },
                )?;

                subst = compose(&s, subst)?;
                let out = out.apply(&subst)?;

                Ok((subst, out))
            }
            Term::Cons(name, content) => {
                let (s, t) = self.infer(content, env)?;
                let mut entries = HashMap::new();
                entries.insert(name.clone(), t);
                let rest = self.fresh_var_name();
                Ok((
                    s,
                    Type::Rec {
                        entries,
                        open: true,
                        union: true,
                        rest,
                    },
                ))
            }
            Term::Case(expr, cases, otherwise) => {
                let (mut subst, in_t) = self.infer(expr, env)?;

                let mut out_t = self.fresh_var();
                let mut entries: HashMap<String, Type> = HashMap::new();
                for (Pattern::Cons(name, var), term) in cases.iter() {
                    // invent a new type of the bound case variable
                    let var_t = self.fresh_var();
                    let mut new_env = env.clone();
                    new_env.insert(var.clone(), Scheme(HashSet::new(), var_t.clone()));

                    // infer type for the case consequence
                    let (s, t) = self.infer(term, &new_env)?;
                    subst = compose(&s, subst)?;

                    // unify case output type with final out type
                    let s = self.unify(&t, &out_t)?;
                    subst = compose(&s, subst)?;

                    // apply our new constaints
                    out_t = out_t.apply(&subst)?;
                    let var_t = var_t.apply(&subst)?;

                    // unify input type with union containing our inferred var_t
                    entries.insert(name.clone(), var_t);
                }

                if let Some(otherwise) = otherwise {
                    let (s, t) = self.infer(otherwise, env)?;
                    subst = compose(&s, subst)?;
                    out_t = out_t.apply(&subst)?;
                    let s = self.unify(&t, &out_t)?;
                    subst = compose(&s, subst)?;
                }

                let rest = self.fresh_var_name();
                let s = self.unify(
                    &in_t,
                    &Type::Rec {
                        entries,
                        open: otherwise.is_some(),
                        union: true,
                        rest,
                    },
                )?;
                subst = compose(&s, subst)?;
                out_t = out_t.apply(&subst)?;

                Ok((subst, out_t))
            }
        }
    }
}
