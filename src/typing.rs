use std::{fmt::Display, vec};
use colored::Colorize;
use im::{HashMap, HashSet, hashmap};
use anyhow::{Result, Ok, bail};
use tree_sitter::Node;

use crate::{term::{Term, Lit}, utils::{Namer, node_text}};

type Id = String;

pub type TypeEnv = HashMap<Id, ForAll>;

#[derive(Debug, Clone)]
pub enum Type {
    Var(Id),
    Record {
        items: HashMap<String, Type>,
        union: bool,
        rest: Option<Id>,
    },
    Cons(String, Vec<Type>),
}

fn sum_maps(a: HashMap<Id, u32>, b: HashMap<Id, u32> ) -> HashMap<Id, u32> {
    let mut out = a.clone();
    for (id, bn) in b {
        out = out.alter(|n| if let Some(n) = n { Some(n + bn) } else {Some(bn)}, id);
    }

    out
}

impl Type {
    pub fn free_type_vars(&self) -> HashMap<Id, u32> {
        match self {
            Type::Var(id) => hashmap! {id.clone() => 1},
            Type::Record { items, union: _, rest } => {
                let mut init = HashMap::new();
                if let Some(id) = rest {
                    init.insert(id.clone(), 1);
                }

                let ftv = items
                    .values()
                    .into_iter()
                    .map(|t| t.free_type_vars())
                    .fold(init, sum_maps);
                ftv
            },
            Type::Cons(_, args) => args.iter().map(|x| x.free_type_vars()).fold(HashMap::new(), sum_maps),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Var(id) => id.green().fmt(f),
            Type::Record { items, union, rest } => {
                fn is_empty_record(t: &Type) -> bool {
                    match t {
                        Type::Record { items, union, rest } if !union && items.len() == 0 && rest.is_none() => true,
                        _ => false
                    }
                }

                let items: Vec<String> = items.into_iter().map(|(k, t)| {
                    if *union && is_empty_record(t) {
                        format!("{}", k.blue())
                    } else if *union {
                        format!("{} {}", k.blue(), t)
                    } else {
                        format!("{}: {}", k, t)
                    }
                }).collect();
                let rest = if let Some(id) = rest { format!(" | {}",id.green()) } else { "".to_string() };
                let inner = format!("{}{}",items.join(", "), rest);
                if *union && items.len() == 1 && rest == ""{
                    write!(f, "{}", inner) 
                } else if *union { 
                    write!(f, "[{}]", inner) 
                } else {
                    write!(f, "{{{}}}", inner)
                }
            },
            Type::Cons(name, args) => {
                if args.len() == 0 { 
                    write!(f, "{}", name.blue()) 
                } else {
                    let args: Vec<String> = args.iter().map(|x| format!("{}", x)).collect();
                    write!(f, "{}<{}>", name.blue(), args.join(", "))
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct ForAll(pub Vec<Id>, pub Type);

impl Display for ForAll {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut namer = Namer::new();


        let ForAll(_, t) = self;

        let ftv = t.free_type_vars();
        let mut subst = HashMap::new();
        for (id, count) in &ftv {
            if *count == 1 {
                subst.insert(id.clone(), Type::Var("*".to_string()));
            } else {
                subst.insert(id.clone(), Type::Var(namer.name()));
            }
        }
        let ids: Vec<String> = ftv.keys().cloned().filter(|k| *k == "*".to_string()).collect();
        let t = apply(&subst, t);
        
        if ids.len() > 1 {
            write!(f, "∀{}. {}", ids.join(" ").green(), t)
        } else {
            write!(f, "{}", t)
        }
    }
}

struct Infer {
    subst: Subst,
    namer: Namer,
}

type Subst = HashMap<Id, Type>;

fn apply(subst: &Subst, t: &Type) -> Type {
    match t {
        Type::Var(id) => {
            if let Some(t) = subst.get(id) {
                t.clone()
            } else {
                t.clone()
            }
        },
        Type::Record { items, union, rest } => {
            let mut items: HashMap<String, Type> = items.into_iter().map(|(k, t)| (k.clone(), apply(subst, t))).collect();

            let mut rest = rest.clone();
            if let Some(id) = &rest {
                if let Some(t) = subst.get(id) {
                    match t {
                        Type::Record { items: other_items, union: other_union, rest: other_rest } if union == other_union => {
                            rest = other_rest.clone();
                            items = other_items.clone().union(items);
                        },
                        Type::Var(v) => {
                            rest = Some(v.clone());
                        }
                        _ => panic!("cannot substitute {} into rest variable {} in {}", t, id, t)
                    }
                }
            }

            Type::Record { items, union: *union, rest }
        }
        Type::Cons(name, args) => {
            let args = args.iter().map(|x| apply(subst, x)).collect();
            return Type::Cons(name.clone(), args);
        },
    }
}

fn compose(s1: &Subst, s2: &Subst) -> Subst {
    let mut out = s1.clone();
    for (k, t) in s2 {
        out.insert(k.clone(), apply(s1, t));
    }
    out
}

pub fn generalize(t: Type) -> ForAll {
    let ftv = t.free_type_vars();
    ForAll(ftv.keys().cloned().collect(), t)
}

fn bind(id: &Id, t: &Type) -> Result<Subst> {
    if let Type::Var(v) = t {
        if v == id {
            return Ok(HashMap::new());
        }
    }

    if t.free_type_vars().contains_key(id) {
        bail!("cannot substitute {} into {} - infinite type", id, t)
    }

    Ok(HashMap::new().update(id.clone(), t.clone()))
}

impl Infer {
    pub fn new(namer: Namer) -> Infer {
        Infer { 
            namer,
            subst: HashMap::new() 
        }
    }

    fn fresh(&mut self) -> String {
        return self.namer.name();
    }

    fn instantiate(&mut self, forall: &ForAll) -> Result<Type> {
        let ForAll(ids, t) = forall;
        let mut subst = HashMap::new();
        for id in ids {
            subst.insert(id.clone(), Type::Var(self.fresh()));
        }

        Ok(apply(&subst, t))
    }
    
    fn lookup(&mut self, id: &Id, env: &TypeEnv) -> Result<Type> {
        let forall = env.get(id).ok_or(anyhow::format_err!("unbound variable {}", id))?;
        self.instantiate(&forall)
    }

    fn unify(&mut self, t1: &Type, t2: &Type) -> Result<Subst> {
        match (t1, t2) {
            (Type::Var(id), t) => bind(id, t),
            (t, Type::Var(id)) => bind(id, t),
            (Type::Cons(f1, args1), Type::Cons(f2, args2)) if f1 == f2 && args1.len() == args2.len() => {
                let mut subst = HashMap::new();
                for (t1, t2) in args1.iter().zip(args2.iter()) {
                    let t1 = &apply(&subst, t1);
                    let t2 = &apply(&subst, t2);
                    let s = self.unify(t1, t2)?;
                    subst = compose(&s, &subst);
                }
                Ok(subst)
            },
            (
                Type::Record { items: items1, union: union1, rest: rest1 }, 
                Type::Record { items: items2, union: union2, rest: rest2 }  
            ) if union1 == union2 => {
                let union = union1;

                let keys1: HashSet<String> = items1.keys().collect();
                let keys2: HashSet<String> = items2.keys().collect();

                // common keys should have the same type
                let mut subst = HashMap::new();
                for k in keys1.clone().intersection(keys2.clone()) {
                    let t1 = items1.get(&k).unwrap();
                    let t2 = items2.get(&k).unwrap();
                    let s = self.unify(t1, t2)?;
                    subst = compose(&s, &subst);
                }

                let rest = match (rest1, rest2) {
                    (Some(r1), Some(r2)) => {
                        let r = self.fresh();
                        
                        let s = self.unify(&Type::Var(r.clone()), &Type::Var(r1.clone()))?;
                        subst = compose(&s, &subst);
                        let s = self.unify(&Type::Var(r.clone()), &Type::Var(r2.clone()))?;
                        subst = compose(&s, &subst);

                        Some(r)
                    },
                    _ => None
                };
        
                
                let t1_minus_t2: HashMap<String, Type> = items1.clone().into_iter().filter(|(k, _)| !items2.contains_key(k)).collect();
                let t2_minus_t1: HashMap<String, Type> = items2.clone().into_iter().filter(|(k, _)| !items1.contains_key(k)).collect();
                
                let assignable_to_t1 = t2_minus_t1.len() == 0 || rest1.is_some();
                let assignable_to_t2 = t1_minus_t2.len() == 0 || rest2.is_some();

                

                if rest.is_some() || (assignable_to_t1 && assignable_to_t2) {
                    if let Some(r1) = rest1 {
                        let s = self.unify(&Type::Var(r1.clone()), &Type::Record { items: t2_minus_t1, union: *union, rest: rest.clone() })?;
                        subst = compose(&s, &subst);
                    }

                    if let Some(r2) = rest2 {
                        let s = self.unify(&Type::Var(r2.clone()), &Type::Record { items: t1_minus_t2, union: *union, rest: rest.clone() })?;
                        subst = compose(&s, &subst);
                    }
                
                } else {
                    bail!("cannot unify records {} ~ {}", t1, t2)
                }

                Ok(subst)
            }
            _ => bail!("could not unify {} ~ {} ", t1, t2)
        }
    }

    pub fn assert_eq(&mut self, t1: &Type, t2: &Type) -> Result<()> {
        let t1 = apply(&self.subst, t1);
        let t2 = apply(&self.subst, t2);
        let subst = self.unify(&t1, &t2)?;
        self.subst = compose(&subst, &self.subst);
        Ok(())
    }

    pub fn infer(&mut self, term: &Term, env: &TypeEnv) -> Result<Type> {
        let t = match term {
            Term::Lit(lit) => match lit {
                Lit::Str(_) => Ok(Type::Cons("Str".to_string(), vec![])),
                Lit::Num(_) => Ok(Type::Cons("Num".to_string(), vec![])),
            },
            Term::Var(id) => {
                self.lookup(id, env)
            },
            Term::Tag(id, payload) => {
                let t = self.infer(payload, env)?;
                let mut items = HashMap::new();
                items.insert(id.clone(), t);
                Ok(Type::Record { items, union: true, rest: Some(self.fresh()) })
            },
            Term::Record(data) => {
                let mut items = HashMap::new();
                for (k, e) in data {
                    let t = self.infer(e, env)?;
                    items.insert(k.clone(), t);
                } 
                Ok(Type::Record { items, union: false, rest: None })
            },
            Term::App(f, args) => {
                let tf = self.infer(f, env)?;

                let mut args_ts = vec![];
                for arg in args {
                    let t = self.infer(arg, env)?;
                    args_ts.push(t);
                }
                let t_var = Type::Var(self.fresh());
                args_ts.push(t_var.clone());
                
                self.assert_eq(&tf, &Type::Cons("Fun".to_string(), args_ts))?;
                Ok(t_var)
            },
            Term::Lam(params, body) => {
                let mut extended_env = env.clone();
                let mut params_ts = vec![];
                for param in params {
                    let t_var = Type::Var(self.fresh());
                    params_ts.push(t_var.clone());
                    extended_env.insert(param.clone(), ForAll(vec![], t_var.clone()));
                }
                let t_body = self.infer(body, &extended_env)?;
                params_ts.push(t_body);
                Ok(Type::Cons("Fun".to_string(), params_ts))
            },
            Term::List(items) => {
                let out = Type::Var(self.fresh());
                for term in items.into_iter() {
                    let t = self.infer(term, env)?;
                    self.assert_eq(&out, &t)?
                }
                
                Ok(Type::Cons("List".to_string(), vec!(out)))
            },
            Term::Match(term, cases, default) => {
                let mut items = HashMap::new();
                let out = Type::Var(self.fresh());

                for (tag, (id, result)) in cases {
                    let var_type = Type::Var(self.fresh());

                    items.insert(tag.clone(), var_type.clone());

                    let t = self.infer(result, &env.update(id.clone(), ForAll(vec![], var_type)))?;
                    self.assert_eq(&out, &t)?;
                }

                if let Some(default) = default {
                    let t = self.infer(default, env)?;
                    self.assert_eq(&t, &out)?
                }

                let t = self.infer(term, env)?;
                let in_t = Type::Record { items, union: true, rest: if let Some(_) = default { Some(self.fresh()) } else { None } };
                self.assert_eq(&t, &in_t)?;

                Ok(out)
            },
            Term::Access(term, property) => {
                let out = Type::Var(self.fresh());
                let t = self.infer(term, env)?;
                let rest = self.fresh();
                self.assert_eq(&t, &Type::Record { items: hashmap!{property.clone() => out.clone()}, union: false, rest: Some(rest) })?;

                Ok(out)
            },
            Term::Block(typings, defs, term) => {
                let mut extended_env = env.clone();

                for (id, def) in defs {
                    let t = self.infer(def, &extended_env)?;
                    if let Some(declared_type) = typings.get(id) {
                        let declared_type = self.instantiate(declared_type)?;
                        self.assert_eq(&t, &declared_type)?;
                    }
                    let t = apply(&self.subst, &t);
                    extended_env = extended_env.update(id.clone(), ForAll(vec![], t));
                }

                
                let t = self.infer(term, &extended_env)?;
                Ok(t)
            },
        }?;
        
        let t = apply(&self.subst, &t);
        Ok(t)
    }
}

pub fn infer(term: &Term, env: &TypeEnv) -> Result<ForAll> {
    let mut infer = Infer::new(Namer::new());
    let t = infer.infer(term, env)?;
    let forall = generalize(t);
    Ok(forall)
}

pub fn to_type_ast(node: Node, src: &str) -> Result<Type> {
    let kind = node.kind();

    let mut cursor = node.walk();
    if node.is_error() || node.is_missing() {
        bail!("Error while parsing: {:?}", node_text(&node, src))
    }
    for node in node.children(&mut cursor) {
        if node.is_error() || node.is_missing() {
            bail!("Error while parsing: {:?}", node_text(&node, src))
        }
    }

    match kind {
        "id" => {
            let term = Type::Var(node_text(&node, src)?);
            Ok(term)
        },
        "sym" => {
            let term = Type::Var(node_text(&node, src)?);
            Ok(term)
        },
        "type_record" => {
            let mut cursor = node.walk();
            let keys: Vec<String> = node
                .children_by_field_name("keys", &mut  cursor)
                .map(|x| node_text(&x, src))
                .collect::<Result<Vec<String>>>()?;

            let mut cursor = node.walk();
            let values: Vec<Type> = node
                .children_by_field_name("types", &mut  cursor)
                .map(|x| to_type_ast(x, src))
                .collect::<Result<Vec<Type>>>()?;
            
            let items = keys
                .into_iter()
                .zip(values.into_iter())
                .collect();

            let rest = node.child_by_field_name("rest");
            let rest = if let Some(node) = rest { 
                Some(node_text(&node, src)?)
            } else {
                None
            };

            Ok(Type::Record { items, union: false, rest })
        },
        "type_union" => {
            let mut cursor = node.walk();
            let keys: Vec<String> = node
                .children_by_field_name("keys", &mut  cursor)
                .map(|x| node_text(&x, src))
                .collect::<Result<Vec<String>>>()?;

            let mut cursor = node.walk();
            let values: Vec<Type> = node
                .children_by_field_name("types", &mut  cursor)
                .map(|x| to_type_ast(x, src))
                .collect::<Result<Vec<Type>>>()?;
            
            let items = keys
                .into_iter()
                .zip(values.into_iter())
                .collect();

            let rest = node.child_by_field_name("rest");
            let rest = if let Some(node) = rest { 
                Some(node_text(&node, src)?)
            } else {
                None
            };

            Ok(Type::Record { items, union: true, rest })
        },
        "type_app" => {
            let f = node.child_by_field_name("f").ok_or(anyhow::format_err!("could not find field 'f' in application node"))?;
            let f = node_text(&f, src)?;

            let args: Vec<Type> = node
                .children_by_field_name("args", &mut  cursor)
                .map(|x| to_type_ast(x, src))
                .collect::<Result<Vec<Type>>>()?;

            let term = Type::Cons(f, args);
            Ok(term)
        },
        _ => Err(anyhow::format_err!("Unknown ast type '{}'", kind))
    }
}
