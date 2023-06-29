use std::{iter::Map, rc::Rc, fmt::Display, fmt::write, error::Error, io::{self, BufRead, Write}, hash::Hash, collections::btree_map::Iter, ops::Add, sync::mpsc::Receiver};
use im::{HashMap, HashSet, hashmap};
use tree_sitter::{Parser, Language, Tree, TreeCursor, Node};
use tree_sitter_fun::language;
use anyhow::{Result, Ok, bail};
use text_io::read;

#[derive(Debug, Clone)]
enum Lit {
    Str(String),
    Num(f64)
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Str(str) => str.fmt(f),
            Lit::Num(n) => n.fmt(f),
        }
    }
}

type Id = String;
type Env = HashMap<Id, Rc<Value>>;

#[derive(Debug, Clone)]
struct Case(Id, Id, Rc<Term>);

#[derive(Debug, Clone)]
enum Value {
    Lit(Lit),
    Cons(Id, Rc<Value>),
    Record(HashMap<String, Rc<Value>>),
    List(Vec<Rc<Value>>),
    Closure(Env, Id, Rc<Term>)
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Lit(lit) => lit.fmt(f),
            Value::Cons(name, payload) => {
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
            Value::Closure(env, arg, body) => write!(f, "<closure {} -> {}>", arg, body)
        }
    }
}

#[derive(Debug, Clone)]
enum Term {
    Lit(Lit),
    Var(Id),
    Cons(Id, Rc<Term>),
    Record(HashMap<Id, Term>),
    Match(Rc<Term>, HashMap<Id, (Id, Term)>, Option<Rc<Term>>),
    List(Vec<Term>),
    App(Rc<Term>, Rc<Term>),
    Lam(Id, Rc<Term>)
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Lit(lit) => lit.fmt(f),
            Term::Var(name) => write!(f, "{}", name),
            Term::Cons(name, payload) => {
                if let Term::Record(data) = payload.as_ref() {
                    if data.len() == 0 {
                        return write!(f, "{}", name)
                    }
                }
                write!(f, "{}({})", name, payload)
            },
            Term::Record(data) => {
                let pairs: Vec<String> = data.iter().map(|(k ,v)| format!("{}: {}", k, v)).collect();
                write!(f, "{{{}}}", pairs.join(", "))
            },
            Term::List(items) => {
                let pairs: Vec<String> = items.iter().map(|item| format!("{}", item)).collect();
                write!(f, "[{}]", pairs.join(", "))
            }
            Term::Match(term, cases, default) => {
                let cases: Vec<String> = cases.iter().map(|(cons, (id, result))| format!("{} {} -> {}", cons, id, result)).collect();
                let default = if let Some(term) = default { format!(" else {}", term) } else { "".to_string() };
                write!(f, "when {} is {}{}", term, cases.join("; "), default)
            },
            Term::App(function, arg) => write!(f, "({})({})", function, arg),
            Term::Lam(arg, body) => write!(f, "\\{} -> {}", arg, body),
        }
    }
}

#[derive(Debug, Clone)]
enum Type {
    Var(Id),
    Record {
        items: HashMap<String, Type>,
        union: bool,
        rest: Option<Id>,
    },
    Cons(String, Vec<Type>),
}

impl Type {
    pub fn free_type_vars(&self) -> HashSet<Id> {
        match self {
            Type::Var(id) => HashSet::new().update(id.clone()),
            Type::Record { items, union: _, rest } => {
                let mut init = HashSet::new();
                if let Some(id) = rest {
                    init.insert(id.clone());
                }

                let ftv = items
                    .values()
                    .into_iter()
                    .map(|t| t.free_type_vars())
                    .fold(init, HashSet::union);
                ftv
            },
            Type::Cons(f, args) => args.iter().map(|x| x.free_type_vars()).fold(HashSet::new(), HashSet::union),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Var(id) => id.fmt(f),
            Type::Record { items, union, rest } => {
                fn is_empty_record(t: &Type) -> bool {
                    match t {
                        Type::Record { items, union, rest } if !union && items.len() == 0 && rest.is_none() => true,
                        _ => false
                    }
                }

                let items: Vec<String> = items.into_iter().map(|(k, t)| {
                    if *union && is_empty_record(t) {
                        format!("{}", k)
                    } else if *union {
                        format!("{} {}", k, t)
                    } else {
                        format!("{}: {}", k, t)
                    }
                }).collect();
                let rest = if let Some(id) = rest { format!(" | {}",id) } else { "".to_string() };
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
                let args: Vec<String> = args.iter().map(|x| format!("{}", x)).collect();
                if args.len() == 0 { 
                    write!(f, "{}", name) 
                } else {
                    write!(f, "{}<{}>", name, args.join(", "))
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
struct ForAll(Vec<Id>, Type);

impl Display for ForAll {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ForAll(ids, t) = self;
        if ids.len() > 1 {
            write!(f, "∀{}. {}", ids.join(" "), t)
        } else {
            write!(f, "{}", t)
        }
    }
}

fn eval(term: &Term, env: &Env) -> Result<Rc<Value>> {
    match term {
        Term::Lit(lit) => Ok(Rc::new(Value::Lit(lit.clone()))),
        Term::Var(id) => {
            if let Some(val) = env.get(id) {
                Ok(Rc::clone(val))
            } else {
                Err(anyhow::format_err!("unbound variable {}", id))
            }
        },
        Term::Cons(id, payload) => {
            let payload = eval(&payload, env)?;
            Ok(Rc::new(Value::Cons(id.clone(), payload)))
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
            if let Value::Cons(id, payload) = val.as_ref() {
                if let Some((var, body)) = cases.get(id) {
                    return eval(body, &env.update(var.clone(), Rc::clone(payload)))
                } else if let Some(default) = default {
                    return eval(default, env)
                } else {
                    bail!("no match branch for cons {}", id)
                }
            } else {
                bail!("matching on non cons value {}", val)
            }
        },
    }
}

fn node_text(node: &Node, src: &str) -> Result<String> {
    let str = node.utf8_text(src.as_bytes())?;
    return Ok(str.into());
}

fn to_ast(node: Node, src: &str) -> Result<Term> {
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
        "source_file" => {
            let term = node.child_by_field_name("term").ok_or(anyhow::format_err!("could not find field 'term' in source_file node"))?;
            to_ast(term, src)
        },
        "parens" => {
            let term = node.child_by_field_name("term").ok_or(anyhow::format_err!("could not find field 'term' in parens node"))?;
            to_ast(term, src)
        },
        "str" => {
            let lit = Lit::Str(node_text(&node, src)?);
            let term = Term::Lit(lit);
            Ok(term)
        }
        "num" => {
            let lit = Lit::Num(node_text(&node, src)?.parse()?);
            let term = Term::Lit(lit);
            Ok(term)
        },
        "id" => {
            let term = Term::Var(node_text(&node, src)?);
            Ok(term)
        },
        "cons" => {
            let payload = node.child_by_field_name("payload");
            
            let payload = if let Some(node) = payload { to_ast(node, src)? } else {Term::Record(HashMap::new())}; 

            let name = node.child_by_field_name("name").ok_or(anyhow::format_err!("could not find field 'name' in cons node"))?;
            let name = node_text(&name, src)?;
            let term = Term::Cons(name, Rc::new(payload));
            Ok(term)
        },
        "record" => {
            let mut cursor = node.walk();
            let keys: Vec<String> = node
                .children_by_field_name("keys", &mut  cursor)
                .map(|x| node_text(&x, src))
                .collect::<Result<Vec<String>>>()?;

            let mut cursor = node.walk();
            let values: Vec<Term> = node
                .children_by_field_name("values", &mut  cursor)
                .map(|x| to_ast(x, src))
                .collect::<Result<Vec<Term>>>()?;
            
            Ok(Term::Record(
                keys
                    .into_iter()
                    .zip(values.into_iter())
                    .collect()
            ))
        },
        "match" => {
            let term = node.child_by_field_name("term").ok_or(anyhow::format_err!("could not find field 'term' in match node"))?;
            let term = to_ast(term, src)?;

            let mut cursor = node.walk();
            let conss: Vec<String> = node
                .children_by_field_name("case_cons", &mut  cursor)
                .map(|x| node_text(&x, src))
                .collect::<Result<Vec<String>>>()?;

            let mut cursor = node.walk();
            let ids: Vec<String> = node
                .children_by_field_name("case_id", &mut  cursor)
                .map(|x| node_text(&x, src))
                .collect::<Result<Vec<String>>>()?;

            let mut cursor = node.walk();
            let results: Vec<Term> = node
                .children_by_field_name("case_result", &mut  cursor)
                .map(|x| to_ast(x, src))
                .collect::<Result<Vec<Term>>>()?;


            let mut cases: HashMap<Id, (Id, Term)> = HashMap::new();
            for (cons, (id, term)) in conss.into_iter().zip(ids.into_iter().zip(results.into_iter())) {
                cases.insert(cons, (id, term));
            }
            
            
            let mut e = None;
            let else_term = node.child_by_field_name("else");
            if let Some(node) = else_term {
                let term = to_ast(node, src)?;
                e = Some(Rc::new(term))
            }

            Ok(Term::Match(Rc::new(term), cases, e))
        }
        "list" => {
            let mut cursor = node.walk();
            let items: Vec<Term> = node
                .children_by_field_name("items", &mut  cursor)
                .map(|x| to_ast(x, src))
                .collect::<Result<Vec<Term>>>()?;
            Ok(Term::List(items))
        },
        "app" => {
            let f = node.child_by_field_name("f").ok_or(anyhow::format_err!("could not find field 'f' in application node"))?;
            let f = to_ast(f, src)?;

            let arg = node.child_by_field_name("arg").ok_or(anyhow::format_err!("could not find field 'arg' in application node"))?;
            let arg = to_ast(arg, src)?;

            let term = Term::App(Rc::new(f), Rc::new(arg));
            Ok(term)
        },
        "lam" => {
            let id = node.child_by_field_name("arg").ok_or(anyhow::format_err!("could not find field 'arg' in lambda node"))?;
            let id = node_text(&id, src)?;

            let body = node.child_by_field_name("body").ok_or(anyhow::format_err!("could not find field 'body' in application node"))?;
            let body = to_ast(body, src)?;

            let term = Term::Lam(id, Rc::new(body));
            Ok(term)
        },
        _ => Err(anyhow::format_err!("Unknown ast type '{}'", kind))
    }
}


struct Infer {
    var_count: u32,
    constraints: Vec<(Type, Type)>,
    subst: Subst,
}

type TypeEnv = HashMap<Id, ForAll>;
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

fn generalize(t: Type) -> ForAll {
    let ftv = t.free_type_vars();
    
    let mut subst = HashMap::new();
    let mut ids = vec![];
    if ftv.len() == 1 {
        let id = ftv.iter().next().unwrap();
        let new_id = "*".to_string();
        ids.push(new_id.clone());
        subst.insert(id.clone() ,Type::Var(new_id));
    } else {
        let alphabet = "abcdefghijklmnopqrstuvwxyz";
        let mut i = 0;
        let mut j: i32 = 0;
    
        for id in ftv.into_iter() {
            let new_id = if j != 0 {format!("{}{}", alphabet.chars().nth(i).unwrap(), j) } else {format!("{}", alphabet.chars().nth(i).unwrap()) };
            ids.push(new_id.clone());
            subst.insert(id, Type::Var(new_id));
            i += 1;
            if i > alphabet.len() {
                i = 0;
                j += 1;
            }
        }
    }

    ForAll(ids, apply(&subst, &t))
}

impl Infer {

    pub fn new() -> Infer {
        Infer { 
            var_count: 0, 
            constraints: vec![], 
            subst: HashMap::new() 
        }
    }

    fn fresh(&mut self) -> String {
        let count = self.var_count;
        self.var_count += 1;
        return format!("t{}", count);
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

    fn bind(&mut self, id: &Id, t: &Type) -> Result<Subst> {
        if t.free_type_vars().contains(id) {
            bail!("cannot substitute {} into {} - infinite type", id, t)
        }

        if let Type::Var(v) = t {
            if v == id {
                return Ok(HashMap::new());
            }
        }

        Ok(HashMap::new().update(id.clone(), t.clone()))
    }

    fn unify(&mut self, t1: &Type, t2: &Type) -> Result<Subst> {
        match (t1, t2) {
            (Type::Var(id), t) => self.bind(id, t),
            (t, Type::Var(id)) => self.bind(id, t),
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
        
                
                let t1_minus_t2: HashMap<String, Type> = items1.clone().into_iter().filter(|(k, v)| !items2.contains_key(k)).collect();
                let t2_minus_t1: HashMap<String, Type> = items2.clone().into_iter().filter(|(k, v)| !items1.contains_key(k)).collect();

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

    pub fn emit(&mut self, t1: Type, t2: Type) {
        self.constraints.push((t1,t2))
    }

    pub fn solve(&mut self) -> Result<()> {
        while self.constraints.len() > 0 {
            let (t1, t2) = self.constraints.pop().unwrap();
            let subst = self.unify(&t1, &t2)?;
            self.subst = compose(&subst, &self.subst);
            self.constraints = self.constraints.iter().map(|(t1, t2)| (apply(&self.subst, t1), apply(&self.subst, t2))).collect()
        }

        Ok(())
    }

    pub fn get_type(&mut self, term: &Term, env: &TypeEnv) -> Result<ForAll> {
        let t = self.infer(term, env)?;
        self.solve()?;
        Ok(generalize(apply(&self.subst, &t)))
    }

    pub fn infer(&mut self, term: &Term, env: &TypeEnv) -> Result<Type> {

        let unit = Type::Record { items: HashMap::new(), union: false, rest: None };
        let void = Type::Record { items: HashMap::new(), union: true, rest: None };
        let any_cons = Type::Record { items: HashMap::new(), union: true, rest: Some(self.fresh()) };
        let any_record = Type::Record { items: HashMap::new(), union: false, rest: Some(self.fresh()) };

        let str = Type::Cons("Str".to_string(), vec![]);
        let num = Type::Cons("Num".to_string(), vec![]);

        match term {
            Term::Lit(lit) => match lit {
                Lit::Str(_) => Ok(str),
                Lit::Num(_) => Ok(num),
            },
            Term::Var(id) => {
                self.lookup(id, env)
            },
            Term::Cons(id, payload) => {
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
            Term::App(f, arg) => {
                let tf = self.infer(f, env)?;
                let t_arg = self.infer(arg, env)?;
                let t_var = Type::Var(self.fresh());
                self.emit(tf, Type::Cons("Fun".to_string(), vec![t_arg, t_var.clone()]));
                Ok(t_var)
            },
            Term::Lam(arg, body) => {
                let t_var = Type::Var(self.fresh());
                let t_body = self.infer(body, &env.update(arg.clone(), ForAll(vec![], t_var.clone())))?;
                Ok(Type::Cons("Fun".to_string(), vec![t_var, t_body]))
            },
            Term::List(items) => {
                let out = Type::Var(self.fresh());
                for term in items.into_iter() {
                    let t = self.infer(term, env)?;
                    self.emit(out.clone(), t)
                }
                
                Ok(Type::Cons("List".to_string(), vec!(out)))
            },
            Term::Match(term, cases, default) => {
                let mut items = HashMap::new();
                let out = Type::Var(self.fresh());

                for (cons, (id, result)) in cases {
                    let var_type = Type::Var(self.fresh());

                    items.insert(cons.clone(), var_type.clone());

                    let t = self.infer(result, &env.update(id.clone(), ForAll(vec![], var_type)))?;
                    self.emit(out.clone(), t);
                }

                if let Some(default) = default {
                    let t = self.infer(default, env)?;
                    self.emit(t, out.clone())
                }

                let t = self.infer(term, env)?;
                let in_t = Type::Record { items, union: true, rest: if let Some(_) = default { Some(self.fresh()) } else { None } };
                self.emit(t, in_t);

                Ok(out)
            },
        }
    }
}


fn repl() -> Result<()> {
    let mut parser = Parser::new();
    parser.set_language(language())?;


    fn step(parser: &mut Parser) -> Result<()> {
        print!("fun> ");
        io::stdout().flush()?;
        let mut line = String::new();
        io::stdin().read_line(&mut line)?;
        
        let src = line;
        let ast = parser.parse(&src, Option::None).ok_or(anyhow::format_err!("could not parse"))?;

        let term = to_ast(ast.root_node(), src.as_str())?;

        let mut infer = Infer::new();
        infer.infer(&term, &HashMap::new())?;
        infer.solve()?;
        let t: ForAll = infer.get_type(&term, &HashMap::new())?;

        let val = eval(&term, &HashMap::new())?;

        println!("{} : {}", val, t);

        Ok(())
    }

    loop {   
        if let Err(err) = step(&mut parser) {
            println!("Error: {}", err);
            io::stdout().flush()?;
        }
    }
}

fn main() -> Result<()> {
    repl()?;

    Ok(())
}
