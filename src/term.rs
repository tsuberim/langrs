use std::{rc::Rc, fmt::Display};
use im::{HashMap};
use anyhow::{Result, Ok, bail};
use tree_sitter::Node;

type Id = String;

#[derive(Debug, Clone)]
pub enum Lit {
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

#[derive(Debug, Clone)]
struct Case(Id, Id, Rc<Term>);

#[derive(Debug, Clone)]
pub enum Term {
    Lit(Lit),
    Var(Id),
    Tag(Id, Rc<Term>),
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
            Term::Tag(name, payload) => {
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
                let cases: Vec<String> = cases.iter().map(|(tag, (id, result))| format!("{} {} -> {}", tag, id, result)).collect();
                let default = if let Some(term) = default { format!(" else {}", term) } else { "".to_string() };
                write!(f, "when {} is {}{}", term, cases.join("; "), default)
            },
            Term::App(function, arg) => write!(f, "({})({})", function, arg),
            Term::Lam(arg, body) => write!(f, "\\{} -> {}", arg, body),
        }
    }
}

fn node_text(node: &Node, src: &str) -> Result<String> {
    let str = node.utf8_text(src.as_bytes())?;
    return Ok(str.into());
}

pub fn to_ast(node: Node, src: &str) -> Result<Term> {
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
        "tag" => {
            let payload = node.child_by_field_name("payload");
            
            let payload = if let Some(node) = payload { to_ast(node, src)? } else {Term::Record(HashMap::new())}; 

            let name = node.child_by_field_name("name").ok_or(anyhow::format_err!("could not find field 'name' in tag node"))?;
            let name = node_text(&name, src)?;
            let term = Term::Tag(name, Rc::new(payload));
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
            let tags: Vec<String> = node
                .children_by_field_name("case_tag", &mut  cursor)
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
            for (tag, (id, term)) in tags.into_iter().zip(ids.into_iter().zip(results.into_iter())) {
                cases.insert(tag, (id, term));
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
