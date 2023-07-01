use std::{rc::Rc, fmt::{Display}};
use colored::Colorize;
use im::{HashMap, HashSet, hashset};
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
            Lit::Str(str) => write!(f, "`{}`", str.blue()),
            Lit::Num(n) => n.to_string().blue().fmt(f),
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
    Access(Rc<Term>, Id),
    App(Rc<Term>, Vec<Term>),
    Block(Vec<(Id, Term)>, Rc<Term>), // like a let
    Lam(Vec<Id>, Rc<Term>)
}

impl Term {
    pub fn free_vars(&self) -> HashSet<Id> {
        match self {
            Term::Lit(_) => hashset![],
            Term::Var(v) => hashset![v.clone()],
            Term::Tag(_, payload) => payload.free_vars(),
            Term::Record(items) => items.into_iter().map(|(_k, v)| v.free_vars()).fold(hashset![], HashSet::union),
            Term::Match(term, cases, default) => {
                let mut fv: HashSet<String> = term.free_vars();
                for (_, (id, result)) in cases {
                    fv = fv.union(result.free_vars().without(id))
                }
                if let Some(default) = default {
                    fv = fv.union(default.free_vars());
                }
                fv
            },
            Term::List(items) => items.into_iter().map(|x| x.free_vars()).fold(hashset![], HashSet::union),
            Term::Access(x, _prop) => x.free_vars(),
            Term::App(f, args) => f.free_vars().union(args.into_iter().map(|x| x.free_vars()).fold(hashset![], HashSet::union)),
            Term::Block(defs, term) => {
                defs
                    .iter()
                    .fold(term.free_vars(), |acc, (id, term)| acc.without(id).union(term.free_vars()))
            },
            Term::Lam(params, body) => {
                let mut fv = body.free_vars();
                for param in params {
                    fv = fv.without(param)
                }
                fv
            },
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Lit(lit) => lit.fmt(f),
            Term::Var(name) => write!(f, "{}", name.green()),
            Term::Tag(name, payload) => {
                if let Term::Record(data) = payload.as_ref() {
                    if data.len() == 0 {
                        return write!(f, "{}", name.blue())
                    }
                }
                write!(f, "{}({})", name.blue(), payload)
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
            Term::App(function, args) => {
                let args: Vec<String> = args.iter().map(|arg| format!("{}", arg)).collect();
                write!(f, "({})({})", function, args.join(", "))
            },
            Term::Lam(params, body) => write!(f, "\\{} -> {}", params.join(", "), body),
            Term::Access(term, property) => write!(f, "{}.{}", term, property),
            Term::Block(defs, term) => {
                let defs: Vec<String> = defs.iter().map(|(k, v)| format!("{} = {}", k, v)).collect();
                write!(f, "({}\n{})", defs.join("\n"), term)
            },
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
        "str_lit" => {
            let text = node_text(&node, src)?;
            let lit = Lit::Str(text);
            let term = Term::Lit(lit);
            Ok(term)
        },
        "str" => {
            let mut cursor = node.walk();
            let terms: Vec<Term> = node
                .named_children(&mut  cursor)
                .map(|x| to_ast(x, src))
                .collect::<Result<Vec<Term>>>()?;

            let t = terms
                .into_iter()
                .fold(
                    Term::Lit(Lit::Str("".to_string())), 
                    |acc, term| 
                        Term::App(
                            Rc::new(Term::Var("++".to_string())), 
                            vec![acc, term]
                        )
                );
            Ok(t)
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
        "sym" => {
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

            let args: Vec<Term> = node
                .children_by_field_name("args", &mut  cursor)
                .map(|x| to_ast(x, src))
                .collect::<Result<Vec<Term>>>()?;

            let term = Term::App(Rc::new(f), args);
            Ok(term)
        },
        "infix_app" => {
            let lhs = node.child_by_field_name("lhs").ok_or(anyhow::format_err!("could not find field 'lhs' in infix application node"))?;
            let lhs = to_ast(lhs, src)?;

            let rhs = node.child_by_field_name("rhs").ok_or(anyhow::format_err!("could not find field 'rhs' in infix application node"))?;
            let rhs = to_ast(rhs, src)?;

            let f = node.child_by_field_name("f").ok_or(anyhow::format_err!("could not find field 'f' in infix application node"))?;
            let f = node_text(&f, src)?;

            let term = Term::App(Rc::new(Term::Var(f)), vec![lhs, rhs]);
            Ok(term)
        },
        "lam" => {
            let params: Vec<Id> = node
                .children_by_field_name("params", &mut  cursor)
                .map(|x| node_text(&x, src))
                .collect::<Result<Vec<Id>>>()?;

            let body = node.child_by_field_name("body").ok_or(anyhow::format_err!("could not find field 'body' in lambda node"))?;
            let body = to_ast(body, src)?;

            let term = Term::Lam(params, Rc::new(body));
            Ok(term)
        },
        "access" => {
            let term = node.child_by_field_name("term").ok_or(anyhow::format_err!("could not find field 'term' in access node"))?;
            let term = to_ast(term, src)?;

            let property = node.child_by_field_name("property").ok_or(anyhow::format_err!("could not find field 'property' in access node"))?;
            let property = node_text(&property, src)?;

            let term = Term::Access(Rc::new(term), property);
            Ok(term)
        },
        "block" => {
            

            let term = node.child_by_field_name("term").ok_or(anyhow::format_err!("could not find field 'term' in access node"))?;
            let mut term = to_ast(term, src)?;

            let mut cursor = node.walk();
            let stmts: Vec<Node> = node.children_by_field_name("statement", &mut cursor).collect();
            for stmt in stmts.iter().rev() {
                match stmt.kind() {
                    "def" => {
                        let lhs = stmt.child_by_field_name("lhs").ok_or(anyhow::format_err!("could not find field 'lhs' in statement node"))?;
                        let lhs = node_text(&lhs, src)?;
                        
                        let rhs = stmt.child_by_field_name("rhs").ok_or(anyhow::format_err!("could not find field 'rhs' in statement node"))?;
                        let rhs = to_ast(rhs, src)?;

                        term = Term::Block(vec![(lhs, rhs)], Rc::new(term));
                    },
                    "bind" => {
                        let lhs = stmt.child_by_field_name("lhs");
                        let lhs = if let Some(lhs) = lhs {node_text(&lhs, src)?} else {"_".to_string()} ;
                        
                        let rhs = stmt.child_by_field_name("rhs").ok_or(anyhow::format_err!("could not find field 'rhs' in statement node"))?;
                        let rhs = to_ast(rhs, src)?;

                        term = Term::App(Rc::new(Term::Var("$".to_string())), vec![rhs, Term::Lam(vec![lhs], Rc::new(term))]);
                    },
                    _ => bail!("invalid statement kind {}", stmt.kind())
                }
            }
            
            Ok(term)
        },
        _ => Err(anyhow::format_err!("Unknown ast type '{}'", kind))
    }
}
