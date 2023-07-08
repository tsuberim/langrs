use std::{fmt::{Display}, sync::Arc};
use colored::Colorize;
use im::{HashMap, HashSet, hashset, hashmap};
use ropey::Rope;
use tree_sitter::Node;

use crate::{typing::{to_type_ast, ForAll}, utils::node_text};

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
struct Case(Id, Id, Arc<Term>);

#[derive(Debug, Clone)]
pub enum Term {
    Lit(usize, Lit),
    Var(usize, Id),
    Tag(usize, Id, Arc<Term>),
    Record(usize, HashMap<Id, Arc<Term>>),
    Match(usize, Arc<Term>, HashMap<Id, (Id, Arc<Term>)>, Option<Arc<Term>>),
    List(usize, Vec<Arc<Term>>),
    Access(usize, Arc<Term>, Id),
    App(usize, Arc<Term>, Vec<Arc<Term>>),
    Block(usize, HashMap<Id, ForAll>, Vec<(Id, Arc<Term>)>, Arc<Term>), // like a let
    Lam(usize, Vec<Id>, Arc<Term>)
}


impl Term {
    pub fn id(&self) -> usize {
        match self {
            Term::Lit(id, _) => *id,
            Term::Var(id, _) => *id,
            Term::Tag(id, _, _) => *id,
            Term::Record(id, _) => *id,
            Term::Match(id, _, _, _) => *id,
            Term::List(id, _) => *id,
            Term::Access(id, _, _) => *id,
            Term::App(id, _, _) => *id,
            Term::Block(id, _, _, _) => *id,
            Term::Lam(id, _, _) => *id,
        }
    }

    pub fn free_vars(&self) -> HashSet<Id> {
        match self {
            Term::Lit(_, _) => hashset![],
            Term::Var(_, v) => hashset![v.clone()],
            Term::Tag(_, _, payload) => payload.free_vars(),
            Term::Record(_, items) => items.into_iter().map(|(_k, v)| v.free_vars()).fold(hashset![], HashSet::union),
            Term::Match(_, term, cases, default) => {
                let mut fv: HashSet<String> = term.free_vars();
                for (_, (id, result)) in cases {
                    fv = fv.union(result.free_vars().without(id))
                }
                if let Some(default) = default {
                    fv = fv.union(default.free_vars());
                }
                fv
            },
            Term::List(_, items) => items.into_iter().map(|x| x.free_vars()).fold(hashset![], HashSet::union),
            Term::Access(_, x, _prop) => x.free_vars(),
            Term::App(_, f, args) => f.free_vars().union(args.into_iter().map(|x| x.free_vars()).fold(hashset![], HashSet::union)),
            Term::Block(_, _, defs, term) => {
                defs
                    .iter()
                    .fold(term.free_vars(), |acc, (id, term)| acc.without(id).union(term.free_vars()))
            },
            Term::Lam(_, params, body) => {
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
            Term::Lit(_, lit) => lit.fmt(f),
            Term::Var(_, name) => write!(f, "{}", name.green()),
            Term::Tag(_, name, payload) => {
                if let Term::Record(_, data) = payload.as_ref() {
                    if data.len() == 0 {
                        return write!(f, "{}", name.blue())
                    }
                }
                write!(f, "{}({})", name.blue(), payload)
            },
            Term::Record(_, data) => {
                let pairs: Vec<String> = data.iter().map(|(k ,v)| format!("{}: {}", k, v)).collect();
                write!(f, "{{{}}}", pairs.join(", "))
            },
            Term::List(_, items) => {
                let pairs: Vec<String> = items.iter().map(|item| format!("{}", item)).collect();
                write!(f, "[{}]", pairs.join(", "))
            }
            Term::Match(_, term, cases, default) => {
                let cases: Vec<String> = cases.iter().map(|(tag, (id, result))| format!("{} {} -> {}", tag, id, result)).collect();
                let default = if let Some(term) = default { format!(" else {}", term) } else { "".to_string() };
                write!(f, "when {} is {}{}", term, cases.join("; "), default)
            },
            Term::App(_, function, args) => {
                let args: Vec<String> = args.iter().map(|arg| format!("{}", arg)).collect();
                write!(f, "({})({})", function, args.join(", "))
            },
            Term::Lam(_, params, body) => write!(f, "\\{} -> {}", params.join(", "), body),
            Term::Access(_, term, property) => write!(f, "{}.{}", term, property),
            Term::Block(_, _, defs, term) => {
                let defs: Vec<String> = defs.iter().map(|(k, v)| format!("{} = {}", k, v)).collect();
                write!(f, "({}\n{})", defs.join("\n"), term)
            },
        }
    }
}

pub fn to_ast(node: Node, src: &Rope, cache: &mut HashMap<usize, Arc<Term>>) -> Arc<Term> {
    let id = node.id();
    let kind = node.kind();

    let term = match kind {
        "source_file" => {
            let term = node.child_by_field_name("term").unwrap();
            to_ast(term, src, cache)
        },
        "parens" => {
            let term = node.child_by_field_name("term").unwrap();
            to_ast(term, src,cache)
        },
        "str_lit" => {
            let text = node_text(&node, src);
            let lit = Lit::Str(text);
            let term = Term::Lit(id, lit);
            Arc::new(term)
        },
        "str" => {
            let mut cursor = node.walk();
            let terms = node
                .named_children(&mut  cursor)
                .map(|x| to_ast(x, src, cache))
                .collect::<Vec<Arc<Term>>>();

            if terms.len() == 1 {
                return Arc::clone(terms.get(0).unwrap())
            }

            let t = terms
                .into_iter()
                .fold(
                    Arc::new(Term::Lit(id, Lit::Str("".to_string()))), 
                    |acc, term| 
                        Arc::new(Term::App(id, 
                            Arc::new(Term::Var(id, "++".to_string())), 
                            vec![acc, term]
                        ))
                );
            t
        }
        "num" => {
            let lit = Lit::Num(node_text(&node, src).parse().unwrap());
            let term = Term::Lit(id, lit);
            Arc::new(term)
        },
        "id" => {
            let term = Term::Var(id, node_text(&node, src));
            Arc::new(term)
        },
        "sym" => {
            let term = Term::Var(id, node_text(&node, src));
            Arc::new(term)
        },
        "tag" => {
            let payload = node.child_by_field_name("payload");
            
            let payload = if let Some(node) = payload { to_ast(node, src, cache) } else {Arc::new(Term::Record(id, HashMap::new()))}; 

            let name = node.child_by_field_name("name").unwrap();
            let name = node_text(&name, src);
            let term = Term::Tag(id, name, payload);
            Arc::new(term)
        },
        "record" => {
            let mut cursor = node.walk();
            let keys: Vec<String> = node
                .children_by_field_name("keys", &mut  cursor)
                .map(|x| node_text(&x, src))
                .collect();

            let mut cursor = node.walk();
            let values: Vec<Arc<Term>> = node
                .children_by_field_name("values", &mut  cursor)
                .map(|x| to_ast(x, src, cache))
                .collect();
            
            Arc::new(Term::Record(id, 
                keys
                    .into_iter()
                    .zip(values.into_iter())
                    .collect()
            ))
        },
        "match" => {
            let term = node.child_by_field_name("term").unwrap();
            let term = to_ast(term, src, cache);

            let mut cursor = node.walk();
            let tags: Vec<String> = node
                .children_by_field_name("case_tag", &mut  cursor)
                .map(|x| node_text(&x, src))
                .collect();

            let mut cursor = node.walk();
            let ids: Vec<String> = node
                .children_by_field_name("case_id", &mut  cursor)
                .map(|x| node_text(&x, src))
                .collect();

            let mut cursor = node.walk();
            let results: Vec<Arc<Term>> = node
                .children_by_field_name("case_result", &mut  cursor)
                .map(|x| to_ast(x, src, cache))
                .collect();


            let mut cases: HashMap<Id, (Id, Arc<Term>)> = HashMap::new();
            for (tag, (id, term)) in tags.into_iter().zip(ids.into_iter().zip(results.into_iter())) {
                cases.insert(tag, (id, term));
            }
            
            
            let mut e = None;
            let else_term = node.child_by_field_name("else");
            if let Some(node) = else_term {
                let term = to_ast(node, src, cache);
                e = Some(term)
            }

            Arc::new(Term::Match(id, term, cases, e))
        }
        "list" => {
            let mut cursor = node.walk();
            let items = node
                .children_by_field_name("items", &mut  cursor)
                .map(|x| to_ast(x, src, cache))
                .collect();
            Arc::new(Term::List(id, items))
        },
        "app" => {
            let f = node.child_by_field_name("f").unwrap();
            let f = to_ast(f, src, cache);

            let mut cursor = node.walk();
            let args = node
                .children_by_field_name("args", &mut  cursor)
                .map(|x| to_ast(x, src, cache))
                .collect();

            let term = Term::App(id, f, args);
            Arc::new(term)
        },
        "infix_app" => {
            let lhs = node.child_by_field_name("lhs").unwrap();
            let lhs = to_ast(lhs, src, cache);

            let rhs = node.child_by_field_name("rhs").unwrap();
            let rhs = to_ast(rhs, src, cache);

            let f = node.child_by_field_name("f").unwrap();
            let f = node_text(&f, src);

            let term = Term::App(id, Arc::new(Term::Var(id, f)), vec![lhs, rhs]);
            Arc::new(term)
        },
        "lam" => {
            let mut cursor = node.walk();
            let params: Vec<Id> = node
                .children_by_field_name("params", &mut  cursor)
                .map(|x| node_text(&x, src))
                .collect();

            let body = node.child_by_field_name("body").unwrap();
            let body = to_ast(body, src, cache);

            let term = Term::Lam(id, params, body);
            Arc::new(term)
        },
        "access" => {
            let term = node.child_by_field_name("term").unwrap();
            let term = to_ast(term, src, cache);

            let property = node.child_by_field_name("property").unwrap();
            let property = node_text(&property, src);

            let term = Term::Access(id, term, property);
            Arc::new(term)
        },
        "curied_access" => {
            let property = node.child_by_field_name("property").unwrap();
            let property = node_text(&property, src);

            let term = Term::Lam(id, vec!["x".to_string()], Arc::new(Term::Access(id, Arc::new(Term::Var(id, "x".to_string())), property)));
            Arc::new(term)
        },
        "block" => {
            let term = node.child_by_field_name("term").unwrap();
            let mut term = to_ast(term, src, cache);

            let mut cursor = node.walk();
            let stmts: Vec<Node> = node.children_by_field_name("statement", &mut cursor).collect();

            let mut defs = vec![];
            let mut type_defs = hashmap!{};

            for stmt in stmts.iter().rev() {
                match stmt.kind() {
                    "def" => {
                        let lhs = stmt.child_by_field_name("lhs").unwrap();
                        let lhs = node_text(&lhs, src);
                        
                        let rhs = stmt.child_by_field_name("rhs").unwrap();
                        let rhs = to_ast(rhs, src, cache);

                        defs.push((lhs, rhs));
                    },
                    "type_def" => {
                        let lhs = stmt.child_by_field_name("lhs").unwrap();
                        let lhs = node_text(&lhs, src);
                        
                        let rhs = stmt.child_by_field_name("rhs").unwrap();
                        let rhs = to_type_ast(rhs, src);
                        let rhs = ForAll(rhs.free_type_vars().keys().cloned().collect(), rhs);

                        type_defs.insert(lhs, rhs);
                    },
                    "bind" => {
                        if defs.len() > 0 || type_defs.len() > 0 {
                            term = Arc::new(Term::Block(id, type_defs, defs.iter().cloned().rev().collect(), term));
                            defs = vec![];
                            type_defs = hashmap!{};
                        }

                        let lhs = stmt.child_by_field_name("lhs");
                        let lhs = if let Some(lhs) = lhs {node_text(&lhs, src)} else {"_".to_string()} ;
                        
                        let rhs = stmt.child_by_field_name("rhs").unwrap();
                        let rhs = to_ast(rhs, src, cache);

                        term = Arc::new(Term::App(id, Arc::new(Term::Var(id, "$".to_string())), vec![rhs, Arc::new(Term::Lam(id, vec![lhs], term))]));
                    },
                    _ => panic!("invalid statement kind {}", stmt.kind())
                }
            }

            if defs.len() > 0 || type_defs.len() > 0 {
                term = Arc::new(Term::Block(id, type_defs.clone(), defs.iter().cloned().rev().collect(), term));
            }
            
            term
        },
        _ => panic!("Unknown ast type '{}'", kind)
    };
    
    cache.insert(node.id(), Arc::clone(&term));

    term
}
