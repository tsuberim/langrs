use std::{collections::HashMap, fmt::Display, rc::Rc};

use anyhow::{anyhow, bail, Result};
use colored::Colorize;
use proptest::{prelude::{any, prop}, prop_oneof, strategy::Strategy};
use proptest_derive::Arbitrary;
use tree_sitter::{Node, Parser};
use tree_sitter_fun::{language, Expr, ExprVisitor, IntoExpr};

use crate::{value::Value, types::{Type, Scheme, Subst}};

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Num(u64),
    Str(String),
    Var(String),
    Cons(String, Rc<Term>),
    Case(Rc<Term>, Vec<(Pattern, Rc<Term>)>, Option<Rc<Term>>),
    App(Rc<Term>, Vec<Rc<Term>>),
    Abs(Vec<String>, Rc<Term>),
    Rec(HashMap<String, Rc<Term>>),
    Acc(Rc<Term>, String),
}

pub fn arb_term() -> impl Strategy<Value = Term> {
    let leaf = prop_oneof!["[a-z][a-zA-Z0-9_]".prop_map(Term::Var),];

    leaf.prop_recursive(
        8,   // 8 levels deep
        256, // Shoot for maximum size of 256 nodes
        10,  // We put up to 10 items per collection
        |inner| {
            prop_oneof![
                ("[A-Z][a-zA-Z0-9_]{1}", inner.clone())
                    .prop_map(|(name, inner)| Term::Cons(name, Rc::new(inner))),
                (inner.clone(), "[a-z][a-zA-Z0-9_]{1}", )
                    .prop_map(|(inner, name)| Term::Acc(Rc::new(inner), name)),
                (inner.clone(), prop::collection::vec(inner.clone(), 1..4))
                    .prop_map(|(f, args)| Term::App(Rc::new(f), args.into_iter().map(Rc::new).collect())),
                // (inner.clone(), prop::collection::vec(inner.clone(), 0..10), inner.clone())
                //     .prop_map(|(e, args, otherwise)| Term::Case(Box::new(e), cases, otherwise)),
                (prop::collection::vec("[a-z][a-zA-Z0-9_]{1}", 1..4), inner.clone())
                    .prop_map(|(names, inner)| Term::Abs(names, Rc::new(inner))),
                prop::collection::hash_map("[a-z][a-zA-Z0-9_]{1}", inner.clone(), 0..4)
                    .prop_map(|entries| Term::Rec(entries.into_iter().map(|(k, v)| (k, Rc::new(v))).collect()))
            ]
        },
    )
}

#[derive(Debug, Clone, PartialEq, Arbitrary)]
pub enum Pattern {
    Cons(String, String),
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Pattern::Cons(name, var) = self;
        write!(f, "{}({})", name.red(), var.green())
    }
}

impl Term {
    pub fn free_vars(&self) -> HashMap<String, u64> {
        match self {
            Term::Num(_) | Term::Str(_) => HashMap::new(),
            Term::Var(name) => {
                let mut map = HashMap::new();
                map.insert(name.clone(), 1);
                map
            }
            Term::App(f, args) => {
                let mut f = f.free_vars();
                for arg in args.iter() {
                    for (k, n) in arg.free_vars().iter() {
                        *f.entry(k.into()).or_insert(0) += n;
                    }
                }
                f
            }
            Term::Abs(args, body) => {
                let mut map = body.free_vars();
                for arg in args.iter() {
                    map.remove(arg);
                }
                map
            }
            Term::Rec(entries) => {
                let mut map = HashMap::new();
                for (_, term) in entries.iter() {
                    for (k, n) in term.free_vars().iter() {
                        *map.entry(k.into()).or_insert(0) += n;
                    }
                }
                map
            }
            Term::Acc(term, _) => term.free_vars(),
            Term::Cons(_, content) => content.free_vars(),
            Term::Case(expr, cases, otherwise) => {
                let mut f = expr.free_vars();

                for (Pattern::Cons(_, name), expr) in cases.iter() {
                    let mut vars = expr.free_vars();
                    vars.remove(name);
                    for (k, n) in vars.iter() {
                        *f.entry(k.into()).or_insert(0) += n;
                    }
                }

                if let Some(t) = otherwise {
                    for (k, n) in t.free_vars().iter() {
                        *f.entry(k.into()).or_insert(0) += n;
                    }
                }

                f
            }
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Num(val) => write!(f, "{}", val),
            Term::Str(val) => write!(f, "'{}'", val),
            Term::Var(name) => write!(f, "{}", name.green()),
            Term::App(func, args) => write!(
                f,
                "({})({})",
                func,
                args.iter()
                    .map(|term| format!("{}", term))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Term::Abs(args, body) => write!(f, "\\{} -> {}", args.join(", ").green(), body),
            Term::Rec(entries) => write!(
                f,
                "{{{}}}",
                entries
                    .iter()
                    .map(|(name, term)| format!("{}: {}", name.bright_black(), term))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Term::Acc(term, prop) => write!(f, "({}).{}", term, prop),
            Term::Cons(name, content) => write!(f, "{}({})", name.bright_black(), content),
            Term::Case(expr, cases, otherwise) => {
                write!(f, "when {} is ", expr)?;
                for (ptn, e) in cases.iter() {
                    write!(f, "{} -> {}, ", ptn, e)?;
                }
                if let Some(e) = otherwise {
                    write!(f, "otherwise {}", e)?;
                }
                Ok(())
            }
        }
    }
}

struct MakeTerm<'a> {
    src: &'a str,
}

impl MakeTerm<'_> {
    fn text(&self, node: &Node) -> String {
        node.utf8_text(self.src.as_bytes()).unwrap().into()
    }
}
impl ExprVisitor<Node<'_>, Term> for MakeTerm<'_> {
    fn visit_id(&mut self, node: Node) -> Term {
        Term::Var(self.text(&node))
    }

    fn visit_parens(&mut self, _node: Node, expr: Term) -> Term {
        expr
    }

    fn visit_lambda(&mut self, _node: Node, arguments: Vec<Node>, body: Term) -> Term {
        Term::Abs(
            arguments.iter().map(|node| self.text(node)).collect(),
            Rc::new(body),
        )
    }

    fn visit_application(&mut self, _node: Node, arguments: Vec<Term>, function: Term) -> Term {
        Term::App(Rc::new(function), arguments.into_iter().map(Rc::new).collect())
    }

    fn visit_match(
        &mut self,
        _node: Node,
        consequences: Vec<Term>,
        expression: Term,
        otherwise: Option<Term>,
        patterns: Vec<Node>,
    ) -> Term {
        let cases = patterns
            .iter()
            .map(|node| -> Pattern {
                match node.into_expr() {
                    Expr::Constructor(var, name) => Pattern::Cons(
                        self.text(&name),
                        match var.into_expr() {
                            Expr::Parens(x) => self.text(&x),
                            _ => unimplemented!(),
                        },
                    ),
                    _ => unimplemented!(),
                }
            })
            .zip(consequences.into_iter().map(Rc::new))
            .collect();
        Term::Case(Rc::new(expression), cases, otherwise.map(Rc::new))
    }

    fn visit_record(&mut self, _node: Node, keys: Vec<Node>, values: Vec<Term>) -> Term {
        Term::Rec(
            keys.iter()
                .map(|node| self.text(&node))
                .zip(values.into_iter().map(Rc::new))
                .collect(),
        )
    }

    fn visit_string(&mut self, node: Node) -> Term {
        let text = self.text(&node);
        Term::Str(text[1..text.len() - 1].into())
    }

    fn visit_access(&mut self, _node: Node, expression: Term, property: Node) -> Term {
        Term::Acc(Rc::new(expression), self.text(&property))
    }

    fn visit_number(&mut self, node: Node) -> Term {
        let text = self.text(&node);
        Term::Num(text.parse().unwrap())
    }

    fn visit_constructor(&mut self, _node: Node, content: Term, name: Node) -> Term {
        Term::Cons(self.text(&name), Rc::new(content))
    }
}

pub fn parse(src: &str) -> Result<Term> {
    let mut parser = Parser::new();
    parser
        .set_language(language())
        .expect("Must be able to set parser");

    let tree = parser
        .parse(src, None)
        .ok_or(anyhow!("Cannot parse line"))?;
    let root = tree.root_node();

    let root = root.named_child(0).ok_or(anyhow!("Expected first child"))?;
    Ok(MakeTerm { src }.visit(root))
}
