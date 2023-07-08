use std::sync::Arc;

use im::HashMap;
use ropey::Rope;
use tree_sitter::{Tree};

use crate::{
    builtins::{get_context, get_type_env},
    term::{to_ast, Term},
    typing::{infer, Type, TypeError},
};

pub struct Module {
    terms: HashMap<usize, Arc<Term>>,
    term: Arc<Term>,
    types: HashMap<usize, Type>,
    type_errors: Vec<TypeError>
}

impl Module {
    pub fn new(tree: &Tree, src: &Rope) -> Module {
        let mut terms = HashMap::new();
        let mut types = HashMap::new();

        let term = to_ast(tree.root_node(), src, &mut terms);

        let context = get_context();
        let type_env = get_type_env(&context);

        let (typ, type_errors) = infer(&term, &type_env, &mut types);

        Module { term, terms, types, type_errors }
    }

    pub fn type_errors(&self) -> &Vec<TypeError> {
        &self.type_errors
    }

    pub fn get_term(&self, id: usize) -> Option<&Arc<Term>> {
        self.terms.get(&id)
    }

    pub fn get_type(&self, id: usize) -> Option<&Type> {
        self.types.get(&id)
    }
}
