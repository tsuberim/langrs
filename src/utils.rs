use ropey::Rope;
use tree_sitter::{Node, Tree};

const ALPHABET: &str = "abcdefghijklmnopqrstuvwxyz";

pub struct Namer {
    letter: usize,
    index: usize
}

impl Namer {
    pub fn new() -> Namer {
        return Namer { letter: 0, index: 0 }
    }

    pub fn name(&mut self) -> String {
        let new_id = if self.index != 0 {
            format!("{}{}", ALPHABET.chars().nth(self.letter).unwrap(), self.index) 
        } else {
            format!("{}", ALPHABET.chars().nth(self.letter).unwrap()) 
        };

        self.letter += 1;
        if self.letter >= ALPHABET.len() {
            self.letter = 0;
            self.index += 1;
        }
        new_id
    }
}

pub fn node_text(node: &Node, src: &Rope) -> String {
    let range = node.byte_range();
    let slice = src.byte_slice(range);
    let str = slice.as_str().unwrap();
    return str.to_string();
}

pub fn scan_tree(tree: &Tree, cb: &mut impl FnMut(&Node) -> ()) {
    let mut cursor = tree.walk();
    loop {
        cb(&cursor.node());
        while cursor.goto_first_child() {cb(&cursor.node());}

        while !cursor.goto_next_sibling() {
            if !cursor.goto_parent() {
                return;
            }
        }
    }
}