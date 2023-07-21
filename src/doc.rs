use std::{fs::File, os::fd::AsRawFd, sync::Arc};

use colored::Colorize;
use indexmap::IndexMap;
use ropey::Rope;
use tower_lsp::{
    jsonrpc,
    lsp_types::{Position, Range, TextDocumentContentChangeEvent},
};
use tree_sitter::{InputEdit, Parser, Point, Tree, Node};
use tree_sitter_fun::language;
use url::Url;

use crate::{module::Module, utils::{scan_tree, node_text}};

const PARSE_CHUNK_SIZE: usize = 128;

#[derive(Clone, Debug)]
pub struct ParseError {
    pub node_id: usize,
    pub message: String
}

#[derive(Clone, Debug)]
pub struct SyntaxNode {
    pub id: usize,
    pub kind: Option<String>,
    pub range: Range,
    pub length: u32,
    pub leaf: bool,
}

impl SyntaxNode {
    pub fn new(node: &Node) -> SyntaxNode {
        let r = node.range();
        SyntaxNode {
            id: node.id(),
            kind: if node.is_named() { Some(node.kind().into()) } else {None},
            range: to_lsp_range(&r),
            length: (r.end_byte - r.start_byte) as u32,
            leaf: node.child_count() == 0
        }
    }
}

pub struct Doc {
    pub url: Url,
    parser: Parser,
    src: Rope,
    tree: Tree,
    parse_errors: Vec<ParseError>,
    module: Result<Arc<Module>, ParseError>,
    nodes: IndexMap<usize, SyntaxNode>
}

pub fn to_lsp_range(r: &tree_sitter::Range) -> Range {
    Range {
        start: Position {
            line: r.start_point.row as u32,
            character: r.start_point.column as u32,
        },
        end: Position {
            line: r.end_point.row as u32,
            character: r.end_point.column as u32,
        },
    }
}

pub fn format_point(p: Position) -> String {
    format!("{}:{}", p.line, p.character + 1)
}

pub fn format_range(range: Range) -> String {
    format!("[{}-{}]", format_point(range.start), format_point(range.end))
}

impl Doc {
    pub fn new(url: Url, src: &str) -> jsonrpc::Result<Doc> {
        let mut parser = Parser::new();
        parser.set_language(language()).map_err(|err| jsonrpc::Error { 
            code: jsonrpc::ErrorCode::InternalError, 
            message: format!("could not initialize parser: {}", err), 
            data: None 
        })?;

        let tree = parser.parse(src, None).ok_or(jsonrpc::Error { 
            code: jsonrpc::ErrorCode::ParseError, 
            message: "could not parse source".into(), 
            data: None
        })?;

        let src = Rope::from_str(src);

        let module = Module::new(&tree, &src).map(|x| Arc::new(x));

        let mut doc = Doc {
            url,
            parser,
            tree,
            src,
            module,
            parse_errors: vec![],
            nodes: IndexMap::new()
        };

        doc.update();

        Ok(doc)
    }

    pub fn edit(&mut self, change: TextDocumentContentChangeEvent) -> jsonrpc::Result<Vec<Range>> {
        let TextDocumentContentChangeEvent {
            range,
            range_length: _,
            text,
        } = change;
        let Range { start, end } = range.unwrap();
        let Position {
            line: start_line,
            character: start_col,
        } = start;
        let Position {
            line: end_line,
            character: end_col,
        } = end;

        
        let start_char_idx = self.src.line_to_char(start_line as usize) + start_col as usize;
        let byte_start = self.src.char_to_byte(start_char_idx);

        let end_char_idx = self.src.line_to_char(end_line as usize) + end_col as usize;
        let byte_end = self.src.char_to_byte(end_char_idx);

        self.src.remove(byte_start..byte_end);
        self.src.insert(byte_start, &text);

        let n_lines = text.lines().count();
        let last_line = text.lines().last().unwrap_or("");

        self.tree.edit(&InputEdit {
            start_byte: byte_start,
            old_end_byte: byte_end,
            new_end_byte: byte_start + text.len(),
            start_position: Point {
                row: start_line as usize,
                column: start_col as usize,
            },
            old_end_position: Point {
                row: end_line as usize,
                column: end_col as usize,
            },
            new_end_position: Point {
                row: start_line as usize + n_lines,
                column: if n_lines > 0 {
                    last_line.len()
                } else {
                    start_col as usize + last_line.len()
                },
            },
        });

        let new_tree = self
            .parser
            .parse_with(
                &mut |offset, _| {
                    if offset >= self.src.len_bytes() {
                        return "";
                    }
                    let mut end = offset + PARSE_CHUNK_SIZE;
                    if end > self.src.len_bytes() {
                        end = self.src.len_bytes()
                    }
                    let slice = self.src.byte_slice(offset..end);
                    slice.as_str().unwrap()
                },
                Some(&self.tree),
            )
            .ok_or(jsonrpc::Error {
                code: jsonrpc::ErrorCode::ParseError,
                message: "could not parse source".to_string(),
                data: None,
            })?;

        let changed: Vec<Range> = new_tree
            .changed_ranges(&self.tree)
            .map(|r| to_lsp_range(&r))
            .collect();
        self.tree = new_tree;

        self.update();

        Ok(changed)
    }

    fn update(&mut self) {
        self.nodes.clear();
        self.parse_errors.clear();
        scan_tree(&self.tree, &mut |node| {
            let syntax_node = SyntaxNode::new(node);
            self.nodes.insert(syntax_node.id, syntax_node);

            if node.is_error() {
                let children: Vec<String> = node.children(&mut node.walk()).map(|node| node.to_sexp()).collect();
                self.parse_errors.push(ParseError {
                    node_id: node.id(),
                    message: format!("Unexpected {}", children.join(" ")),
                })
            } 
    
            if node.is_missing() {
                self.parse_errors.push(ParseError {
                    node_id: node.id(),
                    message: format!("Missing `{}`", node.to_sexp()),
                })
            }
        });

        self.module = Module::new(&self.tree, &self.src).map(|x| Arc::new(x))
    }

    pub fn get_node(&self, id: usize) -> Option<&SyntaxNode> {
        self.nodes.get(&id)
    }

    pub fn leaves(&self) -> impl Iterator<Item = &SyntaxNode> {
        self.nodes.values().filter(|x| x.leaf)
    }

    pub fn get_byte_offset(&self, pos: Position) -> usize {
        self.src.line_to_byte(pos.line as usize) + pos.character as usize
    }

    pub fn find_path(&self, pos: Position) -> Vec<SyntaxNode> {
        let offset = self.get_byte_offset(pos);
        let mut path = vec![];
        let mut node = self.tree.root_node();
        path.push(SyntaxNode::new(&node));
        while node.child_count() > 0 {   
            if let Some(child) = node.children(&mut node.walk()).find(|child| child.byte_range().contains(&offset)) {
                node = child;
                path.push(SyntaxNode::new(&node));
            } else {
                break;
            }
        };

        path
    }

    pub fn find_node_named(&self, pos: Position, name: String) -> Option<SyntaxNode> {
        self.find_path(pos).into_iter().rev().find(|n| if let Some(kind) = &n.kind { *kind == name } else {false})
    }

    pub fn find_named_node(&self, pos: Position) -> Option<SyntaxNode> {
        self.find_path(pos).into_iter().rev().find(|n| n.kind.is_some())
    }

    pub fn find_node(&self, pos: Position) -> Option<SyntaxNode> {
        self.find_path(pos).into_iter().last()
    }

    pub fn parse_errors(&self) -> Vec<ParseError> {
        let mut errs = self.parse_errors.clone();
        if let Err(err) = &self.module {
            errs.push(err.clone())
        }
        errs
    }

    pub fn module(&self) -> Option<Arc<Module>> {
        self.module.clone().ok()
    }

    pub fn get_text(&self, range: Range) -> &str {
        let start = self.get_byte_offset(range.start);
        let end = self.get_byte_offset(range.end);
        self.src.byte_slice(start..end).as_str().unwrap()
    }

    pub fn format_err(&self, node_id: usize, msg: &str) -> String {
        let node = self.get_node(node_id).unwrap();

        let lines: Vec<String> = (node.range.start.line..node.range.end.line+1).map(|line_idx| {
            let mut str = self.src.line(line_idx as usize).as_str().unwrap().to_string();
            if line_idx == node.range.start.line {
                let (start, end) = str.split_at(node.range.start.character as usize);
                str = format!("{}{}", start.bright_black(), end.red())
            } else if line_idx == node.range.end.line {
                let (start, end) = str.split_at(node.range.end.character as usize);
                str = format!("{}{}", start.red(), end.bright_black())
            } else {
                str = format!("{}", str.red())
            }

            format!("{: >3}| {}", line_idx + 1, str)
        }).collect();

        let msg = format!("ERROR: {}", msg).red();
        format!("{}\n   | {}",lines.join("\n"), msg).bright_black().to_string()
    }
}
