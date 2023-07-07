use std::sync::Arc;

use dashmap::DashMap;

use im::HashMap;
use ropey::Rope;
use tower_lsp::jsonrpc::{self, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tree_sitter::{InputEdit, Node, Parser, Point, Tree};
use tree_sitter_fun::language;

use crate::term::{to_ast, Term};
use crate::typing::{infer, Type};


const PARSE_CHUNK_SIZE: usize = 128;
pub struct Doc {
    pub id: Url,
    pub parser: Parser,
    pub tree: Tree,
    pub rope: Rope,
    pub terms: HashMap<usize, Arc<Term>>,
    pub ast: Option<Arc<Term>>,

    pub types: HashMap<usize, Type>,
}

impl Doc {
    pub fn new(id: Url, src: &str) -> Result<Doc> {
        let mut parser = Parser::new();
        parser
            .set_language(language())
            .map_err(|e| jsonrpc::Error {
                code: jsonrpc::ErrorCode::InternalError,
                message: "could not construct parser".to_string(),
                data: None,
            })?;

        let tree = parser.parse(src, None).ok_or(jsonrpc::Error {
            code: jsonrpc::ErrorCode::ParseError,
            message: "parse error".to_string(),
            data: None,
        })?;
        let rope = Rope::from_str(src);

        let mut terms = HashMap::new();

        let ast = to_ast(tree.root_node(), &rope, &mut terms).ok();

        eprintln!("{:?}", ast);

        let mut types = HashMap::new();

        if let Some(term) = &ast {
            let typ = infer(&term, &HashMap::new(), &mut types).ok();
            eprintln!("{:?}", types);
            dbg!(types.clone());
        }

        Ok(Doc { 
            id,
            parser, 
            tree, 
            rope,
            terms,
            ast,
            types,
        })
    }

    pub fn edit(&mut self, change: TextDocumentContentChangeEvent) -> Result<Vec<Range>> {
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

        let byte_start = self.rope.line_to_byte(start_line as usize) + start_col as usize;
        let byte_end = self.rope.line_to_byte(end_line as usize) + end_col as usize;

        self.rope.remove(byte_start..byte_end);
        self.rope.insert(byte_start, &text);

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
                    if offset >= self.rope.len_bytes() {
                        return "";
                    }
                    let mut end = offset + PARSE_CHUNK_SIZE;
                    if end > self.rope.len_bytes() {
                        end = self.rope.len_bytes()
                    }
                    let slice = self.rope.byte_slice(offset..end);
                    slice.as_str().unwrap()
                },
                Some(&self.tree),
            )
            .ok_or(jsonrpc::Error {
                code: jsonrpc::ErrorCode::ParseError,
                message: "parse error".to_string(),
                data: None,
            })?;

        let changed: Vec<Range> = new_tree
            .changed_ranges(&self.tree)
            .map(|r| Range {
                start: Position {
                    line: r.start_point.row as u32,
                    character: r.start_point.column as u32,
                },
                end: Position {
                    line: r.end_point.row as u32,
                    character: r.end_point.column as u32,
                },
            })
            .collect();
        self.tree = new_tree;

        self.terms.clear();
        let term = to_ast(self.tree.root_node(), &self.rope, &mut self.terms).ok();
        self.ast = term;

        if let Some(term) = &self.ast {   
            self.types.clear();
            let typ = infer(&term, &HashMap::new(), &mut self.types).ok();
        }

        Ok(changed)
    }
}

pub struct Backend {
    client: Client,
    docs: DashMap<Url, Doc>,
}

impl Backend {
    pub fn new(client: Client) -> anyhow::Result<Backend> {
        let mut parser = Parser::new();
        parser.set_language(language())?;
        Ok(Backend {
            client,
            docs: DashMap::new(),
        })
    }

    async fn update_diagnostics(&self, doc: &Doc, version: i32) {
        let mut diagnostics = vec![];
        scan_tree(&doc.tree, &mut |node| {
            let range = node.range();

            if node.is_missing() {
                // let text = node_text(&node, &doc.rope).unwrap();
                diagnostics.push(Diagnostic{ 
                    range: Range { 
                        start: Position { line: range.start_point.row as u32, character: range.start_point.column as u32 }, 
                        end: Position { line: range.end_point.row as u32, character: range.end_point.column as u32 },
                    }, 
                    message: format!("Missing: {}", node.kind()),
                    ..Default::default()
                });
            }   

            if node.is_error() {
                // let text = node_text(&node, &doc.rope).unwrap();
                diagnostics.push(Diagnostic{ 
                    range: Range { 
                        start: Position { line: range.start_point.row as u32, character: range.start_point.column as u32 }, 
                        end: Position { line: range.end_point.row as u32, character: range.end_point.column as u32 },
                    }, 
                    message: format!("Unexpected: {}", node.to_sexp()),
                    ..Default::default()
                });
            } 
        });
        self.client.log_message(MessageType::INFO, "Publishing diagnostics").await;
        self.client.publish_diagnostics(doc.id.clone(), diagnostics, Some(version)).await;
        
    }

}

fn scan_tree(tree: &Tree, cb: &mut impl FnMut(Node) -> ()) {
    let mut cursor = tree.walk();
    loop {
        cb(cursor.node());
        while cursor.goto_first_child() {cb(cursor.node());}
        cb(cursor.node());

        while !cursor.goto_next_sibling() {
            if !cursor.goto_parent() {
                return;
            }
        }
    }
}

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::COMMENT,
    SemanticTokenType::NUMBER,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::TYPE,
    SemanticTokenType::CLASS,
];

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string(), "(".to_string()]),
                    ..Default::default()
                }),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::INCREMENTAL),
                        ..Default::default()
                    },
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("fun".to_string()),
                                        scheme: Some("file".to_string()),
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPE.into(),
                                    token_modifiers: vec![],
                                },
                                range: Some(false),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let doc_id = params.text_document.uri;
        let src = params.text_document.text;
        let doc = Doc::new(doc_id, src.as_str()).unwrap();
        self.update_diagnostics(&doc, params.text_document.version).await;
        self.docs.insert(doc.id.clone(), doc);
    }


    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client.log_message(MessageType::INFO, "did_change").await;
        let doc_id = params.text_document.uri;
        if let Some(mut doc) = self.docs.get_mut(&doc_id) {
            for change in params.content_changes {
                doc.edit(change).unwrap();
            }
            self.client.log_message(MessageType::INFO, "diagnosing").await;
            self.update_diagnostics(&doc, params.text_document.version).await;
        } else {
            self.client
                .log_message(
                    MessageType::ERROR,
                    format!("unknown document changed! {}", doc_id),
                )
                .await;
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        self.client
            .log_message(MessageType::INFO, params.text_document.uri.to_string())
            .await;

        let url = params.text_document.uri;

        if let Some(doc) = self.docs.get(&url) {
            let mut tokens = vec![];

            let mut pre_line = 0;
            let mut pre_start = 0;
            scan_tree(&doc.tree, &mut |node| {
                if !node.is_named() || node.child_count() > 0 {
                    return
                }
                let range = node.range();

                // SemanticTokenType::FUNCTION,
                // SemanticTokenType::VARIABLE,
                // SemanticTokenType::STRING,
                // SemanticTokenType::COMMENT,
                // SemanticTokenType::NUMBER,
                // SemanticTokenType::KEYWORD,
                // SemanticTokenType::OPERATOR,
                // SemanticTokenType::PARAMETER,
                // SemanticTokenType::TYPE,

                let token_type = match node.kind() {
                    "num" => 4,
                    "str_lit" => 2,
                    "id" => 1,
                    "comment" => 3,
                    "sym" => 6,
                    "tag_id" => 8,
                    _ => 0,
                };

                let line = range.start_point.row as u32;
                let start = range.start_point.column as u32;
                let length = (range.end_byte - range.start_byte) as u32;

                let delta_line = line - pre_line;
                let delta_start = if delta_line == 0 {
                    start - pre_start
                } else {
                    start
                };
                tokens.push(SemanticToken {
                    delta_line,
                    delta_start,
                    length: length,
                    token_type: token_type,
                    token_modifiers_bitset: 0,
                });
                pre_line = line;
                pre_start = start;
            });

            self.client
                .log_message(MessageType::INFO, format!("{:?}", tokens))
                .await;

            Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens,
            })))
        } else {
            todo!()
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let doc_id = params.text_document_position_params.text_document.uri;

        if let Some(doc) = self.docs.get(&doc_id) {
            let pos = params.text_document_position_params.position;

            let offset_bytes = doc.rope.line_to_byte(pos.line as usize) + pos.character as usize;
            
            let mut term = None;
            scan_tree(&doc.tree, &mut |node| {
                if node.byte_range().contains(&offset_bytes) {
                    if let Some(t) = doc.terms.get(&node.id()) {
                        term = Some(t)
                    }
                }
            });

            if let Some(term) = term {
                if let Some(typ) = doc.types.get(&term.id()) {
                    return Ok(Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: format!(
                                "{} : {}",
                                term, 
                                typ
                            ),
                        }),
                        range: None,
                    }))
                }
            }
             
            Ok(None)
        } else {
            self.client
                .log_message(MessageType::ERROR, format!("could not find doc {}", doc_id))
                .await;
            Ok(None)
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let doc_id = params.text_document_position.text_document.uri;

        if let Some(doc) = self.docs.get(&doc_id) {
            let pos = params.text_document_position.position;

            let offset_bytes = doc.rope.line_to_byte(pos.line as usize) + pos.character as usize;

            let root_node = doc.tree.root_node();
            let node = doc.tree.root_node_with_offset(
                offset_bytes,
                Point {
                    row: pos.line as usize,
                    column: pos.character as usize,
                },
            );

            Ok(Some(CompletionResponse::List(CompletionList {
                is_incomplete: true,
                items: vec![CompletionItem {
                    label: "foobar".to_string(),
                    insert_text: Some("foobar".to_string()) ,
                    ..Default::default()
                }],
            })))
        } else {
            self.client
                .log_message(MessageType::ERROR, format!("could not find doc {}", doc_id))
                .await;
            Ok(None)
        }
    }
}
