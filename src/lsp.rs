use std::future::Future;
use std::path::Iter;

use dashmap::DashMap;

use ropey::Rope;
use tower_lsp::jsonrpc::{Result, self};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tree_sitter::{Parser, Tree, Node, InputEdit, Point};
use tree_sitter_fun::language;

pub struct Doc {
    pub parser: Parser,
    pub tree: Tree,
    pub rope: Rope
}

impl Doc {
    pub fn new(src: &str) -> Result<Doc> {
        let mut parser = Parser::new();
        parser.set_language(language()).map_err(|e| jsonrpc::Error { code: jsonrpc::ErrorCode::InternalError, message: "could not construct parser".to_string(), data: None })?;

        let tree = parser.parse(src, None).ok_or(jsonrpc::Error { code: jsonrpc::ErrorCode::ParseError, message: "parse error".to_string(), data: None })?;
        let rope = Rope::from_str(src);
        Ok(Doc {
            parser,
            tree,
            rope
        })
    }

    pub fn edit(&mut self, change: TextDocumentContentChangeEvent) -> Result<()> {
        let TextDocumentContentChangeEvent { range, range_length:_, text } = change;
        let Range { start, end } = range.unwrap();
        let Position { line: start_line, character: start_col } = start;
        let Position { line: end_line, character: end_col } = end;

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
            start_position: Point { row: start_line as usize, column: start_col as usize }, 
            old_end_position: Point { row: end_line as usize, column: end_col as usize }, 
            new_end_position: Point { row: start_line as usize + n_lines, column: if n_lines > 0 { last_line.len() } else {start_col as usize + last_line.len()} } 
        });

        self.tree = self.parser.parse_with(&mut |offset, _| {
            if offset >= self.rope.len_bytes() {
                return ""
            }
            let mut end = offset + 32;
            if end > self.rope.len_bytes() {
                end = self.rope.len_bytes()
            }
            let slice = self.rope.byte_slice(offset..end);
            slice.as_str().unwrap()
        }, Some(&self.tree)).ok_or(jsonrpc::Error { code: jsonrpc::ErrorCode::ParseError, message: "parse error".to_string(), data: None })?;
    
        Ok(())
    }
}

pub struct Backend {
    client: Client,
    docs: DashMap<Url, Doc>
}

impl Backend {
    pub fn new(client: Client) -> anyhow::Result<Backend> {
        let mut parser = Parser::new();
        parser.set_language(language())?;
        Ok(Backend { 
            client, 
            docs: DashMap::new() 
        })
    }
}

fn iter_tree(tree: &Tree, cb: &mut impl FnMut(Node) -> ()) {
    let mut cursor = tree.walk();
    loop {
        while cursor.goto_first_child() {}

        cb(cursor.node());
        while cursor.goto_next_sibling() {
            cb(cursor.node())
        }
        cursor.goto_parent();
        while !cursor.goto_next_sibling() {
            if !cursor.goto_parent() {
                return
            }
        }
        cursor.goto_next_sibling();
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
];

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                // inlay_hint_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions { 
                        open_close: Some(true), 
                        change: Some(TextDocumentSyncKind::INCREMENTAL), 
                        will_save: Some(true), 
                        will_save_wait_until: Some(true), 
                        save: Some(TextDocumentSyncSaveOptions::Supported(true)) 
                })),
                // completion_provider: Some(CompletionOptions {
                //     resolve_provider: Some(false),
                //     trigger_characters: Some(vec![".".to_string()]),
                //     work_done_progress_options: Default::default(),
                //     all_commit_characters: None,
                //     completion_item: None,
                // }),
                // execute_command_provider: Some(ExecuteCommandOptions {
                //     commands: vec!["dummy.do_something".to_string()],
                //     work_done_progress_options: Default::default(),
                // }),

                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
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
                // definition: Some(GotoCapability::default()),
                // definition_provider: Some(OneOf::Left(true)),
                // references_provider: Some(OneOf::Left(true)),
                // rename_provider: Some(OneOf::Left(true)),
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
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;

        let doc_id = params.text_document.uri;
        
        let mut parser = Parser::new();
        parser.set_language(language()).unwrap();

        let src = params.text_document.text;
        let doc = Doc::new(src.as_str()).unwrap();
        self.docs.insert(doc_id, doc);
        // self.on_change(TextDocumentItem {
        //     language_id: "fun".to_string(),
        //     uri: params.text_document.uri,
        //     text: params.text_document.text,
        //     version: params.text_document.version,
        // })
        // .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        // self.client
        //     .log_message(MessageType::INFO, "file changed!")
        //     .await;

        let doc_id = params.text_document.uri;
        if let Some(mut doc) = self.docs.get_mut(&doc_id) {
            for change in params.content_changes {
                doc.edit(change).unwrap();
            }
            self.client
                .log_message(MessageType::INFO, format!("file changed! {} ", doc.rope.to_string()))
                .await;
        }
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }
    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        self.client.log_message(MessageType::INFO, params.text_document.uri.to_string()).await;

        let url = params.text_document.uri;

        if let Some(doc) = self.docs.get(&url) {
            let mut tokens = vec![];

            let mut pre_line = 0;
            let mut pre_start = 0;
            iter_tree(&doc.tree, &mut |node| {
                let range = node.range();

                let token_type = match node.kind() {
                    "num" => 4,
                    "str" => 5,
                    "id" => 1,
                    _ => 0
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

            self.client.log_message(MessageType::INFO, format!("{:?}", tokens)).await;

            Ok(Some(SemanticTokensResult::Tokens(SemanticTokens { 
                result_id: None, 
                data: tokens
            })))
        } else {
            todo!()
        }

        
    }
}