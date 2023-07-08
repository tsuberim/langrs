use dashmap::DashMap;

use tower_lsp::jsonrpc::{Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};


use crate::doc::Doc;
use crate::typing::Type;

pub struct Backend {
    client: Client,
    docs: DashMap<Url, Doc>,
}

impl Backend {
    pub fn new(client: Client) -> anyhow::Result<Backend> {
        Ok(Backend {
            client,
            docs: DashMap::new(),
        })
    }

    async fn update_diagnostics(&self, doc: &Doc, version: i32) {
        let mut diagnostics = vec![];

        for err in doc.parse_errors() {
            if let Some(node) = doc.get_node(err.node_id) {
                diagnostics.push(Diagnostic {
                    range: node.range,
                    message: err.message.clone(),
                    ..Default::default()
                })
            }
        }

        for err in doc.module().type_errors() {
            if let Some(node) = doc.get_node(err.node_id) {
                diagnostics.push(Diagnostic {
                    range: node.range,
                    message: err.message.clone(),
                    ..Default::default()
                })
            }
        }

        self.client
            .publish_diagnostics(doc.url.clone(), diagnostics, Some(version))
            .await;
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
                inlay_hint_provider: Some(OneOf::Left(false)),
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
        self.update_diagnostics(&doc, params.text_document.version)
            .await;
        self.docs.insert(doc.url.clone(), doc);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let doc_id = params.text_document.uri;
        if let Some(mut doc) = self.docs.get_mut(&doc_id) {
            for change in params.content_changes {
                doc.edit(change).unwrap();
            }
            self.update_diagnostics(&doc, params.text_document.version)
                .await;
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
        let url = params.text_document.uri;

        if let Some(doc) = self.docs.get(&url) {
            let mut tokens = vec![];

            let mut pre_line = 0;
            let mut pre_start = 0;

            for leaf in doc.leaves() {
                if let Some(kind) = &leaf.kind {
                    let range = leaf.range;
                    // SemanticTokenType::FUNCTION,
                    // SemanticTokenType::VARIABLE,
                    // SemanticTokenType::STRING,
                    // SemanticTokenType::COMMENT,
                    // SemanticTokenType::NUMBER,
                    // SemanticTokenType::KEYWORD,
                    // SemanticTokenType::OPERATOR,
                    // SemanticTokenType::PARAMETER,
                    // SemanticTokenType::TYPE,

                    let token_type = match kind.as_str() {
                        "num" => 4,
                        "str_lit" => 2,
                        "id" => 1,
                        "comment" => 3,
                        "sym" => 6,
                        "tag_id" => 8,
                        _ => 0,
                    };

                    let line = range.start.line;
                    let start = range.start.character;
                    let length = leaf.length;

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
                }
            }

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
            if let Some(node) = doc.find_named_node(pos) {
                if let Some(typ) = doc.module().get_type(node.id) {
                    return Ok(Some(Hover {
                        contents: HoverContents::Scalar(MarkedString::LanguageString(
                            LanguageString {
                                language: "fun".to_string(),
                                value: format!("{}", typ),
                            },
                        )),
                        range: Some(node.range),
                    }));
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
            if let Some(node) = doc.find_named_node(pos) {
                if let Some(typ) = doc.module().get_type(node.id) {
                    if let Type::Record { items, union, rest } = typ {
                        if !union {
                            return Ok(Some(CompletionResponse::Array(
                                items
                                    .keys()
                                    .into_iter()
                                    .map(|k| CompletionItem {
                                        label: k.clone(),
                                        ..Default::default()
                                    })
                                    .collect(),
                            )));
                        }
                    }

                    if let Type::Cons(name, args) = typ {
                        if *name == "Fun".to_string() {
                            return Ok(Some(CompletionResponse::Array(vec![CompletionItem {
                                label: (0..args.len() - 1)
                                    .into_iter()
                                    .map(|_| "".to_string())
                                    .collect::<Vec<String>>()
                                    .join(", ")
                                    + ")",
                                ..Default::default()
                            }])));
                        }
                    }
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
}
