mod builder;
mod comment;
mod expr;
mod stats;

use std::collections::HashSet;

use builder::{DocumentSymbolBuilder, LuaSymbol};
use emmylua_code_analysis::SemanticModel;
use emmylua_parser::{LuaAst, LuaAstNode, LuaChunk, LuaExpr, LuaTableExpr};
use expr::{build_closure_expr_symbol, build_table_symbol};
use lsp_types::{
    ClientCapabilities, DocumentSymbol, DocumentSymbolOptions, DocumentSymbolParams,
    DocumentSymbolResponse, OneOf, ServerCapabilities, SymbolKind,
};
use stats::{
    build_assign_stat_symbol, build_for_range_stat_symbol, build_for_stat_symbol,
    build_func_stat_symbol, build_if_stat_symbol, build_local_func_stat_symbol,
    build_local_stat_symbol,
};
use tokio_util::sync::CancellationToken;

use crate::context::ServerContextSnapshot;

use super::RegisterCapabilities;
use comment::build_doc_region_symbol;

pub async fn on_document_symbol(
    context: ServerContextSnapshot,
    params: DocumentSymbolParams,
    _: CancellationToken,
) -> Option<DocumentSymbolResponse> {
    let uri = params.text_document.uri;
    let analysis = context.analysis().read().await;
    let file_id = analysis.get_file_id(&uri)?;
    let semantic_model = analysis.compilation.get_semantic_model(file_id)?;
    let document_symbol_root = build_document_symbol(&semantic_model)?;
    // remove root file symbol
    let children = document_symbol_root.children?;
    let response = DocumentSymbolResponse::Nested(children);
    Some(response)
}

fn build_document_symbol(semantic_model: &SemanticModel) -> Option<DocumentSymbol> {
    let document = semantic_model.get_document();
    let root = semantic_model.get_root();
    let file_id = semantic_model.get_file_id();
    let decl_tree = semantic_model
        .get_db()
        .get_decl_index()
        .get_decl_tree(&file_id)?;
    let db = semantic_model.get_db();

    let mut builder = DocumentSymbolBuilder::new(db, decl_tree, &document);
    let symbol = LuaSymbol::new("".into(), None, SymbolKind::FILE, root.get_range());
    builder.add_node_symbol(root.syntax().clone(), symbol);
    build_child_document_symbols(&mut builder, root);

    Some(builder.build(root))
}

fn build_child_document_symbols(
    builder: &mut DocumentSymbolBuilder,
    root: &LuaChunk,
) -> Option<()> {
    let mut skip_table_exprs: HashSet<LuaTableExpr> = HashSet::new();

    for child in root.descendants::<LuaAst>() {
        match child {
            LuaAst::LuaLocalStat(local_stat) => {
                for value_expr in local_stat.get_value_exprs() {
                    if let LuaExpr::TableExpr(table_expr) = value_expr {
                        skip_table_exprs.insert(table_expr);
                    }
                }
                build_local_stat_symbol(builder, local_stat);
            }
            LuaAst::LuaAssignStat(assign_stat) => {
                let (_, exprs) = assign_stat.get_var_and_expr_list();
                for expr in exprs {
                    if let LuaExpr::TableExpr(table_expr) = expr {
                        skip_table_exprs.insert(table_expr);
                    }
                }
                build_assign_stat_symbol(builder, assign_stat);
            }
            LuaAst::LuaForStat(for_stat) => {
                build_for_stat_symbol(builder, for_stat);
            }
            LuaAst::LuaForRangeStat(for_range_stat) => {
                build_for_range_stat_symbol(builder, for_range_stat);
            }
            LuaAst::LuaLocalFuncStat(local_func) => {
                build_local_func_stat_symbol(builder, local_func);
            }
            LuaAst::LuaFuncStat(func) => {
                build_func_stat_symbol(builder, func);
            }
            LuaAst::LuaClosureExpr(closure) => {
                build_closure_expr_symbol(builder, closure);
            }
            LuaAst::LuaTableExpr(table) => {
                if !skip_table_exprs.contains(&table) {
                    build_table_symbol(builder, table);
                }
            }
            LuaAst::LuaComment(comment) => {
                build_doc_region_symbol(builder, comment);
            }
            LuaAst::LuaIfStat(if_stat) => {
                build_if_stat_symbol(builder, if_stat);
            }
            _ => {}
        }
    }

    Some(())
}

pub struct DocumentSymbolCapabilities;

impl RegisterCapabilities for DocumentSymbolCapabilities {
    fn register_capabilities(server_capabilities: &mut ServerCapabilities, _: &ClientCapabilities) {
        server_capabilities.document_symbol_provider = Some(OneOf::Right(DocumentSymbolOptions {
            label: Some("EmmyLua".into()),
            work_done_progress_options: Default::default(),
        }));
    }
}
