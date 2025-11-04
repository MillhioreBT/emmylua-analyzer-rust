mod emmy_gutter_request;

use std::str::FromStr;

use crate::{
    context::ServerContextSnapshot,
    handlers::emmy_gutter::emmy_gutter_request::{EmmyGutterParams, GutterInfo},
};
pub use emmy_gutter_request::*;
use emmylua_code_analysis::SemanticModel;
use emmylua_parser::{LuaAstNode, LuaDocTag};
use lsp_types::Uri;
use tokio_util::sync::CancellationToken;

pub async fn on_emmy_gutter_handler(
    context: ServerContextSnapshot,
    params: EmmyGutterParams,
    _: CancellationToken,
) -> Option<Vec<GutterInfo>> {
    let uri = Uri::from_str(&params.uri).ok()?;
    let analysis = context.analysis().read().await;
    let file_id = analysis.get_file_id(&uri)?;
    let semantic_model = analysis.compilation.get_semantic_model(file_id)?;

    build_gutter_infos(&semantic_model)
}

fn build_gutter_infos(semantic_model: &SemanticModel) -> Option<Vec<GutterInfo>> {
    let root = semantic_model.get_root().clone();
    let document = semantic_model.get_document();
    let mut gutters = Vec::new();
    for tag in root.descendants::<LuaDocTag>() {
        match tag {
            LuaDocTag::Alias(alias) => {
                let range = alias.get_range();
                let lsp_range = document.to_lsp_range(range)?;
                gutters.push(GutterInfo {
                    range: lsp_range,
                    kind: GutterKind::Alias,
                    detail: Some("type alias".to_string()),
                });
            }
            LuaDocTag::Class(class) => {
                let range = class.get_range();
                let lsp_range = document.to_lsp_range(range)?;
                gutters.push(GutterInfo {
                    range: lsp_range,
                    kind: GutterKind::Class,
                    detail: Some("class".to_string()),
                });
            }
            LuaDocTag::Enum(enm) => {
                let range = enm.get_range();
                let lsp_range = document.to_lsp_range(range)?;
                gutters.push(GutterInfo {
                    range: lsp_range,
                    kind: GutterKind::Enum,
                    detail: Some("enum".to_string()),
                });
            }
            _ => {}
        }
    }

    Some(gutters)
}
