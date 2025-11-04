use lsp_types::{Range, request::Request};
use serde::{Deserialize, Serialize};

#[derive(Debug)]
pub enum EmmyGutterRequest {}

impl Request for EmmyGutterRequest {
    type Params = EmmyGutterParams;
    type Result = Option<Vec<GutterInfo>>;
    const METHOD: &'static str = "emmy/gutter";
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct EmmyGutterParams {
    pub uri: String,
}

// /**
//  * Gutter information returned from LSP
//  */
// data class GutterInfo(
//     val range: Range,
//     val kind: GutterKind,
//     val detail: String?
// )

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct GutterInfo {
    pub range: Range,
    pub kind: GutterKind,
    pub detail: Option<String>,
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(into = "u8", try_from = "u8")]
pub enum GutterKind {
    Class = 0,
    Enum = 1,
    Alias = 2,
    Method = 3,
    Module = 4,
}

impl From<GutterKind> for u8 {
    fn from(kind: GutterKind) -> Self {
        kind as u8
    }
}

impl From<u8> for GutterKind {
    fn from(value: u8) -> Self {
        match value {
            0 => GutterKind::Class,
            1 => GutterKind::Enum,
            2 => GutterKind::Alias,
            3 => GutterKind::Method,
            4 => GutterKind::Module,
            _ => GutterKind::Class, // default case
        }
    }
}
