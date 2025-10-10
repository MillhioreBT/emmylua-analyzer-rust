use lsp_types::{
    ClientCapabilities, DiagnosticOptions, DiagnosticServerCapabilities, DocumentDiagnosticParams,
    DocumentDiagnosticReport, DocumentDiagnosticReportResult, FullDocumentDiagnosticReport,
    RelatedFullDocumentDiagnosticReport, ServerCapabilities,
};
use tokio_util::sync::CancellationToken;

use crate::context::ServerContextSnapshot;

use super::RegisterCapabilities;

pub async fn on_pull_document_diagnostic(
    context: ServerContextSnapshot,
    params: DocumentDiagnosticParams,
    token: CancellationToken,
) -> DocumentDiagnosticReportResult {
    let uri = params.text_document.uri;
    let analysis = context.analysis().read().await;
    let Some(file_id) = analysis.get_file_id(&uri) else {
        return default_diagnostic_report();
    };

    let analysis = context.analysis().read().await;
    let diagnostics = analysis.diagnose_file(file_id, token).unwrap_or_default();

    DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
        related_documents: None,
        full_document_diagnostic_report: FullDocumentDiagnosticReport {
            result_id: None,
            items: diagnostics,
        },
    })
    .into()
}

fn default_diagnostic_report() -> DocumentDiagnosticReportResult {
    DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
        related_documents: None,
        full_document_diagnostic_report: FullDocumentDiagnosticReport {
            result_id: None,
            items: vec![],
        },
    })
    .into()
}

pub struct DocumentDiagnosticCapabilities;

impl RegisterCapabilities for DocumentDiagnosticCapabilities {
    fn register_capabilities(server_capabilities: &mut ServerCapabilities, _: &ClientCapabilities) {
        server_capabilities.diagnostic_provider =
            Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
                identifier: Some("EmmyLua".to_string()),
                inter_file_dependencies: false,
                workspace_diagnostics: false,
                ..Default::default()
            }))
    }
}
