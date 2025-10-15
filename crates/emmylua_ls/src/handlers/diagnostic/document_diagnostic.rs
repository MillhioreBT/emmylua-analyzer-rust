use lsp_types::{
    DocumentDiagnosticParams, DocumentDiagnosticReport, DocumentDiagnosticReportResult,
    FullDocumentDiagnosticReport, RelatedFullDocumentDiagnosticReport,
};
use tokio_util::sync::CancellationToken;

use crate::context::ServerContextSnapshot;

pub async fn on_pull_document_diagnostic(
    context: ServerContextSnapshot,
    params: DocumentDiagnosticParams,
    token: CancellationToken,
) -> DocumentDiagnosticReportResult {
    let uri = params.text_document.uri;
    let diagnostics = context
        .file_diagnostic()
        .pull_file_diagnostics(uri, token)
        .await;

    DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
        related_documents: None,
        full_document_diagnostic_report: FullDocumentDiagnosticReport {
            result_id: None,
            items: diagnostics,
        },
    })
    .into()
}
