use std::collections::HashSet;
use std::path::Path;
use std::sync::atomic::{AtomicI64, AtomicU8, Ordering};
use std::{path::PathBuf, sync::Arc, time::Duration};

use super::{ClientProxy, FileDiagnostic, StatusBar};
use crate::context::lsp_features::LspFeatures;
use crate::handlers::{ClientConfig, init_analysis};
use emmylua_code_analysis::{
    EmmyLibraryItem, EmmyLuaAnalysis, Emmyrc, WorkspaceFolder, WorkspaceImport, load_configs,
};
use emmylua_code_analysis::{update_code_style, uri_to_file_path};
use log::{debug, info};
use lsp_types::Uri;
use tokio::sync::{Mutex, RwLock};
use tokio_util::sync::CancellationToken;
use wax::Pattern;

pub struct WorkspaceManager {
    analysis: Arc<RwLock<EmmyLuaAnalysis>>,
    client: Arc<ClientProxy>,
    status_bar: Arc<StatusBar>,
    update_token: Arc<Mutex<Option<Arc<ReindexToken>>>>,
    file_diagnostic: Arc<FileDiagnostic>,
    lsp_features: Arc<LspFeatures>,
    pub client_config: ClientConfig,
    /// Client-provided workspace roots used for config scope and reindexing.
    pub workspace_folders: Vec<WorkspaceFolder>,
    /// Expanded workspace roots used only for file matching and watching.
    match_workspace_folders: Vec<WorkspaceFolder>,
    pub watcher: Option<notify::RecommendedWatcher>,
    pub current_open_files: HashSet<Uri>,
    pub match_file_pattern: WorkspaceFileMatcher,
    workspace_diagnostic_level: Arc<AtomicU8>,
    workspace_version: Arc<AtomicI64>,
}

impl WorkspaceManager {
    pub fn new(
        analysis: Arc<RwLock<EmmyLuaAnalysis>>,
        client: Arc<ClientProxy>,
        status_bar: Arc<StatusBar>,
        file_diagnostic: Arc<FileDiagnostic>,
        lsp_features: Arc<LspFeatures>,
    ) -> Self {
        Self {
            analysis,
            client,
            status_bar,
            client_config: ClientConfig::default(),
            workspace_folders: Vec::new(),
            match_workspace_folders: Vec::new(),
            update_token: Arc::new(Mutex::new(None)),
            file_diagnostic,
            lsp_features,
            watcher: None,
            current_open_files: HashSet::new(),
            match_file_pattern: WorkspaceFileMatcher::default(),
            workspace_diagnostic_level: Arc::new(AtomicU8::new(
                WorkspaceDiagnosticLevel::Fast.to_u8(),
            )),
            workspace_version: Arc::new(AtomicI64::new(0)),
        }
    }

    pub fn get_workspace_diagnostic_level(&self) -> WorkspaceDiagnosticLevel {
        let value = self.workspace_diagnostic_level.load(Ordering::Acquire);
        WorkspaceDiagnosticLevel::from_u8(value)
    }

    pub fn update_workspace_version(&self, level: WorkspaceDiagnosticLevel, add_version: bool) {
        self.workspace_diagnostic_level
            .store(level.to_u8(), Ordering::Release);
        if add_version {
            self.workspace_version.fetch_add(1, Ordering::AcqRel);
        }
    }

    pub fn get_workspace_version(&self) -> i64 {
        self.workspace_version.load(Ordering::Acquire)
    }

    pub fn set_match_workspace_folders(&mut self, workspaces: Vec<WorkspaceFolder>) {
        self.match_workspace_folders = workspaces;
    }

    pub fn file_match_workspaces(&self) -> &[WorkspaceFolder] {
        if self.match_workspace_folders.is_empty() {
            &self.workspace_folders
        } else {
            &self.match_workspace_folders
        }
    }

    pub async fn add_update_emmyrc_task(&self, file_dir: PathBuf) {
        let mut update_token = self.update_token.lock().await;
        if let Some(token) = update_token.as_ref() {
            token.cancel();
            debug!("cancel update config: {:?}", file_dir);
        }

        let cancel_token = Arc::new(ReindexToken::new(Duration::from_secs(2)));
        update_token.replace(cancel_token.clone());
        drop(update_token);

        let analysis = self.analysis.clone();
        let workspace_folders = self.workspace_folders.clone();
        let config_update_token = self.update_token.clone();
        let client_config = self.client_config.clone();
        let status_bar = self.status_bar.clone();
        let file_diagnostic = self.file_diagnostic.clone();
        let lsp_features = self.lsp_features.clone();
        tokio::spawn(async move {
            cancel_token.wait_for_reindex().await;
            if cancel_token.is_cancelled() {
                return;
            }

            let emmyrc = load_emmy_config(Some(file_dir.clone()), client_config);
            init_analysis(
                &analysis,
                &status_bar,
                &file_diagnostic,
                &lsp_features,
                workspace_folders,
                emmyrc,
            )
            .await;
            // After completion, remove from HashMap
            let mut tokens = config_update_token.lock().await;
            tokens.take();
        });
    }

    pub fn update_editorconfig(&self, path: PathBuf) {
        let parent_dir = path
            .parent()
            .unwrap()
            .to_path_buf()
            .to_string_lossy()
            .to_string()
            .replace("\\", "/");
        let file_normalized = path.to_string_lossy().to_string().replace("\\", "/");
        log::info!("update code style: {:?}", file_normalized);
        update_code_style(&parent_dir, &file_normalized);
    }

    pub fn add_reload_workspace_task(&self) -> Option<()> {
        let config_root: Option<PathBuf> = self.workspace_folders.first().map(|wf| wf.root.clone());

        let emmyrc = load_emmy_config(config_root, self.client_config.clone());
        let analysis = self.analysis.clone();
        let workspace_folders = self.workspace_folders.clone();
        let status_bar = self.status_bar.clone();
        let file_diagnostic = self.file_diagnostic.clone();
        let lsp_features = self.lsp_features.clone();
        let client = self.client.clone();
        let workspace_diagnostic_status = self.workspace_diagnostic_level.clone();
        tokio::spawn(async move {
            // Perform reindex with minimal lock holding time
            init_analysis(
                &analysis,
                &status_bar,
                &file_diagnostic,
                &lsp_features,
                workspace_folders,
                emmyrc,
            )
            .await;

            // Cancel diagnostics and update status without holding analysis lock
            file_diagnostic.cancel_workspace_diagnostic().await;
            workspace_diagnostic_status
                .store(WorkspaceDiagnosticLevel::Fast.to_u8(), Ordering::Release);

            // Trigger diagnostics refresh
            if lsp_features.supports_workspace_diagnostic() {
                client.refresh_workspace_diagnostics();
            } else {
                file_diagnostic
                    .add_workspace_diagnostic_task(500, true)
                    .await;
            }
        });

        Some(())
    }

    pub async fn extend_reindex_delay(&self) -> Option<()> {
        let update_token = self.update_token.lock().await;
        if let Some(token) = update_token.as_ref() {
            token.set_resleep().await;
        }

        Some(())
    }

    pub async fn reindex_workspace(&self, delay: Duration) -> Option<()> {
        log::info!("reindex workspace with delay: {:?}", delay);
        let mut update_token = self.update_token.lock().await;
        if let Some(token) = update_token.as_ref() {
            token.cancel();
            log::info!("cancel reindex workspace");
        }

        let cancel_token = Arc::new(ReindexToken::new(delay));
        update_token.replace(cancel_token.clone());
        drop(update_token);
        let analysis = self.analysis.clone();
        let file_diagnostic = self.file_diagnostic.clone();
        let lsp_features = self.lsp_features.clone();
        let client = self.client.clone();
        let workspace_diagnostic_status = self.workspace_diagnostic_level.clone();
        tokio::spawn(async move {
            cancel_token.wait_for_reindex().await;
            if cancel_token.is_cancelled() {
                return;
            }

            // Perform reindex with minimal lock holding time
            {
                let mut analysis = analysis.write().await;
                // 在重新索引之前清理不存在的文件
                analysis.cleanup_nonexistent_files();
                analysis.reindex();
                // Release lock immediately after reindex
            }

            // Cancel diagnostics and update status without holding analysis lock
            file_diagnostic.cancel_workspace_diagnostic().await;
            workspace_diagnostic_status
                .store(WorkspaceDiagnosticLevel::Fast.to_u8(), Ordering::Release);

            // Trigger diagnostics refresh
            if lsp_features.supports_workspace_diagnostic() {
                client.refresh_workspace_diagnostics();
            } else {
                file_diagnostic
                    .add_workspace_diagnostic_task(500, true)
                    .await;
            }
        });

        Some(())
    }

    /// Returns whether the URI resolves to a file owned by one of the current
    /// workspaces after applying workspace-specific filters.
    pub fn is_workspace_file(&self, uri: &Uri) -> bool {
        if self.workspace_folders.is_empty() {
            return true;
        }

        let Some(file_path) = uri_to_file_path(uri) else {
            return true;
        };

        self.match_file_pattern
            .is_match(self.file_match_workspaces(), &file_path)
    }

    pub async fn check_schema_update(&self) {
        let read_analysis = self.analysis.read().await;
        if read_analysis.check_schema_update() {
            drop(read_analysis);
            let mut write_analysis = self.analysis.write().await;
            write_analysis.update_schema().await;
        }
    }
}

pub fn load_emmy_config(config_root: Option<PathBuf>, client_config: ClientConfig) -> Arc<Emmyrc> {
    // Config load priority.
    // * Global `<os-specific home-dir>/.luarc.json`.
    // * Global `<os-specific home-dir>/.emmyrc.json`.
    // * Global `<os-specific config-dir>/emmylua_ls/.luarc.json`.
    // * Global `<os-specific config-dir>/emmylua_ls/.emmyrc.json`.
    // * Environment-specified config at the $EMMYLUALS_CONFIG path.
    // * Local `.luarc.json`.
    // * Local `.emmyrc.json`.
    let luarc_file = ".luarc.json";
    let emmyrc_file = ".emmyrc.json";
    let emmyrc_lua_file = ".emmyrc.lua";
    let mut config_files = Vec::new();

    let home_dir = dirs::home_dir();
    if let Some(home_dir) = home_dir {
        let global_luarc_path = home_dir.join(luarc_file);
        if global_luarc_path.exists() {
            info!("load config from: {:?}", global_luarc_path);
            config_files.push(global_luarc_path);
        }
        let global_emmyrc_path = home_dir.join(emmyrc_file);
        if global_emmyrc_path.exists() {
            info!("load config from: {:?}", global_emmyrc_path);
            config_files.push(global_emmyrc_path);
        }
        let global_emmyrc_lua_path = home_dir.join(emmyrc_lua_file);
        if global_emmyrc_lua_path.exists() {
            info!("load config from: {:?}", global_emmyrc_lua_path);
            config_files.push(global_emmyrc_lua_path);
        }
    };

    let emmylua_config_dir = "emmylua_ls";
    let config_dir = dirs::config_dir().map(|path| path.join(emmylua_config_dir));
    if let Some(config_dir) = config_dir {
        let global_luarc_path = config_dir.join(luarc_file);
        if global_luarc_path.exists() {
            info!("load config from: {:?}", global_luarc_path);
            config_files.push(global_luarc_path);
        }
        let global_emmyrc_path = config_dir.join(emmyrc_file);
        if global_emmyrc_path.exists() {
            info!("load config from: {:?}", global_emmyrc_path);
            config_files.push(global_emmyrc_path);
        }
        let global_emmyrc_lua_path = config_dir.join(emmyrc_lua_file);
        if global_emmyrc_lua_path.exists() {
            info!("load config from: {:?}", global_emmyrc_lua_path);
            config_files.push(global_emmyrc_lua_path);
        }
    };

    std::env::var("EMMYLUALS_CONFIG")
        .inspect(|path| {
            let config_path = std::path::PathBuf::from(path);
            if config_path.exists() {
                info!("load config from: {:?}", config_path);
                config_files.push(config_path);
            }
        })
        .ok();

    if let Some(config_root) = &config_root {
        let luarc_path = config_root.join(luarc_file);
        if luarc_path.exists() {
            info!("load config from: {:?}", luarc_path);
            config_files.push(luarc_path);
        }
        let emmyrc_path = config_root.join(emmyrc_file);
        if emmyrc_path.exists() {
            info!("load config from: {:?}", emmyrc_path);
            config_files.push(emmyrc_path);
        }
        let emmyrc_lua_path = config_root.join(emmyrc_lua_file);
        if emmyrc_lua_path.exists() {
            info!("load config from: {:?}", emmyrc_lua_path);
            config_files.push(emmyrc_lua_path);
        }
    }

    let mut emmyrc = load_configs(config_files, client_config.partial_emmyrcs.clone());
    merge_client_config(client_config, &mut emmyrc);
    if let Some(workspace_root) = &config_root {
        emmyrc.pre_process_emmyrc(workspace_root);
    }

    log::info!("loaded emmyrc complete");
    emmyrc.into()
}

fn merge_client_config(client_config: ClientConfig, emmyrc: &mut Emmyrc) -> Option<()> {
    emmyrc.runtime.extensions.extend(client_config.extensions);
    emmyrc.workspace.ignore_globs.extend(client_config.exclude);
    if client_config.encoding != "utf-8" {
        emmyrc.workspace.encoding = client_config.encoding;
    }

    Some(())
}

#[derive(Debug)]
pub struct ReindexToken {
    cancel_token: CancellationToken,
    time_sleep: Duration,
    need_re_sleep: Mutex<bool>,
}

impl ReindexToken {
    pub fn new(time_sleep: Duration) -> Self {
        Self {
            cancel_token: CancellationToken::new(),
            time_sleep,
            need_re_sleep: Mutex::new(false),
        }
    }

    pub async fn wait_for_reindex(&self) {
        loop {
            tokio::select! {
                _ = tokio::time::sleep(self.time_sleep) => {
                    // 获取锁来安全地访问和修改 need_re_sleep
                    let mut need_re_sleep = self.need_re_sleep.lock().await;
                    if *need_re_sleep {
                        *need_re_sleep = false;
                    } else {
                        break;
                    }
                }
                _ = self.cancel_token.cancelled() => {
                    break;
                }
            }
        }
    }

    pub fn cancel(&self) {
        self.cancel_token.cancel();
    }

    pub fn is_cancelled(&self) -> bool {
        self.cancel_token.is_cancelled()
    }

    pub async fn set_resleep(&self) {
        // 获取锁来安全地修改 need_re_sleep
        let mut need_re_sleep = self.need_re_sleep.lock().await;
        *need_re_sleep = true;
    }
}

#[derive(Debug, Clone)]
pub struct WorkspaceFileMatcher {
    include: Vec<String>,
    exclude: Vec<String>,
    exclude_dir: Vec<PathBuf>,
    library_exclude: Vec<(PathBuf, Vec<String>, Vec<PathBuf>)>,
}

impl WorkspaceFileMatcher {
    /// Precomputes the effective file patterns for each workspace root.
    pub fn new(
        include: Vec<String>,
        exclude: Vec<String>,
        exclude_dir: Vec<PathBuf>,
        emmyrc: &Emmyrc,
    ) -> Self {
        let library_exclude = emmyrc
            .workspace
            .library
            .iter()
            .filter_map(|lib| {
                if let EmmyLibraryItem::Config(config) = lib {
                    let mut merged_exclude = exclude.clone();
                    merged_exclude.extend(config.ignore_globs.clone());
                    merged_exclude.sort();
                    merged_exclude.dedup();

                    Some((
                        PathBuf::from(&config.path),
                        merged_exclude,
                        config.ignore_dir.iter().map(PathBuf::from).collect(),
                    ))
                } else {
                    None
                }
            })
            .collect();

        Self {
            include,
            exclude,
            exclude_dir,
            library_exclude,
        }
    }

    /// Returns whether `path` belongs to any configured workspace after the
    /// most specific workspace's include and exclude rules are applied.
    pub fn is_match(&self, workspaces: &[WorkspaceFolder], path: &Path) -> bool {
        let Some((workspace, relative_path)) = select_workspace(workspaces, path) else {
            return false;
        };

        let (exclude, exclude_dir) = self.exclude_for_workspace(workspace);
        if exclude_dir.iter().any(|dir| path.starts_with(dir)) {
            return false;
        }

        if !exclude.is_empty() {
            let exclude_matcher = wax::any(exclude.iter().map(|s| s.as_str()));
            if let Ok(exclude_set) = exclude_matcher {
                if exclude_set.is_match(relative_path.as_path()) {
                    return false;
                }
            } else {
                log::error!("Invalid exclude pattern");
            }
        }

        let include_matcher = wax::any(self.include.iter().map(|s| s.as_str()));
        if let Ok(include_set) = include_matcher {
            return include_set.is_match(relative_path.as_path());
        } else {
            log::error!("Invalid include pattern");
        }

        true
    }

    fn exclude_for_workspace(&self, workspace: &WorkspaceFolder) -> (&[String], &[PathBuf]) {
        if workspace.is_library {
            if let Some((_, exclude, exclude_dir)) = self
                .library_exclude
                .iter()
                .find(|(root, _, _)| *root == workspace.root)
            {
                return (exclude, exclude_dir);
            }

            return (&self.exclude, &[]);
        }

        (&self.exclude, &self.exclude_dir)
    }
}

impl Default for WorkspaceFileMatcher {
    fn default() -> Self {
        Self {
            include: vec!["**/*.lua".to_string()],
            exclude: Vec::new(),
            exclude_dir: Vec::new(),
            library_exclude: Vec::new(),
        }
    }
}

fn select_workspace<'a>(
    workspaces: &'a [WorkspaceFolder],
    path: &Path,
) -> Option<(&'a WorkspaceFolder, PathBuf)> {
    let mut selected = None;

    for workspace in workspaces {
        let Ok(relative_path) = path.strip_prefix(&workspace.root) else {
            continue;
        };

        let Some(import_depth) = workspace_import_depth(&workspace.import, relative_path) else {
            continue;
        };

        let candidate = (
            workspace.root.components().count(),
            import_depth,
            usize::from(workspace.is_library),
        );

        let is_better = selected
            .as_ref()
            .map(|(_, _, current_score)| candidate > *current_score)
            .unwrap_or(true);

        if is_better {
            selected = Some((workspace, relative_path.to_path_buf(), candidate));
        }
    }

    selected.map(|(workspace, relative_path, _)| (workspace, relative_path))
}

/// Returns the depth of the matching import path for a workspace, if the file
/// is inside that workspace's import scope.
fn workspace_import_depth(import: &WorkspaceImport, relative_path: &Path) -> Option<usize> {
    match import {
        WorkspaceImport::All => Some(0),
        WorkspaceImport::SubPaths(paths) => paths
            .iter()
            .filter(|path| relative_path.starts_with(path))
            .map(|path| path.components().count())
            .max(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use emmylua_code_analysis::{Emmyrc, calculate_include_and_exclude};
    use lsp_server::Connection;
    use lsp_types::ClientCapabilities;
    use std::{
        fs,
        sync::Arc,
        sync::atomic::{AtomicU64, Ordering},
        time::{SystemTime, UNIX_EPOCH},
    };

    static TEST_WORKSPACE_COUNTER: AtomicU64 = AtomicU64::new(0);

    struct TestWorkspace {
        root: PathBuf,
    }

    impl TestWorkspace {
        fn new() -> Self {
            let unique = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos();
            let counter = TEST_WORKSPACE_COUNTER.fetch_add(1, Ordering::Relaxed);
            let root = std::env::temp_dir().join(format!(
                "emmylua-workspace-matcher-{}-{}-{}",
                std::process::id(),
                unique,
                counter,
            ));
            fs::create_dir_all(&root).unwrap();
            Self { root }
        }

        fn write_file(&self, relative_path: &str) -> PathBuf {
            let path = self.root.join(relative_path);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(&path, "return true\n").unwrap();
            path
        }

        fn path(&self, relative_path: &str) -> PathBuf {
            self.root.join(relative_path)
        }
    }

    impl Drop for TestWorkspace {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.root);
        }
    }

    fn to_string(path: &Path) -> String {
        path.to_string_lossy().to_string()
    }

    fn json_string(value: &str) -> String {
        serde_json::to_string(value).unwrap()
    }

    fn emmyrc_from_json(json: &str) -> Emmyrc {
        serde_json::from_str(json).unwrap()
    }

    fn new_workspace_manager() -> WorkspaceManager {
        let (server, _client) = Connection::memory();
        let client = Arc::new(ClientProxy::new(server));
        let analysis = Arc::new(RwLock::new(EmmyLuaAnalysis::new()));
        let status_bar = Arc::new(StatusBar::new(client.clone()));
        let file_diagnostic = Arc::new(FileDiagnostic::new(
            analysis.clone(),
            status_bar.clone(),
            client.clone(),
        ));
        let lsp_features = Arc::new(LspFeatures::new(ClientCapabilities::default()));

        WorkspaceManager::new(analysis, client, status_bar, file_diagnostic, lsp_features)
    }

    #[test]
    fn nested_library_overrides_parent_ignore_dir() {
        let workspace = TestWorkspace::new();
        let library_root = workspace.path(".test-deps/runtime/lua/vim");
        let library_file = workspace.write_file(".test-deps/runtime/lua/vim/shared.lua");
        let workspaces = vec![
            WorkspaceFolder::new(workspace.root.clone(), false),
            WorkspaceFolder::new(library_root.clone(), true),
        ];

        let emmyrc = emmyrc_from_json(&format!(
            r#"{{
                "workspace": {{
                    "ignoreDir": [{}],
                    "library": [{}]
                }}
            }}"#,
            json_string(&to_string(&workspace.path(".test-deps"))),
            json_string(&to_string(&library_root)),
        ));
        let (include, exclude, exclude_dir) = calculate_include_and_exclude(&emmyrc);

        let matcher = WorkspaceFileMatcher::new(include, exclude, exclude_dir, &emmyrc);

        assert!(matcher.is_match(&workspaces, &library_file));
    }

    #[test]
    fn most_specific_workspace_can_still_exclude_a_library_file() {
        let workspace = TestWorkspace::new();
        let library_root = workspace.path(".test-deps/runtime/lua/vim");
        let library_file = workspace.write_file(".test-deps/runtime/lua/vim/tests/spec.lua");
        let workspaces = vec![
            WorkspaceFolder::new(workspace.root.clone(), false),
            WorkspaceFolder::new(library_root.clone(), true),
        ];

        let emmyrc = emmyrc_from_json(&format!(
            r#"{{
                "workspace": {{
                    "ignoreDir": [{}],
                    "library": [{{
                        "path": {},
                        "ignoreDir": [{}]
                    }}]
                }}
            }}"#,
            json_string(&to_string(&workspace.path(".test-deps"))),
            json_string(&to_string(&library_root)),
            json_string(&to_string(&library_root.join("tests"))),
        ));
        let (include, exclude, exclude_dir) = calculate_include_and_exclude(&emmyrc);

        let matcher = WorkspaceFileMatcher::new(include, exclude, exclude_dir, &emmyrc);

        assert!(!matcher.is_match(&workspaces, &library_file));
    }

    #[test]
    fn real_workspaces_remain_separate_from_match_workspaces() {
        let workspace = TestWorkspace::new();
        let library_root = workspace.path(".test-deps/runtime/lua/vim");
        let mut manager = new_workspace_manager();

        manager.workspace_folders = vec![WorkspaceFolder::new(workspace.root.clone(), false)];
        manager.set_match_workspace_folders(vec![
            WorkspaceFolder::new(workspace.root.clone(), false),
            WorkspaceFolder::new(library_root.clone(), true),
        ]);

        assert_eq!(manager.workspace_folders.len(), 1);
        assert_eq!(manager.workspace_folders[0].root, workspace.root);
        assert!(!manager.workspace_folders[0].is_library);
        assert_eq!(manager.file_match_workspaces().len(), 2);
        assert!(
            manager
                .file_match_workspaces()
                .iter()
                .any(|workspace| workspace.root == library_root && workspace.is_library)
        );
    }

    #[test]
    fn global_ignore_globs_still_apply_to_library_files() {
        let workspace = TestWorkspace::new();
        let library_root = workspace.path(".test-deps/runtime/lua/vim");
        let library_file = workspace.write_file(".test-deps/runtime/lua/vim/tests/spec.skip.lua");
        let workspaces = vec![
            WorkspaceFolder::new(workspace.root.clone(), false),
            WorkspaceFolder::new(library_root.clone(), true),
        ];

        let emmyrc = emmyrc_from_json(&format!(
            r#"{{
                "workspace": {{
                    "ignoreDir": [{}],
                    "ignoreGlobs": ["**/*.skip.lua"],
                    "library": [{}]
                }}
            }}"#,
            json_string(&to_string(&workspace.path(".test-deps"))),
            json_string(&to_string(&library_root)),
        ));
        let (include, exclude, exclude_dir) = calculate_include_and_exclude(&emmyrc);

        let matcher = WorkspaceFileMatcher::new(include, exclude, exclude_dir, &emmyrc);

        assert!(!matcher.is_match(&workspaces, &library_file));
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WorkspaceDiagnosticLevel {
    None = 0,
    Fast = 1,
    Slow = 2,
}

impl WorkspaceDiagnosticLevel {
    pub fn from_u8(value: u8) -> Self {
        match value {
            1 => Self::Fast,
            2 => Self::Slow,
            _ => Self::None,
        }
    }

    pub fn to_u8(self) -> u8 {
        self as u8
    }
}
