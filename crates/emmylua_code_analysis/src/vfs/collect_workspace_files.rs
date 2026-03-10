use std::{collections::HashSet, path::PathBuf};

use crate::{EmmyLibraryItem, Emmyrc, LuaFileInfo, load_workspace_files};

#[derive(Clone, Debug)]
pub enum WorkspaceImport {
    All,
    SubPaths(Vec<PathBuf>),
}

#[derive(Clone, Debug)]
pub struct WorkspaceFolder {
    pub root: PathBuf,
    pub import: WorkspaceImport,
    pub is_library: bool,
}

impl WorkspaceFolder {
    pub fn new(root: PathBuf, is_library: bool) -> Self {
        Self {
            root,
            import: WorkspaceImport::All,
            is_library,
        }
    }

    pub fn with_sub_paths(root: PathBuf, sub_paths: Vec<PathBuf>, is_library: bool) -> Self {
        Self {
            root,
            import: WorkspaceImport::SubPaths(sub_paths),
            is_library,
        }
    }
}

pub fn build_workspace_folders(
    workspace_folders: &[WorkspaceFolder],
    emmyrc: &Emmyrc,
) -> Vec<WorkspaceFolder> {
    let mut resolved = workspace_folders.to_vec();

    resolved.extend(
        emmyrc
            .workspace
            .workspace_roots
            .iter()
            .map(|root| WorkspaceFolder::new(PathBuf::from(root), false)),
    );
    resolved.extend(
        emmyrc
            .workspace
            .library
            .iter()
            .map(|library| WorkspaceFolder::new(PathBuf::from(library.get_path()), true)),
    );
    resolved.extend(
        emmyrc
            .workspace
            .package_dirs
            .iter()
            .filter_map(|package_dir| {
                let package_path = PathBuf::from(package_dir);
                let Some(parent) = package_path.parent() else {
                    log::warn!("package dir {:?} has no parent", package_path);
                    return None;
                };
                let Some(name) = package_path.file_name() else {
                    log::warn!("package dir {:?} has no file name", package_path);
                    return None;
                };

                Some(WorkspaceFolder::with_sub_paths(
                    parent.to_path_buf(),
                    vec![PathBuf::from(name)],
                    true,
                ))
            }),
    );

    resolved
}

pub fn collect_workspace_files(
    workspaces: &[WorkspaceFolder],
    emmyrc: &Emmyrc,
    extra_include: Option<Vec<String>>,
    extra_exclude: Option<Vec<String>>,
) -> Vec<LuaFileInfo> {
    let mut files = Vec::new();
    let mut loaded_paths = HashSet::new(); // Track loaded file paths to avoid duplicates
    let (mut match_pattern, mut exclude, exclude_dir) = calculate_include_and_exclude(emmyrc);
    if let Some(extra_include) = extra_include {
        match_pattern.extend_from_slice(&extra_include);
        match_pattern.sort();
        match_pattern.dedup();
    }
    if let Some(extra_exclude) = extra_exclude {
        exclude.extend_from_slice(&extra_exclude);
        exclude.sort();
        exclude.dedup();
    }

    let encoding = &emmyrc.workspace.encoding;

    log::info!(
        "collect_files from: {:?} match_pattern: {:?} exclude: {:?}, exclude_dir: {:?}",
        workspaces,
        match_pattern,
        exclude,
        exclude_dir
    );

    for (idx, workspace) in workspaces.iter().enumerate() {
        // Build exclude_dirs for this workspace by finding child workspaces
        let mut workspace_exclude_dir = exclude_dir
            .iter()
            .filter(|dir| match &workspace.import {
                WorkspaceImport::All => !workspace.root.starts_with(dir),
                WorkspaceImport::SubPaths(paths) => !paths
                    .iter()
                    .map(|path| workspace.root.join(path))
                    .any(|path| path.starts_with(dir)),
            })
            .cloned()
            .collect();

        // Find all other workspaces that are children of current workspace
        for (other_idx, other_workspace) in workspaces.iter().enumerate() {
            if idx != other_idx {
                // Check if other_workspace is a child of current workspace
                if let Ok(relative) = other_workspace.root.strip_prefix(&workspace.root) {
                    if relative.components().count() > 0 {
                        // other_workspace is a child, add it to exclude_dir
                        workspace_exclude_dir.push(other_workspace.root.clone());
                        log::debug!(
                            "Excluding child workspace {:?} from parent {:?}",
                            other_workspace.root,
                            workspace.root
                        );
                    }
                }
            }
        }

        match &workspace.import {
            WorkspaceImport::All => {
                let loaded = if workspace.is_library {
                    let (lib_exclude, lib_exclude_dir) = find_library_exclude(workspace, emmyrc);
                    // Merge library exclude with workspace exclude
                    let mut merged_exclude = exclude.clone();
                    merged_exclude.extend(lib_exclude);
                    merged_exclude.sort();
                    merged_exclude.dedup();

                    let mut merged_exclude_dir = workspace_exclude_dir.clone();
                    merged_exclude_dir.extend(lib_exclude_dir);

                    load_workspace_files(
                        &workspace.root,
                        &match_pattern,
                        &merged_exclude,
                        &merged_exclude_dir,
                        Some(encoding),
                    )
                    .ok()
                } else {
                    load_workspace_files(
                        &workspace.root,
                        &match_pattern,
                        &exclude,
                        &workspace_exclude_dir,
                        Some(encoding),
                    )
                    .ok()
                };
                if let Some(loaded) = loaded {
                    for file in loaded {
                        // Normalize path and check for duplicates
                        let normalized_path = PathBuf::from(&file.path)
                            .canonicalize()
                            .unwrap_or_else(|_| PathBuf::from(&file.path));

                        if loaded_paths.insert(normalized_path) {
                            files.push(file);
                        } else {
                            log::debug!("Skipping duplicate file: {:?}", file.path);
                        }
                    }
                }
            }
            WorkspaceImport::SubPaths(paths) => {
                for sub in paths {
                    let target = workspace.root.join(sub);
                    let loaded = if workspace.is_library {
                        let (lib_exclude, lib_exclude_dir) =
                            find_library_exclude(workspace, emmyrc);
                        // Merge library exclude with workspace exclude
                        let mut merged_exclude = exclude.clone();
                        merged_exclude.extend(lib_exclude);
                        merged_exclude.sort();
                        merged_exclude.dedup();

                        let mut merged_exclude_dir = workspace_exclude_dir.clone();
                        merged_exclude_dir.extend(lib_exclude_dir);

                        load_workspace_files(
                            &target,
                            &match_pattern,
                            &merged_exclude,
                            &merged_exclude_dir,
                            Some(encoding),
                        )
                        .ok()
                    } else {
                        load_workspace_files(
                            &target,
                            &match_pattern,
                            &exclude,
                            &workspace_exclude_dir,
                            Some(encoding),
                        )
                        .ok()
                    };
                    if let Some(loaded) = loaded {
                        for file in loaded {
                            // Normalize path and check for duplicates
                            let normalized_path = PathBuf::from(&file.path)
                                .canonicalize()
                                .unwrap_or_else(|_| PathBuf::from(&file.path));

                            if loaded_paths.insert(normalized_path) {
                                files.push(file);
                            } else {
                                log::debug!("Skipping duplicate file: {:?}", file.path);
                            }
                        }
                    }
                }
            }
        }
    }

    log::info!("load files from workspace count: {:?}", files.len());

    for file in &files {
        log::debug!("loaded file: {:?}", file.path);
    }

    files
}

pub fn calculate_include_and_exclude(emmyrc: &Emmyrc) -> (Vec<String>, Vec<String>, Vec<PathBuf>) {
    let mut include = vec!["**/*.lua".to_string()];
    let mut exclude = Vec::new();
    let mut exclude_dirs = Vec::new();

    for extension in &emmyrc.runtime.extensions {
        if extension.starts_with(".") {
            include.push(format!("**/*{}", extension));
        } else if extension.starts_with("*.") {
            include.push(format!("**/{}", extension));
        } else {
            include.push(extension.clone());
        }
    }

    for ignore_glob in &emmyrc.workspace.ignore_globs {
        exclude.push(ignore_glob.clone());
    }

    for dir in &emmyrc.workspace.ignore_dir {
        exclude_dirs.push(PathBuf::from(dir));
    }

    // remove duplicate
    include.sort();
    include.dedup();

    // remove duplicate
    exclude.sort();
    exclude.dedup();

    (include, exclude, exclude_dirs)
}

pub fn find_library_exclude(
    library: &WorkspaceFolder,
    emmyrc: &Emmyrc,
) -> (Vec<String>, Vec<PathBuf>) {
    let mut exclude = Vec::new();
    let mut exclude_dirs = Vec::new();

    for lib in &emmyrc.workspace.library {
        if let EmmyLibraryItem::Config(detail_config) = &lib {
            let lib_path = PathBuf::from(&detail_config.path);
            if lib_path == library.root {
                exclude = detail_config.ignore_globs.clone();
                exclude_dirs = detail_config.ignore_dir.iter().map(PathBuf::from).collect();
                break;
            }
        }
    }

    (exclude, exclude_dirs)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Emmyrc;
    use std::{
        fs,
        path::{Path, PathBuf},
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
                "emmylua-collect-workspace-files-{}-{}-{}",
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

    fn loaded_paths(files: Vec<LuaFileInfo>) -> HashSet<PathBuf> {
        files
            .into_iter()
            .map(|file| PathBuf::from(file.path))
            .collect()
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

    #[test]
    fn library_is_indexed_even_when_root_is_globally_ignored() {
        let workspace = TestWorkspace::new();
        let main_file = workspace.write_file("lua/main.lua");
        let library_root = workspace.path(".test-deps/runtime/lua/vim");
        let library_file = workspace.write_file(".test-deps/runtime/lua/vim/shared.lua");

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

        let files = collect_workspace_files(
            &[
                WorkspaceFolder::new(workspace.root.clone(), false),
                WorkspaceFolder::new(library_root.clone(), true),
            ],
            &emmyrc,
            None,
            None,
        );

        let loaded = loaded_paths(files);
        assert!(loaded.contains(&main_file));
        assert!(loaded.contains(&library_file));
    }

    #[test]
    fn library_specific_ignores_still_apply() {
        let workspace = TestWorkspace::new();
        let library_root = workspace.path(".test-deps/runtime/lua/vim");
        let kept_file = workspace.write_file(".test-deps/runtime/lua/vim/keep.lua");
        let ignored_dir_file = workspace.write_file(".test-deps/runtime/lua/vim/tests/spec.lua");
        let ignored_glob_file = workspace.write_file(".test-deps/runtime/lua/vim/async.spec.lua");

        let emmyrc = emmyrc_from_json(&format!(
            r#"{{
                "workspace": {{
                    "ignoreDir": [{}],
                    "library": [{{
                        "path": {},
                        "ignoreDir": [{}],
                        "ignoreGlobs": ["**/*.spec.lua"]
                    }}]
                }}
            }}"#,
            json_string(&to_string(&workspace.path(".test-deps"))),
            json_string(&to_string(&library_root)),
            json_string(&to_string(&library_root.join("tests"))),
        ));

        let files = collect_workspace_files(
            &[
                WorkspaceFolder::new(workspace.root.clone(), false),
                WorkspaceFolder::new(library_root.clone(), true),
            ],
            &emmyrc,
            None,
            None,
        );

        let loaded = loaded_paths(files);
        assert!(loaded.contains(&kept_file));
        assert!(!loaded.contains(&ignored_dir_file));
        assert!(!loaded.contains(&ignored_glob_file));
    }

    #[test]
    fn global_ignore_globs_still_apply_to_libraries() {
        let workspace = TestWorkspace::new();
        let library_root = workspace.path(".test-deps/runtime/lua/vim");
        let kept_file = workspace.write_file(".test-deps/runtime/lua/vim/keep.lua");
        let ignored_file = workspace.write_file(".test-deps/runtime/lua/vim/tests/spec.skip.lua");

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

        let files = collect_workspace_files(
            &[
                WorkspaceFolder::new(workspace.root.clone(), false),
                WorkspaceFolder::new(library_root.clone(), true),
            ],
            &emmyrc,
            None,
            None,
        );

        let loaded = loaded_paths(files);
        assert!(loaded.contains(&kept_file));
        assert!(!loaded.contains(&ignored_file));
    }

    #[test]
    fn configured_workspace_root_is_indexed_even_when_parent_is_globally_ignored() {
        let workspace = TestWorkspace::new();
        let main_file = workspace.write_file("lua/main.lua");
        let configured_root = workspace.path(".generated/runtime");
        let configured_file = workspace.write_file(".generated/runtime/shared.lua");

        let emmyrc = emmyrc_from_json(&format!(
            r#"{{
                "workspace": {{
                    "ignoreDir": [{}],
                    "workspaceRoots": [{}]
                }}
            }}"#,
            json_string(&to_string(&workspace.path(".generated"))),
            json_string(&to_string(&configured_root)),
        ));

        let workspace_folders = build_workspace_folders(
            &[WorkspaceFolder::new(workspace.root.clone(), false)],
            &emmyrc,
        );
        let files = collect_workspace_files(&workspace_folders, &emmyrc, None, None);

        let loaded = loaded_paths(files);
        assert!(loaded.contains(&main_file));
        assert!(loaded.contains(&configured_file));
    }

    #[test]
    fn workspace_root_is_indexed_even_when_parent_is_globally_ignored() {
        let workspace = TestWorkspace::new();
        let nested_root = workspace.path("packages/app");
        let nested_file = workspace.write_file("packages/app/init.lua");

        let emmyrc = emmyrc_from_json(&format!(
            r#"{{
                "workspace": {{
                    "ignoreDir": [{}]
                }}
            }}"#,
            json_string(&to_string(&workspace.path("packages"))),
        ));

        let files = collect_workspace_files(
            &[WorkspaceFolder::new(nested_root.clone(), false)],
            &emmyrc,
            None,
            None,
        );

        let loaded = loaded_paths(files);
        assert!(loaded.contains(&nested_file));
    }

    #[test]
    fn nested_global_ignore_dirs_still_apply_inside_library_roots() {
        let workspace = TestWorkspace::new();
        let library_root = workspace.path("libs/runtime/lua/vim");
        let kept_file = workspace.write_file("libs/runtime/lua/vim/keep.lua");
        let ignored_file = workspace.write_file("libs/runtime/lua/vim/tests/spec.lua");

        let emmyrc = emmyrc_from_json(&format!(
            r#"{{
                "workspace": {{
                    "ignoreDir": [{}],
                    "library": [{}]
                }}
            }}"#,
            json_string(&to_string(&library_root.join("tests"))),
            json_string(&to_string(&library_root)),
        ));

        let files = collect_workspace_files(
            &[
                WorkspaceFolder::new(workspace.root.clone(), false),
                WorkspaceFolder::new(library_root.clone(), true),
            ],
            &emmyrc,
            None,
            None,
        );

        let loaded = loaded_paths(files);
        assert!(loaded.contains(&kept_file));
        assert!(!loaded.contains(&ignored_file));
    }
}
