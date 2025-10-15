use emmylua_code_analysis::{
    EmmyLuaAnalysis, Emmyrc, LuaFileInfo, load_configs, load_workspace_files, update_code_style,
};
use fern::Dispatch;
use log::LevelFilter;
use std::{
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
};

fn root_from_configs(config_paths: &[PathBuf], fallback: &Path) -> PathBuf {
    if config_paths.len() != 1 {
        fallback.to_path_buf()
    } else {
        let config_path = &config_paths[0];
        // Need to convert to canonical path to ensure parent() is not an empty
        // string in the case the path is a relative basename.
        match config_path.canonicalize() {
            Ok(path) => path.parent().unwrap().to_path_buf(),
            Err(err) => {
                log::error!(
                    "Failed to canonicalize config path: \"{:?}\": {}",
                    config_path,
                    err
                );
                fallback.to_path_buf()
            }
        }
    }
}

pub fn setup_logger(verbose: bool) {
    let logger = Dispatch::new()
        .format(move |out, message, record| {
            let (color, reset) = match record.level() {
                log::Level::Error => ("\x1b[31m", "\x1b[0m"), // Red
                log::Level::Warn => ("\x1b[33m", "\x1b[0m"),  // Yellow
                log::Level::Info | log::Level::Debug | log::Level::Trace => ("", ""),
            };
            out.finish(format_args!(
                "{}{}: {}{}",
                color,
                record.level(),
                if verbose {
                    format!("({}) {}", record.target(), message)
                } else {
                    message.to_string()
                },
                reset
            ))
        })
        .level(if verbose {
            LevelFilter::Info
        } else {
            LevelFilter::Warn
        })
        .chain(std::io::stderr());

    if let Err(e) = logger.apply() {
        eprintln!("Failed to apply logger: {:?}", e);
    }
}

pub fn load_workspace(
    main_path: PathBuf,
    mut workspace_folders: Vec<PathBuf>,
    config_paths: Option<Vec<PathBuf>>,
    exclude_pattern: Option<Vec<String>>,
    include_pattern: Option<Vec<String>>,
) -> Option<EmmyLuaAnalysis> {
    let (config_files, config_root): (Vec<PathBuf>, PathBuf) =
        if let Some(config_paths) = config_paths {
            (
                config_paths.clone(),
                root_from_configs(&config_paths, &main_path),
            )
        } else {
            (
                vec![
                    main_path.join(".luarc.json"),
                    main_path.join(".emmyrc.json"),
                ]
                .into_iter()
                .filter(|path| path.exists())
                .collect(),
                main_path.clone(),
            )
        };

    let mut emmyrc = load_configs(config_files, None);
    log::info!(
        "Pre processing configurations using root: \"{}\"",
        config_root.display()
    );
    emmyrc.pre_process_emmyrc(&config_root);

    for lib in &emmyrc.workspace.library {
        workspace_folders.push(PathBuf::from_str(lib).unwrap());
    }

    let mut analysis = EmmyLuaAnalysis::new();

    for path in &workspace_folders {
        analysis.add_main_workspace(path.clone());
    }

    for root in &emmyrc.workspace.workspace_roots {
        analysis.add_main_workspace(PathBuf::from_str(root).unwrap());
    }

    analysis.update_config(Arc::new(emmyrc));
    analysis.init_std_lib(None);

    let file_infos = collect_files(
        &workspace_folders,
        &analysis.emmyrc,
        exclude_pattern,
        include_pattern,
    );
    let files = file_infos
        .into_iter()
        .filter_map(|file| {
            if file.path.ends_with(".editorconfig") {
                let file_path = PathBuf::from(file.path);
                let parent_dir = file_path
                    .parent()
                    .unwrap()
                    .to_path_buf()
                    .to_string_lossy()
                    .to_string()
                    .replace("\\", "/");
                let file_normalized = file_path.to_string_lossy().to_string().replace("\\", "/");
                update_code_style(&parent_dir, &file_normalized);
                None
            } else {
                Some(file.into_tuple())
            }
        })
        .collect();
    analysis.update_files_by_path(files);

    Some(analysis)
}

pub fn collect_files(
    workspaces: &Vec<PathBuf>,
    emmyrc: &Emmyrc,
    exclude_pattern: Option<Vec<String>>,
    include_pattern: Option<Vec<String>>,
) -> Vec<LuaFileInfo> {
    let mut files = Vec::new();
    let (match_pattern, exclude, exclude_dir) =
        calculate_include_and_exclude(emmyrc, exclude_pattern, include_pattern);

    let encoding = &emmyrc.workspace.encoding;

    for workspace in workspaces {
        let loaded = load_workspace_files(
            workspace,
            &match_pattern,
            &exclude,
            &exclude_dir,
            Some(encoding),
        )
        .ok();
        if let Some(loaded) = loaded {
            files.extend(loaded);
        }
    }

    files
}

/// File patterns for workspace scanning: (include_patterns, exclude_patterns, exclude_dirs)
type FilePatterns = (Vec<String>, Vec<String>, Vec<PathBuf>);

pub fn calculate_include_and_exclude(
    emmyrc: &Emmyrc,
    exclude_pattern: Option<Vec<String>>,
    include_pattern: Option<Vec<String>>,
) -> FilePatterns {
    let mut include = Vec::new();
    let mut exclude = Vec::new();
    let mut exclude_dirs = Vec::new();

    if let Some(p) = include_pattern {
        include.extend(p);
    } else {
        include.push("**/*.lua".to_string());
        include.push("**/.editorconfig".to_string());

        for extension in &emmyrc.runtime.extensions {
            if extension.starts_with(".") {
                log::info!("Adding extension: **/*{}", extension);
                include.push(format!("**/*{}", extension));
            } else if extension.starts_with("*.") {
                log::info!("Adding extension: **/{}", extension);
                include.push(format!("**/{}", extension));
            } else {
                log::info!("Adding extension: {}", extension);
                include.push(extension.clone());
            }
        }
    }

    for ignore_glob in &emmyrc.workspace.ignore_globs {
        log::info!("Adding ignore glob: {}", ignore_glob);
        exclude.push(ignore_glob.clone());
    }

    if let Some(p) = exclude_pattern {
        log::info!("Adding excludes from \"--exclude(or --ignore)\": {:?}", p);
        exclude.extend(p);
    }

    for dir in &emmyrc.workspace.ignore_dir {
        log::info!("Adding ignore dir: {}", dir);
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
