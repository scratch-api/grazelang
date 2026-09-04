use std::{
    collections::HashMap,
    ffi::OsStr,
    fs::File,
    io::Read,
    path::{Path, PathBuf},
    rc::Rc,
    time::Instant,
};

use annotate_snippets::Renderer;
use clap::{Parser, Subcommand};
use serde::{Deserialize, Serialize};

use crate::{
    ast::unparse::UnparseAST,
    codegen, detranspiler, lexer,
    messages::{
        annotations::{self, Source},
        types::{CLIError, GrazeDetranspilerError, GrazeDetranspilerMessage, GrazeSourceMessage},
    },
    parser::{
        self,
        context::ParseContext,
        core::{PeekableLexer, emit_message_eager as emit_message_eager_parse_context},
        cst::{GrazeProgram, IntoResultWithSourceSpan, ParseError},
    },
    settings::{
        GrazeBuildSettings, GrazeDetranspilerSettings, GrazeMessageSetting,
        MultiDataDeclarationsMode, UseShadows,
    },
    visitor::GrazeVisitor,
    zipper,
};

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Parser)]
#[command(name = "graze")]
#[command(version = VERSION)]
#[command(about = "Allows you to manage graze projects, to transpile them to sb3 files and to detranspile sb3 files to graze projects", long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Transpile a single file or a project directory
    Build {
        /// Where there should be shadows and what values they should have by default
        #[arg(value_enum, short, long, default_value = "any-shadows-everywhere")]
        shadows: UseShadows,
        #[arg(value_enum, short, long, default_value = "all")]
        logging: GrazeMessageSetting,
        #[arg(long)]
        log_time: bool,
        /// Path for the sb3 file
        #[arg(short, long)]
        target: Option<PathBuf>,
        // 'r' is reserved for a requirements file
        /// Path for the resources of the project (default: project directory)
        #[arg(short = 'R', long)]
        resources: Option<PathBuf>,
        /// Path for external extensions for the project (default: extensions directory in project directory)
        #[arg(short = 'e', long)]
        extensions: Option<PathBuf>,
        /// Should the transpiler try to use cached versions of external extensions
        #[arg(long, default_value = "true", action = clap::ArgAction::Set)]
        use_cached_extensions: bool,
        /// Should the transpiler create cached versions of external extensions
        #[arg(long, default_value = "true", action = clap::ArgAction::Set)]
        create_cached_extensions: bool,
        /// Path of the file or project directory
        #[arg(default_value = ".")]
        path: PathBuf,
    },
    Unbuild {
        #[arg(long)]
        preserve_monitor_ids: bool,
        #[arg(long)]
        preserve_internal_monitor_value: bool,
        #[arg(long)]
        explicitly_typed_string_parameters: bool,
        #[arg(long)]
        multi_asset_declarations: bool,
        #[arg(value_enum, long, default_value = "none")]
        multi_data_declarations: MultiDataDeclarationsMode,
        #[arg(value_enum, short, long, default_value = "all")]
        logging: GrazeMessageSetting,
        #[arg(long)]
        log_time: bool,
        /// Path for the project
        #[arg(short, long)]
        target: Option<PathBuf>,
        /// Path for the resources of the project (default: project directory)
        #[arg(short = 'R', long)]
        resources: Option<PathBuf>,
        /// Path of the file
        #[arg(default_value = ".")]
        path: PathBuf,
    },
}

pub type SourceFiles = HashMap<u32, Source>;
pub type ContextualParseOut = Result<(GrazeProgram, SourceFiles), (ParseError, SourceFiles)>;

#[expect(clippy::result_large_err)]
pub fn parse_project_directory(path: &Path, context: &mut ParseContext) -> ContextualParseOut {
    let mut program = Vec::new();
    let mut file_id = 0_u32;
    let mut source_files = HashMap::from([(
        0,
        Source {
            content: String::new(),
            path: PathBuf::new(),
            line_starts: Vec::new(),
        },
    )]);
    for i in path
        .read_dir()
        .into_result_with_source_span((Default::default(), file_id))
        .map_err(|value| (value, std::mem::take(&mut source_files)))?
    {
        let current_file = i
            .into_result_with_source_span((Default::default(), file_id))
            .map_err(|value| (value, std::mem::take(&mut source_files)))?
            .path();
        if current_file.extension().and_then(OsStr::to_str) != Some("graze") {
            continue;
        }
        let graze_code = {
            let mut file = File::open(&current_file)
                .into_result_with_source_span((Default::default(), file_id))
                .map_err(|value| (value, std::mem::take(&mut source_files)))?;
            let mut buf = String::new();
            file.read_to_string(&mut buf)
                .into_result_with_source_span((Default::default(), file_id))
                .map_err(|value| (value, std::mem::take(&mut source_files)))?;
            buf
        };
        let lexer = lexer::create_lexer(&graze_code);
        let mut lexer = PeekableLexer::new(lexer, file_id);
        let parsed = parser::parse_graze_program(&mut lexer, context);
        let Ok(parsed) = parsed else {
            if context.settings.message_setting == GrazeMessageSetting::ExitOnError {
                source_files.insert(
                    file_id,
                    Source {
                        line_starts: lexer.lexer.extras.0,
                        content: graze_code,
                        path: current_file,
                    },
                );
                file_id += 1;
                continue;
            } else if context.settings.message_setting == GrazeMessageSetting::ExitOnErrorUnlogged {
                std::process::exit(1);
            } else {
                source_files.insert(
                    file_id,
                    Source {
                        line_starts: lexer.lexer.extras.0,
                        content: graze_code,
                        path: current_file,
                    },
                );
                return Err((parsed.unwrap_err(), source_files));
            }
        };
        program.extend(parsed.0);
        source_files.insert(
            file_id,
            Source {
                line_starts: lexer.lexer.extras.0,
                content: graze_code,
                path: current_file,
            },
        );
        file_id += 1;
        source_files.insert(
            file_id,
            Source {
                content: String::new(),
                path: PathBuf::new(),
                line_starts: Vec::new(),
            },
        );
    }
    Ok((GrazeProgram(program), source_files))
}

#[expect(clippy::result_large_err)]
pub fn parse_single_file(path: &Path, context: &mut ParseContext) -> ContextualParseOut {
    let graze_code = {
        let mut file = File::open(path)
            .into_result_with_source_span(Default::default())
            .map_err(|value| {
                (
                    value,
                    HashMap::from([(
                        0,
                        Source {
                            content: String::new(),
                            path: PathBuf::new(),
                            line_starts: Vec::new(),
                        },
                    )]),
                )
            })?;
        let mut buf = String::new();
        file.read_to_string(&mut buf)
            .map_err(|value| ParseError::IoError {
                source: Rc::new(value),
                source_span: Default::default(),
            })
            .map_err(|value| {
                (
                    value,
                    HashMap::from([(
                        0,
                        Source {
                            content: String::new(),
                            path: PathBuf::new(),
                            line_starts: Vec::new(),
                        },
                    )]),
                )
            })?;
        buf
    };
    let lexer = lexer::create_lexer(&graze_code);
    let mut lexer = PeekableLexer::new(lexer, 0);
    let parsed = parser::parse_graze_program(&mut lexer, context);
    let Ok(parsed) = parsed else {
        if context.settings.message_setting == GrazeMessageSetting::ExitOnError {
            return Ok((
                GrazeProgram(Vec::new()),
                HashMap::from([(
                    0,
                    Source {
                        line_starts: lexer.lexer.extras.0,
                        content: graze_code,
                        path: path.to_path_buf(),
                    },
                )]),
            ));
        } else if context.settings.message_setting == GrazeMessageSetting::ExitOnErrorUnlogged {
            std::process::exit(1);
        } else {
            return Err((
                parsed.unwrap_err(),
                HashMap::from([(
                    0,
                    Source {
                        line_starts: lexer.lexer.extras.0,
                        content: graze_code,
                        path: path.to_path_buf(),
                    },
                )]),
            ));
        }
    };
    Ok((
        parsed,
        HashMap::from([(
            0,
            Source {
                line_starts: lexer.lexer.extras.0,
                content: graze_code,
                path: path.to_path_buf(),
            },
        )]),
    ))
}

pub fn count_errors_and_warnings(messages: &[GrazeSourceMessage]) -> (usize, usize) {
    let mut errors = 0;
    let mut warnings = 0;
    for message in messages {
        match message {
            GrazeSourceMessage::Error(..) => {
                errors += 1;
            }
            GrazeSourceMessage::Warning(..) => {
                warnings += 1;
            }
            _ => (),
        }
    }
    (errors, warnings)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Successful {
    Yes,
    No,
}

impl Cli {
    pub fn execute(&self) -> i32 {
        match &self.command {
            Commands::Build {
                shadows,
                logging,
                target,
                resources,
                extensions,
                use_cached_extensions,
                create_cached_extensions,
                path,
                log_time,
            } => Self::build(
                *shadows,
                *logging,
                target.as_deref(),
                resources.as_deref(),
                extensions.as_deref(),
                *use_cached_extensions,
                *create_cached_extensions,
                path,
                *log_time,
            ),
            Commands::Unbuild {
                preserve_monitor_ids,
                preserve_internal_monitor_value,
                explicitly_typed_string_parameters,
                multi_asset_declarations,
                multi_data_declarations,
                logging,
                log_time,
                target,
                resources,
                path,
            } => Self::unbuild(
                *preserve_monitor_ids,
                *preserve_internal_monitor_value,
                *explicitly_typed_string_parameters,
                *multi_asset_declarations,
                *multi_data_declarations,
                *logging,
                *log_time,
                target.as_deref(),
                resources.as_deref(),
                path.as_path(),
            ),
        }
    }

    pub fn print_build_errors(
        messages: &mut Vec<GrazeSourceMessage>,
        source_files: &HashMap<u32, Source>,
        force_error: bool,
    ) -> Successful {
        let renderer = Renderer::styled();
        let (error_count, warning_count) = count_errors_and_warnings(messages);
        let error = error_count > 0 || force_error;
        if error {
            messages.push(GrazeSourceMessage::Unsuccessful {
                error_count,
                warning_count,
            });
        }
        annotations::annotate(
            messages.iter(),
            |id| source_files.get(&id).unwrap().as_descriptor(),
            |ann, _| {
                let rendered = renderer.render(ann);
                anstream::eprintln!("{rendered}");
            },
        );
        if error {
            Successful::No
        } else {
            Successful::Yes
        }
    }

    #[expect(clippy::too_many_arguments)]
    pub fn build(
        shadows: UseShadows,
        logging: GrazeMessageSetting,
        target: Option<&Path>,
        resources: Option<&Path>,
        extensions: Option<&Path>,
        use_cached_extensions: bool,
        create_cached_extensions: bool,
        path: &Path,
        log_time: bool,
    ) -> i32 {
        let total_time = Instant::now();
        let path_is_file = path.is_file();
        let mut context = ParseContext::new(
            GrazeBuildSettings {
                message_setting: logging,
                use_shadows: shadows,
                resources_path: Some(resources.map(Path::to_path_buf).unwrap_or_else(|| {
                    if path_is_file {
                        path.parent().unwrap_or(Path::new("/")).to_path_buf()
                    } else {
                        path.to_path_buf()
                    }
                })),
                extensions_path: Some(extensions.map(Path::to_path_buf).unwrap_or_else(|| {
                    if path_is_file {
                        path.parent().unwrap_or(Path::new("/"))
                    } else {
                        path
                    }
                    .join("extensions")
                })),
                use_cached_extensions,
                create_cached_extensions,
            },
            Default::default(),
        );
        let parse_timer = Instant::now();
        let Ok(path) = path.canonicalize() else {
            emit_message_eager_parse_context(
                &mut context,
                CLIError::PathDoesNotExist.into(),
                GrazeMessageSetting::Errors,
            );
            Self::print_build_errors(&mut context.messages, &HashMap::new(), true);
            std::process::exit(1);
        };
        let (parsed, source_files) = if path.is_dir() {
            match parse_project_directory(&path, &mut context) {
                Ok(value) => value,
                Err((_, source_files)) => {
                    Self::print_build_errors(&mut context.messages, &source_files, true);
                    return 1;
                }
            }
        } else if path_is_file {
            match parse_single_file(&path, &mut context) {
                Ok(value) => value,
                Err((_, source_files)) => {
                    Self::print_build_errors(&mut context.messages, &source_files, true);
                    return 1;
                }
            }
        } else {
            emit_message_eager_parse_context(
                &mut context,
                CLIError::PathNeitherFileNorDirectory.into(),
                GrazeMessageSetting::Errors,
            );
            Self::print_build_errors(&mut context.messages, &HashMap::new(), true);
            return 1;
        };
        let parse_time = parse_timer.elapsed();
        if !context.successful {
            Self::print_build_errors(&mut context.messages, &source_files, true);
            return 1;
        }
        let codegen_timer = Instant::now();
        let mut context = {
            let message_setting = context.settings.message_setting;
            match codegen::core::GrazeSb3GeneratorContext::new(context) {
                Ok(value) => value,
                Err((err, mut messages)) => {
                    if message_setting >= GrazeMessageSetting::ExitOnError {
                        messages.push(err.into());
                    }
                    Self::print_build_errors(&mut messages, &source_files, true);
                    return 1;
                }
            }
        };
        let visitor = codegen::core::GrazeSb3Generator;
        if let Err(err) = visitor.visit_graze_program(&parsed, &mut context) {
            if matches!(
                context.settings.message_setting,
                GrazeMessageSetting::None | GrazeMessageSetting::ExitOnErrorUnlogged
            ) {
                Self::print_build_errors(&mut context.messages, &source_files, true);
                return 1;
            }
            context.messages.push(err.into());
        }
        let codegen_time = codegen_timer.elapsed();
        if !context.successful {
            Self::print_build_errors(&mut context.messages, &source_files, true);
            return 1;
        }
        let (mut output_path, set_extension) = match target {
            Some(target) if target.is_file() || !target.exists() => (target.to_path_buf(), false),
            Some(target) => (
                target.join(
                    path.file_name()
                        .and_then(OsStr::to_str)
                        .unwrap_or("project"),
                ),
                true,
            ),
            None if path_is_file => (
                {
                    let mut path = path;
                    if path
                        .extension()
                        .is_some_and(|value| value == OsStr::new("sb3"))
                    {
                        path.set_extension("out");
                        path.add_extension("sb3");
                    }
                    path
                },
                true,
            ),
            None => (
                path.join(
                    path.file_name()
                        .and_then(OsStr::to_str)
                        .unwrap_or("project"),
                ),
                true,
            ),
        };
        if set_extension {
            output_path.set_extension("sb3");
        }
        let zip_timer = Instant::now();
        if let Err(err) = zipper::write_to_zip_path(&output_path, &context) {
            if context.settings.message_setting >= GrazeMessageSetting::ExitOnError {
                context.messages.push(err.into());
            }
            Self::print_build_errors(&mut context.messages, &source_files, true);
            return 1;
        }
        if Self::print_build_errors(&mut context.messages, &source_files, false) == Successful::No {
            return 1;
        }
        let zip_time = zip_timer.elapsed();
        if log_time {
            println!("Parsing took: {:?}", parse_time);
            println!("Codegen took: {:?}", codegen_time);
            println!("Zipping took: {:?}", zip_time);
            println!("Total time: {:?}", total_time.elapsed());
        }
        0
    }

    pub fn print_unbuild_errors(
        messages: &mut Vec<GrazeDetranspilerMessage>,
        force_error: bool,
    ) -> Successful {
        if messages.is_empty() {
            return if force_error {
                Successful::No
            } else {
                Successful::Yes
            };
        }
        dbg!(messages);
        return Successful::Yes;
        // let renderer = Renderer::styled();
        // let (error_count, warning_count) = count_errors_and_warnings(messages);
        // let error = error_count > 0 || force_error;
        // if error {
        //     messages.push(GrazeSourceMessage::Unsuccessful {
        //         error_count,
        //         warning_count,
        //     });
        // }
        // annotations::annotate(
        //     messages.iter(),
        //     |id| source_files.get(&id).unwrap().as_descriptor(),
        //     |ann, _| {
        //         let rendered = renderer.render(ann);
        //         anstream::eprintln!("{rendered}");
        //     },
        // );
        // if error {
        //     Successful::No
        // } else {
        //     Successful::Yes
        // }
    }

    #[expect(clippy::too_many_arguments)]
    pub fn unbuild(
        preserve_monitor_ids: bool,
        preserve_internal_monitor_value: bool,
        explicitly_typed_string_parameters: bool,
        multi_asset_declarations: bool,
        multi_data_declarations: MultiDataDeclarationsMode,
        logging: GrazeMessageSetting,
        log_time: bool,
        target: Option<&Path>,
        resources: Option<&Path>,
        path: &Path,
    ) -> i32 {
        macro_rules! single_error {
            ($logging:expr, $error:expr) => {{
                let mut errors = if $logging >= GrazeMessageSetting::ExitOnError {
                    vec![$error]
                } else {
                    Vec::new()
                };
                Self::print_unbuild_errors(&mut errors, true);
                return 1;
            }};
        }
        let total_time = Instant::now();
        if !path.is_file() {
            single_error!(
                logging,
                GrazeDetranspilerMessage::Error(GrazeDetranspilerError::PathIsNotAFile {
                    path: path.to_path_buf(),
                })
            );
        }
        let unzip_1_timer = Instant::now();
        let Ok(path) = path.canonicalize() else {
            single_error!(
                logging,
                GrazeDetranspilerMessage::Error(GrazeDetranspilerError::PathIsNotAFile {
                    path: path.to_path_buf(),
                })
            );
        };
        let Ok(reader) = std::fs::File::open(&path) else {
            single_error!(
                logging,
                GrazeDetranspilerMessage::Error(GrazeDetranspilerError::CannotReadFile { path })
            );
        };
        let Ok(mut zip_file) = zip::ZipArchive::new(reader) else {
            single_error!(
                logging,
                GrazeDetranspilerMessage::Error(GrazeDetranspilerError::InvalidZipFile { path })
            );
        };
        let Ok(mut project_file) = zip_file.by_name("project.json") else {
            single_error!(
                logging,
                GrazeDetranspilerMessage::Error(GrazeDetranspilerError::InvalidZipFile { path })
            );
        };
        let mut project = String::with_capacity(project_file.size() as usize);
        let Ok(_) = project_file.read_to_string(&mut project) else {
            single_error!(
                logging,
                GrazeDetranspilerMessage::Error(GrazeDetranspilerError::InvalidZipFile { path })
            );
        };
        drop(project_file);
        let Ok(project) = serde_json::from_str(&project) else {
            single_error!(
                logging,
                GrazeDetranspilerMessage::Error(GrazeDetranspilerError::InvalidProjectJson {
                    path
                })
            );
        };
        let unzip_1_time = unzip_1_timer.elapsed();
        let settings = GrazeDetranspilerSettings {
            preserve_monitor_ids,
            preserve_internal_monitor_value,
            explicitly_typed_string_parameters,
            multi_asset_declarations,
            multi_data_declarations,
            message_setting: logging,
        };
        let build_ast_timer = Instant::now();
        let (ast, assets, mut messages) =
            match detranspiler::core::convert_project(&project, settings) {
                Ok(value) => value,
                Err(mut errors) => {
                    Self::print_unbuild_errors(&mut errors, true);
                    return 1;
                }
            };
        let build_ast_time = build_ast_timer.elapsed();
        let unparse_timer = Instant::now();
        let output_path = match target {
            Some(target) if target.is_file() || !target.exists() => target.to_path_buf(),
            Some(target) => target.join("main.graze"),
            None => {
                let mut path = path.to_path_buf();
                if path
                    .extension()
                    .is_some_and(|value| value == OsStr::new("graze"))
                {
                    path.add_extension("out");
                    path.add_extension("graze");
                } else {
                    path.set_extension("graze");
                }
                path
            }
        };
        let Ok(mut output_file) = std::fs::OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open(&output_path)
        else {
            messages.push(GrazeDetranspilerMessage::Error(
                GrazeDetranspilerError::CannotWriteFile { path: output_path },
            ));
            Self::print_unbuild_errors(&mut messages, true);
            return 1;
        };
        let Ok(()) = ast.unparse_into_io(&mut output_file) else {
            messages.push(GrazeDetranspilerMessage::Error(
                GrazeDetranspilerError::CannotWriteFile { path: output_path },
            ));
            Self::print_unbuild_errors(&mut messages, true);
            return 1;
        };
        let unparse_time = unparse_timer.elapsed();
        let unzip_2_timer = Instant::now();
        let resource_path = match resources {
            Some(resource_path) => resource_path,
            None => match target {
                Some(target) if target.is_file() || !target.exists() => {
                    target.parent().unwrap_or(Path::new("/"))
                }
                Some(target) => target,
                None => path.parent().unwrap_or(Path::new("/")),
            },
        };
        for (zip_asset, out_asset) in assets {
            let output_path = resource_path.join(out_asset.as_str());
            let Ok(mut output_file) = std::fs::OpenOptions::new()
                .write(true)
                .truncate(true)
                .create(true)
                .open(&output_path)
            else {
                messages.push(GrazeDetranspilerMessage::Error(
                    GrazeDetranspilerError::CannotWriteFile { path: output_path },
                ));
                Self::print_unbuild_errors(&mut messages, true);
                return 1;
            };
            let Ok(mut asset_file) = zip_file.by_name(&zip_asset) else {
                messages.push(GrazeDetranspilerMessage::Error(
                    GrazeDetranspilerError::MissingAsset { md3ext: zip_asset },
                ));
                Self::print_unbuild_errors(&mut messages, true);
                return 1;
            };
            let Ok(_) = std::io::copy(&mut asset_file, &mut output_file) else {
                messages.push(GrazeDetranspilerMessage::Error(
                    GrazeDetranspilerError::CannotWriteFile { path: output_path },
                ));
                Self::print_unbuild_errors(&mut messages, true);
                return 1;
            };
        }
        if Self::print_unbuild_errors(&mut messages, false) == Successful::No {
            return 1;
        }
        let unzip_2_time = unzip_2_timer.elapsed();
        if log_time {
            println!("Extracting project json took: {:?}", unzip_1_time);
            println!("Building AST took {:?}", build_ast_time);
            println!("Unparsing AST took: {:?}", unparse_time);
            println!("Unzipping assets took: {:?}", unzip_2_time);
            println!("Total time: {:?}", total_time.elapsed());
        }
        0
    }
}
