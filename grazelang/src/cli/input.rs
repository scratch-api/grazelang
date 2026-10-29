use std::{
    collections::HashMap,
    ffi::OsStr,
    fs::File,
    io::Read,
    ops::Range,
    path::{Path, PathBuf},
    rc::Rc,
    time::Instant,
};

use annotate_snippets::Renderer;
use clap::{Parser, Subcommand};
use serde::{Deserialize, Serialize};

use crate::{
    codegen, lexer,
    messages::{
        annotations::{self, Source},
        types::{CLIError, GrazeSourceMessage},
    },
    parser::{
        self,
        context::ParseContext,
        core::{PeekableLexer, emit_message_eager as emit_message_eager_parse_context},
        cst::{GrazeProgram, IntoResultWithSourceSpan, ParseError},
    },
    settings::{GrazeBuildSettings, GrazeMessageSetting, MultiDataDeclarationsMode, UseShadows},
    visitor::GrazeVisitor,
    zipper,
};

#[cfg(all(feature = "detranspiler", not(feature = "formatter")))]
use crate::{
    ast::{
        types as ast_types,
        unparse::UnparseAST,
        unparse_formatted::{UnparseASTFormatted, UnparseFormattedSettings},
    },
    detranspiler,
    messages::types::{GrazeDetranspilerError, GrazeDetranspilerMessage},
    settings::GrazeDetranspilerSettings,
};

#[cfg(all(feature = "detranspiler", feature = "formatter"))]
use crate::{
    ast::{
        types as ast_types,
        unparse::UnparseAST,
        unparse_cst_formatted::{UnparseCSTFormatted, UnparseCSTFormattedContext},
        unparse_formatted::{UnparseASTFormatted, UnparseFormattedSettings},
    },
    detranspiler,
    lexer::StyleToken,
    messages::types::{FormatterUnsuccessful, GrazeDetranspilerError, GrazeDetranspilerMessage},
    parser::cst,
    settings::GrazeDetranspilerSettings,
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
        output: Option<PathBuf>,
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
    /// Detranspile an sb3 file into a single file or a project directory
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
        #[arg(long)]
        multi_file_project: bool,
        #[arg(long)]
        no_formatting: bool,
        #[arg(long, default_value = "100")]
        formatting_width: usize,
        #[arg(long, default_value = "4")]
        formatting_indentation: isize,
        /// Forces code blocks to use new lines for inner statements
        #[arg(long)]
        formatting_hard_lines_in_code_blocks: bool,
        #[arg(value_enum, short, long, default_value = "all")]
        logging: GrazeMessageSetting,
        #[arg(long)]
        log_time: bool,
        /// Path for the project
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Path for the resources of the project (default: project directory)
        #[arg(short = 'R', long)]
        resources: Option<PathBuf>,
        /// Path of the file
        #[arg(default_value = ".")]
        path: PathBuf,
    },
    #[clap(visible_alias("fmt"))]
    /// Format a single file or a project directory
    Format {
        #[arg(long, default_value = "100")]
        width: usize,
        #[arg(long, default_value = "4")]
        indentation: isize,
        /// Force code blocks to use new lines for inner statements
        #[arg(long)]
        hard_lines_in_code_blocks: bool,
        #[arg(value_enum, short, long, default_value = "all")]
        logging: GrazeMessageSetting,
        #[arg(long)]
        log_time: bool,
        /// Path of the file or project directory
        #[arg(default_value = ".")]
        path: PathBuf,
    },
}

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct StringArena {
    pub value: String,
    entries: Vec<Range<usize>>,
}

impl StringArena {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn allocate_str<'a>(&'a mut self, string: &str) -> (usize, &'a str) {
        let start = self.value.len();
        self.value.push_str(string);
        let end = self.value.len();
        self.entries.push(start..end);
        // SAFETY: `String::push_str` was just used to add `string` to `self.value`.
        // The bounds are determined by the state of `self.value` before and after the `String::push_str` call.
        let string = unsafe { self.value.get_unchecked(start..end) };
        (self.entries.len() - 1, string)
    }
    pub fn allocate_from_io<'a, R>(&'a mut self, io: &mut R) -> std::io::Result<(usize, &'a str)>
    where
        R: std::io::Read,
    {
        let start = self.value.len();
        match io.read_to_string(&mut self.value) {
            Ok(_) => {
                let end = self.value.len();
                self.entries.push(start..end);
                // SAFETY: `std::io::Read::read_to_string` was just successfully used to read the io to `self.value`.
                // The bounds are determined by the state of `self.value` before and after the `std::io::Read::read_to_string` call.
                let string = unsafe { self.value.get_unchecked(start..end) };
                Ok((self.entries.len() - 1, string))
            }
            Err(err) => {
                self.value.truncate(start);
                Err(err)
            }
        }
    }
    pub fn get_str(&self, index: usize) -> Option<&str> {
        let range = self.entries.get(index)?.clone();
        // SAFETY: The ranges can only be pushed by internal functions.
        // These functions also create a `&str` with `self.value.get_unchecked` over the same range.
        Some(unsafe { self.value.get_unchecked(range) })
    }
    pub fn len(&self) -> usize {
        self.entries.len()
    }
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
    pub fn clear(&mut self) {
        self.value.clear();
        self.entries.clear();
    }
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
            if context.settings.message_setting == GrazeMessageSetting::ExitOnErrorUnlogged {
                std::process::exit(1);
            } else {
                source_files.insert(
                    file_id,
                    Source {
                        line_starts: lexer.lexer.extras.line_starts,
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
                line_starts: lexer.lexer.extras.line_starts,
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

#[cfg(feature = "formatter")]
pub type ContextualFormatterParseOut<'a> = Result<
    (
        HashMap<PathBuf, (GrazeProgram, Vec<StyleToken<'a>>)>,
        SourceFiles,
    ),
    (ParseError, SourceFiles),
>;

#[cfg(feature = "formatter")]
#[expect(clippy::result_large_err)]
pub fn parse_project_directory_for_formatter<'a>(
    path: &Path,
    string_arena: &'a mut StringArena,
    context: &mut ParseContext,
) -> ContextualFormatterParseOut<'a> {
    let mut program = HashMap::new();
    let mut file_id = 0_u32;
    let mut source_files = HashMap::from([(
        0,
        Source {
            content: String::new(),
            path: PathBuf::new(),
            line_starts: Vec::new(),
        },
    )]);
    string_arena.clear();
    for i in IntoResultWithSourceSpan::<ParseError>::into_result_with_source_span(
        path.read_dir(),
        (Default::default(), file_id),
    )
    .map_err(|value| (value, std::mem::take(&mut source_files)))?
    {
        let current_file = IntoResultWithSourceSpan::<ParseError>::into_result_with_source_span(
            i,
            (Default::default(), file_id),
        )
        .map_err(|value| (value, std::mem::take(&mut source_files)))?
        .path();
        if current_file.extension().and_then(OsStr::to_str) != Some("graze") {
            continue;
        }
        let (_, graze_code) = {
            let mut file = IntoResultWithSourceSpan::<ParseError>::into_result_with_source_span(
                File::open(&current_file),
                (Default::default(), file_id),
            )
            .map_err(|value| (value, std::mem::take(&mut source_files)))?;
            IntoResultWithSourceSpan::<ParseError>::into_result_with_source_span(
                string_arena.allocate_from_io(&mut file),
                (Default::default(), file_id),
            )
            .map_err(|value| (value, std::mem::take(&mut source_files)))?
        };
        source_files.insert(
            file_id,
            Source {
                content: graze_code.to_string(),
                path: current_file,
                line_starts: Vec::new(),
            },
        );
        file_id += 1;
    }
    for (
        index,
        Source {
            content: _,
            path: current_file,
            line_starts,
        },
    ) in &mut source_files
    {
        let graze_code = string_arena.get_str(*index as usize).unwrap();
        let lexer = lexer::create_lexer(graze_code);
        let mut lexer = PeekableLexer::new(lexer, file_id);
        let parsed = parser::parse_graze_program(&mut lexer, context);
        let Ok(parsed) = parsed else {
            if context.settings.message_setting == GrazeMessageSetting::ExitOnErrorUnlogged {
                std::process::exit(1);
            } else {
                *line_starts = lexer.lexer.extras.line_starts;
                return Err((parsed.unwrap_err(), std::mem::take(&mut source_files)));
            }
        };
        program.insert(
            current_file.clone(),
            (cst::GrazeProgram(parsed.0), lexer.lexer.extras.style_tokens),
        );
        *line_starts = lexer.lexer.extras.line_starts;
    }
    Ok((program, source_files))
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
        if context.settings.message_setting == GrazeMessageSetting::ExitOnErrorUnlogged {
            std::process::exit(1);
        } else {
            return Err((
                parsed.unwrap_err(),
                HashMap::from([(
                    0,
                    Source {
                        line_starts: lexer.lexer.extras.line_starts,
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
                line_starts: lexer.lexer.extras.line_starts,
                content: graze_code,
                path: path.to_path_buf(),
            },
        )]),
    ))
}

#[cfg(feature = "formatter")]
#[expect(clippy::result_large_err)]
pub fn parse_single_file_for_formatter<'a>(
    path: &Path,
    string_arena: &'a mut StringArena,
    context: &mut ParseContext,
) -> ContextualFormatterParseOut<'a> {
    let mut source_files = HashMap::new();
    string_arena.clear();
    let (_, graze_code) = {
        let mut file = IntoResultWithSourceSpan::<ParseError>::into_result_with_source_span(
            File::open(path),
            Default::default(),
        )
        .map_err(|value| (value, std::mem::take(&mut source_files)))?;
        string_arena
            .allocate_from_io(&mut file)
            .into_result_with_source_span(Default::default())
            .map_err(|value| (value, std::mem::take(&mut source_files)))?
    };
    let (path, parsed) = {
        let Source {
            content: _,
            path,
            line_starts,
        } = source_files
            .entry(0)
            .insert_entry(Source {
                content: graze_code.to_string(),
                path: path.to_path_buf(),
                line_starts: Vec::new(),
            })
            .into_mut();
        let lexer = lexer::create_lexer(graze_code);
        let mut lexer = PeekableLexer::new(lexer, 0);
        let parsed = parser::parse_graze_program(&mut lexer, context);
        *line_starts = lexer.lexer.extras.line_starts;
        let Ok(parsed) = parsed else {
            if context.settings.message_setting == GrazeMessageSetting::ExitOnErrorUnlogged {
                std::process::exit(1);
            } else {
                return Err((parsed.unwrap_err(), std::mem::take(&mut source_files)));
            }
        };
        (
            path.to_path_buf(),
            (parsed, lexer.lexer.extras.style_tokens),
        )
    };
    Ok((HashMap::from([(path, parsed)]), source_files))
}

#[cfg(feature = "detranspiler")]
pub fn unparse_into_file<A>(
    ast: &A,
    path: PathBuf,
    messages: &mut Vec<GrazeDetranspilerMessage>,
    formatting_settings: Option<&UnparseFormattedSettings>,
) -> Result<(), i32>
where
    for<'a> &'a A: UnparseASTFormatted,
    A: UnparseAST,
{
    let Ok(mut output_file) = std::fs::OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(&path)
    else {
        messages.push(GrazeDetranspilerMessage::Error(
            GrazeDetranspilerError::CannotWriteFile { path },
        ));
        Cli::print_unbuild_errors(messages, true);
        return Err(1);
    };
    let Ok(()) = (if let Some(settings) = formatting_settings {
        ast.unparse_formatted_into_io(&mut output_file, settings)
    } else {
        ast.unparse_into_io(&mut output_file)
    }) else {
        messages.push(GrazeDetranspilerMessage::Error(
            GrazeDetranspilerError::CannotWriteFile { path },
        ));
        Cli::print_unbuild_errors(messages, true);
        return Err(1);
    };
    Ok(())
}

#[cfg(feature = "formatter")]
pub fn unparse_into_file_for_formatter<'a, C>(
    cst: &'a C,
    path: PathBuf,
    messages: &mut Vec<GrazeDetranspilerMessage>,
    parse_messages: &mut [GrazeSourceMessage],
    source_files: &HashMap<u32, Source>,
    formatting_context: &mut UnparseCSTFormattedContext<'a>,
) -> Result<(), i32>
where
    &'a C: UnparseCSTFormatted,
{
    let Ok(mut output_file) = std::fs::OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(&path)
    else {
        messages.push(GrazeDetranspilerMessage::Error(
            GrazeDetranspilerError::CannotWriteFile { path },
        ));
        Cli::print_formatter_errors(parse_messages, messages, source_files, true);
        return Err(1);
    };
    let Ok(()) = cst.unparse_formatted_into_io(&mut output_file, formatting_context) else {
        messages.push(GrazeDetranspilerMessage::Error(
            GrazeDetranspilerError::CannotWriteFile { path },
        ));
        Cli::print_formatter_errors(parse_messages, messages, source_files, true);
        return Err(1);
    };
    Ok(())
}

#[cfg(feature = "detranspiler")]
pub fn unparse_multi_file(
    ast: ast_types::GrazeProgram,
    path: &Path,
    output: Option<&Path>,
    messages: &mut Vec<GrazeDetranspilerMessage>,
    formatting_settings: Option<&UnparseFormattedSettings>,
) -> Result<(), i32> {
    let mut stage_top_level_statements = Vec::new();
    for statement in ast.0 {
        if let ast_types::TopLevelStatement::Sprite { identifier, .. } = &statement {
            let output_path = match output {
                Some(output) => {
                    let mut output = output.join(identifier.value.as_str());
                    output.set_extension("graze");
                    output
                }
                None => {
                    let mut output = path.to_path_buf();
                    output.set_file_name(identifier.value.as_str());
                    output.add_extension("graze");
                    if path.file_name() == output.file_name() {
                        output.set_extension("out");
                        output.add_extension("graze");
                    }
                    output
                }
            };
            unparse_into_file(&statement, output_path, messages, formatting_settings)?;
        } else {
            stage_top_level_statements.push(statement);
        }
    }
    let stage_output_path = match output {
        Some(output) => output.join("stage.graze"),
        None => {
            let mut output = path.to_path_buf();
            output.set_file_name("stage.graze");
            if path.file_name() == output.file_name() {
                output.set_extension("out");
                output.add_extension("graze");
            }
            output
        }
    };
    unparse_into_file(
        &ast_types::GrazeProgram(stage_top_level_statements),
        stage_output_path,
        messages,
        formatting_settings,
    )?;
    Ok(())
}

#[cfg(feature = "formatter")]
pub fn unparse_files_for_formatter(
    parsed: HashMap<PathBuf, (GrazeProgram, Vec<StyleToken<'_>>)>,
    messages: &mut Vec<GrazeDetranspilerMessage>,
    parse_messages: &mut [GrazeSourceMessage],
    source_files: &HashMap<u32, Source>,
    formatting_context: &mut UnparseCSTFormattedContext,
) -> Result<(), i32> {
    for (output, (program, style_tokens)) in parsed {
        let mut context = formatting_context.new_reused(&style_tokens);
        unparse_into_file_for_formatter(
            &program,
            output,
            messages,
            parse_messages,
            source_files,
            &mut context,
        )?;
        context.return_resources_to(formatting_context);
    }
    Ok(())
}

#[cfg(feature = "detranspiler")]
pub fn unparse_single_file(
    ast: ast_types::GrazeProgram,
    path: &Path,
    output: Option<&Path>,
    messages: &mut Vec<GrazeDetranspilerMessage>,
    formatting_settings: Option<&UnparseFormattedSettings>,
) -> Result<(), i32> {
    let output_path = match output {
        Some(output) if output.is_file() || !output.exists() => output.to_path_buf(),
        Some(output) => output.join("main.graze"),
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
    unparse_into_file(&ast, output_path, messages, formatting_settings)?;
    Ok(())
}

pub fn count_build_errors_and_warnings(messages: &[GrazeSourceMessage]) -> (usize, usize) {
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

#[cfg(feature = "detranspiler")]
pub fn count_unbuild_errors_and_warnings(messages: &[GrazeDetranspilerMessage]) -> (usize, usize) {
    let mut errors = 0;
    let mut warnings = 0;
    for message in messages {
        match message {
            GrazeDetranspilerMessage::Error(..) => {
                errors += 1;
            }
            GrazeDetranspilerMessage::Warning(..) => {
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
                output,
                resources,
                extensions,
                use_cached_extensions,
                create_cached_extensions,
                path,
                log_time,
            } => Self::build(
                *shadows,
                *logging,
                output.as_deref(),
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
                multi_file_project,
                no_formatting,
                formatting_width,
                formatting_indentation,
                formatting_hard_lines_in_code_blocks,
                logging,
                log_time,
                output,
                resources,
                path,
            } => Self::unbuild(
                *preserve_monitor_ids,
                *preserve_internal_monitor_value,
                *explicitly_typed_string_parameters,
                *multi_asset_declarations,
                *multi_data_declarations,
                *multi_file_project,
                *no_formatting,
                *formatting_width,
                *formatting_indentation,
                *formatting_hard_lines_in_code_blocks,
                *logging,
                *log_time,
                output.as_deref(),
                resources.as_deref(),
                path.as_path(),
            ),
            Commands::Format {
                width,
                indentation,
                hard_lines_in_code_blocks,
                logging,
                log_time,
                path,
            } => Self::format(
                *width,
                *indentation,
                *hard_lines_in_code_blocks,
                *logging,
                path,
                *log_time,
            ),
        }
    }

    pub fn print_build_errors(
        messages: &mut Vec<GrazeSourceMessage>,
        source_files: &HashMap<u32, Source>,
        force_error: bool,
    ) -> Successful {
        let renderer = Renderer::styled();
        let (error_count, warning_count) = count_build_errors_and_warnings(messages);
        let error = error_count > 0 || force_error;
        if error {
            messages.push(GrazeSourceMessage::Unsuccessful {
                error_count,
                warning_count,
            });
        }
        annotations::annotate_build(
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
        output: Option<&Path>,
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
        let (mut output_path, set_extension) = match output {
            Some(output) if output.is_file() || !output.exists() => (output.to_path_buf(), false),
            Some(output) => (
                output.join(
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

    #[cfg(feature = "detranspiler")]
    pub fn print_unbuild_errors(
        messages: &mut Vec<GrazeDetranspilerMessage>,
        force_error: bool,
    ) -> Successful {
        let renderer = Renderer::styled();
        let (error_count, warning_count) = count_unbuild_errors_and_warnings(messages);
        let error = error_count > 0 || force_error;
        if error {
            messages.push(GrazeDetranspilerMessage::Unsuccessful {
                error_count,
                warning_count,
            });
        }
        annotations::annotate_unbuild(messages.iter(), |ann, _| {
            let rendered = renderer.render(ann);
            anstream::eprintln!("{rendered}");
        });
        if error {
            Successful::No
        } else {
            Successful::Yes
        }
    }

    #[cfg(feature = "detranspiler")]
    #[expect(clippy::too_many_arguments)]
    pub fn unbuild(
        preserve_monitor_ids: bool,
        preserve_internal_monitor_value: bool,
        explicitly_typed_string_parameters: bool,
        multi_asset_declarations: bool,
        multi_data_declarations: MultiDataDeclarationsMode,
        multi_file_project: bool,
        no_formatting: bool,
        formatting_width: usize,
        formatting_indentation: isize,
        formatting_hard_lines_in_code_blocks: bool,
        logging: GrazeMessageSetting,
        log_time: bool,
        output: Option<&Path>,
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
        let Ok(reader) = File::open(&path) else {
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
            formatting_settings: (!no_formatting).then_some(UnparseFormattedSettings {
                indentation: formatting_indentation,
                width: formatting_width,
                hard_lines_in_code_blocks: formatting_hard_lines_in_code_blocks,
            }),
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
        if let Err(code) = if multi_file_project {
            unparse_multi_file(
                ast,
                &path,
                output,
                &mut messages,
                settings.formatting_settings.as_ref(),
            )
        } else {
            unparse_single_file(
                ast,
                &path,
                output,
                &mut messages,
                settings.formatting_settings.as_ref(),
            )
        } {
            return code;
        }
        let unparse_time = unparse_timer.elapsed();
        let unzip_2_timer = Instant::now();
        let resource_path = match resources {
            Some(resource_path) => resource_path,
            None => match output {
                Some(output) if output.is_file() || !output.exists() => {
                    output.parent().unwrap_or(Path::new("/"))
                }
                Some(output) => output,
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

    #[cfg(not(feature = "detranspiler"))]
    #[expect(clippy::too_many_arguments)]
    pub fn unbuild(
        _preserve_monitor_ids: bool,
        _preserve_internal_monitor_value: bool,
        _explicitly_typed_string_parameters: bool,
        _multi_asset_declarations: bool,
        _multi_data_declarations: MultiDataDeclarationsMode,
        _multi_file_project: bool,
        _no_formatting: bool,
        _formatting_width: usize,
        _formatting_indentation: isize,
        _formatting_hard_lines_in_code_blocks: bool,
        _logging: GrazeMessageSetting,
        _log_time: bool,
        _output: Option<&Path>,
        _resources: Option<&Path>,
        _path: &Path,
    ) -> i32 {
        let renderer = annotate_snippets::Renderer::styled();
        let ann = &[annotate_snippets::Group::with_title(
            annotate_snippets::Level::ERROR.secondary_title("The detranspiler is not included in this build. The corresponding feature is `detranspiler`."),
        )];
        let rendered = renderer.render(ann);
        anstream::eprintln!("{rendered}");
        1
    }

    #[cfg(feature = "formatter")]
    pub fn print_formatter_errors(
        parse_messages: &[GrazeSourceMessage],
        unparse_messages: &[GrazeDetranspilerMessage],
        source_files: &HashMap<u32, Source>,
        force_error: bool,
    ) -> Successful {
        let renderer = Renderer::styled();
        let (parse_error_count, parse_warning_count) =
            count_build_errors_and_warnings(parse_messages);
        let (unparse_error_count, unparse_warning_count) =
            count_unbuild_errors_and_warnings(unparse_messages);
        let error = unparse_error_count + parse_error_count > 0 || force_error;
        annotations::annotate_build(
            parse_messages.iter(),
            |id| source_files.get(&id).unwrap().as_descriptor(),
            |ann, _| {
                let rendered = renderer.render(ann);
                anstream::eprintln!("{rendered}");
            },
        );
        annotations::annotate_unbuild(unparse_messages.iter(), |ann, _| {
            let rendered = renderer.render(ann);
            anstream::eprintln!("{rendered}");
        });
        if error {
            annotations::annotate_format(
                std::iter::once(&FormatterUnsuccessful {
                    error_count: parse_error_count + unparse_error_count,
                    warning_count: parse_warning_count + unparse_warning_count,
                }),
                |ann, _| {
                    let rendered = renderer.render(ann);
                    anstream::eprintln!("{rendered}");
                },
            );
            Successful::No
        } else {
            Successful::Yes
        }
    }

    #[cfg(feature = "formatter")]
    pub fn format(
        width: usize,
        indentation: isize,
        hard_lines_in_code_blocks: bool,
        logging: GrazeMessageSetting,
        path: &Path,
        log_time: bool,
    ) -> i32 {
        let total_time = Instant::now();
        let path_is_file = path.is_file();
        let mut context = ParseContext::new(
            GrazeBuildSettings {
                message_setting: logging,
                use_shadows: UseShadows::NotEverywhere,
                resources_path: None,
                extensions_path: None,
                use_cached_extensions: false,
                create_cached_extensions: false,
            },
            Default::default(),
        );
        let mut unparse_messages = Vec::new();
        let source_files = HashMap::new();
        let parse_timer = Instant::now();
        let Ok(path) = path.canonicalize() else {
            emit_message_eager_parse_context(
                &mut context,
                CLIError::PathDoesNotExist.into(),
                GrazeMessageSetting::Errors,
            );
            Self::print_formatter_errors(&context.messages, &unparse_messages, &source_files, true);
            std::process::exit(1);
        };
        let mut string_arena = StringArena::new();
        let (parsed, source_files) = if path.is_dir() {
            match parse_project_directory_for_formatter(&path, &mut string_arena, &mut context) {
                Ok(value) => value,
                Err((_, source_files)) => {
                    Self::print_formatter_errors(
                        &context.messages,
                        &unparse_messages,
                        &source_files,
                        true,
                    );
                    return 1;
                }
            }
        } else if path_is_file {
            match parse_single_file_for_formatter(&path, &mut string_arena, &mut context) {
                Ok(value) => value,
                Err((_, source_files)) => {
                    Self::print_formatter_errors(
                        &context.messages,
                        &unparse_messages,
                        &source_files,
                        true,
                    );
                    return 1;
                }
            }
        } else {
            emit_message_eager_parse_context(
                &mut context,
                CLIError::PathNeitherFileNorDirectory.into(),
                GrazeMessageSetting::Errors,
            );
            Self::print_formatter_errors(&context.messages, &unparse_messages, &source_files, true);
            return 1;
        };
        let parse_time = parse_timer.elapsed();
        if !context.successful {
            Self::print_formatter_errors(&context.messages, &unparse_messages, &source_files, true);
            return 1;
        }
        let formatting_settings = UnparseFormattedSettings {
            indentation,
            width,
            hard_lines_in_code_blocks,
        };
        let unparse_timer = Instant::now();
        let mut formatting_context = UnparseCSTFormattedContext::new(formatting_settings, &[]);
        if let Err(code) = unparse_files_for_formatter(
            parsed,
            &mut unparse_messages,
            &mut context.messages,
            &source_files,
            &mut formatting_context,
        ) {
            return code;
        }
        let unparse_time = unparse_timer.elapsed();
        if Self::print_formatter_errors(&context.messages, &unparse_messages, &source_files, false)
            == Successful::No
        {
            return 1;
        }
        if log_time {
            println!("Parsing took: {:?}", parse_time);
            println!("Unparsing AST took: {:?}", unparse_time);
            println!("Total time: {:?}", total_time.elapsed());
        }
        0
    }

    #[cfg(not(feature = "formatter"))]
    pub fn format(
        width: usize,
        indentation: isize,
        hard_lines_in_code_blocks: bool,
        logging: GrazeMessageSetting,
        path: &Path,
        log_time: bool,
    ) -> i32 {
        let renderer = annotate_snippets::Renderer::styled();
        let ann = &[annotate_snippets::Group::with_title(
            annotate_snippets::Level::ERROR.secondary_title("The formatter is not included in this build. The corresponding feature is `formatter`."),
        )];
        let rendered = renderer.render(ann);
        anstream::eprintln!("{rendered}");
        1
    }
}
