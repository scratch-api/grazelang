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
    codegen, lexer,
    messages::{
        annotations::{self, Source},
        types::{CLIError, GrazeMessage},
    },
    parser::{
        self,
        context::ParseContext,
        core::{PeekableLexer, emit_message},
        cst::{GrazeProgram, IntoResultWithSourceSpan, ParseError},
    },
    settings::{GrazeMessageSetting, GrazeSettings, UseShadows},
    visitor::GrazeVisitor,
    zipper,
};

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Parser)]
#[command(name = "graze")]
#[command(version = VERSION)]
#[command(about = "Allows you to manage graze projects, to transpile them to sb3 files and to detranspile sb3 files to ", long_about = None)]
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
        #[arg(value_enum, short, long)]
        target: Option<PathBuf>,
        // 'r' is reserved for a requirements file
        /// Path for the resources of the project (default: project directory)
        #[arg(value_enum, short = 'R', long)]
        resources: Option<PathBuf>,
        /// Path of the file or project directory
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

pub fn count_errors_and_warnings(messages: &[GrazeMessage]) -> (usize, usize) {
    let mut errors = 0;
    let mut warnings = 0;
    for message in messages {
        match message {
            GrazeMessage::Error(..) => {
                errors += 1;
            }
            GrazeMessage::Warning(..) => {
                warnings += 1;
            }
            _ => (),
        }
    }
    (errors, warnings)
}

// TODO: Check unwraps and possibly replace
// Issue: #52

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Successful {
    Yes,
    No,
}

impl Cli {
    pub fn execute(&self) {
        match &self.command {
            Commands::Build {
                shadows,
                logging,
                target,
                resources,
                path,
                log_time,
            } => Self::build(
                shadows,
                logging,
                target.as_ref().map(PathBuf::as_path),
                resources.as_ref().map(PathBuf::as_path),
                path,
                *log_time,
            ),
        }
    }

    pub fn print_errors(
        messages: &mut Vec<GrazeMessage>,
        source_files: &HashMap<u32, Source>,
        force_error: bool,
    ) -> Successful {
        let renderer = Renderer::styled();
        let (error_count, warning_count) = count_errors_and_warnings(messages);
        let error = error_count > 0 || force_error;
        if error {
            messages.push(GrazeMessage::Unsuccessful {
                error_count,
                warning_count,
            });
        }
        let anns = annotations::annotate(messages.iter(), |id| {
            source_files.get(&id).unwrap().as_descriptor()
        });
        if !anns.is_empty() {
            anstream::println!("{}", renderer.render(&anns));
        }
        if error {
            Successful::No
        } else {
            Successful::Yes
        }
    }

    pub fn build(
        shadows: &UseShadows,
        logging: &GrazeMessageSetting,
        target: Option<&Path>,
        resources: Option<&Path>,
        path: &Path,
        log_time: bool,
    ) {
        let total_time = Instant::now();
        // TODO: Improve logging
        //  - [ ] Indicate which source file a message originated from
        //  - [ ] Improve error messages
        //  - [ ] Decide on a consistent logging format
        //  - [ ] Replace panics
        // Issue: #54
        let is_file = path.is_file();
        let mut context = ParseContext::new(
            GrazeSettings {
                message_setting: *logging,
                use_shadows: *shadows,
                resources_path: Some(resources.map(Path::to_path_buf).unwrap_or_else(|| {
                    if is_file {
                        path.parent().unwrap_or(Path::new("/")).to_path_buf()
                    } else {
                        path.to_path_buf()
                    }
                })),
            },
            Default::default(),
        );
        let parse_timer = Instant::now();
        let Ok(path) = path.canonicalize() else {
            emit_message(
                &mut context,
                CLIError::PathDoesNotExist.into(),
                GrazeMessageSetting::Errors,
            );
            Self::print_errors(&mut context.messages, &HashMap::new(), true);
            std::process::exit(1);
        };
        let (parsed, source_files) = if path.is_dir() {
            parse_project_directory(&path, &mut context).unwrap_or_else(|(_, source_files)| {
                Self::print_errors(&mut context.messages, &source_files, true);
                std::process::exit(1);
            })
        } else if is_file {
            parse_single_file(&path, &mut context).unwrap_or_else(|(_, source_files)| {
                Self::print_errors(&mut context.messages, &source_files, true);
                std::process::exit(1);
            })
        } else {
            panic!();
        };
        let parse_time = parse_timer.elapsed();
        if !context.successful {
            Self::print_errors(&mut context.messages, &source_files, true);
            std::process::exit(1);
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
                    Self::print_errors(&mut messages, &source_files, true);
                    std::process::exit(1);
                }
            }
        };
        let visitor = codegen::core::GrazeSb3Generator;
        if let Err(err) = visitor.visit_graze_program(&parsed, &mut context) {
            if context.settings.message_setting == GrazeMessageSetting::None {
                Self::print_errors(&mut context.messages, &source_files, true);
                std::process::exit(1);
            }
            context.messages.push(err.into());
        }
        let codegen_time = codegen_timer.elapsed();
        if Self::print_errors(&mut context.messages, &source_files, false) == Successful::No {
            std::process::exit(1);
        }
        let (mut output_path, set_extension) = match target {
            Some(target) if target.is_file() => (target.to_path_buf(), false),
            Some(target) => (
                target.join(
                    path.file_name()
                        .and_then(OsStr::to_str)
                        .unwrap_or("project"),
                ),
                true,
            ),
            None if is_file => (path, true),
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
        zipper::write_to_zip_path(&output_path, &context).unwrap();
        let zip_time = zip_timer.elapsed();
        if log_time {
            println!("Parsing took: {:?}", parse_time);
            println!("Codegen took: {:?}", codegen_time);
            println!("Zipping took: {:?}", zip_time);
            println!("Total time: {:?}", total_time.elapsed());
        }
    }
}
