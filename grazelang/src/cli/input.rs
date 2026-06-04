use std::{
    collections::HashMap,
    ffi::OsStr,
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};

use clap::{Parser, Subcommand};

use crate::{
    codegen, lexer,
    parser::{
        self,
        context::ParseContext,
        core::PeekableLexer,
        cst::{GrazeProgram, ParseError},
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

pub fn parse_project_directory(
    path: &Path,
    context: &mut ParseContext,
) -> Result<(GrazeProgram, HashMap<u32, PathBuf>), ParseError> {
    let mut program = Vec::new();
    let mut file_id = 0_u32;
    let mut source_files = HashMap::new();
    for i in path.read_dir()? {
        let current_file = i?.path();
        if current_file.extension().and_then(OsStr::to_str) != Some("graze") {
            continue;
        }
        let graze_code = {
            let mut file = File::open(&current_file)?;
            let mut buf = String::new();
            file.read_to_string(&mut buf)?;
            buf
        };
        let lexer = lexer::create_lexer(&graze_code);
        let parsed = parser::parse_graze_program(&mut PeekableLexer::new(lexer, file_id), context)?;
        program.extend(parsed.0);
        source_files.insert(file_id, current_file);
        file_id += 1;
    }
    Ok((GrazeProgram(program), source_files))
}

pub fn parse_single_file(
    path: &Path,
    context: &mut ParseContext,
) -> Result<GrazeProgram, ParseError> {
    let graze_code = {
        let mut file = File::open(path)?;
        let mut buf = String::new();
        file.read_to_string(&mut buf)?;
        buf
    };
    let lexer = lexer::create_lexer(&graze_code);
    parser::parse_graze_program(&mut PeekableLexer::new(lexer, 0), context)
}

// TODO: Check unwraps and possibly replace
// Issue: #52

impl Cli {
    pub fn execute(&self) {
        match &self.command {
            Commands::Build {
                shadows,
                logging,
                target,
                resources,
                path,
            } => Self::build(
                shadows,
                logging,
                target.as_ref().map(PathBuf::as_path),
                resources.as_ref().map(PathBuf::as_path),
                path,
            ),
        }
    }

    pub fn build(
        shadows: &UseShadows,
        logging: &GrazeMessageSetting,
        target: Option<&Path>,
        resources: Option<&Path>,
        path: &Path,
    ) {
        // TODO: Use source files for errors
        // Issue: #54
        let is_file = path.is_file();
        let mut context = ParseContext::new(
            GrazeSettings {
                message_setting: *logging,
                use_shadows: *shadows,
                resources_path: Some(resources.map(Path::to_path_buf).unwrap_or_else(|| {
                    if is_file {
                        path.parent().unwrap().to_path_buf()
                    } else {
                        path.to_path_buf()
                    }
                })),
            },
            Default::default(),
        );
        let path = path.canonicalize().unwrap();
        let (parsed, _source_files) = if path.is_dir() {
            parse_project_directory(&path, &mut context).unwrap()
        } else if is_file {
            (
                parse_single_file(&path, &mut context).unwrap(),
                HashMap::from([(0, path.clone())]),
            )
        } else {
            // TODO: Better error message
            // Issue: #53
            panic!();
        };
        if !context.successful {
            for message in &context.messages {
                dbg!(message);
            }
            dbg!(parsed);
            panic!("Parsing unsuccessful.");
        }
        let mut context = codegen::core::GrazeSb3GeneratorContext::new(context).unwrap();
        let visitor = codegen::core::GrazeSb3Generator;
        visitor.visit_graze_program(&parsed, &mut context).unwrap();
        for message in &context.messages {
            dbg!(message);
        }
        // target.unwrap_or(&path).join(
        //     path.file_name()
        //         .and_then(OsStr::to_str)
        //         .unwrap_or("project"),
        // )
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
        zipper::write_to_zip_path(&output_path, &context).unwrap();
    }
}
