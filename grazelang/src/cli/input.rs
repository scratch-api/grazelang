use std::{
    collections::HashMap,
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
        if current_file.extension().and_then(|value| value.to_str()) == Some("graze") {
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

impl Cli {
    pub fn execute(&self) {
        match &self.command {
            Commands::Build {
                shadows,
                logging,
                target,
                resources,
                path,
            } => {
                // TODO: Implement project directories
                // Issue: #51
                let mut context = ParseContext::new(
                    GrazeSettings {
                        message_setting: *logging,
                        use_shadows: *shadows,
                        resources_path: resources.clone(),
                    },
                    Default::default(),
                );
                let parsed = parse_single_file(&path.join("main.graze"), &mut context).unwrap();
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
                // dbg!(&context.asset_files);
                // println!("{}", serde_json::to_string(&context.sb3).unwrap());
                zipper::write_to_zip_path(
                    &target.as_ref().unwrap_or(path).join("main.sb3"),
                    &context,
                )
                .unwrap();
            }
        }
    }
}
