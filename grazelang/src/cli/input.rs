use std::path::PathBuf;

use clap::{Parser, Subcommand};

use crate::settings::{GrazeMessageSetting, UseShadows};

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
