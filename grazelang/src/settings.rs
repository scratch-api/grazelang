use std::path::PathBuf;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[derive(Default)]
pub struct GrazeSettings {
    pub message_setting: GrazeMessageSetting,
    pub use_shadows: UseShadows,
    /// Path for the resources of the project or None if the resources are in the current directory
    pub resources_path: Option<PathBuf>,
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize, clap::ValueEnum)]
pub enum GrazeMessageSetting {
    // Maybe there will be settings inbetween later
    #[default]
    // tag: 255, is default
    All,
    // tag: 9
    Infos,
    // tag: 6
    Warnings,
    // tag: 3
    Errors,
    // tag: 2
    ExitOnError,
    // tag: 1
    ExitOnErrorUnlogged,
    // tag: 0
    None,
}

impl GrazeMessageSetting {
    pub fn get_numeric(&self) -> u8 {
        match self {
            GrazeMessageSetting::All => 255,
            GrazeMessageSetting::Infos => 9,
            GrazeMessageSetting::Warnings => 6,
            GrazeMessageSetting::Errors => 3,
            GrazeMessageSetting::ExitOnError => 2,
            GrazeMessageSetting::ExitOnErrorUnlogged => 1,
            GrazeMessageSetting::None => 0,
        }
    }
}

impl PartialOrd for GrazeMessageSetting {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for GrazeMessageSetting {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.get_numeric().cmp(&other.get_numeric())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize, clap::ValueEnum)]
pub enum UseShadows {
    /// Guarantees shadows with correct defaults in all places there would normally be
    CorrectShadowsEverywhere,
    /// Guarantees shadows in all places there would normally be
    #[default]
    AnyShadowsEverywhere,
    /// No shadows for e.g. formatted strings
    NotEverywhere,
}
