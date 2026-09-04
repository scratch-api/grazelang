use std::path::PathBuf;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct GrazeBuildSettings {
    pub message_setting: GrazeMessageSetting,
    pub use_shadows: UseShadows,
    /// Path for the resources of the project or None if the resources are located in the current directory
    pub resources_path: Option<PathBuf>,
    /// Path for external extensions for the project or None if the extensions are located in the current directory
    pub extensions_path: Option<PathBuf>,
    pub use_cached_extensions: bool,
    pub create_cached_extensions: bool,
}

#[cfg(feature = "detranspiler")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct GrazeDetranspilerSettings {
    pub message_setting: GrazeMessageSetting,
    pub preserve_monitor_ids: bool,
    pub preserve_internal_monitor_value: bool,
    pub explicitly_typed_string_parameters: bool,
    pub multi_asset_declarations: bool,
    pub multi_data_declarations: MultiDataDeclarationsMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize, Default, clap::ValueEnum)]
pub enum MultiDataDeclarationsMode {
    #[default]
    None,
    HomogeneousDeclarations,
    MixedDeclarations,
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
