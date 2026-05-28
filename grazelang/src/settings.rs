use serde::{Deserialize, Serialize};

use crate::parser::context::GrazeMessageSetting;

#[derive(Debug, Clone, PartialEq, Default, Serialize, Deserialize)]
pub struct GrazeSettings {
    pub message_setting: GrazeMessageSetting,
    pub use_shadows: UseShadows
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum UseShadows {
    CorrectShadowsEverywhere,
    #[default]
    AnyShadowsEverywhere,
    /// No shadows for e.g. formatted strings
    NotEverywhere,
}
