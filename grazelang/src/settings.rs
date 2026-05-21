use serde::{Deserialize, Serialize};

use crate::parser::context::GrazeMessageSetting;

#[derive(Debug, Clone, PartialEq, Default, Serialize, Deserialize)]
pub struct GrazeSettings {
    pub message_setting: GrazeMessageSetting,
}
