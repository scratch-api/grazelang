use arcstr::{ArcStr as IString, literal};
use grazelang_types::project_json;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BlockKindInfo {
    pub arguments: Vec<Argument>,
    pub block_category: IString,
    pub block_name: IString,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FieldValueInfo {
    pub field_value_name: IString,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Argument {
    pub name: IString,
    pub kind: ArgumentKind,
    /// Whether the argument should be skipped as a parameter in graze code
    pub ignore: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ArgumentKind {
    Field,
    Input,
    StackInput,
    MenuInput {
        menu_opcode: IString,
        menu_field: IString,
    },
}

pub const MOTION_CATEGORY: &IString = &literal!("motion");
pub const LOOKS_CATEGORY: &IString = &literal!("looks");
pub const COSTUME_CATEGORY: &IString = &literal!("costume");
pub const SOUND_CATEGORY: &IString = &literal!("sound");
pub const EVENTS_CATEGORY: &IString = &literal!("events");
pub const CONTROL_CATEGORY: &IString = &literal!("control");
pub const SENSING_CATEGORY: &IString = &literal!("sensing");
pub const OPERATORS_CATEGORY: &IString = &literal!("operators");
pub const MATH_OPS_CATEGORY: &IString = &literal!("math_ops");
pub const DATA_CATEGORY: &IString = &literal!("data");
pub const PEN_CATEGORY: &IString = &literal!("pen");
pub const MUSIC_CATEGORY: &IString = &literal!("music");

pub const MATH_NUMBER_ISTRING: &IString = &literal!("math_number");
pub const MATH_INTEGER_ISTRING: &IString = &literal!("math_integer");
pub const MATH_POSITIVE_NUMBER_ISTRING: &IString = &literal!("math_positive_number");
pub const MATH_POSITIVE_INTEGER_ISTRING: &IString = &literal!("math_whole_number");

pub const NUM_ISTRING: &IString = &literal!("NUM");
pub const NUM1_ISTRING: &IString = &literal!("NUM1");
pub const NUM2_ISTRING: &IString = &literal!("NUM2");
pub const TEXT_ISTRING: &IString = &literal!("TEXT");

pub fn get_block_kind_info(block: &project_json::Sb3NormalBlock) -> Option<BlockKindInfo> {
    Some(match block.opcode.as_str() {
        "motion_movesteps" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("STEPS"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_NUMBER_ISTRING.clone(),
                    menu_field: literal!("STEPS"),
                },
                ignore: false,
            }],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("move_steps"),
        },
        "motion_turnright" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("DEGREES"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_NUMBER_ISTRING.clone(),
                    menu_field: literal!("DEGREES"),
                },
                ignore: false,
            }],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("turn_right"),
        },
        "motion_turnleft" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("DEGREES"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_NUMBER_ISTRING.clone(),
                    menu_field: literal!("DEGREES"),
                },
                ignore: false,
            }],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("turn_left"),
        },
        "motion_goto" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("TO"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: literal!("motion_goto_menu"),
                    menu_field: literal!("TO"),
                },
                ignore: false,
            }],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("go_to"),
        },
        "motion_gotoxy" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("X"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("X"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("Y"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("Y"),
                    },
                    ignore: false,
                },
            ],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("go_to_xy"),
        },
        "motion_glideto" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("SECS"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("SECS"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("TO"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: literal!("motion_glideto_menu"),
                        menu_field: literal!("TO"),
                    },
                    ignore: false,
                },
            ],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("glide_to"),
        },
        "motion_glidesecstoxy" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("SECS"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("SECS"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("X"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("X"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("Y"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("Y"),
                    },
                    ignore: false,
                },
            ],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("glide_to_xy"),
        },
        "motion_pointindirection" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("DIRECTION"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: literal!("math_angle"),
                    menu_field: literal!("DIRECTION"),
                },
                ignore: false,
            }],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("point_in_direction"),
        },
        "motion_pointtowards" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("TOWARDS"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: literal!("motion_pointtowards_menu"),
                    menu_field: literal!("TOWARDS"),
                },
                ignore: false,
            }],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("point_towards"),
        },
        "motion_changexby" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("DX"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_NUMBER_ISTRING.clone(),
                    menu_field: literal!("DX"),
                },
                ignore: false,
            }],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("change_x_by"),
        },
        "motion_setx" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("X"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_NUMBER_ISTRING.clone(),
                    menu_field: literal!("X"),
                },
                ignore: false,
            }],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("set_x"),
        },
        "motion_changeyby" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("DY"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_NUMBER_ISTRING.clone(),
                    menu_field: literal!("DY"),
                },
                ignore: false,
            }],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("change_y_by"),
        },
        "motion_sety" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("Y"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_NUMBER_ISTRING.clone(),
                    menu_field: literal!("Y"),
                },
                ignore: false,
            }],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("set_y"),
        },
        "motion_ifonedgebounce" => BlockKindInfo {
            arguments: vec![],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("if_on_edge_bounce"),
        },
        "motion_setrotationstyle" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("STYLE"),
                kind: ArgumentKind::Field,
                ignore: false,
            }],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("set_rotation_style"),
        },
        "motion_xposition" => BlockKindInfo {
            arguments: vec![],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("x_position"),
        },
        "motion_yposition" => BlockKindInfo {
            arguments: vec![],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("y_position"),
        },
        "motion_direction" => BlockKindInfo {
            arguments: vec![],
            block_category: MOTION_CATEGORY.clone(),
            block_name: literal!("direction"),
        },

        "looks_sayforsecs" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("MESSAGE"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("MESSAGE"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("SECS"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("SECS"),
                    },
                    ignore: false,
                },
            ],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("say_for"),
        },
        "looks_say" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("MESSAGE"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: TEXT_ISTRING.clone(),
                    menu_field: literal!("MESSAGE"),
                },
                ignore: false,
            }],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("say"),
        },
        "looks_thinkforsecs" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("MESSAGE"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("MESSAGE"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("SECS"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("SECS"),
                    },
                    ignore: false,
                },
            ],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("think_for"),
        },
        "looks_think" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("MESSAGE"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: TEXT_ISTRING.clone(),
                    menu_field: literal!("MESSAGE"),
                },
                ignore: false,
            }],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("think"),
        },
        "looks_switchcostumeto" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("COSTUME"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: literal!("looks_costume"),
                    menu_field: literal!("COSTUME"),
                },
                ignore: false,
            }],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("switch_to_costume"),
        },
        "looks_nextcostume" => BlockKindInfo {
            arguments: vec![],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("next_costume"),
        },
        "looks_switchbackdropto" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("BACKDROP"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: literal!("looks_backdrops"),
                    menu_field: literal!("BACKDROP"),
                },
                ignore: false,
            }],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("switch_to_backdrop"),
        },
        "looks_switchbackdroptoandwait" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("BACKDROP"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: literal!("looks_backdrops"),
                    menu_field: literal!("BACKDROP"),
                },
                ignore: false,
            }],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("switch_to_backdrop_and_wait"),
        },
        "looks_nextbackdrop" => BlockKindInfo {
            arguments: vec![],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("next_backdrop"),
        },
        "looks_changesizeby" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("CHANGE"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_NUMBER_ISTRING.clone(),
                    menu_field: literal!("CHANGE"),
                },
                ignore: false,
            }],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("change_size_by"),
        },
        "looks_setsizeto" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("SIZE"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_NUMBER_ISTRING.clone(),
                    menu_field: literal!("SIZE"),
                },
                ignore: false,
            }],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("set_size_to"),
        },
        "looks_changeeffectby" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("EFFECT"),
                    kind: ArgumentKind::Field,
                    ignore: false,
                },
                Argument {
                    name: literal!("CHANGE"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("CHANGE"),
                    },
                    ignore: false,
                },
            ],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("change_graphic_effect_by"),
        },
        "looks_seteffectto" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("EFFECT"),
                    kind: ArgumentKind::Field,
                    ignore: false,
                },
                Argument {
                    name: literal!("VALUE"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("VALUE"),
                    },
                    ignore: false,
                },
            ],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("set_graphic_effect_to"),
        },
        "looks_cleargraphiceffects" => BlockKindInfo {
            arguments: vec![],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("clear_graphic_effects"),
        },
        "looks_show" => BlockKindInfo {
            arguments: vec![],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("show"),
        },
        "looks_hide" => BlockKindInfo {
            arguments: vec![],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("hide"),
        },
        "looks_gotofrontback" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("FRONT_BACK"),
                kind: ArgumentKind::Field,
                ignore: false,
            }],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("go_to_layer"),
        },
        "looks_goforwardbackwardlayers" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("FORWARD_BACKWARD"),
                    kind: ArgumentKind::Field,
                    ignore: false,
                },
                Argument {
                    name: NUM_ISTRING.clone(),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_INTEGER_ISTRING.clone(),
                        menu_field: NUM_ISTRING.clone(),
                    },
                    ignore: false,
                },
            ],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("change_layer"),
        },
        "looks_costumenumbername" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("NUMBER_NAME"),
                kind: ArgumentKind::Field,
                ignore: true,
            }],
            block_category: COSTUME_CATEGORY.clone(),
            block_name: {
                let number_name = block.fields.get("NUMBER_NAME")?;
                if let project_json::Sb3FieldValue::Normal(project_json::Sb3Primitive::String(
                    value,
                )) = number_name
                {
                    match value.as_str() {
                        "number" => literal!("costume_number"),
                        "name" => literal!("costume_name"),
                        _ => return None,
                    }
                } else {
                    return None;
                }
            },
        },
        "looks_backdropnumbername" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("NUMBER_NAME"),
                kind: ArgumentKind::Field,
                ignore: true,
            }],
            block_category: COSTUME_CATEGORY.clone(),
            block_name: {
                let number_name = block.fields.get("NUMBER_NAME")?;
                if let project_json::Sb3FieldValue::Normal(project_json::Sb3Primitive::String(
                    value,
                )) = number_name
                {
                    match value.as_str() {
                        "number" => literal!("backdrop_number"),
                        "name" => literal!("backdrop_name"),
                        _ => return None,
                    }
                } else {
                    return None;
                }
            },
        },
        "looks_size" => BlockKindInfo {
            arguments: vec![],
            block_category: LOOKS_CATEGORY.clone(),
            block_name: literal!("size"),
        },

        "sound_playuntildone" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("SOUND_MENU"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: literal!("sound_sounds_menu"),
                    menu_field: literal!("SOUND_MENU"),
                },
                ignore: false,
            }],
            block_category: SOUND_CATEGORY.clone(),
            block_name: literal!("play_sound_until_done"),
        },
        "sound_play" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("SOUND_MENU"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: literal!("sound_sounds_menu"),
                    menu_field: literal!("SOUND_MENU"),
                },
                ignore: false,
            }],
            block_category: SOUND_CATEGORY.clone(),
            block_name: literal!("start_sound"),
        },
        "sound_stopallsounds" => BlockKindInfo {
            arguments: vec![],
            block_category: SOUND_CATEGORY.clone(),
            block_name: literal!("stop_all_sounds"),
        },
        "sound_changeeffectby" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("EFFECT"),
                    kind: ArgumentKind::Field,
                    ignore: false,
                },
                Argument {
                    name: literal!("VALUE"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("VALUE"),
                    },
                    ignore: false,
                },
            ],
            block_category: SOUND_CATEGORY.clone(),
            block_name: literal!("change_sound_effect_by"),
        },
        "sound_seteffectto" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("EFFECT"),
                    kind: ArgumentKind::Field,
                    ignore: false,
                },
                Argument {
                    name: literal!("VALUE"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("VALUE"),
                    },
                    ignore: false,
                },
            ],
            block_category: SOUND_CATEGORY.clone(),
            block_name: literal!("set_sound_effect_to"),
        },
        "sound_cleareffects" => BlockKindInfo {
            arguments: vec![],
            block_category: SOUND_CATEGORY.clone(),
            block_name: literal!("clear_sound_effects"),
        },
        "sound_changevolumeby" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("VOLUME"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_NUMBER_ISTRING.clone(),
                    menu_field: literal!("VOLUME"),
                },
                ignore: false,
            }],
            block_category: SOUND_CATEGORY.clone(),
            block_name: literal!("change_volume_by"),
        },
        "sound_setvolumeto" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("VOLUME"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_NUMBER_ISTRING.clone(),
                    menu_field: literal!("VOLUME"),
                },
                ignore: false,
            }],
            block_category: SOUND_CATEGORY.clone(),
            block_name: literal!("set_volume_to"),
        },
        "sound_volume" => BlockKindInfo {
            arguments: vec![],
            block_category: SOUND_CATEGORY.clone(),
            block_name: literal!("volume"),
        },

        "event_whenflagclicked" => BlockKindInfo {
            arguments: vec![],
            block_category: EVENTS_CATEGORY.clone(),
            block_name: literal!("when_green_flag_clicked"),
        },
        "event_whenkeypressed" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("KEY_OPTION"),
                kind: ArgumentKind::Field,
                ignore: false,
            }],
            block_category: EVENTS_CATEGORY.clone(),
            block_name: literal!("when_key_pressed"),
        },
        "event_whenthisspriteclicked" => BlockKindInfo {
            arguments: vec![],
            block_category: EVENTS_CATEGORY.clone(),
            block_name: literal!("when_this_sprite_clicked"),
        },
        "event_whenbackdropswitchesto" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("BACKDROP"),
                kind: ArgumentKind::Field,
                ignore: false,
            }],
            block_category: EVENTS_CATEGORY.clone(),
            block_name: literal!("when_backdrop_switches_to"),
        },
        "event_whengreaterthan" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("WHENGREATERTHANMENU"),
                    kind: ArgumentKind::Field,
                    ignore: false,
                },
                Argument {
                    name: literal!("VALUE"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("VALUE"),
                    },
                    ignore: false,
                },
            ],
            block_category: EVENTS_CATEGORY.clone(),
            block_name: literal!("when_greater_than"),
        },
        "event_whenbroadcastreceived" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("BROADCAST_OPTION"),
                kind: ArgumentKind::Field,
                ignore: false,
            }],
            block_category: EVENTS_CATEGORY.clone(),
            block_name: literal!("when_broadcast_received"),
        },
        "event_broadcast" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("BROADCAST_INPUT"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: literal!("event_broadcast_menu"),
                    menu_field: literal!("BROADCAST_OPTION"),
                },
                ignore: false,
            }],
            block_category: EVENTS_CATEGORY.clone(),
            block_name: literal!("broadcast"),
        },
        "event_broadcastandwait" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("BROADCAST_INPUT"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: literal!("event_broadcast_menu"),
                    menu_field: literal!("BROADCAST_INPUT"),
                },
                ignore: false,
            }],
            block_category: EVENTS_CATEGORY.clone(),
            block_name: literal!("broadcast_and_wait"),
        },

        "control_wait" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("DURATION"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_POSITIVE_NUMBER_ISTRING.clone(),
                    menu_field: literal!("DURATION"),
                },
                ignore: false,
            }],
            block_category: CONTROL_CATEGORY.clone(),
            block_name: literal!("wait"),
        },
        "control_repeat" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("TIMES"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_POSITIVE_INTEGER_ISTRING.clone(),
                        menu_field: literal!("TIMES"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("SUBSTACK"),
                    kind: ArgumentKind::StackInput,
                    ignore: false,
                },
            ],
            block_category: CONTROL_CATEGORY.clone(),
            block_name: literal!("repeat"),
        },
        "control_forever" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("SUBSTACK"),
                kind: ArgumentKind::StackInput,
                ignore: false,
            }],
            block_category: CONTROL_CATEGORY.clone(),
            block_name: literal!("forever_loop"),
        },
        "control_if" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("CONDITION"),
                    kind: ArgumentKind::Input,
                    ignore: false,
                },
                Argument {
                    name: literal!("SUBSTACK"),
                    kind: ArgumentKind::StackInput,
                    ignore: false,
                },
            ],
            block_category: CONTROL_CATEGORY.clone(),
            block_name: literal!("if"),
        },
        "control_if_else" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("CONDITION"),
                    kind: ArgumentKind::Input,
                    ignore: false,
                },
                Argument {
                    name: literal!("SUBSTACK"),
                    kind: ArgumentKind::StackInput,
                    ignore: false,
                },
            ],
            block_category: CONTROL_CATEGORY.clone(),
            block_name: literal!("if_else"),
        },
        "control_wait_until" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("CONDITION"),
                kind: ArgumentKind::Input,
                ignore: false,
            }],
            block_category: CONTROL_CATEGORY.clone(),
            block_name: literal!("wait_until"),
        },
        "control_repeat_until" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("CONDITION"),
                    kind: ArgumentKind::Input,
                    ignore: false,
                },
                Argument {
                    name: literal!("SUBSTACK"),
                    kind: ArgumentKind::StackInput,
                    ignore: false,
                },
            ],
            block_category: CONTROL_CATEGORY.clone(),
            block_name: literal!("repeat_until"),
        },
        "control_while" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("CONDITION"),
                    kind: ArgumentKind::Input,
                    ignore: false,
                },
                Argument {
                    name: literal!("SUBSTACK"),
                    kind: ArgumentKind::StackInput,
                    ignore: false,
                },
            ],
            block_category: CONTROL_CATEGORY.clone(),
            block_name: literal!("while_loop"),
        },
        "control_stop" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("STOP_OPTION"),
                kind: ArgumentKind::Field,
                ignore: false,
            }],
            block_category: CONTROL_CATEGORY.clone(),
            block_name: literal!("stop"),
        },
        "control_start_as_clone" => BlockKindInfo {
            arguments: vec![],
            block_category: CONTROL_CATEGORY.clone(),
            block_name: literal!("when_start_as_clone"),
        },
        "control_create_clone_of" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("CLONE_OPTION"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: literal!("control_create_clone_of_menu"),
                    menu_field: literal!("CLONE_OPTION"),
                },
                ignore: false,
            }],
            block_category: CONTROL_CATEGORY.clone(),
            block_name: literal!("create_clone_of"),
        },
        "control_delete_this_clone" => BlockKindInfo {
            arguments: vec![],
            block_category: CONTROL_CATEGORY.clone(),
            block_name: literal!("delete_this_clone"),
        },

        "sensing_touchingobject" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("TOUCHINGOBJECTMENU"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: literal!("sensing_touchingobjectmenu"),
                    menu_field: literal!("TOUCHINGOBJECTMENU"),
                },
                ignore: false,
            }],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("is_touching_object"),
        },
        "sensing_touchingcolor" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("COLOR"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: literal!("colour_picker"),
                    menu_field: literal!("COLOR"),
                },
                ignore: false,
            }],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("is_touching_color"),
        },
        "sensing_coloristouchingcolor" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("COLOR"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: literal!("colour_picker"),
                        menu_field: literal!("COLOR"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("COLOR2"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: literal!("colour_picker"),
                        menu_field: literal!("COLOR2"),
                    },
                    ignore: false,
                },
            ],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("color_is_touching_color"),
        },
        "sensing_distanceto" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("DISTANCETOMENU"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: literal!("sensing_distancetomenu"),
                    menu_field: literal!("DISTANCETOMENU"),
                },
                ignore: false,
            }],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("distance_to"),
        },
        "sensing_askandwait" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("QUESTION"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: TEXT_ISTRING.clone(),
                    menu_field: literal!("QUESTION"),
                },
                ignore: false,
            }],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("ask_and_wait"),
        },
        "sensing_answer" => BlockKindInfo {
            arguments: vec![],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("answer"),
        },
        "sensing_keypressed" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("KEY_OPTION"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: literal!("sensing_keyoptions"),
                    menu_field: literal!("KEY_OPTION"),
                },
                ignore: false,
            }],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("is_key_pressed"),
        },
        "sensing_mousedown" => BlockKindInfo {
            arguments: vec![],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("is_mouse_down"),
        },
        "sensing_mousex" => BlockKindInfo {
            arguments: vec![],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("mouse_x"),
        },
        "sensing_mousey" => BlockKindInfo {
            arguments: vec![],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("mouse_y"),
        },
        "sensing_setdragmode" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("DRAG_MODE"),
                kind: ArgumentKind::Field,
                ignore: false,
            }],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("set_drag_mode"),
        },
        "sensing_loudness" => BlockKindInfo {
            arguments: vec![],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("loudness"),
        },
        "sensing_timer" => BlockKindInfo {
            arguments: vec![],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("timer"),
        },
        "sensing_resettimer" => BlockKindInfo {
            arguments: vec![],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("reset_timer"),
        },
        "sensing_of" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("PROPERTY"),
                    kind: ArgumentKind::Field,
                    ignore: false,
                },
                Argument {
                    name: literal!("OBJECT"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: literal!("sensing_of_object_menu"),
                        menu_field: literal!("OBJECT"),
                    },
                    ignore: false,
                },
            ],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("property_of_object"),
        },
        "sensing_current" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("CURRENTMENU"),
                kind: ArgumentKind::Field,
                ignore: false,
            }],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("current_time_unit"),
        },
        "sensing_dayssince2000" => BlockKindInfo {
            arguments: vec![],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("days_since_2000"),
        },
        "sensing_online" => BlockKindInfo {
            arguments: vec![],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("is_online"),
        },
        "sensing_username" => BlockKindInfo {
            arguments: vec![],
            block_category: SENSING_CATEGORY.clone(),
            block_name: literal!("username"),
        },

        "operator_add" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: NUM1_ISTRING.clone(),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: NUM1_ISTRING.clone(),
                    },
                    ignore: false,
                },
                Argument {
                    name: NUM2_ISTRING.clone(),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: NUM2_ISTRING.clone(),
                    },
                    ignore: false,
                },
            ],
            block_category: OPERATORS_CATEGORY.clone(),
            block_name: literal!("add_values"),
        },
        "operator_subtract" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: NUM1_ISTRING.clone(),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: NUM1_ISTRING.clone(),
                    },
                    ignore: false,
                },
                Argument {
                    name: NUM2_ISTRING.clone(),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: NUM2_ISTRING.clone(),
                    },
                    ignore: false,
                },
            ],
            block_category: OPERATORS_CATEGORY.clone(),
            block_name: literal!("subtract_values"),
        },
        "operator_multiply" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: NUM1_ISTRING.clone(),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: NUM1_ISTRING.clone(),
                    },
                    ignore: false,
                },
                Argument {
                    name: NUM2_ISTRING.clone(),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: NUM2_ISTRING.clone(),
                    },
                    ignore: false,
                },
            ],
            block_category: OPERATORS_CATEGORY.clone(),
            block_name: literal!("multiply_values"),
        },
        "operator_divide" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: NUM1_ISTRING.clone(),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: NUM1_ISTRING.clone(),
                    },
                    ignore: false,
                },
                Argument {
                    name: NUM2_ISTRING.clone(),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: NUM2_ISTRING.clone(),
                    },
                    ignore: false,
                },
            ],
            block_category: OPERATORS_CATEGORY.clone(),
            block_name: literal!("divide_values"),
        },
        "operator_random" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("FROM"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("FROM"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("TO"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("TO"),
                    },
                    ignore: false,
                },
            ],
            block_category: OPERATORS_CATEGORY.clone(),
            block_name: literal!("pick_random_number"),
        },
        "operator_gt" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("OPERAND1"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("OPERAND1"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("OPERAND2"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("OPERAND2"),
                    },
                    ignore: false,
                },
            ],
            block_category: OPERATORS_CATEGORY.clone(),
            block_name: literal!("is_greater_than"),
        },
        "operator_lt" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("OPERAND1"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("OPERAND1"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("OPERAND2"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("OPERAND2"),
                    },
                    ignore: false,
                },
            ],
            block_category: OPERATORS_CATEGORY.clone(),
            block_name: literal!("is_less_than"),
        },
        "operator_equals" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("OPERAND1"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("OPERAND1"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("OPERAND2"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("OPERAND2"),
                    },
                    ignore: false,
                },
            ],
            block_category: OPERATORS_CATEGORY.clone(),
            block_name: literal!("is_equal_to"),
        },
        "operator_and" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("OPERAND1"),
                    kind: ArgumentKind::Input,
                    ignore: false,
                },
                Argument {
                    name: literal!("OPERAND2"),
                    kind: ArgumentKind::Input,
                    ignore: false,
                },
            ],
            block_category: OPERATORS_CATEGORY.clone(),
            block_name: literal!("logical_and"),
        },
        "operator_or" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("OPERAND1"),
                    kind: ArgumentKind::Input,
                    ignore: false,
                },
                Argument {
                    name: literal!("OPERAND2"),
                    kind: ArgumentKind::Input,
                    ignore: false,
                },
            ],
            block_category: OPERATORS_CATEGORY.clone(),
            block_name: literal!("logical_or"),
        },
        "operator_not" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("OPERAND"),
                kind: ArgumentKind::Input,
                ignore: false,
            }],
            block_category: OPERATORS_CATEGORY.clone(),
            block_name: literal!("logical_not"),
        },
        "operator_join" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("STRING1"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("STRING1"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("STRING2"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("STRING2"),
                    },
                    ignore: false,
                },
            ],
            block_category: OPERATORS_CATEGORY.clone(),
            block_name: literal!("join_strings"),
        },
        "operator_letter_of" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("LETTER"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_POSITIVE_INTEGER_ISTRING.clone(),
                        menu_field: literal!("LETTER"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("STRING"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("STRING"),
                    },
                    ignore: false,
                },
            ],
            block_category: OPERATORS_CATEGORY.clone(),
            block_name: literal!("letter_of_string"),
        },
        "operator_length" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("STRING"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: TEXT_ISTRING.clone(),
                    menu_field: literal!("STRING"),
                },
                ignore: false,
            }],
            block_category: OPERATORS_CATEGORY.clone(),
            block_name: literal!("string_length"),
        },
        "operator_contains" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("STRING1"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("STRING1"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("STRING2"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("STRING2"),
                    },
                    ignore: false,
                },
            ],
            block_category: OPERATORS_CATEGORY.clone(),
            block_name: literal!("string_contains"),
        },
        "operator_mod" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: NUM1_ISTRING.clone(),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: NUM1_ISTRING.clone(),
                    },
                    ignore: false,
                },
                Argument {
                    name: NUM2_ISTRING.clone(),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: NUM2_ISTRING.clone(),
                    },
                    ignore: false,
                },
            ],
            block_category: OPERATORS_CATEGORY.clone(),
            block_name: literal!("modulo"),
        },
        "operator_round" => BlockKindInfo {
            arguments: vec![Argument {
                name: NUM_ISTRING.clone(),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_NUMBER_ISTRING.clone(),
                    menu_field: NUM_ISTRING.clone(),
                },
                ignore: false,
            }],
            block_category: OPERATORS_CATEGORY.clone(),
            block_name: literal!("round"),
        },
        "operator_mathop" => {
            let math_op_name = match block.fields.get("OPERATOR") {
                Some(project_json::Sb3FieldValue::Normal(project_json::Sb3Primitive::String(
                    value,
                ))) => match value.as_str() {
                    "abs" => literal!("abs"),
                    "floor" => literal!("floor"),
                    "ceiling" => literal!("ceil"),
                    "sqrt" => literal!("sqrt"),
                    "sin" => literal!("sin"),
                    "cos" => literal!("cos"),
                    "tan" => literal!("tan"),
                    "asin" => literal!("asin"),
                    "acos" => literal!("acos"),
                    "atan" => literal!("atan"),
                    "ln" => literal!("ln"),
                    "log" => literal!("log"),
                    "e ^" => literal!("exp"),
                    "10 ^" => literal!("pow"),
                    _ => return None,
                },
                _ => return None,
            };
            BlockKindInfo {
                arguments: vec![
                    Argument {
                        name: literal!("OPERATOR"),
                        kind: ArgumentKind::Field,
                        ignore: true,
                    },
                    Argument {
                        name: NUM_ISTRING.clone(),
                        kind: ArgumentKind::MenuInput {
                            menu_opcode: MATH_NUMBER_ISTRING.clone(),
                            menu_field: NUM_ISTRING.clone(),
                        },
                        ignore: false,
                    },
                ],
                block_category: MATH_OPS_CATEGORY.clone(),
                block_name: math_op_name,
            }
        }

        "data_variable" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("VARIABLE"),
                kind: ArgumentKind::Field,
                ignore: false,
            }],
            block_category: DATA_CATEGORY.clone(),
            block_name: literal!("get_variable_value"),
        },
        "data_setvariableto" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("VARIABLE"),
                    kind: ArgumentKind::Field,
                    ignore: false,
                },
                Argument {
                    name: literal!("VALUE"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("VALUE"),
                    },
                    ignore: false,
                },
            ],
            block_category: DATA_CATEGORY.clone(),
            block_name: literal!("set_variable_to"),
        },
        "data_changevariableby" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("VARIABLE"),
                    kind: ArgumentKind::Field,
                    ignore: false,
                },
                Argument {
                    name: literal!("VALUE"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("VALUE"),
                    },
                    ignore: false,
                },
            ],
            block_category: DATA_CATEGORY.clone(),
            block_name: literal!("change_variable_by"),
        },
        "data_showvariable" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("VARIABLE"),
                kind: ArgumentKind::Field,
                ignore: false,
            }],
            block_category: DATA_CATEGORY.clone(),
            block_name: literal!("show_variable"),
        },
        "data_hidevariable" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("VARIABLE"),
                kind: ArgumentKind::Field,
                ignore: false,
            }],
            block_category: DATA_CATEGORY.clone(),
            block_name: literal!("hide_variable"),
        },
        "data_listcontents" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("LIST"),
                kind: ArgumentKind::Field,
                ignore: false,
            }],
            block_category: DATA_CATEGORY.clone(),
            block_name: literal!("get_list_contents"),
        },
        "data_addtolist" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("LIST"),
                    kind: ArgumentKind::Field,
                    ignore: false,
                },
                Argument {
                    name: literal!("ITEM"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("ITEM"),
                    },
                    ignore: false,
                },
            ],
            block_category: DATA_CATEGORY.clone(),
            block_name: literal!("add_to_list"),
        },
        "data_deleteoflist" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("LIST"),
                    kind: ArgumentKind::Field,
                    ignore: false,
                },
                Argument {
                    name: literal!("INDEX"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_INTEGER_ISTRING.clone(),
                        menu_field: literal!("INDEX"),
                    },
                    ignore: false,
                },
            ],
            block_category: DATA_CATEGORY.clone(),
            block_name: literal!("delete_item_of_list"),
        },
        "data_deletealloflist" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("LIST"),
                kind: ArgumentKind::Field,
                ignore: false,
            }],
            block_category: DATA_CATEGORY.clone(),
            block_name: literal!("delete_all_of_list"),
        },
        "data_insertatlist" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("LIST"),
                    kind: ArgumentKind::Field,
                    ignore: false,
                },
                Argument {
                    name: literal!("INDEX"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_INTEGER_ISTRING.clone(),
                        menu_field: literal!("INDEX"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("ITEM"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("ITEM"),
                    },
                    ignore: false,
                },
            ],
            block_category: DATA_CATEGORY.clone(),
            block_name: literal!("insert_at_list"),
        },
        "data_replaceitemoflist" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("LIST"),
                    kind: ArgumentKind::Field,
                    ignore: false,
                },
                Argument {
                    name: literal!("INDEX"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_INTEGER_ISTRING.clone(),
                        menu_field: literal!("INDEX"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("ITEM"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("ITEM"),
                    },
                    ignore: false,
                },
            ],
            block_category: DATA_CATEGORY.clone(),
            block_name: literal!("replace_item_of_list"),
        },
        "data_itemoflist" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("LIST"),
                    kind: ArgumentKind::Field,
                    ignore: false,
                },
                Argument {
                    name: literal!("INDEX"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_INTEGER_ISTRING.clone(),
                        menu_field: literal!("INDEX"),
                    },
                    ignore: false,
                },
            ],
            block_category: DATA_CATEGORY.clone(),
            block_name: literal!("get_item_of_list"),
        },
        "data_itemnumoflist" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("LIST"),
                    kind: ArgumentKind::Field,
                    ignore: false,
                },
                Argument {
                    name: literal!("ITEM"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("ITEM"),
                    },
                    ignore: false,
                },
            ],
            block_category: DATA_CATEGORY.clone(),
            block_name: literal!("get_index_of_item_in_list"),
        },
        "data_lengthoflist" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("LIST"),
                kind: ArgumentKind::Field,
                ignore: false,
            }],
            block_category: DATA_CATEGORY.clone(),
            block_name: literal!("get_list_length"),
        },
        "data_listcontainsitem" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("LIST"),
                    kind: ArgumentKind::Field,
                    ignore: false,
                },
                Argument {
                    name: literal!("ITEM"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: TEXT_ISTRING.clone(),
                        menu_field: literal!("ITEM"),
                    },
                    ignore: false,
                },
            ],
            block_category: DATA_CATEGORY.clone(),
            block_name: literal!("list_contains_item"),
        },
        "data_showlist" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("LIST"),
                kind: ArgumentKind::Field,
                ignore: false,
            }],
            block_category: DATA_CATEGORY.clone(),
            block_name: literal!("show_list"),
        },
        "data_hidelist" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("LIST"),
                kind: ArgumentKind::Field,
                ignore: false,
            }],
            block_category: DATA_CATEGORY.clone(),
            block_name: literal!("hide_list"),
        },

        "pen_clear" => BlockKindInfo {
            arguments: vec![],
            block_category: PEN_CATEGORY.clone(),
            block_name: literal!("clear"),
        },
        "pen_stamp" => BlockKindInfo {
            arguments: vec![],
            block_category: PEN_CATEGORY.clone(),
            block_name: literal!("stamp"),
        },
        "pen_penDown" => BlockKindInfo {
            arguments: vec![],
            block_category: PEN_CATEGORY.clone(),
            block_name: literal!("pen_down"),
        },
        "pen_penUp" => BlockKindInfo {
            arguments: vec![],
            block_category: PEN_CATEGORY.clone(),
            block_name: literal!("pen_up"),
        },
        "pen_setPenColorToColor" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("COLOR"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: literal!("colour_picker"),
                    menu_field: literal!("COLOR"),
                },
                ignore: false,
            }],
            block_category: PEN_CATEGORY.clone(),
            block_name: literal!("set_pen_color_to"),
        },
        "pen_changePenColorParamBy" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("COLOR_PARAM"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: literal!("pen_menu_colorParam"),
                        menu_field: literal!("colorParam"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("VALUE"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("VALUE"),
                    },
                    ignore: false,
                },
            ],
            block_category: PEN_CATEGORY.clone(),
            block_name: literal!("change_pen_param_by"),
        },
        "pen_setPenColorParamTo" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("COLOR_PARAM"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: literal!("pen_menu_colorParam"),
                        menu_field: literal!("colorParam"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("VALUE"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("VALUE"),
                    },
                    ignore: false,
                },
            ],
            block_category: PEN_CATEGORY.clone(),
            block_name: literal!("set_pen_param_to"),
        },
        "pen_changePenSizeBy" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("SIZE"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_NUMBER_ISTRING.clone(),
                    menu_field: literal!("SIZE"),
                },
                ignore: false,
            }],
            block_category: PEN_CATEGORY.clone(),
            block_name: literal!("change_pen_size_by"),
        },
        "pen_setPenSizeTo" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("SIZE"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_NUMBER_ISTRING.clone(),
                    menu_field: literal!("SIZE"),
                },
                ignore: false,
            }],
            block_category: PEN_CATEGORY.clone(),
            block_name: literal!("set_pen_size_to"),
        },

        "music_playDrumForBeats" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("DRUM"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: literal!("music_menu_DRUM"),
                        menu_field: literal!("DRUM"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("BEATS"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("BEATS"),
                    },
                    ignore: false,
                },
            ],
            block_category: MUSIC_CATEGORY.clone(),
            block_name: literal!("play_drum_for_beats"),
        },
        "music_restForBeats" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("BEATS"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_NUMBER_ISTRING.clone(),
                    menu_field: literal!("BEATS"),
                },
                ignore: false,
            }],
            block_category: MUSIC_CATEGORY.clone(),
            block_name: literal!("rest_for_beats"),
        },
        "music_playNoteForBeats" => BlockKindInfo {
            arguments: vec![
                Argument {
                    name: literal!("NOTE"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: literal!("note"),
                        menu_field: literal!("NOTE"),
                    },
                    ignore: false,
                },
                Argument {
                    name: literal!("BEATS"),
                    kind: ArgumentKind::MenuInput {
                        menu_opcode: MATH_NUMBER_ISTRING.clone(),
                        menu_field: literal!("BEATS"),
                    },
                    ignore: false,
                },
            ],
            block_category: MUSIC_CATEGORY.clone(),
            block_name: literal!("play_note_for_beats"),
        },
        "music_setInstrument" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("INSTRUMENT"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: literal!("music_menu_INSTRUMENT"),
                    menu_field: literal!("INSTRUMENT"),
                },
                ignore: false,
            }],
            block_category: MUSIC_CATEGORY.clone(),
            block_name: literal!("set_instrument"),
        },
        "music_setTempo" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("TEMPO"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_NUMBER_ISTRING.clone(),
                    menu_field: literal!("TEMPO"),
                },
                ignore: false,
            }],
            block_category: MUSIC_CATEGORY.clone(),
            block_name: literal!("set_tempo"),
        },
        "music_changeTempo" => BlockKindInfo {
            arguments: vec![Argument {
                name: literal!("TEMPO"),
                kind: ArgumentKind::MenuInput {
                    menu_opcode: MATH_NUMBER_ISTRING.clone(),
                    menu_field: literal!("TEMPO"),
                },
                ignore: false,
            }],
            block_category: MUSIC_CATEGORY.clone(),
            block_name: literal!("change_tempo"),
        },
        "music_getTempo" => BlockKindInfo {
            arguments: vec![],
            block_category: MUSIC_CATEGORY.clone(),
            block_name: literal!("tempo"),
        },
        _ => return None,
    })
}

pub fn get_field_value_info(
    field_value: &project_json::Sb3FieldValue,
    block_opcode: &str,
) -> Option<FieldValueInfo> {
    if let project_json::Sb3FieldValue::Normal(project_json::Sb3Primitive::String(value)) =
        field_value
    {
        match block_opcode {
            "music_menu_DRUM" => {
                return Some(match value.as_str() {
                    "1" => FieldValueInfo {
                        field_value_name: literal!("snare_drum"),
                    },
                    "2" => FieldValueInfo {
                        field_value_name: literal!("bass_drum"),
                    },
                    "3" => FieldValueInfo {
                        field_value_name: literal!("side_stick"),
                    },
                    "4" => FieldValueInfo {
                        field_value_name: literal!("crash_cymbal"),
                    },
                    "5" => FieldValueInfo {
                        field_value_name: literal!("open_hi_hat"),
                    },
                    "6" => FieldValueInfo {
                        field_value_name: literal!("closed_hi_hat"),
                    },
                    "7" => FieldValueInfo {
                        field_value_name: literal!("tambourine"),
                    },
                    "8" => FieldValueInfo {
                        field_value_name: literal!("hand_clap"),
                    },
                    "9" => FieldValueInfo {
                        field_value_name: literal!("claves"),
                    },
                    "10" => FieldValueInfo {
                        field_value_name: literal!("wood_block"),
                    },
                    "11" => FieldValueInfo {
                        field_value_name: literal!("cowbell"),
                    },
                    "12" => FieldValueInfo {
                        field_value_name: literal!("triangle"),
                    },
                    "13" => FieldValueInfo {
                        field_value_name: literal!("bongo"),
                    },
                    "14" => FieldValueInfo {
                        field_value_name: literal!("conga"),
                    },
                    "15" => FieldValueInfo {
                        field_value_name: literal!("cabasa"),
                    },
                    "16" => FieldValueInfo {
                        field_value_name: literal!("guiro"),
                    },
                    "17" => FieldValueInfo {
                        field_value_name: literal!("vibraslap"),
                    },
                    "18" => FieldValueInfo {
                        field_value_name: literal!("cuica"),
                    },
                    _ => return None,
                });
            }
            "music_menu_INSTRUMENT" => {
                return Some(match value.as_str() {
                    "1" => FieldValueInfo {
                        field_value_name: literal!("piano"),
                    },
                    "2" => FieldValueInfo {
                        field_value_name: literal!("electric_piano"),
                    },
                    "3" => FieldValueInfo {
                        field_value_name: literal!("organ"),
                    },
                    "4" => FieldValueInfo {
                        field_value_name: literal!("guitar"),
                    },
                    "5" => FieldValueInfo {
                        field_value_name: literal!("electric_guitar"),
                    },
                    "6" => FieldValueInfo {
                        field_value_name: literal!("bass"),
                    },
                    "7" => FieldValueInfo {
                        field_value_name: literal!("pizzicato"),
                    },
                    "8" => FieldValueInfo {
                        field_value_name: literal!("cello"),
                    },
                    "9" => FieldValueInfo {
                        field_value_name: literal!("trombone"),
                    },
                    "10" => FieldValueInfo {
                        field_value_name: literal!("clarinet"),
                    },
                    "11" => FieldValueInfo {
                        field_value_name: literal!("saxophone"),
                    },
                    "12" => FieldValueInfo {
                        field_value_name: literal!("flute"),
                    },
                    "13" => FieldValueInfo {
                        field_value_name: literal!("wooden_flute"),
                    },
                    "14" => FieldValueInfo {
                        field_value_name: literal!("bassoon"),
                    },
                    "15" => FieldValueInfo {
                        field_value_name: literal!("choir"),
                    },
                    "16" => FieldValueInfo {
                        field_value_name: literal!("vibraphone"),
                    },
                    "17" => FieldValueInfo {
                        field_value_name: literal!("music_box"),
                    },
                    "18" => FieldValueInfo {
                        field_value_name: literal!("steel_drum"),
                    },
                    "19" => FieldValueInfo {
                        field_value_name: literal!("marimba"),
                    },
                    "20" => FieldValueInfo {
                        field_value_name: literal!("synth_lead"),
                    },
                    "21" => FieldValueInfo {
                        field_value_name: literal!("synth_pad"),
                    },
                    _ => return None,
                });
            }
            _ => (),
        }
        Some(match value.as_str() {
            "_random_" => FieldValueInfo {
                field_value_name: literal!("random_position"),
            },
            "_mouse_" => FieldValueInfo {
                field_value_name: literal!("mouse_pointer"),
            },
            "left-right" => FieldValueInfo {
                field_value_name: literal!("left_right"),
            },
            "don't rotate" => FieldValueInfo {
                field_value_name: literal!("dont_rotate"),
            },
            "all around" => FieldValueInfo {
                field_value_name: literal!("all_around"),
            },
            "next backdrop" => FieldValueInfo {
                field_value_name: literal!("next_backdrop"),
            },
            "previous backdrop" => FieldValueInfo {
                field_value_name: literal!("previous_backdrop"),
            },
            "random backdrop" => FieldValueInfo {
                field_value_name: literal!("random_backdrop"),
            },
            "COLOR" => FieldValueInfo {
                field_value_name: literal!("color"),
            },
            "FISHEYE" => FieldValueInfo {
                field_value_name: literal!("fisheye"),
            },
            "WHIRL" => FieldValueInfo {
                field_value_name: literal!("whirl"),
            },
            "PIXELATE" => FieldValueInfo {
                field_value_name: literal!("pixelate"),
            },
            "MOSAIC" => FieldValueInfo {
                field_value_name: literal!("mosaic"),
            },
            "BRIGHTNESS" => FieldValueInfo {
                field_value_name: literal!("brightness"),
            },
            "GHOST" => FieldValueInfo {
                field_value_name: literal!("ghost"),
            },
            "front" => FieldValueInfo {
                field_value_name: literal!("front"),
            },
            "back" => FieldValueInfo {
                field_value_name: literal!("back"),
            },
            "forward" => FieldValueInfo {
                field_value_name: literal!("forward"),
            },
            "backward" => FieldValueInfo {
                field_value_name: literal!("backward"),
            },
            "number" => FieldValueInfo {
                field_value_name: literal!("number"),
            },
            "name" => FieldValueInfo {
                field_value_name: literal!("name"),
            },
            "PITCH" => FieldValueInfo {
                field_value_name: literal!("pitch"),
            },
            "PAN" => FieldValueInfo {
                field_value_name: literal!("pan_left_right"),
            },
            "space" => FieldValueInfo {
                field_value_name: literal!("space"),
            },
            "up arrow" => FieldValueInfo {
                field_value_name: literal!("up_arrow"),
            },
            "down arrow" => FieldValueInfo {
                field_value_name: literal!("down_arrow"),
            },
            "right arrow" => FieldValueInfo {
                field_value_name: literal!("right_arrow"),
            },
            "left arrow" => FieldValueInfo {
                field_value_name: literal!("left_arrow"),
            },
            "any" => FieldValueInfo {
                field_value_name: literal!("any"),
            },
            "a" => FieldValueInfo {
                field_value_name: literal!("key_a"),
            },
            "b" => FieldValueInfo {
                field_value_name: literal!("key_b"),
            },
            "c" => FieldValueInfo {
                field_value_name: literal!("key_c"),
            },
            "d" => FieldValueInfo {
                field_value_name: literal!("key_d"),
            },
            "e" => FieldValueInfo {
                field_value_name: literal!("key_e"),
            },
            "f" => FieldValueInfo {
                field_value_name: literal!("key_f"),
            },
            "g" => FieldValueInfo {
                field_value_name: literal!("key_g"),
            },
            "h" => FieldValueInfo {
                field_value_name: literal!("key_h"),
            },
            "i" => FieldValueInfo {
                field_value_name: literal!("key_i"),
            },
            "j" => FieldValueInfo {
                field_value_name: literal!("key_j"),
            },
            "k" => FieldValueInfo {
                field_value_name: literal!("key_k"),
            },
            "l" => FieldValueInfo {
                field_value_name: literal!("key_l"),
            },
            "m" => FieldValueInfo {
                field_value_name: literal!("key_m"),
            },
            "n" => FieldValueInfo {
                field_value_name: literal!("key_n"),
            },
            "o" => FieldValueInfo {
                field_value_name: literal!("key_o"),
            },
            "p" => FieldValueInfo {
                field_value_name: literal!("key_p"),
            },
            "q" => FieldValueInfo {
                field_value_name: literal!("key_q"),
            },
            "r" => FieldValueInfo {
                field_value_name: literal!("key_r"),
            },
            "s" => FieldValueInfo {
                field_value_name: literal!("key_s"),
            },
            "t" => FieldValueInfo {
                field_value_name: literal!("key_t"),
            },
            "u" => FieldValueInfo {
                field_value_name: literal!("key_u"),
            },
            "v" => FieldValueInfo {
                field_value_name: literal!("key_v"),
            },
            "w" => FieldValueInfo {
                field_value_name: literal!("key_w"),
            },
            "x" => FieldValueInfo {
                field_value_name: literal!("key_x"),
            },
            "y" => FieldValueInfo {
                field_value_name: literal!("key_y"),
            },
            "z" => FieldValueInfo {
                field_value_name: literal!("key_z"),
            },
            "0" => FieldValueInfo {
                field_value_name: literal!("key_0"),
            },
            "1" => FieldValueInfo {
                field_value_name: literal!("key_1"),
            },
            "2" => FieldValueInfo {
                field_value_name: literal!("key_2"),
            },
            "3" => FieldValueInfo {
                field_value_name: literal!("key_3"),
            },
            "4" => FieldValueInfo {
                field_value_name: literal!("key_4"),
            },
            "5" => FieldValueInfo {
                field_value_name: literal!("key_5"),
            },
            "6" => FieldValueInfo {
                field_value_name: literal!("key_6"),
            },
            "7" => FieldValueInfo {
                field_value_name: literal!("key_7"),
            },
            "8" => FieldValueInfo {
                field_value_name: literal!("key_8"),
            },
            "9" => FieldValueInfo {
                field_value_name: literal!("key_9"),
            },
            "LOUDNESS" => FieldValueInfo {
                field_value_name: literal!("loudness"),
            },
            "TIMER" => FieldValueInfo {
                field_value_name: literal!("timer"),
            },
            "all" => FieldValueInfo {
                field_value_name: literal!("all"),
            },
            "this script" => FieldValueInfo {
                field_value_name: literal!("this_script"),
            },
            "other scripts in sprite" => FieldValueInfo {
                field_value_name: literal!("other_scripts_in_sprite"),
            },
            "_myself_" => FieldValueInfo {
                field_value_name: literal!("myself"),
            },
            "_edge_" => FieldValueInfo {
                field_value_name: literal!("edge"),
            },
            "draggable" => FieldValueInfo {
                field_value_name: literal!("draggable"),
            },
            "not draggable" => FieldValueInfo {
                field_value_name: literal!("not_draggable"),
            },
            "YEAR" => FieldValueInfo {
                field_value_name: literal!("year"),
            },
            "MONTH" => FieldValueInfo {
                field_value_name: literal!("month"),
            },
            "DATE" => FieldValueInfo {
                field_value_name: literal!("date"),
            },
            "DAYOFWEEK" => FieldValueInfo {
                field_value_name: literal!("day_of_week"),
            },
            "HOUR" => FieldValueInfo {
                field_value_name: literal!("hour"),
            },
            "MINUTE" => FieldValueInfo {
                field_value_name: literal!("minute"),
            },
            "SECOND" => FieldValueInfo {
                field_value_name: literal!("second"),
            },
            "abs" => FieldValueInfo {
                field_value_name: literal!("op_abs"),
            },
            "floor" => FieldValueInfo {
                field_value_name: literal!("op_floor"),
            },
            "ceiling" => FieldValueInfo {
                field_value_name: literal!("op_ceil"),
            },
            "sqrt" => FieldValueInfo {
                field_value_name: literal!("op_sqrt"),
            },
            "sin" => FieldValueInfo {
                field_value_name: literal!("op_sin"),
            },
            "cos" => FieldValueInfo {
                field_value_name: literal!("op_cos"),
            },
            "tan" => FieldValueInfo {
                field_value_name: literal!("op_tan"),
            },
            "asin" => FieldValueInfo {
                field_value_name: literal!("op_asin"),
            },
            "acos" => FieldValueInfo {
                field_value_name: literal!("op_acos"),
            },
            "atan" => FieldValueInfo {
                field_value_name: literal!("op_atan"),
            },
            "ln" => FieldValueInfo {
                field_value_name: literal!("op_ln"),
            },
            "log" => FieldValueInfo {
                field_value_name: literal!("op_log"),
            },
            "e ^" => FieldValueInfo {
                field_value_name: literal!("op_exp"),
            },
            "10 ^" => FieldValueInfo {
                field_value_name: literal!("op_pow"),
            },

            "color" => FieldValueInfo {
                field_value_name: literal!("pen_color"),
            },
            "saturation" => FieldValueInfo {
                field_value_name: literal!("pen_saturation"),
            },
            "brightness" => FieldValueInfo {
                field_value_name: literal!("pen_brightness"),
            },
            "transparency" => FieldValueInfo {
                field_value_name: literal!("pen_transparency"),
            },
            _ => return None,
        })
    } else {
        None
    }
}
