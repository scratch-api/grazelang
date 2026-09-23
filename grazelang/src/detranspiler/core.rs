use std::{cmp::Ordering, collections::HashMap};

use arcstr::{ArcStr as IString, format as format_istring, literal};
use grazelang_types::project_json;
use serde::{Deserialize, Serialize, de::DeserializeOwned};

use super::{
    get_info::{
        Argument, ArgumentKind, BlockKindInfo, DynamicMenuInputKind, FieldValueInfo,
        check_special_reporter, check_special_stack_block, get_block_kind_info,
        get_field_value_info, get_normal_field_value_info,
    },
    into_ast::{
        IntoAST, assets_to_asset_declaration, data_to_data_declaration,
        data_to_split_data_declaration,
    },
    special_blocks::{convert_special_reporter_block, convert_special_stack_block},
};
use crate::{
    ast::types::{self as ast_types},
    codegen::core::{STAGE_FIELD_VALUE_ISTRING, STAGE_ISTRING},
    library::{BlockShape, get_block_shape},
    messages::types::{GrazeDetranspilerError, GrazeDetranspilerMessage, GrazeDetranspilerWarning},
    names::{DetranspilerAssetNamespace, DetranspilerTargetNamespace},
    parser::cst::EMPTY_ISTRING_REF,
    settings::{GrazeDetranspilerSettings, GrazeMessageSetting},
};

#[derive(Debug, Clone, PartialEq)]
pub struct DetranspilerContext {
    pub stage_target_index: usize,
    pub targets: Vec<DetranspilerTarget>,
    pub target_indices: HashMap<IString, usize>,
    pub asset_namespace: DetranspilerAssetNamespace,
    pub assets: HashMap<AssetPath, OutAssetPath>,
    pub messages: Vec<GrazeDetranspilerMessage>,
    pub unreturned_failure: bool,
    pub settings: GrazeDetranspilerSettings,
    pub broadcasts: HashMap<DataId, DetranspilerBroadcast>,
    pub current_procedure_parameters:
        HashMap<ProcedureParameterOriginalName, ProcedureParameterInternalName>,
    pub global_namespace: DetranspilerTargetNamespace,
}

pub(super) type DetranspilerResult<T> = Result<T, GrazeDetranspilerError>;

type OutAssetPath = IString;
type AssetPath = String;
type AssetId = String;
type DataId = String;
type ProcedureParameterOriginalName = IString;
type ProcedureParameterInternalName = IString;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DetranspilerTarget {
    pub is_stage: bool,
    pub internal_name: IString,
    pub costumes: Vec<(AssetId, DetranspilerAsset<DetranspilerCostumeUncommonData>)>,
    pub costume_indices: HashMap<IString, usize>,
    pub sounds: Vec<(AssetId, DetranspilerAsset<DetranspilerSoundUncommonData>)>,
    pub sound_indices: HashMap<IString, usize>,
    pub data: HashMap<DataId, DetranspilerVarOrList>,
    pub namespace: DetranspilerTargetNamespace,
    pub monitors: Vec<DetranspilerMonitor>,
    pub config: Vec<ast_types::DictionaryEntry>,
    pub procedures: HashMap<IString, DetranspilerCustomBlockDescriptor>,
    pub scripts: Vec<(String, DetranspilerTargetBlockStack)>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DetranspilerTargetBlockStack {
    HatBlock {
        hat_function: ast_types::Identifier,
        arguments: Vec<ast_types::Expression>,
        code_block: ast_types::CodeBlock,
    },
    CustomBlock {
        is_warp: ast_types::WarpSpecifier,
        canonical_identifier: Option<ast_types::CanonicalIdentifier>,
        identifier: ast_types::SingleIdentifier,
        parameters: Vec<(
            Option<ast_types::CustomBlockParamKind>,
            Option<ast_types::CanonicalIdentifier>,
            ast_types::SingleIdentifier,
        )>,
        code_block: ast_types::CodeBlock,
    },
    IsolatedStack {
        code_block: ast_types::CodeBlock,
    },
    IsolatedExpression {
        expression: ast_types::Expression,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BorrowedDetranspilerVLB<'a> {
    Broadcast(&'a DetranspilerBroadcast),
    VarOrList(&'a DetranspilerVarOrList),
}

impl<'a> From<BorrowedDetranspilerVLB<'a>> for InternalVLBIdentifier {
    fn from(value: BorrowedDetranspilerVLB<'a>) -> Self {
        match value {
            BorrowedDetranspilerVLB::Broadcast(value) => {
                InternalVLBIdentifier::Broadcast(value.name.clone())
            }
            BorrowedDetranspilerVLB::VarOrList(value) => {
                InternalVLBIdentifier::VarOrList(value.name.clone())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum InternalVLBIdentifier {
    Broadcast(IString),
    VarOrList(IString),
}

impl<'a> BorrowedDetranspilerVLB<'a> {
    pub fn get_canonical_name(self) -> Option<&'a IString> {
        match self {
            BorrowedDetranspilerVLB::Broadcast(broadcast) => broadcast.canonical_name.as_ref(),
            BorrowedDetranspilerVLB::VarOrList(var_or_list) => var_or_list.canonical_name.as_ref(),
        }
    }

    pub fn get_name(self) -> &'a IString {
        match self {
            BorrowedDetranspilerVLB::Broadcast(broadcast) => &broadcast.name,
            BorrowedDetranspilerVLB::VarOrList(var_or_list) => &var_or_list.name,
        }
    }

    pub fn get_original_name(self) -> &'a IString {
        match self {
            BorrowedDetranspilerVLB::Broadcast(broadcast) => {
                broadcast.canonical_name.as_ref().unwrap_or(&broadcast.name)
            }
            BorrowedDetranspilerVLB::VarOrList(var_or_list) => var_or_list
                .canonical_name
                .as_ref()
                .unwrap_or(&var_or_list.name),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DetranspilerBroadcast {
    pub canonical_name: Option<IString>,
    pub name: IString,
}

impl DetranspilerBroadcast {
    #[inline]
    pub fn get_original_name(&self) -> &IString {
        self.canonical_name.as_ref().unwrap_or(&self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DetranspilerVarOrList {
    pub canonical_name: Option<IString>,
    pub name: IString,
    pub kind: DetranspilerVarOrListKind,
}

impl DetranspilerVarOrList {
    #[inline]
    pub fn get_original_name(&self) -> &IString {
        self.canonical_name.as_ref().unwrap_or(&self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DetranspilerVarOrListKind {
    Variable {
        value: project_json::Sb3PrimitiveOrBool,
    },
    List {
        value: Vec<project_json::Sb3PrimitiveOrBool>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(bound(deserialize = "D: DeserializeOwned"))]
pub struct DetranspilerAsset<D>
where
    D: std::fmt::Debug + Clone + PartialEq + Serialize + DeserializeOwned,
{
    pub canonical_name: Option<IString>,
    pub name: IString,
    pub file_extension: String,
    pub uncommon_data: D,
    pub asset_path: IString,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DetranspilerCostumeUncommonData {
    pub rotation_center_x: f64,
    pub rotation_center_y: f64,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DetranspilerSoundUncommonData;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DetranspilerMonitor {
    pub value: ast_types::MonitorValue,
    pub config: Vec<ast_types::DictionaryEntry>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DetranspilerCustomBlockDescriptor {
    pub canonical_name: Option<IString>,
    pub name: IString,
    pub argument_count: usize,
}

#[inline]
pub fn emit_message<M>(
    context: &mut DetranspilerContext,
    message: M,
    message_type: GrazeMessageSetting,
) where
    M: FnOnce() -> GrazeDetranspilerMessage,
{
    if context.settings.message_setting >= message_type {
        context.messages.push(message());
    }
}

#[inline]
pub fn emit_message_eager(
    context: &mut DetranspilerContext,
    message: GrazeDetranspilerMessage,
    message_type: GrazeMessageSetting,
) {
    if context.settings.message_setting >= message_type {
        context.messages.push(message);
    }
}

macro_rules! try_or_emit_message {
    ($value:expr, $context:expr, $default_value:expr) => {
        unwrap_or_emit_message!($value, $context, return $default_value)
    };
}

#[expect(unused_imports)]
pub(super) use try_or_emit_message;

macro_rules! unwrap_or_emit_message {
    ($value:expr, $context:expr, $default_value:expr) => {
        match $value {
            Ok(value) => value,
            Err(err) => {
                emit_error!(err, $context);
                $default_value
            }
        }
    };
}

pub(super) use unwrap_or_emit_message;

macro_rules! emit_error_inline {
    ($err:expr, $context:expr) => {{
        let err = $err;
        if matches!(
            $context.settings.message_setting,
            GrazeMessageSetting::ExitOnError | GrazeMessageSetting::ExitOnErrorUnlogged
        ) {
            return Err(err);
        }
        $context.unreturned_failure = true;
        if $context.settings.message_setting >= GrazeMessageSetting::Errors {
            $context.messages.push(err.into());
        }
    }};
}

#[expect(unused_imports)]
pub(super) use emit_error_inline;

macro_rules! emit_error {
    ($err:expr, $context:expr) => {{
        let err = $err;
        let context = &mut *$context;
        if matches!(
            context.settings.message_setting,
            GrazeMessageSetting::ExitOnError | GrazeMessageSetting::ExitOnErrorUnlogged
        ) {
            return Err(err);
        }
        context.unreturned_failure = true;
        emit_message_eager(context, err.into(), GrazeMessageSetting::Errors);
    }};
}

pub(super) use emit_error;

pub fn name_matches<S>(check_name: &str, canonical_name: Option<&S>, name: &S) -> bool
where
    S: AsRef<str>,
{
    canonical_name.unwrap_or(name).as_ref() == check_name
}

/// Result is bubbled
pub fn lookup_vlb<'a>(
    name: &str,
    id: &str,
    target_index: usize,
    context: &'a mut DetranspilerContext,
) -> DetranspilerResult<Option<BorrowedDetranspilerVLB<'a>>> {
    let result = context
        .targets
        .get(context.stage_target_index)
        .and_then(|value| value.data.get(id))
        .or_else(|| {
            context
                .targets
                .get(target_index)
                .and_then(|value| value.data.get(id))
        })
        .map(BorrowedDetranspilerVLB::VarOrList)
        .or_else(|| {
            context
                .broadcasts
                .get(id)
                .map(BorrowedDetranspilerVLB::Broadcast)
        });
    if let Some(value) = &result
        && let expected_name = value.get_original_name().as_str()
        && expected_name != name
    {
        emit_error_inline!(
            GrazeDetranspilerError::VLBNameIncorrect {
                id: id.to_string(),
                name: name.to_string(),
                expected_name: expected_name.to_string()
            },
            context
        )
    }
    Ok(result)
}

/// Result is bubbled
pub fn lookup_broadcast<'a>(
    name: &str,
    id: &str,
    context: &'a mut DetranspilerContext,
) -> DetranspilerResult<Option<&'a DetranspilerBroadcast>> {
    let result = context.broadcasts.get(id);
    if let Some(value) = &result
        && let expected_name = value.get_original_name().as_str()
        && expected_name != name
    {
        emit_error_inline!(
            GrazeDetranspilerError::VLBNameIncorrect {
                id: id.to_string(),
                name: name.to_string(),
                expected_name: expected_name.to_string()
            },
            context
        )
    }
    Ok(result)
}

/// Result is bubbled
pub fn lookup_var_or_list<'a>(
    name: &str,
    id: &str,
    target_index: usize,
    context: &'a mut DetranspilerContext,
) -> DetranspilerResult<Option<&'a DetranspilerVarOrList>> {
    let result = context
        .targets
        .get(context.stage_target_index)
        .and_then(|value| value.data.get(id))
        .or_else(|| {
            context
                .targets
                .get(target_index)
                .and_then(|value| value.data.get(id))
        });
    if let Some(value) = &result
        && let expected_name = value.get_original_name().as_str()
        && expected_name != name
    {
        emit_error_inline!(
            GrazeDetranspilerError::VLBNameIncorrect {
                id: id.to_string(),
                name: name.to_string(),
                expected_name: expected_name.to_string()
            },
            context
        )
    }
    Ok(result)
}

pub fn find_var_or_list_by_name<'a>(
    name: &str,
    target_index: usize,
    context: &'a mut DetranspilerContext,
) -> Option<&'a DetranspilerVarOrList> {
    context
        .targets
        .get(context.stage_target_index)
        .and_then(|value: &DetranspilerTarget| {
            value
                .data
                .iter()
                .find(|value| value.1.get_original_name().as_str() == name)
        })
        .or_else(|| {
            context.targets.get(target_index).and_then(|value| {
                value
                    .data
                    .iter()
                    .find(|value| value.1.get_original_name().as_str() == name)
            })
        })
        .map(|value| value.1)
}

pub fn find_property_name(target: &DetranspilerTarget, property: &str) -> Option<IString> {
    if target.is_stage {
        match property {
            "volume" => return Some(literal!("volume")),
            "backdrop #" => return Some(literal!("backdrop_number")),
            "backdrop name" => return Some(literal!("backdrop_name")),
            _ => (),
        }
    } else {
        match property {
            "volume" => return Some(literal!("volume")),
            "costume #" => return Some(literal!("costume_number")),
            "costume name" => return Some(literal!("costume_name")),
            "x position" => return Some(literal!("x_position")),
            "y position" => return Some(literal!("y_position")),
            "direction" => return Some(literal!("direction")),
            "size" => return Some(literal!("size")),
            _ => (),
        }
    }
    target
        .data
        .iter()
        .find(|value| value.1.get_original_name().as_str() == property)
        .map(|(_, value)| value.name.clone())
}

pub type DetranspiledProjectData = (
    ast_types::GrazeProgram,
    HashMap<AssetPath, OutAssetPath>,
    Vec<GrazeDetranspilerMessage>,
);

pub fn convert_project(
    project: &project_json::Sb3Root,
    settings: GrazeDetranspilerSettings,
) -> Result<DetranspiledProjectData, Vec<GrazeDetranspilerMessage>> {
    macro_rules! emit_error_top_level {
        ($context:expr, $err:expr) => {{
            let context = &mut $context;
            emit_message_eager(context, $err.into(), GrazeMessageSetting::Errors);
            match context.settings.message_setting {
                GrazeMessageSetting::ExitOnError => {
                    context.messages.push($err.into());
                    return Err($context.messages);
                }
                GrazeMessageSetting::ExitOnErrorUnlogged => {
                    return Err($context.messages);
                }
                _ => (),
            }
        }};
    }
    macro_rules! unwrap_bubbled_result_or {
        ($context_ident:pat => $result:expr, $context:expr) => {{
            let context = &mut $context;
            match {
                let $context_ident = &mut *context;
                $result
            } {
                Ok(value) => value,
                Err(err) => {
                    if context.settings.message_setting >= GrazeMessageSetting::ExitOnError {
                        context.messages.push(err.into());
                    }
                    return Err($context.messages);
                }
            }
        }};
    }
    let mut context = DetranspilerContext {
        stage_target_index: 0,
        targets: Vec::with_capacity(project.targets.len()),
        target_indices: HashMap::with_capacity(project.targets.len()),
        asset_namespace: DetranspilerAssetNamespace::new(),
        assets: HashMap::new(),
        messages: Vec::new(),
        unreturned_failure: false,
        settings,
        broadcasts: HashMap::new(),
        current_procedure_parameters: HashMap::new(),
        global_namespace: DetranspilerTargetNamespace::new(),
    };
    let mut has_stage = false;
    let target_internal_names = project
        .targets
        .iter()
        .map(|value| {
            if value.is_stage {
                (EMPTY_ISTRING_REF.clone(), EMPTY_ISTRING_REF.clone())
            } else {
                let canonical_name = IString::from(&value.name);
                let name = context
                    .global_namespace
                    .introduce_new_name(canonical_name.clone(), None);
                (canonical_name, name)
            }
        })
        .collect::<Vec<_>>();
    for target in &project.targets {
        if !target.is_stage {
            continue;
        }
        if has_stage {
            emit_error_top_level!(context, GrazeDetranspilerError::MultipleStages);
        }
        context.broadcasts.reserve(target.broadcasts.len());
        for (id, canonical_name) in &target.broadcasts {
            let canonical_name = IString::from(canonical_name);
            let name = context
                .global_namespace
                .introduce_new_name(canonical_name.clone(), None);
            context.broadcasts.insert(
                id.to_string(),
                DetranspilerBroadcast {
                    canonical_name: (name != canonical_name).then_some(canonical_name),
                    name,
                },
            );
        }
        has_stage = true;
        let target = unwrap_bubbled_result_or!(
            context => convert_target(target, context, STAGE_ISTRING.clone()),
            context
        );
        context.targets.push(target);
    }
    let mut stage = unwrap_bubbled_result_or!(
        context => context.targets.pop().ok_or({
            GrazeDetranspilerError::StageMissing
        }).map(Some),
        context
    );
    for (index, target) in project.targets.iter().enumerate() {
        if target.is_stage
            && let Some(stage) = stage.take()
        {
            context.stage_target_index = context.targets.len();
            context.targets.push(stage);
            context
                .target_indices
                .insert(STAGE_FIELD_VALUE_ISTRING.clone(), index);
            continue;
        } else {
            context
                .target_indices
                .insert(target.name.as_str().into(), index);
        }
        let target = unwrap_bubbled_result_or!(
            context => convert_target(target, context, target_internal_names.get(index).unwrap().1.clone()),
            context
        );
        context.targets.push(target);
    }
    for monitor in &project.monitors {
        let target_index = monitor
            .sprite_name
            .as_deref()
            .and_then(|value| context.target_indices.get(value))
            .copied()
            .unwrap_or(context.stage_target_index);
        unwrap_bubbled_result_or!(
            context => add_monitor(monitor, context, target_index),
            context
        );
    }
    if context.unreturned_failure {
        return Err(context.messages);
    }
    for (index, target) in project.targets.iter().enumerate() {
        unwrap_bubbled_result_or!(
            context => fill_target(target, context, index),
            context
        );
    }
    let mut statements = Vec::with_capacity(
        project.extensions.len() + context.broadcasts.len() + context.targets.len(),
    );
    for extension in &project.extensions {
        match extension.as_str() {
            "pen" => statements.push(ast_types::TopLevelStatement::UseExtensionStatement(
                ast_types::UseStatementContent::SingleUse {
                    identifier: create_simple_identifier(literal!("pen")),
                    rename: None,
                },
            )),
            "music" => statements.push(ast_types::TopLevelStatement::UseExtensionStatement(
                ast_types::UseStatementContent::SingleUse {
                    identifier: create_simple_identifier(literal!("music")),
                    rename: None,
                },
            )),
            _ => {
                emit_message(
                    &mut context,
                    || {
                        GrazeDetranspilerWarning::UnknownExtension {
                            extension: extension.clone(),
                        }
                        .into()
                    },
                    GrazeMessageSetting::Warnings,
                );
                statements.push(ast_types::TopLevelStatement::UseExtensionStatement(
                    ast_types::UseStatementContent::SingleUse {
                        identifier: create_simple_identifier(extension.as_str().into()),
                        rename: None,
                    },
                ))
            }
        }
    }
    for broadcast in context.broadcasts.values() {
        statements.push(broadcast.into_ast());
    }
    for (target_index, target) in context.targets.iter_mut().enumerate() {
        statements.push(if target.is_stage {
            let mut stage_statements = Vec::with_capacity(
                1 + target.costumes.len()
                    + target.sounds.len()
                    + target.data.len()
                    + target.monitors.len()
                    + target.scripts.len(),
            );
            stage_statements.push(ast_types::StageStatement::ConfigStatement(std::mem::take(
                &mut target.config,
            )));
            if context.settings.multi_asset_declarations {
                if !target.costumes.is_empty() {
                    stage_statements.push(ast_types::StageStatement::BackdropDeclaration(
                        assets_to_asset_declaration(target.costumes.iter().map(|(_, value)| value)),
                    ));
                }
                if !target.sounds.is_empty() {
                    stage_statements.push(ast_types::StageStatement::SoundDeclaration(
                        assets_to_asset_declaration(target.sounds.iter().map(|(_, value)| value)),
                    ));
                }
            } else {
                target
                    .costumes
                    .iter()
                    .map(|(_, value)| value)
                    .for_each(|value| stage_statements.push(value.into_ast()));
                target
                    .sounds
                    .iter()
                    .map(|(_, value)| value)
                    .for_each(|value| stage_statements.push(value.into_ast()));
            }
            match context.settings.multi_data_declarations {
                crate::settings::MultiDataDeclarationsMode::None => {
                    target
                        .data
                        .values()
                        .for_each(|value| stage_statements.push(value.into_ast()));
                }
                crate::settings::MultiDataDeclarationsMode::HomogeneousDeclarations => {
                    let (vars, lists) = data_to_split_data_declaration(target.data.values());
                    if !vars.is_empty() {
                        stage_statements.push(ast_types::StageStatement::DataDeclaration(
                            ast_types::DataDeclaration::Vars {
                                scope: Default::default(),
                                declarations: vars,
                            },
                        ));
                    }
                    if !lists.is_empty() {
                        stage_statements.push(ast_types::StageStatement::DataDeclaration(
                            ast_types::DataDeclaration::Lists {
                                scope: Default::default(),
                                declarations: lists,
                            },
                        ));
                    }
                }
                crate::settings::MultiDataDeclarationsMode::MixedDeclarations => {
                    if !target.data.is_empty() {
                        stage_statements.push(ast_types::StageStatement::DataDeclaration(
                            ast_types::DataDeclaration::Mixed {
                                scope: Default::default(),
                                declarations: data_to_data_declaration(target.data.values()),
                            },
                        ));
                    }
                }
            }
            std::mem::take(&mut target.monitors)
                .into_iter()
                .for_each(|value| stage_statements.push(value.into_ast()));
            std::mem::take(&mut target.scripts)
                .into_iter()
                .for_each(|(_, value)| stage_statements.push(value.into_ast()));
            ast_types::TopLevelStatement::Stage {
                code_block: ast_types::StageCodeBlock {
                    statements: stage_statements,
                },
            }
        } else {
            let mut sprite_statements = Vec::with_capacity(
                1 + target.costumes.len()
                    + target.sounds.len()
                    + target.data.len()
                    + target.monitors.len()
                    + target.scripts.len(),
            );
            sprite_statements.push(ast_types::SpriteStatement::ConfigStatement(std::mem::take(
                &mut target.config,
            )));
            if context.settings.multi_asset_declarations {
                if !target.costumes.is_empty() {
                    sprite_statements.push(ast_types::SpriteStatement::CostumeDeclaration(
                        assets_to_asset_declaration(target.costumes.iter().map(|(_, value)| value)),
                    ));
                }
                if !target.sounds.is_empty() {
                    sprite_statements.push(ast_types::SpriteStatement::SoundDeclaration(
                        assets_to_asset_declaration(target.sounds.iter().map(|(_, value)| value)),
                    ));
                }
            } else {
                target
                    .costumes
                    .iter()
                    .map(|(_, value)| value)
                    .for_each(|value| sprite_statements.push(value.into_ast()));
                target
                    .sounds
                    .iter()
                    .map(|(_, value)| value)
                    .for_each(|value| sprite_statements.push(value.into_ast()));
            }
            match context.settings.multi_data_declarations {
                crate::settings::MultiDataDeclarationsMode::None => {
                    target
                        .data
                        .values()
                        .for_each(|value| sprite_statements.push(value.into_ast()));
                }
                crate::settings::MultiDataDeclarationsMode::HomogeneousDeclarations => {
                    let (vars, lists) = data_to_split_data_declaration(target.data.values());
                    if !vars.is_empty() {
                        sprite_statements.push(ast_types::SpriteStatement::DataDeclaration(
                            ast_types::DataDeclaration::Vars {
                                scope: Default::default(),
                                declarations: vars,
                            },
                        ));
                    }
                    if !lists.is_empty() {
                        sprite_statements.push(ast_types::SpriteStatement::DataDeclaration(
                            ast_types::DataDeclaration::Lists {
                                scope: Default::default(),
                                declarations: lists,
                            },
                        ));
                    }
                }
                crate::settings::MultiDataDeclarationsMode::MixedDeclarations => {
                    if !target.data.is_empty() {
                        sprite_statements.push(ast_types::SpriteStatement::DataDeclaration(
                            ast_types::DataDeclaration::Mixed {
                                scope: Default::default(),
                                declarations: data_to_data_declaration(target.data.values()),
                            },
                        ));
                    }
                }
            }
            std::mem::take(&mut target.monitors)
                .into_iter()
                .for_each(|value| sprite_statements.push(value.into_ast()));
            std::mem::take(&mut target.scripts)
                .into_iter()
                .for_each(|(_, value)| sprite_statements.push(value.into_ast()));
            let (canonical_name, name) = target_internal_names.get(target_index).unwrap();
            ast_types::TopLevelStatement::Sprite {
                canonical_identifier: (canonical_name != name)
                    .then(|| ast_types::CanonicalIdentifier::new(canonical_name.clone())),
                identifier: ast_types::SingleIdentifier::new(name.clone()),
                code_block: ast_types::SpriteCodeBlock {
                    statements: sprite_statements,
                },
            }
        });
    }
    Ok((
        ast_types::GrazeProgram(statements),
        context.assets,
        context.messages,
    ))
}

// TODO: Improve error handling in order to allow emitting more messages at once
// Issue: #137

// TODO: Implement list methods in detranspiler
//  - [x] `clear`
//  - [x] `push`
//  - [x] `remove`
//  - [x] `insert`
//  - [x] `find`
//  - [x] `len`
//  - [x] `contains`
//  - [x] `show`
//  - [x] `hide`
//  - [x] `set` as a `Statement::SetItem`
//  - [x] `get` as a `Expression::GetItem`
// Issue: #120

// TODO: Add option to configure formatting after detranspilation
// Issue: #134

// TODO: Allow detranspiler to use a single file per target
// Issue: #132

// TODO: Ensure that `grazelang` compiles with `#[cfg(not(feature = "detranspiler"))]`
// Issue: #128

// A function is unbubbled iff it tries (`?`) any unbubbled result or returns a Err at any point without checking if
// ExitOnError or ExitOnErrorUnlogged is on. A function is bubbled iff it is not unbubbled.
// A Result is unbubbled iff it results from an unbubbled function or is an Err that is created without checking if
// ExitOnError or ExitOnErrorUnlogged is on. A Result is bubbled iff it is not unbubbled.

// Bubbling is done to allow the detranspiler to catch as many errors or warnings at once as possible

/// Result is bubbled
pub fn convert_target(
    target: &project_json::Sb3Target,
    context: &mut DetranspilerContext,
    internal_name: IString,
) -> DetranspilerResult<DetranspilerTarget> {
    fn get_asset_path_and_register_asset(
        context: &mut DetranspilerContext,
        name: &str,
        asset_id: &str,
        data_format: &str,
        md5ext: &str,
    ) -> IString {
        let asset_name = context.asset_namespace.get_symbol(name, asset_id);
        let out_asset_path = arcstr::format!("{asset_name}.{}", data_format);
        if !context.assets.contains_key(md5ext) {
            context
                .assets
                .insert(md5ext.to_string(), out_asset_path.clone());
        }
        out_asset_path
    }
    fn get_canonical_name_and_name(
        namespace: &mut DetranspilerTargetNamespace,
        canonical_name: IString,
        is_stage: bool,
        context: &mut DetranspilerContext,
    ) -> (Option<IString>, IString) {
        let name = if is_stage {
            context
                .global_namespace
                .introduce_new_name(canonical_name.clone(), None)
        } else {
            namespace.introduce_new_name(
                canonical_name.clone(),
                Some(&context.global_namespace.used_names),
            )
        };
        ((name != canonical_name).then_some(canonical_name), name)
    }
    let mut namespace = DetranspilerTargetNamespace::new();
    let mut costumes = Vec::with_capacity(target.costumes.len());
    let mut costume_indices = HashMap::with_capacity(target.costumes.len());
    for costume in &target.costumes {
        let original_name = IString::from(costume.name.as_str());
        let (canonical_name, name) = get_canonical_name_and_name(
            &mut namespace,
            original_name.clone(),
            target.is_stage,
            context,
        );
        costumes.push((
            costume.asset_id.clone(),
            DetranspilerAsset {
                canonical_name,
                name,
                file_extension: costume.data_format.clone(),
                uncommon_data: DetranspilerCostumeUncommonData {
                    rotation_center_x: costume.rotation_center_x,
                    rotation_center_y: costume.rotation_center_y,
                },
                asset_path: get_asset_path_and_register_asset(
                    context,
                    &costume.name,
                    &costume.asset_id,
                    &costume.data_format,
                    &costume.md5ext,
                ),
            },
        ));
        let index = costume_indices.len();
        costume_indices.insert(original_name, index);
    }
    let mut sounds = Vec::with_capacity(target.costumes.len());
    let mut sound_indices = HashMap::with_capacity(target.costumes.len());
    for sound in &target.sounds {
        let original_name = IString::from(sound.name.as_str());
        let (canonical_name, name) = get_canonical_name_and_name(
            &mut namespace,
            original_name.clone(),
            target.is_stage,
            context,
        );
        sounds.push((
            sound.asset_id.clone(),
            DetranspilerAsset {
                canonical_name,
                name,
                file_extension: sound.data_format.clone(),
                uncommon_data: DetranspilerSoundUncommonData,
                asset_path: get_asset_path_and_register_asset(
                    context,
                    &sound.name,
                    &sound.asset_id,
                    &sound.data_format,
                    &sound.md5ext,
                ),
            },
        ));
        let index = sound_indices.len();
        sound_indices.insert(original_name, index);
    }
    let mut data = HashMap::with_capacity(target.variables.len() + target.lists.len());
    for (id, variable) in &target.variables {
        let (canonical_name, name) = get_canonical_name_and_name(
            &mut namespace,
            variable.name.as_str().into(),
            target.is_stage,
            context,
        );
        data.insert(
            id.to_string(),
            DetranspilerVarOrList {
                canonical_name,
                name,
                kind: DetranspilerVarOrListKind::Variable {
                    value: variable.value.clone(),
                },
            },
        );
    }
    for (id, list) in &target.lists {
        let (canonical_name, name) = get_canonical_name_and_name(
            &mut namespace,
            list.0.as_str().into(),
            target.is_stage,
            context,
        );
        data.insert(
            id.to_string(),
            DetranspilerVarOrList {
                canonical_name,
                name,
                kind: DetranspilerVarOrListKind::List {
                    value: list.1.clone(),
                },
            },
        );
    }
    let mut config = Vec::with_capacity(if target.is_stage {
        1 + (target.current_costume != 0) as usize
            + (target.volume != 100.0) as usize
            + target.text_to_speech_language.is_some() as usize
            + (target.video_transparency != Some(50.0)) as usize
            + (!matches!(target.video_state.as_deref(), Some("on"))) as usize
    } else {
        1 + (target.current_costume != 0) as usize
            + (target.x != Some(0.0)) as usize
            + (target.y != Some(0.0)) as usize
            + (target.direction != Some(90.0)) as usize
            + (target.size != Some(100.0)) as usize
            + (target.volume != 100.0) as usize
            + (target.draggable != Some(false)) as usize
            + (target.visible != Some(true)) as usize
            + (!matches!(target.rotation_style.as_deref(), Some("all_around"))) as usize
    });
    if target.is_stage {
        config.push(ast_types::DictionaryEntry {
            identifier: ast_types::SingleIdentifier::new(literal!("layer_order")),
            value: ast_types::DictionaryValue::Primitive(ast_types::Literal::DecimalInt(
                format_istring!("{}", target.layer_order),
            )),
        });
        if target.current_costume != 0 {
            config.push(ast_types::DictionaryEntry {
                identifier: ast_types::SingleIdentifier::new(literal!("backdrop")),
                value: ast_types::DictionaryValue::Primitive(ast_types::Literal::DecimalInt(
                    format_istring!("{}", target.current_costume + 1),
                )),
            });
        }
        if target.volume != 100.0 {
            config.push(ast_types::DictionaryEntry {
                identifier: ast_types::SingleIdentifier::new(literal!("volume")),
                value: ast_types::DictionaryValue::Primitive(ast_types::Literal::DecimalFloat(
                    format_istring!("{}", target.volume),
                )),
            });
        }
        if let Some(text_to_speech_language) = &target.text_to_speech_language {
            config.push(ast_types::DictionaryEntry {
                identifier: ast_types::SingleIdentifier::new(literal!("text_to_speech_language")),
                value: ast_types::DictionaryValue::Primitive(ast_types::Literal::String(
                    text_to_speech_language.as_str().into(),
                )),
            });
        }
        if target.video_transparency != Some(50.0) {
            config.push(ast_types::DictionaryEntry {
                identifier: ast_types::SingleIdentifier::new(literal!("video_transparency")),
                value: ast_types::DictionaryValue::Primitive(
                    if let Some(video_transparency) = target.video_transparency {
                        ast_types::Literal::DecimalFloat(format_istring!("{video_transparency}"))
                    } else {
                        ast_types::Literal::EmptyExpression
                    },
                ),
            });
        }
        if !matches!(target.video_state.as_deref(), Some("on")) {
            config.push(ast_types::DictionaryEntry {
                identifier: ast_types::SingleIdentifier::new(literal!("video_state")),
                value: ast_types::DictionaryValue::Primitive(
                    if let Some(video_state) = &target.video_state {
                        ast_types::Literal::String(video_state.as_str().into())
                    } else {
                        ast_types::Literal::EmptyExpression
                    },
                ),
            });
        }
    } else {
        config.push(ast_types::DictionaryEntry {
            identifier: ast_types::SingleIdentifier::new(literal!("layer_order")),
            value: ast_types::DictionaryValue::Primitive(ast_types::Literal::DecimalInt(
                format_istring!("{}", target.layer_order),
            )),
        });
        if target.current_costume != 0 {
            config.push(ast_types::DictionaryEntry {
                identifier: ast_types::SingleIdentifier::new(literal!("costume")),
                value: ast_types::DictionaryValue::Primitive(ast_types::Literal::DecimalInt(
                    format_istring!("{}", target.current_costume + 1),
                )),
            });
        }
        if target.x != Some(0.0) {
            config.push(ast_types::DictionaryEntry {
                identifier: ast_types::SingleIdentifier::new(literal!("x_position")),
                value: ast_types::DictionaryValue::Primitive(if let Some(x) = target.x {
                    ast_types::Literal::DecimalFloat(format_istring!("{x}"))
                } else {
                    ast_types::Literal::EmptyExpression
                }),
            });
        }
        if target.y != Some(0.0) {
            config.push(ast_types::DictionaryEntry {
                identifier: ast_types::SingleIdentifier::new(literal!("y_position")),
                value: ast_types::DictionaryValue::Primitive(if let Some(y) = target.y {
                    ast_types::Literal::DecimalFloat(format_istring!("{y}"))
                } else {
                    ast_types::Literal::EmptyExpression
                }),
            });
        }
        if target.direction != Some(90.0) {
            config.push(ast_types::DictionaryEntry {
                identifier: ast_types::SingleIdentifier::new(literal!("direction")),
                value: ast_types::DictionaryValue::Primitive(
                    if let Some(direction) = target.direction {
                        ast_types::Literal::DecimalFloat(format_istring!("{direction}"))
                    } else {
                        ast_types::Literal::EmptyExpression
                    },
                ),
            });
        }
        if target.size != Some(100.0) {
            config.push(ast_types::DictionaryEntry {
                identifier: ast_types::SingleIdentifier::new(literal!("size")),
                value: ast_types::DictionaryValue::Primitive(if let Some(size) = target.size {
                    ast_types::Literal::DecimalFloat(format_istring!("{size}"))
                } else {
                    ast_types::Literal::EmptyExpression
                }),
            });
        }
        if target.volume != 100.0 {
            config.push(ast_types::DictionaryEntry {
                identifier: ast_types::SingleIdentifier::new(literal!("volume")),
                value: ast_types::DictionaryValue::Primitive(ast_types::Literal::DecimalFloat(
                    format_istring!("{}", target.volume),
                )),
            });
        }
        if target.draggable != Some(false) {
            config.push(ast_types::DictionaryEntry {
                identifier: ast_types::SingleIdentifier::new(literal!("draggable")),
                value: ast_types::DictionaryValue::Primitive(
                    if let Some(draggable) = target.draggable {
                        ast_types::Literal::Bool(draggable)
                    } else {
                        ast_types::Literal::EmptyExpression
                    },
                ),
            });
        }
        if target.visible != Some(true) {
            config.push(ast_types::DictionaryEntry {
                identifier: ast_types::SingleIdentifier::new(literal!("visible")),
                value: ast_types::DictionaryValue::Primitive(
                    if let Some(visible) = target.visible {
                        ast_types::Literal::Bool(visible)
                    } else {
                        ast_types::Literal::EmptyExpression
                    },
                ),
            });
        }
        if !matches!(target.rotation_style.as_deref(), Some("all_around")) {
            config.push(ast_types::DictionaryEntry {
                identifier: ast_types::SingleIdentifier::new(literal!("rotation_style")),
                value: ast_types::DictionaryValue::Primitive(
                    if let Some(rotation_style) = &target.rotation_style {
                        ast_types::Literal::String(rotation_style.as_str().into())
                    } else {
                        ast_types::Literal::EmptyExpression
                    },
                ),
            });
        }
    }
    let mut custom_blocks = HashMap::new();
    for (block_id, block) in &target.blocks {
        let project_json::Sb3Block::Normal(block) = block else {
            continue;
        };
        if !block.top_level || block.opcode != "procedures_definition" {
            continue;
        }
        let Some(proto_block_id) = block.inputs.get("custom_block") else {
            emit_error!(
                GrazeDetranspilerError::MissingInput {
                    input: "custom_block".to_string(),
                    block_id: block_id.clone()
                },
                context
            );
            continue;
        };
        let (project_json::Sb3InputValue::NoShadow(project_json::Sb3InputRepr::Reference(
            proto_block_id,
        ))
        | project_json::Sb3InputValue::ObscuredShadow {
            value: project_json::Sb3InputRepr::Reference(proto_block_id),
            shadow: _,
        }
        | project_json::Sb3InputValue::Shadow(project_json::Sb3InputRepr::Reference(
            proto_block_id,
        ))) = proto_block_id
        else {
            emit_error!(
                GrazeDetranspilerError::MalformedBlockReference {
                    block_id: Box::new(proto_block_id.clone())
                },
                context
            );
            continue;
        };
        let Some(project_json::Sb3Block::Normal(proto_block)) = target.blocks.get(proto_block_id)
        else {
            emit_error!(
                GrazeDetranspilerError::InvalidBlockReference {
                    block_id: proto_block_id.clone()
                },
                context
            );
            continue;
        };
        let (proccode, descriptor) = unwrap_or_emit_message!(
            convert_procedure_prototype_for_namespace(
                proto_block,
                proto_block_id,
                &mut namespace,
                Some(&context.global_namespace.used_names)
            ),
            context,
            continue
        );
        custom_blocks.insert(proccode, descriptor);
    }
    Ok(DetranspilerTarget {
        is_stage: target.is_stage,
        internal_name,
        costumes,
        costume_indices,
        sounds,
        sound_indices,
        data,
        namespace,
        monitors: Vec::new(),
        config,
        procedures: custom_blocks,
        scripts: Vec::new(),
    })
}

/// Result is bubbled
pub fn add_monitor(
    monitor: &project_json::Sb3Monitor,
    context: &mut DetranspilerContext,
    target_index: usize,
) -> DetranspilerResult<()> {
    enum MonitorKind {
        List,
        Variable,
        Generic,
    }
    let (value, kind): (_, MonitorKind) = match monitor.opcode.as_str() {
        "data_variable" => {
            let Some(var_or_list) = try_or_emit_message!(
                lookup_var_or_list(
                    &monitor
                        .params
                        .get("VARIABLE")
                        .map(|value| value.as_cow_str())
                        .unwrap_or_default(),
                    &monitor.id,
                    target_index,
                    context,
                ),
                context,
                Ok(())
            ) else {
                emit_error!(
                    GrazeDetranspilerError::UnknownVariable {
                        id: monitor.id.clone(),
                        name: monitor
                            .params
                            .get("VARIABLE")
                            .map(ToString::to_string)
                            .unwrap_or_default()
                    },
                    context
                );
                return Ok(());
            };
            (
                ast_types::MonitorValue::Identifier(create_simple_identifier(
                    var_or_list.name.clone(),
                )),
                MonitorKind::Variable,
            )
        }
        "data_listcontents" => {
            let Some(var_or_list) = try_or_emit_message!(
                lookup_var_or_list(
                    &monitor
                        .params
                        .get("LIST")
                        .map(|value| value.as_cow_str())
                        .unwrap_or_default(),
                    &monitor.id,
                    target_index,
                    context,
                ),
                context,
                Ok(())
            ) else {
                emit_error!(
                    GrazeDetranspilerError::UnknownList {
                        id: monitor.id.clone(),
                        name: monitor
                            .params
                            .get("VARIABLE")
                            .map(ToString::to_string)
                            .unwrap_or_default()
                    },
                    context
                );
                return Ok(());
            };
            (
                ast_types::MonitorValue::Identifier(create_simple_identifier(
                    var_or_list.name.clone(),
                )),
                MonitorKind::List,
            )
        }
        _ => {
            let block_kind_info = try_or_emit_message!(
                get_block_kind_info(&monitor.opcode, |value| monitor.params.get(value)),
                context,
                Ok(())
            );
            if block_kind_info.arguments.is_empty() && block_kind_info.is_singleton {
                (
                    ast_types::MonitorValue::Identifier(create_simple_identifier(
                        block_kind_info.block_name,
                    )),
                    MonitorKind::Generic,
                )
            } else {
                let mut arguments = Vec::new();
                for Argument {
                    name: argument_name,
                    kind,
                    ignore,
                } in &block_kind_info.arguments
                {
                    if matches!(
                        kind,
                        ArgumentKind::Input
                            | ArgumentKind::MenuInput { .. }
                            | ArgumentKind::StackInput
                            | ArgumentKind::BroadcastField
                            | ArgumentKind::BackdropField
                    ) {
                        emit_error!(
                            GrazeDetranspilerError::InvalidMonitorOpcode {
                                opcode: monitor.opcode.clone()
                            },
                            context
                        );
                        continue;
                    }
                    if *ignore {
                        continue;
                    }
                    arguments.push(match kind {
                        ArgumentKind::Field => {
                            let Some(field_value) = monitor.params.get(argument_name.as_str())
                            else {
                                emit_error!(
                                    GrazeDetranspilerError::MissingFieldInMonitor {
                                        field: argument_name.to_string(),
                                        monitor_id: monitor.id.clone(),
                                    },
                                    context
                                );
                                continue;
                            };
                            if let Some(value) =
                                convert_field_value_info_for_monitor(get_normal_field_value_info(
                                    &field_value.as_cow_str(),
                                    &monitor.opcode,
                                ))
                            {
                                value
                            } else {
                                emit_error!(
                                    GrazeDetranspilerError::UnknownFieldValueInMonitor {
                                        name: argument_name.to_string(),
                                        value: field_value.to_string(),
                                        monitor_id: monitor.id.clone(),
                                    },
                                    context
                                );
                                continue;
                            }
                        }
                        ArgumentKind::VariableOrListField => {
                            let Some(name) = monitor.params.get(argument_name.as_str()) else {
                                emit_error!(
                                    GrazeDetranspilerError::MissingFieldInMonitor {
                                        field: argument_name.to_string(),
                                        monitor_id: monitor.id.clone(),
                                    },
                                    context
                                );
                                continue;
                            };
                            let Some(var_or_list) =
                                find_var_or_list_by_name(&name.as_cow_str(), target_index, context)
                            else {
                                emit_error!(
                                    GrazeDetranspilerError::UnknownVLBName {
                                        name: name.to_string(),
                                    },
                                    context
                                );
                                continue;
                            };
                            create_simple_identifier(var_or_list.name.clone())
                        }
                        ArgumentKind::BroadcastField
                        | ArgumentKind::BackdropField
                        | ArgumentKind::Input
                        | ArgumentKind::StackInput
                        | ArgumentKind::MenuInput { .. } => unreachable!(),
                    });
                }
                (
                    ast_types::MonitorValue::Call {
                        function: create_simple_identifier(block_kind_info.block_name),
                        arguments,
                    },
                    MonitorKind::Generic,
                )
            }
        }
    };
    let internal_value = context
        .settings
        .preserve_internal_monitor_value
        .then_some(&monitor.value);
    let id = if matches!(kind, MonitorKind::Generic) {
        context
            .settings
            .preserve_monitor_ids
            .then(|| monitor.id.as_str().into())
    } else {
        None
    };
    let mode = if matches!(kind, MonitorKind::List) {
        (monitor.mode != project_json::Sb3MonitorMode::List).then_some(monitor.mode)
    } else {
        (monitor.mode != project_json::Sb3MonitorMode::Default).then_some(monitor.mode)
    };
    let width = monitor.width;
    let height = monitor.height;
    let x = monitor.x;
    let y = monitor.y;
    let visible = monitor.visible;
    let slider_min = if matches!(kind, MonitorKind::List) {
        monitor.slider_min.map(Some)
    } else {
        monitor
            .slider_min
            .is_none_or(|value| value != 0.0)
            .then_some(monitor.slider_min)
    };
    let slider_max = if matches!(kind, MonitorKind::List) {
        monitor.slider_max.map(Some)
    } else {
        monitor
            .slider_max
            .is_none_or(|value| value != 0.0)
            .then_some(monitor.slider_max)
    };
    let is_discrete = if matches!(kind, MonitorKind::List) {
        monitor.is_discrete.map(Some)
    } else {
        monitor
            .is_discrete
            .is_none_or(|value| !value)
            .then_some(monitor.is_discrete)
    };
    let mut config = Vec::with_capacity(
        2 + id.is_some() as usize
            + mode.is_some() as usize
            + x.is_some() as usize
            + y.is_some() as usize
            + !visible as usize
            + slider_min.is_some() as usize
            + slider_max.is_some() as usize
            + is_discrete.is_some() as usize
            + internal_value.is_some() as usize,
    );
    config.push(ast_types::DictionaryEntry {
        identifier: ast_types::SingleIdentifier::new(literal!("width")),
        value: ast_types::DictionaryValue::Primitive(ast_types::Literal::DecimalFloat(
            format_istring!("{width}"),
        )),
    });
    config.push(ast_types::DictionaryEntry {
        identifier: ast_types::SingleIdentifier::new(literal!("height")),
        value: ast_types::DictionaryValue::Primitive(ast_types::Literal::DecimalFloat(
            format_istring!("{height}"),
        )),
    });
    if let Some(id) = id {
        config.push(ast_types::DictionaryEntry {
            identifier: ast_types::SingleIdentifier::new(literal!("id")),
            value: ast_types::DictionaryValue::Primitive(ast_types::Literal::String(id)),
        });
    }
    if let Some(mode) = mode {
        config.push(ast_types::DictionaryEntry {
            identifier: ast_types::SingleIdentifier::new(literal!("mode")),
            value: ast_types::DictionaryValue::Primitive(ast_types::Literal::String(
                format_istring!("{mode}"),
            )),
        });
    }
    if let Some(x) = x {
        config.push(ast_types::DictionaryEntry {
            identifier: ast_types::SingleIdentifier::new(literal!("x")),
            value: ast_types::DictionaryValue::Primitive(ast_types::Literal::DecimalFloat(
                format_istring!("{x}"),
            )),
        });
    }
    if let Some(y) = y {
        config.push(ast_types::DictionaryEntry {
            identifier: ast_types::SingleIdentifier::new(literal!("y")),
            value: ast_types::DictionaryValue::Primitive(ast_types::Literal::DecimalFloat(
                format_istring!("{y}"),
            )),
        });
    }
    if !visible {
        config.push(ast_types::DictionaryEntry {
            identifier: ast_types::SingleIdentifier::new(literal!("visible")),
            value: ast_types::DictionaryValue::Primitive(ast_types::Literal::Bool(false)),
        });
    }
    if let Some(slider_min) = slider_min {
        config.push(ast_types::DictionaryEntry {
            identifier: ast_types::SingleIdentifier::new(literal!("slider_min")),
            value: ast_types::DictionaryValue::Primitive(if let Some(slider_min) = slider_min {
                ast_types::Literal::DecimalFloat(format_istring!("{slider_min}"))
            } else {
                ast_types::Literal::EmptyExpression
            }),
        });
    }
    if let Some(slider_max) = slider_max {
        config.push(ast_types::DictionaryEntry {
            identifier: ast_types::SingleIdentifier::new(literal!("slider_max")),
            value: ast_types::DictionaryValue::Primitive(if let Some(slider_max) = slider_max {
                ast_types::Literal::DecimalFloat(format_istring!("{slider_max}"))
            } else {
                ast_types::Literal::EmptyExpression
            }),
        });
    }
    if let Some(is_discrete) = is_discrete {
        config.push(ast_types::DictionaryEntry {
            identifier: ast_types::SingleIdentifier::new(literal!("is_discrete")),
            value: ast_types::DictionaryValue::Primitive(if let Some(is_discrete) = is_discrete {
                ast_types::Literal::Bool(is_discrete)
            } else {
                ast_types::Literal::EmptyExpression
            }),
        });
    }
    if let Some(value) = internal_value {
        config.push(ast_types::DictionaryEntry {
            identifier: ast_types::SingleIdentifier::new(literal!("value")),
            value: match value {
                project_json::Sb3MonitorValue::List(values) => ast_types::DictionaryValue::List(
                    values
                        .iter()
                        .map(|value| ast_types::DictionaryValue::Primitive(value.into()))
                        .collect(),
                ),
                project_json::Sb3MonitorValue::Primitive(value) => {
                    ast_types::DictionaryValue::Primitive(value.into())
                }
            },
        });
    }
    context
        .targets
        .get_mut(target_index)
        .unwrap()
        .monitors
        .push(DetranspilerMonitor { value, config });
    Ok(())
}

/// Result is bubbled
pub fn fill_target(
    target: &project_json::Sb3Target,
    context: &mut DetranspilerContext,
    target_index: usize,
) -> DetranspilerResult<()> {
    for (block_id, block) in &target.blocks {
        let project_json::Sb3Block::Normal(normal_block) = block else {
            match block {
                project_json::Sb3Block::Primitive(project_json::Sb3PrimitiveBlock::Variable {
                    x: Some(_),
                    y: Some(_),
                    ..
                })
                | project_json::Sb3Block::Primitive(project_json::Sb3PrimitiveBlock::List {
                    x: Some(_),
                    y: Some(_),
                    ..
                }) => {
                    let block_stack = DetranspilerTargetBlockStack::IsolatedExpression {
                        expression: convert_reporter_block(
                            block,
                            block_id,
                            &target.blocks,
                            context,
                            target_index,
                        )?,
                    };
                    context
                        .targets
                        .get_mut(target_index)
                        .unwrap()
                        .scripts
                        .push((block_id.clone(), block_stack));
                }
                _ => (),
            }
            continue;
        };
        if !normal_block.top_level {
            continue;
        }
        let block_stack = match get_block_shape(&normal_block.opcode) {
            BlockShape::Hat => {
                let (hat_function, arguments) = unwrap_or_emit_message!(
                    convert_hat_block(
                        normal_block,
                        block_id,
                        &target.blocks,
                        context,
                        target_index
                    ),
                    context,
                    {
                        let block_stack = if let Some(next_block_id) = &normal_block.next
                            && let Some(next_block) = unwrap_or_emit_message!(
                                resolve_block_ref(next_block_id, &target.blocks).map(Some),
                                context,
                                None
                            ) {
                            convert_block_stack(
                                next_block,
                                next_block_id,
                                &target.blocks,
                                context,
                                target_index,
                            )?
                        } else {
                            ast_types::CodeBlock::default()
                        };
                        context
                            .targets
                            .get_mut(target_index)
                            .unwrap()
                            .scripts
                            .push((
                                block_id.clone(),
                                DetranspilerTargetBlockStack::IsolatedStack {
                                    code_block: block_stack,
                                },
                            ));
                        continue;
                    }
                );
                DetranspilerTargetBlockStack::HatBlock {
                    hat_function,
                    arguments,
                    code_block: {
                        if let Some(next_block_id) = &normal_block.next
                            && let Some(next_block) = unwrap_or_emit_message!(
                                resolve_block_ref(next_block_id, &target.blocks).map(Some),
                                context,
                                None
                            )
                        {
                            convert_block_stack(
                                next_block,
                                next_block_id,
                                &target.blocks,
                                context,
                                target_index,
                            )?
                        } else {
                            ast_types::CodeBlock::default()
                        }
                    },
                }
            }
            BlockShape::ProcedureDefinition => {
                let project_json::Sb3Block::Normal(block) = block else {
                    continue;
                };
                let Some(proto_block_id) = block.inputs.get("custom_block") else {
                    continue;
                };
                let (project_json::Sb3InputValue::NoShadow(project_json::Sb3InputRepr::Reference(
                    proto_block_id,
                ))
                | project_json::Sb3InputValue::ObscuredShadow {
                    value: project_json::Sb3InputRepr::Reference(proto_block_id),
                    shadow: _,
                }
                | project_json::Sb3InputValue::Shadow(project_json::Sb3InputRepr::Reference(
                    proto_block_id,
                ))) = proto_block_id
                else {
                    continue;
                };
                let Some(project_json::Sb3Block::Normal(proto_block)) =
                    target.blocks.get(proto_block_id)
                else {
                    continue;
                };
                let Some(mutation) = &proto_block.mutation else {
                    return Err(GrazeDetranspilerError::MissingMutation {
                        block_id: proto_block_id.to_string(),
                    });
                };
                let project_json::Sb3BlockMutation::ProceduresPrototype {
                    procedure_code: proccode,
                    argument_ids,
                    warp,
                    argument_names,
                    argument_defaults,
                } = mutation
                else {
                    return Err(GrazeDetranspilerError::IncorrectMutationType {
                        block_id: proto_block_id.to_string(),
                    });
                };
                let Some(procedure) = context
                    .targets
                    .get(target_index)
                    .unwrap()
                    .procedures
                    .get(proccode.as_str())
                else {
                    continue;
                };
                unwrap_or_emit_message!(
                    convert_procedure_definition(
                        normal_block,
                        ProcedureInfo {
                            prototype_block: proto_block,
                            prototype_block_id: proto_block_id,
                            proccode,
                            argument_ids,
                            warp: *warp,
                            argument_names,
                            argument_defaults,
                            canonical_name: procedure.canonical_name.clone(),
                            name: procedure.name.clone(),
                        },
                        &target.blocks,
                        context,
                        target_index
                    ),
                    context,
                    continue
                )
            }
            BlockShape::Stack => DetranspilerTargetBlockStack::IsolatedStack {
                code_block: convert_block_stack(
                    block,
                    block_id,
                    &target.blocks,
                    context,
                    target_index,
                )?,
            },
            BlockShape::Reporter => DetranspilerTargetBlockStack::IsolatedExpression {
                expression: convert_reporter_block(
                    block,
                    block_id,
                    &target.blocks,
                    context,
                    target_index,
                )?,
            },
        };
        context
            .targets
            .get_mut(target_index)
            .unwrap()
            .scripts
            .push((block_id.clone(), block_stack));
    }
    context
        .targets
        .get_mut(target_index)
        .unwrap()
        .scripts
        .sort_by(|(id_a, _), (id_b, _)| id_cmp(id_a, id_b));
    Ok(())
}

pub fn id_cmp(a: &str, b: &str) -> Ordering {
    const SOUP_TABLE: [u8; 256] = [
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 62, 255, 63, 64,
        65, 255, 255, 66, 67, 68, 69, 70, 71, 72, 73, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 74,
        75, 255, 76, 255, 77, 78, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41,
        42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 79, 255, 80, 81, 82, 83, 0, 1, 2, 3, 4, 5, 6, 7, 8,
        9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 84, 85, 86, 87, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255,
    ];
    let a = a.as_bytes();
    let b = b.as_bytes();
    match a.len().cmp(&b.len()) {
        Ordering::Equal => (),
        ord => return ord,
    }
    for i in 0..a.len() {
        match SOUP_TABLE[a[i] as usize].cmp(&SOUP_TABLE[b[i] as usize]) {
            Ordering::Equal => (),
            ord => return ord,
        }
    }
    Ordering::Equal
}

/// Result is unbubbled
pub fn convert_procedure_prototype_for_namespace(
    block: &project_json::Sb3NormalBlock,
    block_id: &str,
    namespace: &mut DetranspilerTargetNamespace,
    global_namespace: Option<&HashMap<IString, IString>>,
) -> DetranspilerResult<(IString, DetranspilerCustomBlockDescriptor)> {
    let Some(mutation) = &block.mutation else {
        return Err(GrazeDetranspilerError::MissingMutation {
            block_id: block_id.to_string(),
        });
    };
    let project_json::Sb3BlockMutation::ProceduresPrototype {
        procedure_code: proccode,
        argument_ids: _,
        warp: _,
        argument_names,
        argument_defaults: _,
    } = mutation
    else {
        return Err(GrazeDetranspilerError::IncorrectMutationType {
            block_id: block_id.to_string(),
        });
    };
    let proccode = IString::from(proccode);
    if let Some(proccode_name) = check_proccode_name_eligibility(&proccode, argument_names.len())
        && DetranspilerTargetNamespace::is_case_conforming_and_uppercase(proccode_name).0
        && !namespace.used_names.contains_key(proccode_name)
    {
        let proccode_name = IString::from(proccode_name);
        let chosen_name = namespace.introduce_new_name(proccode_name.clone(), global_namespace);
        if let Some(value) = namespace.used_names.get_mut(&chosen_name) {
            *value = proccode.clone();
        }
        return Ok((
            proccode.clone(),
            DetranspilerCustomBlockDescriptor {
                canonical_name: (chosen_name != proccode_name).then_some(proccode),
                name: chosen_name,
                argument_count: argument_names.len(),
            },
        ));
    }
    let name = IString::from(get_name_from_proccode(&proccode));
    let chosen_name = namespace.introduce_new_name(name, global_namespace);
    if let Some(value) = namespace.used_names.get_mut(&chosen_name) {
        *value = proccode.clone();
    }
    Ok((
        proccode.clone(),
        DetranspilerCustomBlockDescriptor {
            canonical_name: Some(proccode),
            name: chosen_name,
            argument_count: argument_names.len(),
        },
    ))
}

pub fn check_proccode_name_eligibility(proccode: &str, arguments: usize) -> Option<&str> {
    let start_pos = proccode.len().checked_sub(arguments * 3)?;
    for i in 0..arguments {
        let index = start_pos + i * 3;
        if !matches!(&proccode.get(index..index + 3), Some(" %s" | " %b" | " %n")) {
            return None;
        }
    }
    proccode.get(..start_pos)
}

pub fn get_name_from_proccode(proccode: &str) -> String {
    let mut new_name = String::with_capacity(proccode.len());
    let (mut alphanumeric, mut uppercase) = proccode
        .chars()
        .next()
        .map(|c| (c.is_ascii_alphanumeric(), c.is_ascii_uppercase()))
        .unwrap_or((true, false));
    let mut percent = false;
    for c in proccode.chars() {
        if percent && matches!(c, 'n' | 's' | 'b') {
            percent = false;
            continue;
        }
        percent = c == '%';
        if c.is_ascii_alphanumeric() {
            if !uppercase && c.is_ascii_uppercase() {
                new_name.push('_');
            }
            new_name.push(c.to_ascii_lowercase());
            alphanumeric = true;
            uppercase = c.is_ascii_uppercase();
        } else if alphanumeric {
            uppercase = true;
            alphanumeric = false;
            new_name.push('_');
        }
    }
    if let Some(c) = new_name.pop()
        && c != '_'
    {
        new_name.push(c);
    }
    new_name
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcedureInfo<'a> {
    pub prototype_block: &'a project_json::Sb3NormalBlock,
    pub prototype_block_id: &'a str,
    pub proccode: &'a str,
    pub argument_ids: &'a [String],
    pub warp: bool,
    pub argument_names: &'a [String],
    pub argument_defaults: &'a [serde_json::Value],
    pub canonical_name: Option<IString>,
    pub name: IString,
}

/// Result is unbubbled
pub fn convert_procedure_definition(
    block: &project_json::Sb3NormalBlock,
    procedure_info: ProcedureInfo<'_>,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_index: usize,
) -> DetranspilerResult<DetranspilerTargetBlockStack> {
    let mut parameters = Vec::with_capacity(procedure_info.argument_names.len());
    let mut proccode_chars = procedure_info.proccode.chars();
    let mut argument_names = procedure_info.argument_names.iter();
    while let Some(c) = proccode_chars.next() {
        if c == '%'
            && let Some(c) = proccode_chars.next()
        {
            let param_kind = match c {
                's' => context
                    .settings
                    .explicitly_typed_string_parameters
                    .then_some(ast_types::CustomBlockParamKind::String),
                'b' => Some(ast_types::CustomBlockParamKind::Boolean),
                'n' => Some(ast_types::CustomBlockParamKind::Number),
                _ => continue,
            };
            let Some(original_name) = argument_names.next() else {
                return Err(GrazeDetranspilerError::InvalidMutationValue {
                    block_id: procedure_info.prototype_block_id.to_string(),
                });
            };
            let original_name = IString::from(original_name);
            let chosen_name = context
                .targets
                .get_mut(target_index)
                .unwrap()
                .namespace
                .introduce_new_name(
                    original_name.clone(),
                    Some(&context.global_namespace.used_names),
                );
            context
                .current_procedure_parameters
                .insert(original_name.clone(), chosen_name.clone());
            parameters.push((
                param_kind,
                (chosen_name != original_name)
                    .then_some(ast_types::CanonicalIdentifier::new(original_name)),
                ast_types::SingleIdentifier::new(chosen_name),
            ));
        }
    }
    let code_block = if let Some(next_block_id) = &block.next
        && let Some(next_block) = unwrap_or_emit_message!(
            resolve_block_ref(next_block_id, blocks).map(Some),
            context,
            None
        ) {
        convert_block_stack(next_block, next_block_id, blocks, context, target_index)?
    } else {
        ast_types::CodeBlock::default()
    };
    let namespace = &mut context.targets.get_mut(target_index).unwrap().namespace;
    for (_, _, ast_types::SingleIdentifier { value: name }) in &parameters {
        namespace.used_names.remove(name);
    }
    context.current_procedure_parameters.clear();
    Ok(DetranspilerTargetBlockStack::CustomBlock {
        is_warp: ast_types::WarpSpecifier {
            is_warp: procedure_info.warp,
        },
        canonical_identifier: procedure_info
            .canonical_name
            .map(ast_types::CanonicalIdentifier::new),
        identifier: ast_types::SingleIdentifier::new(procedure_info.name),
        parameters,
        code_block,
    })
}

/// Result is unbubbled
pub fn resolve_block_ref<'a>(
    block_id: &str,
    blocks: &'a HashMap<String, project_json::Sb3Block>,
) -> DetranspilerResult<&'a project_json::Sb3Block> {
    let Some(next_block) = blocks.get(block_id) else {
        return Err(GrazeDetranspilerError::InvalidBlockReference {
            block_id: block_id.to_string(),
        });
    };
    Ok(next_block)
}

/// Result is bubbled
pub fn convert_reporter_block(
    block: &project_json::Sb3Block,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_index: usize,
) -> DetranspilerResult<ast_types::Expression> {
    Ok(try_or_emit_message!(
        match block {
            project_json::Sb3Block::Normal(sb3_normal_block) => {
                convert_normal_reporter_block(
                    sb3_normal_block,
                    block_id,
                    blocks,
                    context,
                    target_index,
                )
            }
            project_json::Sb3Block::Primitive(sb3_primitive_block) => {
                convert_primitive_reporter_block(sb3_primitive_block, context, target_index)
            }
        },
        context,
        Ok(ast_types::Expression::default())
    ))
}

#[inline]
pub fn get_primary_input_repr(
    input_value: &project_json::Sb3InputValue,
) -> &project_json::Sb3InputRepr {
    let (project_json::Sb3InputValue::Shadow(input_repr)
    | project_json::Sb3InputValue::NoShadow(input_repr)
    | project_json::Sb3InputValue::ObscuredShadow {
        value: input_repr,
        shadow: _,
    }) = input_value;
    input_repr
}

/// Result is unbubbled
pub fn convert_block<F>(
    block: &project_json::Sb3NormalBlock,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_index: usize,
    mut on_stack_input: F,
) -> DetranspilerResult<(
    BlockKindInfo,
    Vec<ast_types::Expression>,
    Vec<ast_types::CodeBlock>,
)>
where
    F: FnMut(&mut DetranspilerContext, &str) -> DetranspilerResult<()>,
{
    let block_kind_info = get_block_kind_info(&block.opcode, |value| {
        block.fields.get(value).and_then(|value| {
            if let project_json::Sb3FieldValue::Normal(value) = value {
                Some(value)
            } else {
                None
            }
        })
    })?;
    let mut tracked_args = 0_usize;
    let mut parameters = Vec::new();
    let mut stack_params = Vec::new();
    for Argument {
        name: argument_name,
        kind,
        ignore,
    } in &block_kind_info.arguments
    {
        if if kind.is_field() {
            block.fields.contains_key(argument_name.as_str())
        } else {
            block.inputs.contains_key(argument_name.as_str())
        } {
            tracked_args += 1;
        }
        if *ignore {
            continue;
        }
        match kind {
            ArgumentKind::Field => {
                let Some(field_value) = block.fields.get(argument_name.as_str()) else {
                    emit_error!(
                        GrazeDetranspilerError::MissingField {
                            field: argument_name.to_string(),
                            block_id: block_id.to_string(),
                        },
                        context
                    );
                    parameters.push(ast_types::Expression::default());
                    continue;
                };
                parameters.push(unwrap_or_emit_message!(
                    convert_field_value_info(
                        get_field_value_info(field_value, &block.opcode),
                        field_value,
                        target_index,
                        context,
                    ),
                    context,
                    {
                        parameters.push(ast_types::Expression::default());
                        continue;
                    }
                ));
            }
            ArgumentKind::VariableOrListField => {
                let Some(field_value) = block.fields.get(argument_name.as_str()) else {
                    emit_error!(
                        GrazeDetranspilerError::MissingField {
                            field: argument_name.to_string(),
                            block_id: block_id.to_string(),
                        },
                        context
                    );
                    parameters.push(ast_types::Expression::default());
                    continue;
                };
                let project_json::Sb3FieldValue::WithId { value: name, id } = field_value else {
                    parameters.push(ast_types::Expression::default());
                    continue;
                };
                parameters.push(
                    if let Some(value) = unwrap_or_emit_message!(
                        lookup_var_or_list(&name.as_cow_str(), id, target_index, context),
                        context,
                        {
                            parameters.push(ast_types::Expression::default());
                            continue;
                        }
                    ) {
                        ast_types::Expression::Identifier(create_simple_identifier(
                            value.name.clone(),
                        ))
                    } else {
                        emit_error!(
                            GrazeDetranspilerError::UnknownVarOrList {
                                id: id.clone(),
                                name: name.to_string()
                            },
                            context
                        );
                        ast_types::Expression::default()
                    },
                );
            }
            ArgumentKind::BroadcastField => {
                let Some(field_value) = block.fields.get(argument_name.as_str()) else {
                    emit_error!(
                        GrazeDetranspilerError::MissingField {
                            field: argument_name.to_string(),
                            block_id: block_id.to_string(),
                        },
                        context
                    );
                    parameters.push(ast_types::Expression::default());
                    continue;
                };
                let project_json::Sb3FieldValue::WithId { value: name, id } = field_value else {
                    parameters.push(ast_types::Expression::default());
                    continue;
                };
                parameters.push(
                    if let Some(value) = unwrap_or_emit_message!(
                        lookup_broadcast(&name.as_cow_str(), id, context),
                        context,
                        {
                            parameters.push(ast_types::Expression::default());
                            continue;
                        }
                    ) {
                        ast_types::Expression::Identifier(create_simple_identifier(
                            value.name.clone(),
                        ))
                    } else {
                        emit_error!(
                            GrazeDetranspilerError::UnknownVarOrList {
                                id: id.clone(),
                                name: name.to_string()
                            },
                            context
                        );
                        ast_types::Expression::default()
                    },
                );
            }
            ArgumentKind::BackdropField => {
                let Some(field_value) = block.fields.get(argument_name.as_str()) else {
                    emit_error!(
                        GrazeDetranspilerError::MissingField {
                            field: argument_name.to_string(),
                            block_id: block_id.to_string(),
                        },
                        context
                    );
                    parameters.push(ast_types::Expression::default());
                    continue;
                };
                let project_json::Sb3FieldValue::Normal(name) = field_value else {
                    parameters.push(ast_types::Expression::default());
                    continue;
                };
                let target = context.targets.get(context.stage_target_index).unwrap();
                parameters.push(
                    target
                        .costume_indices
                        .get(&*name.as_cow_str())
                        .and_then(|value| target.costumes.get(*value))
                        .map(|(_, value)| {
                            ast_types::Expression::Identifier(
                                if target_index != context.stage_target_index {
                                    ast_types::identifier![literal!("stage"), value.name.clone()]
                                } else {
                                    create_simple_identifier(value.name.clone())
                                },
                            )
                        })
                        .unwrap_or_else(|| {
                            emit_message(
                                context,
                                || {
                                    GrazeDetranspilerWarning::UnknownBackdrop {
                                        name: name.to_string(),
                                    }
                                    .into()
                                },
                                GrazeMessageSetting::Warnings,
                            );
                            ast_types::Expression::default()
                        }),
                );
            }
            ArgumentKind::Input => {
                let Some(input) = block.inputs.get(argument_name.as_str()) else {
                    parameters.push(ast_types::Expression::default());
                    // ArgumentKind::Input is only for possibly empty inputs
                    continue;
                };
                let input_repr = get_primary_input_repr(input);
                parameters.push(match input_repr {
                    project_json::Sb3InputRepr::Reference(block_id) => convert_reporter_block(
                        unwrap_or_emit_message!(
                            blocks.get(block_id).ok_or_else(|| {
                                GrazeDetranspilerError::InvalidBlockReference {
                                    block_id: block_id.clone(),
                                }
                            }),
                            context,
                            {
                                parameters.push(ast_types::Expression::default());
                                continue;
                            }
                        ),
                        block_id,
                        blocks,
                        context,
                        target_index,
                    )?,
                    project_json::Sb3InputRepr::PrimitiveBlock(block) => unwrap_or_emit_message!(
                        convert_primitive_reporter_block(block, context, target_index),
                        context,
                        ast_types::Expression::default()
                    ),
                    project_json::Sb3InputRepr::Missing => {
                        parameters.push(ast_types::Expression::default());
                        // ArgumentKind::Input is only for possibly empty inputs
                        continue;
                    }
                });
            }
            ArgumentKind::StackInput => {
                let Some(input) = block.inputs.get(argument_name.as_str()) else {
                    stack_params.push(ast_types::CodeBlock::default());
                    continue;
                };
                let input_repr = get_primary_input_repr(input);
                stack_params.push(match input_repr {
                    project_json::Sb3InputRepr::Reference(block_id) => convert_block_stack(
                        unwrap_or_emit_message!(
                            blocks.get(block_id).ok_or_else(|| {
                                GrazeDetranspilerError::InvalidBlockReference {
                                    block_id: block_id.clone(),
                                }
                            }),
                            context,
                            {
                                stack_params.push(ast_types::CodeBlock::default());
                                continue;
                            }
                        ),
                        block_id,
                        blocks,
                        context,
                        target_index,
                    )?,
                    project_json::Sb3InputRepr::PrimitiveBlock(_) => {
                        emit_error!(
                            GrazeDetranspilerError::PrimitiveBlockAsSubstack {
                                block_id: block_id.to_string(),
                                input_name: argument_name.to_string()
                            },
                            context
                        );
                        ast_types::CodeBlock::default()
                    }
                    project_json::Sb3InputRepr::Missing => {
                        stack_params.push(ast_types::CodeBlock::default());
                        continue;
                    }
                });
                on_stack_input(context, argument_name.as_str())?;
            }
            ArgumentKind::MenuInput {
                menu_opcode,
                menu_field,
                is_primitive,
                dynamic_menu_input_kind,
            } => {
                let Some(input) = block.inputs.get(argument_name.as_str()) else {
                    parameters.push(ast_types::Expression::default());
                    emit_error!(
                        GrazeDetranspilerError::MissingInput {
                            input: argument_name.to_string(),
                            block_id: block_id.to_string(),
                        },
                        context
                    );
                    continue;
                };
                let input_repr = get_primary_input_repr(input);
                parameters.push(match input_repr {
                    project_json::Sb3InputRepr::Reference(block_id) => {
                        let inner_block = unwrap_or_emit_message!(
                            blocks.get(block_id).ok_or_else(|| {
                                GrazeDetranspilerError::InvalidBlockReference {
                                    block_id: block_id.clone(),
                                }
                            }),
                            context,
                            {
                                parameters.push(ast_types::Expression::default());
                                continue;
                            }
                        );
                        if let project_json::Sb3Block::Normal(inner_block) = inner_block
                            && inner_block.opcode.as_str() == menu_opcode.as_str()
                        {
                            for key in inner_block.fields.keys() {
                                if key.as_str() == menu_field.as_str() {
                                    continue;
                                }
                                let arg = key.clone();
                                emit_message(
                                    context,
                                    || {
                                        GrazeDetranspilerWarning::UnusedField {
                                            field: arg,
                                            block_id: block_id.to_string(),
                                        }
                                        .into()
                                    },
                                    GrazeMessageSetting::Warnings,
                                );
                            }
                            for key in inner_block.inputs.keys() {
                                let arg = key.clone();
                                emit_message(
                                    context,
                                    || {
                                        GrazeDetranspilerWarning::UnusedInput {
                                            input: arg,
                                            block_id: block_id.to_string(),
                                        }
                                        .into()
                                    },
                                    GrazeMessageSetting::Warnings,
                                );
                            }
                            if let Some(field_value) = inner_block.fields.get(menu_field.as_str()) {
                                if menu_opcode.as_str() == "event_broadcast_menu"
                                    && let project_json::Sb3FieldValue::WithId { value: name, id } =
                                        field_value
                                    && let Ok(Some(value)) =
                                        lookup_broadcast(&name.as_cow_str(), id, context)
                                {
                                    ast_types::Expression::Identifier(create_simple_identifier(
                                        value.name.clone(),
                                    ))
                                } else if let project_json::Sb3FieldValue::Normal(value) =
                                    field_value
                                    && *is_primitive
                                {
                                    ast_types::Expression::Literal(value.into())
                                } else if let project_json::Sb3FieldValue::Normal(field_value) =
                                    field_value
                                    && let Some(dynamic_menu_input_kind) = dynamic_menu_input_kind
                                    && let Some(expression) = convert_dynamic_menu_input(
                                        &field_value.as_cow_str(),
                                        context,
                                        target_index,
                                        *dynamic_menu_input_kind,
                                    )
                                {
                                    expression
                                } else {
                                    unwrap_or_emit_message!(
                                        convert_field_value_info(
                                            get_field_value_info(field_value, menu_opcode),
                                            field_value,
                                            target_index,
                                            context
                                        ),
                                        context,
                                        {
                                            parameters.push(ast_types::Expression::default());
                                            continue;
                                        }
                                    )
                                }
                            } else {
                                parameters.push(ast_types::Expression::default());
                                emit_error!(
                                    GrazeDetranspilerError::MissingMenuField {
                                        field: menu_field.to_string(),
                                        block_id: block_id.to_string(),
                                    },
                                    context
                                );
                                continue;
                            }
                        } else {
                            convert_reporter_block(
                                inner_block,
                                block_id,
                                blocks,
                                context,
                                target_index,
                            )?
                        }
                    }
                    project_json::Sb3InputRepr::PrimitiveBlock(block) => unwrap_or_emit_message!(
                        convert_primitive_reporter_block(block, context, target_index),
                        context,
                        ast_types::Expression::default()
                    ),
                    project_json::Sb3InputRepr::Missing => {
                        parameters.push(ast_types::Expression::default());
                        emit_error!(
                            GrazeDetranspilerError::MissingInput {
                                input: argument_name.to_string(),
                                block_id: block_id.to_string(),
                            },
                            context
                        );
                        continue;
                    }
                });
            }
        }
    }
    if tracked_args != block.fields.len() + block.inputs.len() {
        type IsField = bool;
        let mut tracked_args =
            HashMap::<String, IsField>::with_capacity(block.fields.len() + block.inputs.len());
        for key in block.fields.keys() {
            tracked_args.insert(key.clone(), true);
        }
        for key in block.inputs.keys() {
            tracked_args.insert(key.clone(), false);
        }
        for Argument {
            name,
            kind: _,
            ignore: _,
        } in &block_kind_info.arguments
        {
            tracked_args.remove(name.as_str());
        }
        for (arg, is_field) in tracked_args {
            emit_message(
                context,
                || {
                    if is_field {
                        GrazeDetranspilerWarning::UnusedField {
                            field: arg,
                            block_id: block_id.to_string(),
                        }
                    } else {
                        GrazeDetranspilerWarning::UnusedInput {
                            input: arg,
                            block_id: block_id.to_string(),
                        }
                    }
                    .into()
                },
                GrazeMessageSetting::Warnings,
            );
        }
    }
    Ok((block_kind_info, parameters, stack_params))
}

pub fn convert_dynamic_menu_input(
    field_value: &str,
    context: &mut DetranspilerContext,
    target_index: usize,
    dynamic_menu_input_kind: DynamicMenuInputKind,
) -> Option<ast_types::Expression> {
    fn convert_costume_dynamic_input_menu(
        field_value: &str,
        context: &DetranspilerContext,
        target_index: usize,
    ) -> Option<IString> {
        let target = context.targets.get(target_index).unwrap();
        let costume_index = *target.costume_indices.get(field_value)?;
        let costume_info = &target.costumes.get(costume_index)?.1;
        Some(costume_info.name.clone())
    }
    Some(match dynamic_menu_input_kind {
        DynamicMenuInputKind::Costume => {
            ast_types::Expression::Identifier(create_simple_identifier(
                convert_costume_dynamic_input_menu(field_value, context, target_index)?,
            ))
        }
        DynamicMenuInputKind::Backdrop => {
            let name = convert_costume_dynamic_input_menu(
                field_value,
                context,
                context.stage_target_index,
            )?;
            ast_types::Expression::Identifier(if target_index != context.stage_target_index {
                ast_types::identifier![literal!("stage"), name]
            } else {
                create_simple_identifier(name)
            })
        }
        DynamicMenuInputKind::Sound => {
            let target = context.targets.get(target_index).unwrap();
            let sound_index = *target.sound_indices.get(field_value)?;
            let sound_info = &target.sounds.get(sound_index)?.1;
            ast_types::Expression::Identifier(create_simple_identifier(sound_info.name.clone()))
        }
        DynamicMenuInputKind::Target => {
            let target = *context.target_indices.get(field_value)?;
            let target_info = context.targets.get(target)?;
            ast_types::Expression::Identifier(create_simple_identifier(
                target_info.internal_name.clone(),
            ))
        }
    })
}

/// Result is unbubbled
pub fn convert_normal_reporter_block(
    block: &project_json::Sb3NormalBlock,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_index: usize,
) -> DetranspilerResult<ast_types::Expression> {
    if let Some(reporter) = check_special_reporter(block, blocks) {
        return convert_special_reporter_block(
            reporter,
            block,
            block_id,
            blocks,
            context,
            target_index,
        );
    }
    let (block_kind_info, parameters, _) = convert_block(
        block,
        block_id,
        blocks,
        context,
        target_index,
        |context, input_name| {
            emit_error!(
                GrazeDetranspilerError::SubstackInReporter {
                    block_id: block_id.to_string(),
                    input_name: input_name.to_string(),
                },
                context
            );
            Ok(())
        },
    )?;
    let function = create_simple_identifier(block_kind_info.block_name.clone());
    if block_kind_info.is_singleton && parameters.is_empty() {
        return Ok(ast_types::Expression::Identifier(function));
    }
    Ok(ast_types::Expression::Call {
        function,
        arguments: parameters,
    })
}

/// Result is unbubbled
pub fn convert_field_value_info(
    field_value_info: Option<FieldValueInfo>,
    field_value: &project_json::Sb3FieldValue,
    target_index: usize,
    context: &mut DetranspilerContext,
) -> DetranspilerResult<ast_types::Expression> {
    Ok(match field_value_info {
        Some(value) => {
            ast_types::Expression::Identifier(create_simple_identifier(value.field_value_name))
        }
        None => match field_value {
            project_json::Sb3FieldValue::Normal(value) => {
                ast_types::Expression::Literal(value.into())
            }
            project_json::Sb3FieldValue::WithId { value, id } => {
                ast_types::Expression::Identifier({
                    let vlb = lookup_vlb(&value.as_cow_str(), id, target_index, context)?
                        .ok_or_else(|| GrazeDetranspilerError::UnknownVariable {
                            id: id.clone(),
                            name: value.to_string(),
                        })?;
                    create_vlb_identifier(vlb.into())
                })
            }
        },
    })
}

pub fn convert_field_value_info_for_monitor(
    field_value_info: Option<FieldValueInfo>,
) -> Option<ast_types::Identifier> {
    field_value_info.map(|value| create_simple_identifier(value.field_value_name))
}

#[inline]
pub fn create_simple_identifier(name: IString) -> ast_types::Identifier {
    ast_types::identifier![name]
}

pub fn create_vlb_identifier(broadcast: InternalVLBIdentifier) -> ast_types::Identifier {
    match broadcast {
        InternalVLBIdentifier::Broadcast(value) | InternalVLBIdentifier::VarOrList(value) => {
            create_simple_identifier(value)
        }
    }
}

/// Result is unbubbled
pub fn convert_primitive_reporter_block(
    block: &project_json::Sb3PrimitiveBlock,
    context: &mut DetranspilerContext,
    target_index: usize,
) -> DetranspilerResult<ast_types::Expression> {
    match block {
        project_json::Sb3PrimitiveBlock::Number(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::PositiveNumber(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::PositiveInteger(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::Integer(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::Angle(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::Color(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::String(sb3_primitive) => {
            Ok(ast_types::Expression::Literal(sb3_primitive.into()))
        }
        project_json::Sb3PrimitiveBlock::Broadcast { name, id } => {
            let broadcast = lookup_broadcast(name, id, context)?.ok_or_else(|| {
                GrazeDetranspilerError::UnknownBroadcast {
                    id: id.clone(),
                    name: name.clone(),
                }
            })?;
            Ok(ast_types::Expression::Identifier(create_simple_identifier(
                broadcast.name.clone(),
            )))
        }
        project_json::Sb3PrimitiveBlock::Variable {
            name,
            id,
            x: _,
            y: _,
        } => {
            let variable =
                lookup_var_or_list(name, id, target_index, context)?.ok_or_else(|| {
                    GrazeDetranspilerError::UnknownVariable {
                        id: id.clone(),
                        name: name.clone(),
                    }
                })?;
            Ok(ast_types::Expression::Identifier(create_simple_identifier(
                variable.name.clone(),
            )))
        }
        project_json::Sb3PrimitiveBlock::List {
            name,
            id,
            x: _,
            y: _,
        } => {
            let list = lookup_var_or_list(name, id, target_index, context)?.ok_or_else(|| {
                GrazeDetranspilerError::UnknownVariable {
                    id: id.clone(),
                    name: name.clone(),
                }
            })?;
            Ok(ast_types::Expression::Identifier(create_simple_identifier(
                list.name.clone(),
            )))
        }
    }
}

/// Result is unbubbled
pub fn convert_hat_block(
    block: &project_json::Sb3NormalBlock,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_index: usize,
) -> DetranspilerResult<(ast_types::Identifier, Vec<ast_types::Expression>)> {
    let (block_kind_info, parameters, _) = convert_block(
        block,
        block_id,
        blocks,
        context,
        target_index,
        |context, input_name| {
            emit_error!(
                GrazeDetranspilerError::SubstackInHatBlock {
                    block_id: block_id.to_string(),
                    input_name: input_name.to_string(),
                },
                context
            );
            Ok(())
        },
    )?;
    let function = create_simple_identifier(block_kind_info.block_name.clone());
    Ok((function, parameters))
}

// IString allows for more efficient cycle detection
pub(super) type NextBlockId = IString;

/// Result is bubbled
pub fn convert_block_stack(
    block: &project_json::Sb3Block,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_index: usize,
) -> DetranspilerResult<ast_types::CodeBlock> {
    // TODO: Implement cycle detection in block conversions
    // Issue: #109
    let mut statements = Vec::new();
    let mut current_block = block;
    let mut current_block_id = IString::from(block_id);
    loop {
        let (statement, next_block_id) = unwrap_or_emit_message!(
            convert_stack_block(
                current_block,
                &current_block_id,
                blocks,
                context,
                target_index
            ),
            context,
            break
        );
        statements.push(statement);
        let Some(next_block_id) = next_block_id else {
            break;
        };
        let next_block =
            unwrap_or_emit_message!(resolve_block_ref(&next_block_id, blocks), context, break);
        current_block = next_block;
        current_block_id = next_block_id;
    }
    Ok(ast_types::CodeBlock { statements })
}

/// Result is unbubbled
pub fn convert_stack_block(
    block: &project_json::Sb3Block,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_index: usize,
) -> DetranspilerResult<(ast_types::Statement, Option<NextBlockId>)> {
    let project_json::Sb3Block::Normal(block) = block else {
        return Err(GrazeDetranspilerError::PrimitiveBlockAsStackBlock {
            block_id: block_id.to_string(),
        });
    };
    if let Some(stack_block) = check_special_stack_block(block) {
        return convert_special_stack_block(
            stack_block,
            block,
            block_id,
            blocks,
            context,
            target_index,
        );
    }
    let mut has_substack = false;
    let (block_kind_info, parameters, stack_params) = convert_block(
        block,
        block_id,
        blocks,
        context,
        target_index,
        |context, input_name| {
            if has_substack {
                emit_error!(
                    GrazeDetranspilerError::SubstackInReporter {
                        block_id: block_id.to_string(),
                        input_name: input_name.to_string()
                    },
                    context
                );
            }
            has_substack = true;
            Ok(())
        },
    )?;
    let function = create_simple_identifier(block_kind_info.block_name.clone());
    let next_block = block.next.as_deref().map(Into::into);
    if let Some(substack) = stack_params.into_iter().next() {
        return Ok((
            ast_types::Statement::Control {
                control_function: function,
                arguments: parameters,
                code_block: substack,
            },
            next_block,
        ));
    }
    Ok((
        ast_types::Statement::Call {
            function,
            arguments: parameters,
        },
        next_block,
    ))
}
