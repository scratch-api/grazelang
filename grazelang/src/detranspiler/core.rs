use std::collections::HashMap;

use arcstr::{ArcStr as IString, format as format_istring, literal};
use grazelang_types::project_json;
use serde::{Deserialize, Serialize, de::DeserializeOwned};

use super::{
    get_info::{
        Argument, ArgumentKind, BlockKindInfo, FieldValueInfo, SpecialReporterInfo,
        check_special_reporter, get_block_kind_info, get_field_value_info,
        get_normal_field_value_info,
    },
    into_ast::{IntoAST, assets_to_asset_declaration},
};
use crate::{
    ast::types::{self as ast_types},
    detranspiler::into_ast::{data_to_data_declaration, data_to_split_data_declaration},
    library::{BlockShape, get_block_shape},
    messages::types::{GrazeDetranspilerError, GrazeDetranspilerMessage, GrazeDetranspilerWarning},
    names::{DetranspilerAssetNamespace, DetranspilerTargetNamespace},
    settings::{GrazeDetranspilerSettings, GrazeMessageSetting},
};

#[derive(Debug, Clone, PartialEq)]
pub struct DetranspilerContext {
    pub stage_target_idx: usize,
    pub targets: Vec<DetranspilerTarget>,
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
    pub costumes: Vec<(AssetId, DetranspilerAsset<DetranspilerCostumeUncommonData>)>,
    pub sounds: Vec<(AssetId, DetranspilerAsset<DetranspilerSoundUncommonData>)>,
    pub data: HashMap<DataId, DetranspilerVarOrList>,
    pub namespace: DetranspilerTargetNamespace,
    pub monitors: Vec<DetranspilerMonitor>,
    pub procedures: HashMap<IString, DetranspilerCustomBlockDescriptor>,
    pub scripts: Vec<DetranspilerTargetBlockStack>,
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
    target_idx: usize,
    context: &'a mut DetranspilerContext,
) -> DetranspilerResult<Option<BorrowedDetranspilerVLB<'a>>> {
    let result = context
        .targets
        .get(context.stage_target_idx)
        .and_then(|value| value.data.get(id))
        .or_else(|| {
            context
                .targets
                .get(target_idx)
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
    target_idx: usize,
    context: &'a mut DetranspilerContext,
) -> DetranspilerResult<Option<&'a DetranspilerVarOrList>> {
    let result = context
        .targets
        .get(context.stage_target_idx)
        .and_then(|value| value.data.get(id))
        .or_else(|| {
            context
                .targets
                .get(target_idx)
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
    target_idx: usize,
    context: &'a mut DetranspilerContext,
) -> Option<&'a DetranspilerVarOrList> {
    context
        .targets
        .get(context.stage_target_idx)
        .and_then(|value: &DetranspilerTarget| {
            value
                .data
                .iter()
                .find(|value| value.1.get_original_name().as_str() == name)
        })
        .or_else(|| {
            context.targets.get(target_idx).and_then(|value| {
                value
                    .data
                    .iter()
                    .find(|value| value.1.get_original_name().as_str() == name)
            })
        })
        .map(|value| value.1)
}

pub fn get_literal_from_sb3_primitive(value: &project_json::Sb3Primitive) -> ast_types::Literal {
    match value {
        project_json::Sb3Primitive::String(value) => ast_types::Literal::String(value.into()),
        project_json::Sb3Primitive::Int128(value) => {
            ast_types::Literal::DecimalInt(arcstr::format!("{value}"))
        }
        project_json::Sb3Primitive::Int(value) => {
            ast_types::Literal::DecimalInt(arcstr::format!("{value}"))
        }
        project_json::Sb3Primitive::Float(value) => {
            ast_types::Literal::DecimalFloat(arcstr::format!("{value}"))
        }
    }
}

pub fn convert_project(
    project: &project_json::Sb3Root,
    settings: GrazeDetranspilerSettings,
) -> Result<
    (
        ast_types::GrazeProgram,
        HashMap<AssetPath, OutAssetPath>,
        Vec<GrazeDetranspilerMessage>,
    ),
    Vec<GrazeDetranspilerMessage>,
> {
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
        stage_target_idx: 0,
        targets: Vec::new(),
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
    let mut target_names = HashMap::with_capacity(project.targets.len());
    let target_internal_names = project
        .targets
        .iter()
        .map(|value| {
            let canonical_name = IString::from(&value.name);
            let name = context
                .global_namespace
                .introduce_new_name(canonical_name.clone(), None);
            (canonical_name, name)
        })
        .collect::<Vec<_>>();
    for target in &project.targets {
        if target.is_stage {
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
        } else {
            continue;
        }
        let target = unwrap_bubbled_result_or!(
            context => convert_target(target, context),
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
    for (idx, target) in project.targets.iter().enumerate() {
        if target.is_stage
            && let Some(stage) = stage.take()
        {
            context.stage_target_idx = context.targets.len();
            context.targets.push(stage);
            continue;
        } else {
            target_names.insert(target.name.clone(), idx);
        }
        let target = unwrap_bubbled_result_or!(
            context => convert_target(target, context),
            context
        );
        context.targets.push(target);
    }
    for monitor in &project.monitors {
        let target_idx = monitor
            .sprite_name
            .as_ref()
            .and_then(|value| target_names.get(value))
            .copied()
            .unwrap_or(context.stage_target_idx);
        unwrap_bubbled_result_or!(
            context => add_monitor(monitor, context, target_idx),
            context
        );
    }
    if context.unreturned_failure {
        return Err(context.messages);
    }
    for (idx, target) in project.targets.iter().enumerate() {
        unwrap_bubbled_result_or!(
            context => fill_target(target, context, idx),
            context
        );
    }
    let mut statements = Vec::with_capacity(context.broadcasts.len() + context.targets.len());
    for broadcast in context.broadcasts.values() {
        statements.push(broadcast.into_ast());
    }
    for (target_idx, target) in context.targets.iter_mut().enumerate() {
        statements.push(if target.is_stage {
            let mut stage_statements = Vec::with_capacity(
                target.costumes.len()
                    + target.sounds.len()
                    + target.data.len()
                    + target.monitors.len()
                    + target.scripts.len(),
            );
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
                .for_each(|value| stage_statements.push(value.into_ast()));
            ast_types::TopLevelStatement::Stage {
                code_block: ast_types::StageCodeBlock {
                    statements: stage_statements,
                },
            }
        } else {
            let mut sprite_statements = Vec::with_capacity(
                target.costumes.len()
                    + target.sounds.len()
                    + target.data.len()
                    + target.monitors.len()
                    + target.scripts.len(),
            );
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
                .for_each(|value| sprite_statements.push(value.into_ast()));
            let (canonical_name, name) = target_internal_names.get(target_idx).unwrap();
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

// TODO: Implement extensions in detranspiler

// TODO: Implement `procedures_call` in detranspiler

// TODO: Implement assignments in detranspiler

// TODO: Implement list entries with `..` in detranspiler

// TODO: Implement list methods in detranspiler

// A function is unbubbled iff it tries (`?`) any unbubbled result or returns a Err at any point without checking if
// ExitOnError or ExitOnErrorUnlogged is on. A function is bubbled iff it is not unbubbled.
// A Result is unbubbled iff it results from an unbubbled function or is an Err that is created without checking if
// ExitOnError or ExitOnErrorUnlogged is on. A Result is bubbled iff it is not unbubbled.

// Bubbling is done to allow the detranspiler to catch as many errors or warnings at once as possible

/// Result is bubbled
pub fn convert_target(
    target: &project_json::Sb3Target,
    context: &mut DetranspilerContext,
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
    let costumes = target
        .costumes
        .iter()
        .map(|value| {
            (value.asset_id.clone(), {
                let (canonical_name, name) = get_canonical_name_and_name(
                    &mut namespace,
                    value.name.as_str().into(),
                    target.is_stage,
                    context,
                );
                DetranspilerAsset {
                    canonical_name,
                    name,
                    file_extension: value.data_format.clone(),
                    uncommon_data: DetranspilerCostumeUncommonData {
                        rotation_center_x: value.rotation_center_x,
                        rotation_center_y: value.rotation_center_y,
                    },
                    asset_path: get_asset_path_and_register_asset(
                        context,
                        &value.name,
                        &value.asset_id,
                        &value.data_format,
                        &value.md5ext,
                    ),
                }
            })
        })
        .collect();
    let sounds = target
        .sounds
        .iter()
        .map(|value| {
            (value.asset_id.clone(), {
                let (canonical_name, name) = get_canonical_name_and_name(
                    &mut namespace,
                    value.name.as_str().into(),
                    target.is_stage,
                    context,
                );
                DetranspilerAsset {
                    canonical_name,
                    name,
                    file_extension: value.data_format.clone(),
                    uncommon_data: DetranspilerSoundUncommonData,
                    asset_path: get_asset_path_and_register_asset(
                        context,
                        &value.name,
                        &value.asset_id,
                        &value.data_format,
                        &value.md5ext,
                    ),
                }
            })
        })
        .collect();
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
                (!target.is_stage).then_some(&context.global_namespace.used_names)
            ),
            context,
            continue
        );
        custom_blocks.insert(proccode, descriptor);
    }
    Ok(DetranspilerTarget {
        is_stage: target.is_stage,
        costumes,
        sounds,
        data,
        namespace,
        monitors: Vec::new(),
        procedures: custom_blocks,
        scripts: Vec::new(),
    })
}

/// Result is bubbled
pub fn add_monitor(
    monitor: &project_json::Sb3Monitor,
    context: &mut DetranspilerContext,
    target_idx: usize,
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
                    target_idx,
                    context,
                ),
                context,
                Ok(())
            ) else {
                emit_message(
                    context,
                    || {
                        GrazeDetranspilerWarning::UnknownVLBValueInMonitor {
                            monitor_id: monitor.id.clone(),
                        }
                        .into()
                    },
                    GrazeMessageSetting::Warnings,
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
                    target_idx,
                    context,
                ),
                context,
                Ok(())
            ) else {
                emit_message(
                    context,
                    || {
                        GrazeDetranspilerWarning::UnknownVLBValueInMonitor {
                            monitor_id: monitor.id.clone(),
                        }
                        .into()
                    },
                    GrazeMessageSetting::Warnings,
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
                                        value: field_value.to_string()
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
                                    },
                                    context
                                );
                                continue;
                            };
                            let Some(var_or_list) =
                                find_var_or_list_by_name(&name.as_cow_str(), target_idx, context)
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
                        ArgumentKind::BroadcastField => unreachable!(),
                        ArgumentKind::Input => unreachable!(),
                        ArgumentKind::StackInput => unreachable!(),
                        ArgumentKind::MenuInput { .. } => unreachable!(),
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
        .get_mut(target_idx)
        .unwrap()
        .monitors
        .push(DetranspilerMonitor { value, config });
    Ok(())
}

/// Result is bubbled
pub fn fill_target(
    target: &project_json::Sb3Target,
    context: &mut DetranspilerContext,
    target_idx: usize,
) -> DetranspilerResult<()> {
    for (block_id, block) in &target.blocks {
        let project_json::Sb3Block::Normal(normal_block) = block else {
            continue;
        };
        if !normal_block.top_level {
            continue;
        }
        let block_stack = match get_block_shape(&normal_block.opcode) {
            BlockShape::Hat => {
                let (hat_function, arguments) = unwrap_or_emit_message!(
                    convert_hat_block(normal_block, block_id, &target.blocks, context, target_idx),
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
                                target_idx,
                            )?
                        } else {
                            ast_types::CodeBlock {
                                statements: Vec::new(),
                            }
                        };
                        context.targets.get_mut(target_idx).unwrap().scripts.push(
                            DetranspilerTargetBlockStack::IsolatedStack {
                                code_block: block_stack,
                            },
                        );
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
                                target_idx,
                            )?
                        } else {
                            ast_types::CodeBlock {
                                statements: Vec::new(),
                            }
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
                    .get(target_idx)
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
                        target_idx,
                        target.is_stage
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
                    target_idx,
                )?,
            },
            BlockShape::Reporter => DetranspilerTargetBlockStack::IsolatedExpression {
                expression: convert_reporter_block(
                    block,
                    block_id,
                    &target.blocks,
                    context,
                    target_idx,
                )?,
            },
        };
        context
            .targets
            .get_mut(target_idx)
            .unwrap()
            .scripts
            .push(block_stack);
    }
    Ok(())
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
        let idx = start_pos + i * 3;
        if !matches!(&proccode.get(idx..idx + 3), Some(" %s" | " %b" | " %n")) {
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
    target_idx: usize,
    is_stage: bool,
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
                .get_mut(target_idx)
                .unwrap()
                .namespace
                .introduce_new_name(
                    original_name.clone(),
                    (!is_stage).then_some(&context.global_namespace.used_names),
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
        convert_block_stack(next_block, next_block_id, blocks, context, target_idx)?
    } else {
        ast_types::CodeBlock {
            statements: Vec::new(),
        }
    };
    let namespace = &mut context.targets.get_mut(target_idx).unwrap().namespace;
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
    target_idx: usize,
) -> DetranspilerResult<ast_types::Expression> {
    Ok(try_or_emit_message!(
        match block {
            project_json::Sb3Block::Normal(sb3_normal_block) => {
                convert_normal_reporter_block(
                    sb3_normal_block,
                    block_id,
                    blocks,
                    context,
                    target_idx,
                )
            }
            project_json::Sb3Block::Primitive(sb3_primitive_block) => {
                convert_primitive_reporter_block(sb3_primitive_block, context, target_idx)
            }
        },
        context,
        Ok(ast_types::Expression::Literal(
            ast_types::Literal::EmptyExpression,
        ))
    ))
}

/// Result is unbubbled
pub fn convert_block<F>(
    block: &project_json::Sb3NormalBlock,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_idx: usize,
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
                    parameters.push(ast_types::Expression::Literal(
                        ast_types::Literal::EmptyExpression,
                    ));
                    continue;
                };
                parameters.push(unwrap_or_emit_message!(
                    convert_field_value_info(
                        get_field_value_info(field_value, &block.opcode),
                        field_value,
                        target_idx,
                        context,
                    ),
                    context,
                    {
                        parameters.push(ast_types::Expression::Literal(
                            ast_types::Literal::EmptyExpression,
                        ));
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
                    parameters.push(ast_types::Expression::Literal(
                        ast_types::Literal::EmptyExpression,
                    ));
                    continue;
                };
                let project_json::Sb3FieldValue::WithId { value: name, id } = field_value else {
                    parameters.push(ast_types::Expression::Literal(
                        ast_types::Literal::EmptyExpression,
                    ));
                    continue;
                };
                parameters.push(
                    unwrap_or_emit_message!(
                        lookup_var_or_list(&name.as_cow_str(), id, target_idx, context,),
                        context,
                        {
                            parameters.push(ast_types::Expression::Literal(
                                ast_types::Literal::EmptyExpression,
                            ));
                            continue;
                        }
                    )
                    .map(|value| {
                        ast_types::Expression::Identifier(create_simple_identifier(
                            value.name.clone(),
                        ))
                    })
                    .unwrap_or_else(|| {
                        emit_message(
                            context,
                            || {
                                GrazeDetranspilerWarning::UnknownVLBValue {
                                    field: argument_name.to_string(),
                                    block_id: block_id.to_string(),
                                }
                                .into()
                            },
                            GrazeMessageSetting::Warnings,
                        );
                        ast_types::Expression::Literal(ast_types::Literal::EmptyExpression)
                    }),
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
                    parameters.push(ast_types::Expression::Literal(
                        ast_types::Literal::EmptyExpression,
                    ));
                    continue;
                };
                let project_json::Sb3FieldValue::WithId { value: name, id } = field_value else {
                    parameters.push(ast_types::Expression::Literal(
                        ast_types::Literal::EmptyExpression,
                    ));
                    continue;
                };
                parameters.push(
                    unwrap_or_emit_message!(
                        lookup_broadcast(&name.as_cow_str(), id, context,),
                        context,
                        {
                            parameters.push(ast_types::Expression::Literal(
                                ast_types::Literal::EmptyExpression,
                            ));
                            continue;
                        }
                    )
                    .map(|value| {
                        ast_types::Expression::Identifier(create_simple_identifier(
                            value.name.clone(),
                        ))
                    })
                    .unwrap_or_else(|| {
                        emit_message(
                            context,
                            || {
                                GrazeDetranspilerWarning::UnknownVLBValue {
                                    field: argument_name.to_string(),
                                    block_id: block_id.to_string(),
                                }
                                .into()
                            },
                            GrazeMessageSetting::Warnings,
                        );
                        ast_types::Expression::Literal(ast_types::Literal::EmptyExpression)
                    }),
                );
            }
            ArgumentKind::Input => {
                let Some(input) = block.inputs.get(argument_name.as_str()) else {
                    parameters.push(ast_types::Expression::Literal(
                        ast_types::Literal::EmptyExpression,
                    ));
                    // ArgumentKind::Input is only for possibly empty inputs
                    continue;
                };
                let (project_json::Sb3InputValue::Shadow(input_repr)
                | project_json::Sb3InputValue::NoShadow(input_repr)
                | project_json::Sb3InputValue::ObscuredShadow {
                    value: input_repr,
                    shadow: _,
                }) = input;
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
                                parameters.push(ast_types::Expression::Literal(
                                    ast_types::Literal::EmptyExpression,
                                ));
                                continue;
                            }
                        ),
                        block_id,
                        blocks,
                        context,
                        target_idx,
                    )?,
                    project_json::Sb3InputRepr::PrimitiveBlock(block) => unwrap_or_emit_message!(
                        convert_primitive_reporter_block(block, context, target_idx),
                        context,
                        ast_types::Expression::Literal(ast_types::Literal::EmptyExpression)
                    ),
                });
            }
            ArgumentKind::StackInput => {
                let Some(input) = block.inputs.get(argument_name.as_str()) else {
                    stack_params.push(ast_types::CodeBlock {
                        statements: Vec::new(),
                    });
                    continue;
                };
                let (project_json::Sb3InputValue::Shadow(input_repr)
                | project_json::Sb3InputValue::NoShadow(input_repr)
                | project_json::Sb3InputValue::ObscuredShadow {
                    value: input_repr,
                    shadow: _,
                }) = input;
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
                                stack_params.push(ast_types::CodeBlock {
                                    statements: Vec::new(),
                                });
                                continue;
                            }
                        ),
                        block_id,
                        blocks,
                        context,
                        target_idx,
                    )?,
                    project_json::Sb3InputRepr::PrimitiveBlock(_) => {
                        emit_error!(
                            GrazeDetranspilerError::PrimitiveBlockAsSubstack {
                                block_id: block_id.to_string(),
                                input_name: argument_name.to_string()
                            },
                            context
                        );
                        ast_types::CodeBlock {
                            statements: Vec::new(),
                        }
                    }
                });
                on_stack_input(context, argument_name.as_str())?;
            }
            ArgumentKind::MenuInput {
                menu_opcode,
                menu_field,
            } => {
                let Some(input) = block.inputs.get(argument_name.as_str()) else {
                    parameters.push(ast_types::Expression::Literal(
                        ast_types::Literal::EmptyExpression,
                    ));
                    emit_message(
                        context,
                        || {
                            GrazeDetranspilerWarning::UnexpectedEmptyInput {
                                input: argument_name.to_string(),
                                block_id: block_id.to_string(),
                            }
                            .into()
                        },
                        GrazeMessageSetting::Warnings,
                    );
                    continue;
                };
                let (project_json::Sb3InputValue::Shadow(input_repr)
                | project_json::Sb3InputValue::NoShadow(input_repr)
                | project_json::Sb3InputValue::ObscuredShadow {
                    value: input_repr,
                    shadow: _,
                }) = input;
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
                                parameters.push(ast_types::Expression::Literal(
                                    ast_types::Literal::EmptyExpression,
                                ));
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
                                } else {
                                    unwrap_or_emit_message!(
                                        convert_field_value_info(
                                            get_field_value_info(field_value, menu_opcode),
                                            field_value,
                                            target_idx,
                                            context,
                                        ),
                                        context,
                                        {
                                            parameters.push(ast_types::Expression::Literal(
                                                ast_types::Literal::EmptyExpression,
                                            ));
                                            continue;
                                        }
                                    )
                                }
                            } else {
                                parameters.push(ast_types::Expression::Literal(
                                    ast_types::Literal::EmptyExpression,
                                ));
                                emit_message(
                                    context,
                                    || {
                                        GrazeDetranspilerWarning::MissingMenuField {
                                            field: menu_field.to_string(),
                                            block_id: block_id.clone(),
                                        }
                                        .into()
                                    },
                                    GrazeMessageSetting::Warnings,
                                );
                                continue;
                            }
                        } else {
                            convert_reporter_block(
                                inner_block,
                                block_id,
                                blocks,
                                context,
                                target_idx,
                            )?
                        }
                    }
                    project_json::Sb3InputRepr::PrimitiveBlock(block) => unwrap_or_emit_message!(
                        convert_primitive_reporter_block(block, context, target_idx),
                        context,
                        ast_types::Expression::Literal(ast_types::Literal::EmptyExpression)
                    ),
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

/// Result is bubbled
pub fn convert_special_reporter_block(
    reporter: SpecialReporterInfo,
    block: &project_json::Sb3NormalBlock,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_idx: usize,
) -> DetranspilerResult<ast_types::Expression> {
    /// Result is bubbled
    fn convert_operand_input_value(
        operand: &project_json::Sb3InputValue,
        blocks: &HashMap<String, project_json::Sb3Block>,
        context: &mut DetranspilerContext,
        target_idx: usize,
    ) -> DetranspilerResult<ast_types::Expression> {
        let (project_json::Sb3InputValue::Shadow(input_repr)
        | project_json::Sb3InputValue::NoShadow(input_repr)
        | project_json::Sb3InputValue::ObscuredShadow {
            value: input_repr,
            shadow: _,
        }) = operand;
        Ok(match input_repr {
            project_json::Sb3InputRepr::Reference(block_id) => unwrap_or_emit_message!(
                blocks
                    .get(block_id)
                    .ok_or_else(|| {
                        GrazeDetranspilerError::InvalidBlockReference {
                            block_id: block_id.clone(),
                        }
                    })
                    .map(|block| {
                        convert_reporter_block(block, block_id, blocks, context, target_idx)
                    }),
                context,
                Ok(ast_types::Expression::Literal(
                    ast_types::Literal::EmptyExpression
                ))
            )?,
            project_json::Sb3InputRepr::PrimitiveBlock(block) => {
                unwrap_or_emit_message!(
                    convert_primitive_reporter_block(block, context, target_idx),
                    context,
                    ast_types::Expression::Literal(ast_types::Literal::EmptyExpression)
                )
            }
        })
    }
    Ok(match reporter {
        crate::detranspiler::get_info::SpecialReporterInfo::BinOp {
            binop,
            left_operand,
            right_operand,
        } => {
            let mut operands_present = 0;
            let left_operand_expression =
                if let Some(operand) = block.inputs.get(left_operand.as_str()) {
                    operands_present += 1;
                    convert_operand_input_value(operand, blocks, context, target_idx)?
                } else {
                    ast_types::Expression::Literal(ast_types::Literal::EmptyExpression)
                };
            let right_operand_expression =
                if let Some(operand) = block.inputs.get(right_operand.as_str()) {
                    operands_present += 1;
                    convert_operand_input_value(operand, blocks, context, target_idx)?
                } else {
                    ast_types::Expression::Literal(ast_types::Literal::EmptyExpression)
                };
            for key in block.fields.keys() {
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
            if block.inputs.len() != operands_present {
                for key in block.inputs.keys() {
                    if key.as_str() == left_operand.as_str()
                        || key.as_str() == right_operand.as_str()
                    {
                        continue;
                    }
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
            }
            ast_types::Expression::BinOp {
                operator: binop,
                left_operand: Box::new(left_operand_expression),
                right_operand: Box::new(right_operand_expression),
            }
        }
        crate::detranspiler::get_info::SpecialReporterInfo::NegatedBinOp {
            binop,
            outer_operand,
            inner_left_operand,
            inner_right_operand,
        } => {
            let Some(
                project_json::Sb3InputValue::Shadow(project_json::Sb3InputRepr::Reference(
                    inner_block_id,
                ))
                | project_json::Sb3InputValue::NoShadow(project_json::Sb3InputRepr::Reference(
                    inner_block_id,
                ))
                | project_json::Sb3InputValue::ObscuredShadow {
                    value: project_json::Sb3InputRepr::Reference(inner_block_id),
                    shadow: _,
                },
            ) = block.inputs.get(outer_operand.as_str())
            else {
                unreachable!()
            };
            let Some(project_json::Sb3Block::Normal(inner_block)) = blocks.get(inner_block_id)
            else {
                unreachable!()
            };
            let mut inner_operands_present = 0;
            let left_operand_expression =
                if let Some(operand) = inner_block.inputs.get(inner_left_operand.as_str()) {
                    inner_operands_present += 1;
                    convert_operand_input_value(operand, blocks, context, target_idx)?
                } else {
                    ast_types::Expression::Literal(ast_types::Literal::EmptyExpression)
                };
            let right_operand_expression =
                if let Some(operand) = inner_block.inputs.get(inner_right_operand.as_str()) {
                    inner_operands_present += 1;
                    convert_operand_input_value(operand, blocks, context, target_idx)?
                } else {
                    ast_types::Expression::Literal(ast_types::Literal::EmptyExpression)
                };
            for key in block.fields.keys() {
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
            for key in inner_block.fields.keys() {
                let arg = key.clone();
                emit_message(
                    context,
                    || {
                        GrazeDetranspilerWarning::UnusedField {
                            field: arg,
                            block_id: inner_block_id.to_string(),
                        }
                        .into()
                    },
                    GrazeMessageSetting::Warnings,
                );
            }
            if block.inputs.len() != 1 {
                for key in block.inputs.keys() {
                    if key.as_str() == outer_operand.as_str() {
                        continue;
                    }
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
            }
            if inner_block.inputs.len() != inner_operands_present {
                for key in block.inputs.keys() {
                    if key.as_str() == inner_left_operand.as_str()
                        || key.as_str() == inner_right_operand.as_str()
                    {
                        continue;
                    }
                    let arg = key.clone();
                    emit_message(
                        context,
                        || {
                            GrazeDetranspilerWarning::UnusedInput {
                                input: arg,
                                block_id: inner_block_id.to_string(),
                            }
                            .into()
                        },
                        GrazeMessageSetting::Warnings,
                    );
                }
            }
            ast_types::Expression::BinOp {
                operator: binop,
                left_operand: Box::new(left_operand_expression),
                right_operand: Box::new(right_operand_expression),
            }
        }
        crate::detranspiler::get_info::SpecialReporterInfo::UnOp {
            unop,
            operand,
            unused_operand,
            unused_field,
        } => {
            let mut operands_present = 0;
            let mut unused_field_present = 0;
            let operand_expression = if let Some(operand) = block.inputs.get(operand.as_str()) {
                convert_operand_input_value(operand, blocks, context, target_idx)?
            } else {
                ast_types::Expression::Literal(ast_types::Literal::EmptyExpression)
            };
            if let Some(unused_operand) = &unused_operand
                && block.inputs.contains_key(unused_operand.as_str())
            {
                operands_present += 1;
            }
            if let Some(unused_field) = &unused_field
                && block.fields.contains_key(unused_field.as_str())
            {
                unused_field_present = 1;
            }
            if block.fields.len() != unused_field_present {
                for key in block.fields.keys() {
                    if let Some(unused_field) = &unused_field
                        && key.as_str() == unused_field.as_str()
                    {
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
            }
            if block.inputs.len() != operands_present {
                for key in block.inputs.keys() {
                    if key.as_str() == operand.as_str() {
                        continue;
                    }
                    if let Some(unused_operand) = &unused_operand
                        && key.as_str() == unused_operand.as_str()
                    {
                        continue;
                    }
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
            }
            ast_types::Expression::UnOp {
                operator: unop,
                operand: Box::new(operand_expression),
            }
        }
        crate::detranspiler::get_info::SpecialReporterInfo::ProcedureArgument { is_bool } => {
            let project_json::Sb3FieldValue::Normal(name) = block.fields.get("VALUE").unwrap()
            else {
                unreachable!()
            };
            if let project_json::Sb3Primitive::String(name) = name {
                if let Some(name) = context.current_procedure_parameters.get(name.as_str()) {
                    return Ok(ast_types::Expression::Identifier(create_simple_identifier(
                        name.clone(),
                    )));
                }
                match name.as_str() {
                    "is TurboWarp?" => {
                        return Ok(ast_types::Expression::Identifier(create_simple_identifier(
                            literal!("is_turbowarp"),
                        )));
                    }
                    "is compiled?" => {
                        return Ok(ast_types::Expression::Identifier(create_simple_identifier(
                            literal!("is_compiled"),
                        )));
                    }
                    _ => (),
                }
            }
            ast_types::Expression::Call {
                function: create_simple_identifier(if is_bool {
                    literal!("boolean_argument")
                } else {
                    literal!("string_number_argument")
                }),
                arguments: vec![ast_types::Expression::Literal(name.into())],
            }
        }
    })
}

/// Result is unbubbled
pub fn convert_normal_reporter_block(
    block: &project_json::Sb3NormalBlock,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_idx: usize,
) -> DetranspilerResult<ast_types::Expression> {
    // TODO: Implement normal block primitives in detranspiler
    // Issue: #119
    if let Some(reporter) = check_special_reporter(block, blocks) {
        return convert_special_reporter_block(
            reporter, block, block_id, blocks, context, target_idx,
        );
    }
    let (block_kind_info, parameters, _) = convert_block(
        block,
        block_id,
        blocks,
        context,
        target_idx,
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
    target_idx: usize,
    context: &mut DetranspilerContext,
) -> DetranspilerResult<ast_types::Expression> {
    Ok(match field_value_info {
        Some(value) => {
            ast_types::Expression::Identifier(create_simple_identifier(value.field_value_name))
        }
        None => match field_value {
            project_json::Sb3FieldValue::Normal(value) => {
                ast_types::Expression::Literal(get_literal_from_sb3_primitive(value))
            }
            project_json::Sb3FieldValue::WithId { value, id } => {
                ast_types::Expression::Identifier({
                    let vlb = lookup_vlb(&value.as_cow_str(), id, target_idx, context)?
                        .ok_or_else(|| GrazeDetranspilerError::UnknownVariable {
                            id: id.clone(),
                            name: value.to_string(),
                        })?;
                    create_vlb_identifier(vlb.into(), &context.targets, target_idx)
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

pub fn create_broadcast_identifier(
    broadcast_name: IString,
    targets: &[DetranspilerTarget],
    target_idx: usize,
) -> ast_types::Identifier {
    if targets
        .get(target_idx)
        .unwrap()
        .namespace
        .used_names
        .contains_key(&broadcast_name)
    {
        ast_types::Identifier::new(vec![
            ast_types::SingleIdentifier::new(literal!("broadcasts")),
            ast_types::SingleIdentifier::new(broadcast_name),
        ])
    } else {
        create_simple_identifier(broadcast_name)
    }
}

#[inline]
pub fn create_simple_identifier(name: IString) -> ast_types::Identifier {
    ast_types::Identifier::new(vec![ast_types::SingleIdentifier::new(name)])
}

pub fn create_vlb_identifier(
    broadcast: InternalVLBIdentifier,
    targets: &[DetranspilerTarget],
    target_idx: usize,
) -> ast_types::Identifier {
    match broadcast {
        InternalVLBIdentifier::Broadcast(value) => {
            create_broadcast_identifier(value, targets, target_idx)
        }
        InternalVLBIdentifier::VarOrList(value) => create_simple_identifier(value),
    }
}

/// Result is unbubbled
pub fn convert_primitive_reporter_block(
    block: &project_json::Sb3PrimitiveBlock,
    context: &mut DetranspilerContext,
    target_idx: usize,
) -> DetranspilerResult<ast_types::Expression> {
    match block {
        project_json::Sb3PrimitiveBlock::Number(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::PositiveNumber(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::PositiveInteger(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::Integer(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::Angle(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::Color(sb3_primitive)
        | project_json::Sb3PrimitiveBlock::String(sb3_primitive) => Ok(
            ast_types::Expression::Literal(get_literal_from_sb3_primitive(sb3_primitive)),
        ),
        project_json::Sb3PrimitiveBlock::Broadcast { name, id } => {
            let broadcast = context
                .broadcasts
                .get(id)
                .filter(|value| name_matches(name, value.canonical_name.as_ref(), &value.name))
                .ok_or_else(|| GrazeDetranspilerError::UnknownBroadcast {
                    id: id.clone(),
                    name: name.clone(),
                })?;
            Ok(ast_types::Expression::Identifier(
                create_broadcast_identifier(broadcast.name.clone(), &context.targets, target_idx),
            ))
        }
        project_json::Sb3PrimitiveBlock::Variable {
            name,
            id,
            x: _,
            y: _,
        } => {
            let variable = lookup_var_or_list(name, id, target_idx, context)?.ok_or_else(|| {
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
            let list = lookup_var_or_list(name, id, target_idx, context)?.ok_or_else(|| {
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
    target_idx: usize,
) -> DetranspilerResult<(ast_types::Identifier, Vec<ast_types::Expression>)> {
    let (block_kind_info, parameters, _) = convert_block(
        block,
        block_id,
        blocks,
        context,
        target_idx,
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
type NextBlockId = IString;

/// Result is bubbled
pub fn convert_block_stack(
    block: &project_json::Sb3Block,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_idx: usize,
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
                target_idx
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
    target_idx: usize,
) -> DetranspilerResult<(ast_types::Statement, Option<NextBlockId>)> {
    // TODO: Implement if else block and if else chains in detranspiler
    // Issue: #107

    // TODO: Implement list assignment in detranspiler
    // Issue: #106
    let project_json::Sb3Block::Normal(block) = block else {
        return Err(GrazeDetranspilerError::PrimitiveBlockAsStackBlock {
            block_id: block_id.to_string(),
        });
    };
    let mut has_substack = false;
    let (block_kind_info, parameters, stack_params) = convert_block(
        block,
        block_id,
        blocks,
        context,
        target_idx,
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
