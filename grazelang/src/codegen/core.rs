#![allow(clippy::result_large_err)]
use std::{
    collections::{HashMap, HashSet},
    io::Read,
    iter::{self, zip},
    path::{Path, PathBuf},
    rc::Rc,
};

use arcstr::{ArcStr as IString, literal};
use grazelang_library::{
    BACKDROP_TARGETS_CATEGORY_ID, BACKDROPS_CATEGORY_ID, BindInfo, CLONABLES_CATEGORY_ID,
    COLLIDERS_CATEGORY_ID, COSTUMES_CATEGORY_ID, CallBlockParam, CallBlockParamKind,
    CallableKnownBlockSignature, DESTINATIONS_CATEGORY_ID, DIRECTIONS_CATEGORY_ID, HasShadow,
    LOCATIONS_CATEGORY_ID, NO_CATEGORY_ID, OBJECTS_CATEGORY_ID, SOUNDS_CATEGORY_ID,
    SimpleCallableKnownBlockSignature,
    project_json::{
        IsShadow, Sb3Block, Sb3BlockMutation, Sb3FieldValue, Sb3InputRepr, Sb3InputValue,
        Sb3NormalBlock, Sb3Primitive, Sb3PrimitiveBlock, Sb3Root, Sb3Target, TargetAttachment,
    },
};
use rand::SeedableRng;
use rand_xoshiro::Xoshiro256StarStar;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use super::ids::IdCounter;

use crate::{
    lexer::SourceSpan,
    library::{self, create_sprite_dependent_symbols, create_stage_dependent_symbols},
    messages::{GrazeMessage, GrazeWarning, GrazeWarningKind},
    names::Namespace,
    parser::{
        context::{
            BROADCAST_CATEGORIES, IdString, KnownBlock, NO_CATEGORIES, ParseContext,
            ResolveKnownBlock, Symbol, SymbolId, SymbolTable, Target, TargetSymbolDescriptor,
        },
        cst::{
            self, BinOpDescriptor, CustomBlockParamKind, CustomBlockParamKindValue,
            DataDeclarationScope, Expression, FormattedStringContent, GetPos, Identifier,
            ListEntry, Literal, UnOpDescriptor,
        },
    },
    settings::{GrazeMessageSetting, GrazeSettings, UseShadows},
    visitor::{
        GrazeVisitor, default_visit_code_block, default_visit_custom_block_definition,
        default_visit_expression_binary_operation, default_visit_expression_call,
        default_visit_expression_formatted_string, default_visit_expression_get_item,
        default_visit_expression_get_letter, default_visit_expression_identifier,
        default_visit_expression_literal, default_visit_expression_unary_operation,
        default_visit_formatted_string_content, default_visit_isolated_block,
        default_visit_isolated_expression, default_visit_multi_input_hat_statement,
        default_visit_no_input_hat_statement, default_visit_single_input_hat_statement,
        default_visit_statement_assignment, default_visit_statement_call,
        default_visit_statement_forever, default_visit_statement_multi_input_control,
        default_visit_statement_set_item, default_visit_statement_single_input_control,
        default_visit_top_level_statement_sprite, default_visit_top_level_statement_stage,
    },
};

#[derive(Debug, Clone, Error)]
pub enum GrazeSb3GeneratorError {
    #[error("the identifier {identifier:?} was not found")]
    UnknownIdentifier { identifier: Identifier },
    #[error("the identifier {identifier:?} is not a block")]
    IdentifierIsNotABlock { identifier: Identifier },
    #[error(
        "in this context, no menu input was expected, found {input_menu_value:?} at {source_span:?}"
    )]
    UnexpectedInputMenu {
        input_menu_value: Sb3FieldValue,
        source_span: SourceSpan,
    },
    #[error(
        "the amount of parameters for this block was {unexpected:?} at {source_span:?}, expected {expected:?}"
    )]
    IncorrectParamCount {
        unexpected: usize,
        expected: usize,
        source_span: SourceSpan,
    },
    #[error("tried to get a list item for a non list, {identifier:?}")]
    ListAccessForNonLists { identifier: Identifier },
    #[error("cannot initialize stage multiple times")]
    RepeatedStageInitialization { stage_keyword: cst::StageKeyword },
    #[error("tried to call the identifier {identifier:?} as a c block when it was not possible")]
    BlockIsNotCBlock { identifier: Identifier },
    // This should only be able to happen if the transpiler has a bug
    #[error(
        "tried to pass normal parameter {param:?} as a block stack, this is likely a bug in graze"
    )]
    PassedNormalParamAsBlockStack {
        param: Box<Param>,
        source_span: SourceSpan,
    },
    #[error("tried to get the known block of a block stack")]
    TriedGetKnownBlockOfBlockStack { source_span: SourceSpan },
    #[error("tried to name two separate sprites {identifier:?}, try canonical names")]
    ShadowedSprite { identifier: Identifier },
    #[error("the identifier {identifier:?} is not callable")]
    IdentifierNotCallable { identifier: Identifier },
}

impl GetPos for GrazeSb3GeneratorError {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            GrazeSb3GeneratorError::UnknownIdentifier { identifier } => {
                identifier.get_source_span()
            }
            GrazeSb3GeneratorError::IdentifierIsNotABlock { identifier } => {
                identifier.get_source_span()
            }
            GrazeSb3GeneratorError::UnexpectedInputMenu {
                input_menu_value: _,
                source_span,
            } => source_span,
            GrazeSb3GeneratorError::IncorrectParamCount {
                unexpected: _,
                expected: _,
                source_span,
            } => source_span,
            GrazeSb3GeneratorError::ListAccessForNonLists { identifier } => {
                identifier.get_source_span()
            }
            GrazeSb3GeneratorError::RepeatedStageInitialization { stage_keyword } => {
                stage_keyword.get_source_span()
            }
            GrazeSb3GeneratorError::BlockIsNotCBlock { identifier } => identifier.get_source_span(),
            GrazeSb3GeneratorError::PassedNormalParamAsBlockStack {
                param: _,
                source_span,
            } => source_span,
            GrazeSb3GeneratorError::TriedGetKnownBlockOfBlockStack { source_span } => source_span,
            GrazeSb3GeneratorError::ShadowedSprite { identifier } => identifier.get_source_span(),
            GrazeSb3GeneratorError::IdentifierNotCallable { identifier } => {
                identifier.get_source_span()
            }
        }
    }
}

#[derive(Debug, Error)]
pub enum GrazeSb3GeneratorCreationError {
    #[error(transparent)]
    IoError {
        #[from]
        error: std::io::Error,
    },
    #[error("you cannot call two sprites the same name: {name:?}")]
    ShadowedSprite { name: String },
    #[error("you cannot call a sprite \"stage\", try using a canonical name")]
    ShadowedStage,
    #[error("path {path:?} tries to escape the resource directory")]
    PathTriesToEscapeResourceDirectory { path: PathBuf },
}

pub struct GrazeSb3Generator;

#[derive(Debug, Clone)]
pub struct GrazeSb3GeneratorContext {
    pub sb3: Sb3Root,
    pub targets: Vec<Target>,
    // Root symbol is always 0.
    pub symbol_table: SymbolTable,
    pub field_category_entries: HashMap<u32, HashSet<IString>>,
    pub field_entry_categories: HashMap<IString, HashSet<u32>>,
    pub block_counter: IdCounter,
    pub arg_stack: Vec<Param>,
    pub current_block_id: IdString,
    pub current_parent: Option<String>,
    pub current_sb3_target: Option<Sb3Target>,
    pub current_target_symbol_name: Option<IString>,
    /// Is None while and after being initialized
    pub uninitialized_stage: Option<Sb3Target>,
    pub current_previous_block: Option<IdString>,
    pub formatted_string_context: FormattedStringContext,
    pub target_attachments: HashMap<IString, Vec<TargetAttachment>>,
    pub asset_files: HashMap<String, IString>,
    pub settings: GrazeSettings,
    pub messages: Vec<GrazeMessage>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AssetFile {
    pub file_name: String,
    pub file_path: IString,
}

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct FormattedStringContext {
    pub ids: Vec<Option<String>>,
    pub current_idx: usize,
}

impl FormattedStringContext {
    fn new() -> Self {
        Self::default()
    }
}

pub fn add_bind_info(symbol: &mut Symbol, parent_target: &IString) {
    if let Some(known_block) = &mut symbol.known_block {
        match Rc::get_mut(known_block).unwrap() {
            KnownBlock::Variable {
                bind_info,
                canonical_name,
                id,
                ..
            } => {
                bind_info.replace(BindInfo {
                    parent_target: parent_target.clone(),
                    property_of_params: vec![
                        (
                            CallBlockParam {
                                kind: CallBlockParamKind::Field {
                                    default: None,
                                    category: NO_CATEGORY_ID,
                                },
                                name: literal!("PROPERTY"),
                            },
                            KnownBlock::FieldValue {
                                value: Sb3FieldValue::WithId {
                                    value: Sb3Primitive::String(canonical_name.to_string()),
                                    id: id.to_string(),
                                },
                                categories: HashSet::from([NO_CATEGORY_ID]),
                            },
                        ),
                        {
                            let name = literal!("OBJECT");
                            (
                                CallBlockParam {
                                    kind: CallBlockParamKind::MenuInput {
                                        opcode: literal!("sensing_of_object_menu"),
                                        field_name: name.clone(),
                                        default: Sb3FieldValue::Normal("_stage_".into()),
                                        category: NO_CATEGORY_ID,
                                    },
                                    name,
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(parent_target.as_str().into()),
                                    categories: HashSet::from([NO_CATEGORY_ID]),
                                },
                            )
                        },
                    ],
                });
            }
            KnownBlock::List { .. }
            | KnownBlock::FieldValue { .. }
            | KnownBlock::BlockRef { .. }
            | KnownBlock::PrimitiveBlock { .. }
            | KnownBlock::Callable(..)
            | KnownBlock::PartialCallable(..)
            | KnownBlock::SingletonReporter { .. }
            | KnownBlock::CustomBlock { .. }
            | KnownBlock::Empty => (),
        }
    }
}

pub const STAGE_ISTRING: &IString = &literal!("stage");
pub const STAGE_FIELD_VALUE_ISTRING: &IString = &literal!("_stage_");
pub const SPRITES_ISTRING: &IString = &literal!("sprites");
pub const MY_BLOCKS_ISTRING: &IString = &literal!("my_blocks");
pub const CURRENT_DIRECTORY_STR: &str = ".";

impl GrazeSb3GeneratorContext {
    pub fn new(parse_context: ParseContext) -> Result<Self, GrazeSb3GeneratorCreationError> {
        let mut this = Self::without_standard_namespaces(parse_context)?;
        library::add_standard_library_namespaces(&mut this, Default::default());
        Self::alias_standard_namespaces(&mut this.symbol_table, Default::default());
        Ok(this)
    }

    fn alias_standard_namespaces(symbol_table: &mut SymbolTable, root_symbol: SymbolId) {
        let namespaces = &symbol_table[root_symbol].namespace;
        let mut flattened = HashMap::<IString, Option<SymbolId>>::new();
        for &namespace in namespaces.values() {
            for (key, &value) in &symbol_table[namespace].namespace {
                if let Some(mut_value) = flattened.get_mut(key) {
                    mut_value.take();
                } else {
                    flattened.insert(key.clone(), Some(value));
                }
            }
        }
        for (key, value) in flattened {
            if let Some(value) = value {
                symbol_table.insert_alias(root_symbol, key, value);
            }
        }
    }

    pub fn canonicalize_resource_directory(
        settings: &mut GrazeSettings,
    ) -> Result<(), GrazeSb3GeneratorCreationError> {
        if let Some(current_path) = &mut settings.resources_path {
            *current_path = current_path.canonicalize()?;
        } else {
            settings.resources_path = Some(Path::new(CURRENT_DIRECTORY_STR).canonicalize()?);
        }
        Ok(())
    }

    pub fn without_standard_namespaces(
        mut parse_context: ParseContext,
    ) -> Result<Self, GrazeSb3GeneratorCreationError> {
        fn extend_categories_for_field_value<I>(
            categories: I,
            field_name_istring: IString,
            field_category_entries: &mut HashMap<u32, HashSet<IString>>,
            field_entry_categories: &mut HashMap<IString, HashSet<u32>>,
        ) where
            I: Iterator<Item = u32>,
        {
            let categories = categories.collect::<HashSet<_>>();
            for &category in &categories {
                if let Some(current) = field_category_entries.get_mut(&category) {
                    current.insert(field_name_istring.clone());
                } else {
                    field_category_entries
                        .insert(category, HashSet::from([field_name_istring.clone()]));
                }
            }
            if let Some(current) = field_entry_categories.get_mut(&field_name_istring) {
                current.extend(categories);
            } else {
                field_entry_categories.insert(field_name_istring, categories);
            }
        }
        fn add_category_for_field_value(
            category: u32,
            field_name_istring: IString,
            field_category_entries: &mut HashMap<u32, HashSet<IString>>,
            field_entry_categories: &mut HashMap<IString, HashSet<u32>>,
        ) {
            if let Some(current) = field_entry_categories.get_mut(&field_name_istring) {
                current.insert(category);
            } else {
                field_entry_categories
                    .insert(field_name_istring.clone(), HashSet::from([category]));
            }
            if let Some(current) = field_category_entries.get_mut(&category) {
                current.insert(field_name_istring);
            } else {
                field_category_entries.insert(category, HashSet::from([field_name_istring]));
            }
        }
        Self::canonicalize_resource_directory(&mut parse_context.settings)?;
        let mut rng = Xoshiro256StarStar::from_seed(parse_context.random_seed);
        let targets: Vec<Target> = parse_context.parsed_targets.into();
        let standard_library_namespace_count = library::get_standard_library_namespace_count();
        let mut symbol_table =
            SymbolTable::with_capacity(1 + standard_library_namespace_count + 4 + targets.len());
        let root_symbol = symbol_table.new_symbol(Symbol {
            known_block: None,
            namespace: HashMap::with_capacity(standard_library_namespace_count + 4),
            parent: Default::default(),
        });

        let targets_symbol = symbol_table.new_child_symbol(
            root_symbol,
            SPRITES_ISTRING.clone(),
            None,
            targets.len(),
        );
        let mut field_category_entries = HashMap::<u32, HashSet<IString>>::with_capacity(32);
        let mut field_entry_categories =
            HashMap::<IString, HashSet<u32>>::with_capacity(targets.len());
        let mut asset_files = HashMap::new();
        let mut target_attachments = HashMap::with_capacity(targets.len());
        for target in &targets {
            if let Target::Sprite {
                name,
                canonical_name: _,
                symbols: _,
            } = target
                && name.as_str() == "stage"
            {
                return Err(GrazeSb3GeneratorCreationError::ShadowedStage);
            }
            let mut namespace = Namespace::new();

            let is_stage = matches!(target, Target::Stage { .. });
            let symbol_count = target.borrow_symbols().len()
                // Accounts for the symbols that every target has e.g. volume
                + if is_stage {
                    3
                } else {
                    7
                };
            let categories: &[u32] = if is_stage {
                &[OBJECTS_CATEGORY_ID]
            } else {
                &[
                    DESTINATIONS_CATEGORY_ID,
                    DIRECTIONS_CATEGORY_ID,
                    CLONABLES_CATEGORY_ID,
                    COLLIDERS_CATEGORY_ID,
                    LOCATIONS_CATEGORY_ID,
                    OBJECTS_CATEGORY_ID,
                ]
            };
            extend_categories_for_field_value(
                categories.iter().copied(),
                target.get_field_value().clone(),
                &mut field_category_entries,
                &mut field_entry_categories,
            );
            let target_symbol = symbol_table.new_child_symbol(
                targets_symbol,
                target.get_namespace_name().clone(),
                Some(Rc::new(KnownBlock::FieldValue {
                    value: Sb3FieldValue::Normal(Sb3Primitive::String(
                        target.get_field_value().to_string(),
                    )),
                    categories: categories.iter().copied().collect(),
                })),
                symbol_count,
            );
            let (mut symbols, attachments) = target
                .borrow_symbols()
                .iter()
                .map(|(key, value)| {
                    derive_related_data_of_target_symbol(
                        value,
                        &mut rng,
                        &mut namespace,
                        parse_context
                            .settings
                            .resources_path
                            .as_deref()
                            .unwrap_or(Path::new(CURRENT_DIRECTORY_STR)),
                    )
                    .map(|(mut symbol, attachment, asset_file)| {
                        add_bind_info(&mut symbol, target.get_field_value());
                        if let Some(AssetFile {
                            file_name,
                            file_path,
                        }) = asset_file
                        {
                            asset_files.insert(file_name, file_path);
                        }
                        ((key.clone(), symbol), attachment)
                    })
                })
                .try_fold::<_, _, Result<_, GrazeSb3GeneratorCreationError>>(
                    (HashMap::with_capacity(symbol_count), Vec::new()),
                    |(mut symbols, mut target_attachments), item| {
                        let ((key, symbol), asset) = item?;
                        symbols.insert(key, symbol);
                        if let Some(asset) = asset {
                            target_attachments.push(asset);
                        }
                        Ok((symbols, target_attachments))
                    },
                )?;
            symbols.extend(if is_stage {
                create_stage_dependent_symbols(STAGE_FIELD_VALUE_ISTRING)
            } else {
                create_sprite_dependent_symbols(target.get_field_value())
            });
            for attachment in &attachments {
                match attachment {
                    TargetAttachment::Costume(sb3_costume) => {
                        let field_name_istring: IString = sb3_costume.name.as_str().into();
                        if is_stage {
                            extend_categories_for_field_value(
                                [BACKDROPS_CATEGORY_ID, BACKDROP_TARGETS_CATEGORY_ID].into_iter(),
                                field_name_istring,
                                &mut field_category_entries,
                                &mut field_entry_categories,
                            );
                        } else {
                            add_category_for_field_value(
                                COSTUMES_CATEGORY_ID,
                                field_name_istring,
                                &mut field_category_entries,
                                &mut field_entry_categories,
                            );
                        }
                    }
                    TargetAttachment::Sound(sb3_sound) => {
                        add_category_for_field_value(
                            SOUNDS_CATEGORY_ID,
                            sb3_sound.name.as_str().into(),
                            &mut field_category_entries,
                            &mut field_entry_categories,
                        );
                    }
                    TargetAttachment::Var(..)
                    | TargetAttachment::List(..)
                    | TargetAttachment::CustomBlock(..)
                    | TargetAttachment::Broadcast { .. } => (),
                }
            }
            if target_attachments
                .insert(target.get_namespace_name().clone(), attachments)
                .is_some()
            {
                return Err(GrazeSb3GeneratorCreationError::ShadowedSprite {
                    name: target.get_namespace_name().to_string(),
                });
            }
            for (key, symbol) in symbols {
                let is_list = symbol
                    .known_block
                    .as_ref()
                    .map(|value| matches!(value.as_ref(), KnownBlock::List { .. }))
                    .unwrap_or(false);
                let symbol = symbol_table.new_symbol(symbol);
                if is_list {
                    patch_in_list_methods(&mut symbol_table, symbol);
                }
                symbol_table.insert_child(target_symbol, key, symbol);
            }
        }
        let stage_symbol = symbol_table
            .get_child(targets_symbol, STAGE_ISTRING)
            .unwrap();

        let broadcasts_symbol = symbol_table.new_child_symbol(
            root_symbol,
            literal!("broadcasts"),
            None,
            parse_context.broadcasts.len(),
        );
        {
            let stage_target_attachments = target_attachments.get_mut("stage").unwrap();
            for (key, value) in &parse_context.broadcasts {
                let (symbol, target_attachment) = value.derive_related_data(&mut rng);
                stage_target_attachments.push(target_attachment);
                let symbol = symbol_table.new_symbol(symbol);
                symbol_table.insert_child(broadcasts_symbol, key.clone(), symbol);
            }
        }
        let variables_symbol =
            symbol_table.new_child_symbol(root_symbol, literal!("vars"), None, 0);
        let lists_symbol = symbol_table.new_child_symbol(root_symbol, literal!("lists"), None, 0);
        let mut global_namespace = Namespace::new();
        {
            let stage_target_attachments = target_attachments.get_mut("stage").unwrap();
            for (name, symbol) in parse_context.global_symbols.drain() {
                match &symbol {
                    TargetSymbolDescriptor::Var(_) => {
                        let (symbol, attachment, asset_file) =
                            derive_related_data_of_target_symbol(
                                &symbol,
                                &mut rng,
                                &mut global_namespace,
                                parse_context
                                    .settings
                                    .resources_path
                                    .as_deref()
                                    .unwrap_or(Path::new(CURRENT_DIRECTORY_STR)),
                            )
                            .unwrap();
                        // TODO: Add variables, lists, broadcasts, sprites etc to corresponding field categories
                        //  Implement if necessary:
                        //  - [ ] variables
                        //  - [ ] lists
                        //  - [ ] broadcasts
                        //  - [x] sprites
                        //  - [x] costumes
                        //  - [x] backdrops
                        //  - [x] sounds
                        // Issue: #50
                        if let Some(AssetFile {
                            file_name,
                            file_path,
                        }) = asset_file
                        {
                            asset_files.insert(file_name, file_path);
                        }
                        let mut symbol_for_stage: Symbol = Symbol {
                            known_block: symbol
                                .known_block
                                .as_ref()
                                .map(|value| Rc::new(value.as_ref().clone())),
                            namespace: symbol.namespace.clone(),
                            parent: symbol.parent,
                        };
                        add_bind_info(&mut symbol_for_stage, STAGE_FIELD_VALUE_ISTRING);
                        if let Some(attachment) = attachment {
                            stage_target_attachments.push(attachment);
                        }
                        let symbol_for_stage = symbol_table.new_symbol(symbol_for_stage);
                        let symbol = symbol_table.new_symbol(symbol);
                        symbol_table.insert_child(stage_symbol, name.clone(), symbol_for_stage);
                        symbol_table.insert_child(variables_symbol, name, symbol);
                    }
                    TargetSymbolDescriptor::List(_) => {
                        let (symbol, attachment, asset_file) =
                            derive_related_data_of_target_symbol(
                                &symbol,
                                &mut rng,
                                &mut global_namespace,
                                parse_context
                                    .settings
                                    .resources_path
                                    .as_deref()
                                    .unwrap_or(Path::new(CURRENT_DIRECTORY_STR)),
                            )
                            .unwrap();
                        if let Some(AssetFile {
                            file_name,
                            file_path,
                        }) = asset_file
                        {
                            asset_files.insert(file_name, file_path);
                        }
                        if let Some(attachment) = attachment {
                            stage_target_attachments.push(attachment);
                        }
                        let symbol = symbol_table.new_symbol(symbol);
                        patch_in_list_methods(&mut symbol_table, symbol);
                        symbol_table.insert_child(lists_symbol, name, symbol);
                    }
                    _ => (), // Handled just to be sure although it shouldn't happen
                }
            }
        }
        let mut block_counter = IdCounter::new();
        let next_block_id = block_counter.get_new_id();
        Ok(Self {
            sb3: Sb3Root::default(),
            targets,
            symbol_table,
            field_category_entries,
            field_entry_categories,
            block_counter,
            arg_stack: Vec::new(),
            current_block_id: next_block_id,
            current_parent: None,
            current_sb3_target: None,
            current_target_symbol_name: None,
            uninitialized_stage: Some(Sb3Target::new_stage()),
            current_previous_block: None,
            formatted_string_context: FormattedStringContext::new(),
            target_attachments,
            asset_files,
            settings: std::mem::take(&mut parse_context.settings),
            messages: Vec::new(),
        })
    }

    fn new_block(&mut self) {
        self.current_block_id = self.block_counter.get_new_id();
    }

    fn get_current_block_id(&mut self) -> IdString {
        self.current_block_id.clone()
    }

    fn push_param(&mut self, block_arg: Param) {
        self.arg_stack.push(block_arg);
    }

    fn pop_param(&mut self) -> Option<Param> {
        self.arg_stack.pop()
    }

    pub fn resolve_path<'a, I>(&self, mut iterator: I) -> Option<SymbolId>
    where
        I: Iterator<Item = &'a IString>,
    {
        iterator.try_fold(SymbolId::default(), |current, next| {
            if next.as_str() == "super" {
                Some(self.symbol_table[current].parent)
            } else {
                self.symbol_table.get_child(current, next)
            }
        })
    }

    pub fn resolve_identifier(&self, identifier: &Identifier) -> Option<SymbolId> {
        self.resolve_path(
            identifier
                .path
                .iter()
                .chain(identifier.fields.iter())
                .map(|(next, _)| next),
        )
    }
}

pub fn compute_hash(path: &Path) -> Result<String, std::io::Error> {
    use std::fs::File;
    let mut file = File::open(path)?;
    let mut data: Vec<u8> = Vec::new();
    file.read_to_end(&mut data)?;
    let digest = md5::compute(data);
    let hex_digest = format!("{:x}", digest);
    Ok(hex_digest)
}

pub use symbol_data_derivation::{derive_related_data_of_target_symbol, patch_in_list_methods};
pub mod symbol_data_derivation {
    use std::{
        collections::{HashMap, HashSet},
        ffi::OsStr,
        path::{Path, PathBuf},
        rc::Rc,
    };

    use arcstr::{ArcStr as IString, literal};
    use grazelang_library::{
        BACKDROP_TARGETS_CATEGORY_ID, BACKDROPS_CATEGORY_ID, COSTUMES_CATEGORY_ID, CallBlockParam,
        CallBlockParamKind, HasShadow, LISTS_CATEGORY_ID, SOUNDS_CATEGORY_ID,
        project_json::{
            Sb3Costume, Sb3FieldValue, Sb3ListDeclaration, Sb3Primitive, Sb3PrimitiveBlock,
            Sb3Sound, Sb3VariableDeclaration, TargetAttachment,
        },
    };
    use rand::Rng;

    use crate::{
        codegen::{
            core::{AssetFile, GrazeSb3GeneratorCreationError, compute_hash},
            ids::generate_random_id_string,
        },
        names::Namespace,
        parser::{
            context::{
                CustomBlockDescriptor, KnownBlock, ListDescriptor, Symbol, SymbolId, SymbolTable,
                TargetSymbolDescriptor, VarDescriptor,
            },
            cst::CustomBlockParamKindValue,
        },
    };
    pub type TargetSymbolData = (Symbol, Option<TargetAttachment>, Option<AssetFile>);

    pub fn patch_in_list_methods(symbol_table: &mut SymbolTable, symbol_id: SymbolId) {
        let known_block = symbol_table[symbol_id]
            .known_block
            .as_ref()
            .unwrap()
            .as_ref()
            .clone();
        let list_select_param = (
            CallBlockParam {
                kind: CallBlockParamKind::Field {
                    default: None,
                    category: LISTS_CATEGORY_ID,
                },
                name: literal!("LIST"),
            },
            known_block,
        );
        for (name, known_block) in [
            (
                literal!("push"),
                KnownBlock::PartialCallable(
                    literal!("data_addtolist"),
                    vec![list_select_param.clone()],
                    vec![CallBlockParam {
                        kind: CallBlockParamKind::Input {
                            default: Some("thing".into()),
                        },
                        name: literal!("ITEM"),
                    }],
                ),
            ),
            (
                literal!("remove"),
                KnownBlock::PartialCallable(
                    literal!("data_deleteoflist"),
                    vec![list_select_param.clone()],
                    vec![CallBlockParam {
                        kind: CallBlockParamKind::Input {
                            default: Some(Sb3PrimitiveBlock::Integer("1".into())),
                        },
                        name: literal!("INDEX"),
                    }],
                ),
            ),
            (
                literal!("clear"),
                KnownBlock::PartialCallable(
                    literal!("data_deletealloflist"),
                    vec![list_select_param.clone()],
                    vec![],
                ),
            ),
            (
                literal!("insert"),
                KnownBlock::PartialCallable(
                    literal!("data_insertatlist"),
                    vec![list_select_param.clone()],
                    vec![
                        CallBlockParam {
                            kind: CallBlockParamKind::Input {
                                default: Some(Sb3PrimitiveBlock::Integer("1".into())),
                            },
                            name: literal!("INDEX"),
                        },
                        CallBlockParam {
                            kind: CallBlockParamKind::Input {
                                default: Some("thing".into()),
                            },
                            name: literal!("ITEM"),
                        },
                    ],
                ),
            ),
            (
                literal!("set"),
                KnownBlock::PartialCallable(
                    literal!("data_replaceitemoflist"),
                    vec![list_select_param.clone()],
                    vec![
                        CallBlockParam {
                            kind: CallBlockParamKind::Input {
                                default: Some(Sb3PrimitiveBlock::Integer("1".into())),
                            },
                            name: literal!("INDEX"),
                        },
                        CallBlockParam {
                            kind: CallBlockParamKind::Input {
                                default: Some("thing".into()),
                            },
                            name: literal!("ITEM"),
                        },
                    ],
                ),
            ),
            (
                literal!("get"),
                KnownBlock::PartialCallable(
                    literal!("data_itemoflist"),
                    vec![list_select_param.clone()],
                    vec![CallBlockParam {
                        kind: CallBlockParamKind::Input {
                            default: Some(Sb3PrimitiveBlock::Integer("1".into())),
                        },
                        name: literal!("INDEX"),
                    }],
                ),
            ),
            (
                literal!("find"),
                KnownBlock::PartialCallable(
                    literal!("data_itemnumoflist"),
                    vec![list_select_param.clone()],
                    vec![CallBlockParam {
                        kind: CallBlockParamKind::Input {
                            default: Some("thing".into()),
                        },
                        name: literal!("ITEM"),
                    }],
                ),
            ),
            (
                literal!("len"),
                KnownBlock::PartialCallable(
                    literal!("data_lengthoflist"),
                    vec![list_select_param.clone()],
                    vec![],
                ),
            ),
            (
                literal!("contains"),
                KnownBlock::PartialCallable(
                    literal!("data_listcontainsitem"),
                    vec![list_select_param.clone()],
                    vec![CallBlockParam {
                        kind: CallBlockParamKind::Input {
                            default: Some("thing".into()),
                        },
                        name: literal!("ITEM"),
                    }],
                ),
            ),
            (
                literal!("show"),
                KnownBlock::PartialCallable(
                    literal!("data_showlist"),
                    vec![list_select_param.clone()],
                    vec![],
                ),
            ),
            (
                literal!("data_hidelist"),
                KnownBlock::PartialCallable(
                    literal!("data_lengthoflist"),
                    vec![list_select_param.clone()],
                    vec![],
                ),
            ),
        ] {
            symbol_table.new_child_symbol(symbol_id, name, Some(Rc::new(known_block)), 0);
        }
    }

    pub fn derive_related_data_of_target_symbol<T>(
        this: &TargetSymbolDescriptor,
        rng: &mut T,
        namespace: &mut Namespace,
        resources_directory: &Path,
    ) -> Result<TargetSymbolData, GrazeSb3GeneratorCreationError>
    where
        T: Rng,
    {
        match this {
            TargetSymbolDescriptor::CustomBlockDescriptor(descriptor) => {
                Ok(handle_custom_block(descriptor, rng))
            }
            TargetSymbolDescriptor::Var(descriptor) => {
                Ok(handle_variable(descriptor, rng, namespace))
            }
            TargetSymbolDescriptor::List(descriptor) => Ok(handle_list(descriptor, rng, namespace)),
            TargetSymbolDescriptor::Costume(descriptor) => process_asset(
                &descriptor.name,
                descriptor.canonical_name.as_ref(),
                &descriptor.source,
                resources_directory,
                HashSet::from([COSTUMES_CATEGORY_ID]),
                |asset_id, name, md5ext, data_format| {
                    TargetAttachment::Costume(Sb3Costume {
                        asset_id,
                        name,
                        md5ext,
                        data_format,
                        bitmap_resolution: Some(2.0),
                        rotation_center_x: 0.0, // TODO: Allow user to control the rotation center of a costume or backdrop
                        // Issue: #55
                        rotation_center_y: 0.0,
                    })
                },
            ),
            TargetSymbolDescriptor::Backdrop(descriptor) => process_asset(
                &descriptor.name,
                descriptor.canonical_name.as_ref(),
                &descriptor.source,
                resources_directory,
                HashSet::from([BACKDROPS_CATEGORY_ID, BACKDROP_TARGETS_CATEGORY_ID]),
                |asset_id, name, md5ext, data_format| {
                    TargetAttachment::Costume(Sb3Costume {
                        asset_id,
                        name,
                        md5ext,
                        data_format,
                        bitmap_resolution: Some(2.0),
                        rotation_center_x: 0.0,
                        rotation_center_y: 0.0,
                    })
                },
            ),
            TargetSymbolDescriptor::Sound(descriptor) => process_asset(
                &descriptor.name,
                descriptor.canonical_name.as_ref(),
                &descriptor.source,
                resources_directory,
                HashSet::from([SOUNDS_CATEGORY_ID]),
                |asset_id, name, md5ext, data_format| {
                    TargetAttachment::Sound(Sb3Sound {
                        asset_id,
                        name,
                        md5ext,
                        data_format,
                        rate: 48000.0,
                        sample_count: 1124,
                    })
                },
            ),
        }
    }

    pub fn extend_path_safely(
        directory: &Path,
        rest: &str,
    ) -> Result<PathBuf, GrazeSb3GeneratorCreationError> {
        let new_path = directory.join(rest).canonicalize()?;
        if !new_path.starts_with(directory) {
            return Err(
                GrazeSb3GeneratorCreationError::PathTriesToEscapeResourceDirectory {
                    path: new_path,
                },
            );
        }
        Ok(new_path)
    }

    fn process_asset<F>(
        name: &str,
        canonical_name: Option<&IString>,
        source: &str,
        resources_directory: &Path,
        categories: HashSet<u32>,
        create_attachment: F,
    ) -> Result<TargetSymbolData, GrazeSb3GeneratorCreationError>
    where
        F: FnOnce(String, String, String, String) -> TargetAttachment,
    {
        let path = extend_path_safely(resources_directory, source)?;
        let asset_id = compute_hash(&path)?;
        let ext = Path::new(source)
            .extension()
            .and_then(OsStr::to_str)
            .unwrap_or("");
        let md5ext = format!("{}.{}", asset_id, ext);
        let canonical_name = canonical_name
            .map(ToString::to_string)
            .unwrap_or_else(|| name.to_string());
        let symbol = Symbol {
            known_block: Some(Rc::new(KnownBlock::FieldValue {
                value: Sb3FieldValue::Normal(Sb3Primitive::String(canonical_name.clone())),
                categories,
            })),
            namespace: HashMap::new(),
            parent: Default::default(),
        };
        let attachment =
            create_attachment(asset_id, canonical_name, md5ext.clone(), ext.to_string());
        let asset_file = AssetFile {
            file_name: md5ext,
            file_path: source.into(),
        };
        Ok((symbol, Some(attachment), Some(asset_file)))
    }

    fn handle_custom_block<T>(descriptor: &CustomBlockDescriptor, rng: &mut T) -> TargetSymbolData
    where
        T: Rng,
    {
        let proccode = descriptor.canonical_name.clone().unwrap_or_else(|| {
            if descriptor.args.is_empty() {
                return descriptor.name.clone();
            }
            let mut proccode =
                String::with_capacity(descriptor.name.len() + 3 * descriptor.args.len());
            proccode.push_str(descriptor.name.as_str());

            for arg in &descriptor.args {
                proccode.push_str(" %");
                proccode.push(match &arg.kind {
                    CustomBlockParamKindValue::Number => 'n',
                    CustomBlockParamKindValue::String => 's',
                    CustomBlockParamKindValue::Boolean => 'b',
                });
            }
            proccode.into()
        });

        let mut call_params = Vec::with_capacity(descriptor.args.len());
        let mut params = Vec::with_capacity(descriptor.args.len());

        for arg in &descriptor.args {
            let is_bool = arg.kind == CustomBlockParamKindValue::Boolean;
            let arg_id = generate_random_id_string(rng);

            call_params.push(CallBlockParam {
                kind: grazelang_library::CallBlockParamKind::Input {
                    default: (!is_bool).then(|| {
                        grazelang_library::project_json::Sb3PrimitiveBlock::String("".into())
                    }),
                },
                name: arg_id.clone(),
            });

            params.push((
                arg_id,
                if is_bool {
                    HasShadow::No
                } else {
                    HasShadow::Yes
                },
            ));
        }

        let custom_block = KnownBlock::CustomBlock {
            proccode,
            call_params,
            params,
            is_warp: descriptor.is_warp,
        };

        (
            Symbol {
                known_block: Some(Rc::new(custom_block.clone())),
                namespace: HashMap::new(),
                parent: Default::default(),
            },
            Some(
                grazelang_library::project_json::TargetAttachment::CustomBlock(
                    descriptor.name.clone(),
                    custom_block,
                ),
            ),
            None,
        )
    }

    fn handle_variable<T>(
        descriptor: &VarDescriptor,
        rng: &mut T,
        namespace: &mut Namespace,
    ) -> TargetSymbolData
    where
        T: Rng,
    {
        let id = generate_random_id_string(rng);
        let namespace_input_name = if descriptor.is_cloud {
            ("☁ ".to_string() + descriptor.name.as_str()).into()
        } else {
            descriptor.name.clone()
        };
        let canonical_name = namespace.introduce_new_symbol(
            descriptor.canonical_name.as_ref().map(|v| v.to_string()),
            namespace_input_name,
        );

        let initial_value = if descriptor.value_is_initial_value {
            descriptor.value.clone()
        } else {
            "".into()
        };

        (
            Symbol {
                known_block: Some(Rc::new(KnownBlock::new_variable(
                    canonical_name.clone(),
                    id.clone(),
                    None,
                ))),
                namespace: HashMap::new(),
                parent: Default::default(),
            },
            Some(grazelang_library::project_json::TargetAttachment::Var(
                id.to_string(),
                Sb3VariableDeclaration {
                    name: canonical_name,
                    value: initial_value,
                    is_cloud: descriptor.is_cloud,
                },
            )),
            None,
        )
    }

    fn handle_list<T: Rng>(
        descriptor: &ListDescriptor,
        rng: &mut T,
        namespace: &mut Namespace,
    ) -> TargetSymbolData {
        let id = generate_random_id_string(rng);
        let canonical_name = namespace.introduce_new_symbol(
            descriptor.canonical_name.as_ref().map(|v| v.to_string()),
            descriptor.name.clone(),
        );

        let initial_value = if descriptor.value_is_initial_value {
            descriptor.value.clone()
        } else {
            Vec::new()
        };

        (
            Symbol {
                known_block: Some(Rc::new(KnownBlock::List {
                    canonical_name: canonical_name.clone(),
                    id: id.clone(),
                })), // TODO: add list length as method
                // Issue: #6
                namespace: HashMap::new(),
                parent: Default::default(),
            },
            Some(grazelang_library::project_json::TargetAttachment::List(
                id.to_string(),
                Sb3ListDeclaration(canonical_name, initial_value),
            )),
            None,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Param {
    Owned(Box<KnownBlock>),
    LazyIdentifier(Identifier),
    BlockStack(Option<String>),
}

pub fn make_block(
    parent: Option<String>,
    opcode: String,
    inputs: HashMap<String, Sb3InputValue>,
    fields: HashMap<String, Sb3FieldValue>,
    shadow: bool,
    mutation: Option<Sb3BlockMutation>,
) -> Sb3Block {
    let top_level = parent.is_none();
    Sb3Block::Normal(Sb3NormalBlock {
        opcode,
        next: None,
        parent,
        inputs,
        fields,
        shadow,
        top_level,
        mutation,
        x: top_level.then_some(0.0),
        y: top_level.then_some(0.0),
    })
}

pub fn make_top_level_block(
    opcode: String,
    inputs: HashMap<String, Sb3InputValue>,
    fields: HashMap<String, Sb3FieldValue>,
    shadow: bool,
    mutation: Option<Sb3BlockMutation>,
    pos: (f64, f64),
) -> Sb3Block {
    Sb3Block::Normal(Sb3NormalBlock {
        opcode,
        next: None,
        parent: None,
        inputs,
        fields,
        shadow,
        top_level: true,
        mutation,
        x: Some(pos.0),
        y: Some(pos.1),
    })
}

pub fn set_params_or_unreachable(
    block: Option<&mut Sb3Block>,
    inputs: HashMap<String, Sb3InputValue>,
    fields: HashMap<String, Sb3FieldValue>,
) {
    match block {
        Some(Sb3Block::Normal(block)) => {
            block.inputs = inputs;
            block.fields = fields;
        }
        _ => unreachable!(),
    }
}

pub fn wrap_in_reporter<F, O>(context: &mut GrazeSb3GeneratorContext, action: F) -> O
where
    O: Sized,
    F: FnOnce(&mut GrazeSb3GeneratorContext, Option<String>, IString) -> O,
{
    let block_id = context.get_current_block_id();
    let old_parent = context.current_parent.replace(block_id.to_string());
    context.new_block();
    let out = action(context, old_parent.clone(), block_id);
    context.current_parent = old_parent;
    out
}

pub fn wrap_in_statement<F, O>(context: &mut GrazeSb3GeneratorContext, action: F) -> O
where
    O: Sized,
    F: FnOnce(&mut GrazeSb3GeneratorContext, Option<String>, IString) -> O,
{
    let block_id = context.get_current_block_id();
    let old_parent = context.current_parent.replace(block_id.to_string());
    context.new_block();
    if let Some(previous) = context.current_previous_block.take() {
        match context
            .current_sb3_target
            .as_mut()
            .unwrap()
            .blocks
            .get_mut(previous.as_str())
        {
            Some(Sb3Block::Normal(block)) => {
                block.next = Some(block_id.to_string());
            }
            _ => unreachable!(),
        }
    } else if old_parent.is_some() {
        // First statement in block stack is used in STACK argument of parent or similar
        let value = Param::BlockStack(Some(block_id.to_string()));
        context.push_param(value);
    }
    let out = action(context, old_parent.clone(), block_id.clone());
    context.current_parent = Some(block_id.to_string());
    context.current_previous_block = Some(block_id);
    out
}

macro_rules! static_resolve_identifier {
    ($context:expr, [$start:expr $(, $segments:expr)* $(,)?]) => {
        static_resolve_identifier!(
            $context,
            $context.symbol_table.get_child(Default::default(), $start).unwrap(),
            [$($segments)*]
        )
    };
    ($context:expr, $current:expr, [$start:expr $(, $segments:expr)* $(,)?]) => {
        static_resolve_identifier!(
            $context,
            $context.symbol_table.get_child($current, $start).unwrap(),
            [$($segments)*]
        )
    };
    ($context:expr, $current:expr, []) => {
        $current
    };
}

macro_rules! with_known_block {
    ($context:expr, $param:expr, $param_source_span:expr, $known_block:ident => $action:expr) => {
        match $param {
            Param::Owned(ref $known_block) => $action,
            Param::LazyIdentifier(value) => {
                let symbol_id = $context.resolve_identifier(&value).ok_or_else(|| {
                    GrazeSb3GeneratorError::UnknownIdentifier {
                        identifier: value.clone(),
                    }
                })?;
                let symbol = &$context.symbol_table[symbol_id];
                let known_block_rc = symbol.known_block.clone();
                let $known_block = known_block_rc.as_ref().ok_or_else(|| {
                    GrazeSb3GeneratorError::IdentifierIsNotABlock {
                        identifier: value.clone(),
                    }
                })?;
                $action
            }
            Param::BlockStack(_) => {
                return Err(GrazeSb3GeneratorError::TriedGetKnownBlockOfBlockStack {
                    source_span: $param_source_span,
                })
            }
        }
    };
}

pub fn get_symbol_id(
    context: &mut GrazeSb3GeneratorContext,
    identifier: &Identifier,
) -> Result<SymbolId, GrazeSb3GeneratorError> {
    context.resolve_identifier(identifier).ok_or_else(|| {
        GrazeSb3GeneratorError::UnknownIdentifier {
            identifier: identifier.clone(),
        }
    })
}

pub fn get_known_block<'a>(
    symbol: &'a Symbol,
    identifier: &Identifier,
) -> Result<&'a Rc<KnownBlock>, GrazeSb3GeneratorError> {
    symbol
        .known_block
        .as_ref()
        .ok_or_else(|| GrazeSb3GeneratorError::IdentifierIsNotABlock {
            identifier: identifier.clone(),
        })
}

pub fn introduce_input_menu(
    opcode: &IString,
    field_name: &IString,
    value: Sb3FieldValue,
    context: &mut GrazeSb3GeneratorContext,
) -> IdString {
    wrap_in_reporter(context, |context, parent, this_id| {
        add_block(
            context,
            &this_id,
            make_block(
                parent,
                opcode.to_string(),
                HashMap::new(),
                HashMap::from([(field_name.to_string(), value)]),
                true,
                None,
            ),
        );
        this_id
    })
}

pub fn create_input_value<D>(
    input_repr: Option<(Sb3InputRepr, IsShadow)>,
    default: Option<D>,
) -> Option<Sb3InputValue>
where
    ((Sb3InputRepr, IsShadow), Option<D>): Into<Sb3InputValue>,
    D: Into<Sb3InputValue>,
{
    if let Some(input_repr) = input_repr {
        Some((input_repr, default).into())
    } else {
        default.map(Into::into)
    }
}

pub fn add_known_block_to_params(
    context: &mut GrazeSb3GeneratorContext,
    param: &CallBlockParam,
    value: &KnownBlock,
    known_block_source_span: SourceSpan,
    inputs: &mut HashMap<String, Sb3InputValue>,
    fields: &mut HashMap<String, Sb3FieldValue>,
) -> Result<(), GrazeSb3GeneratorError> {
    macro_rules! LITERAL_FIELD_VALUE_INCORRECT_MSG {
        () => {
            concat!(
                "Cannot reasonably use KnownBlock {:?} as a field parameter in this context, ",
                "maybe you meant to use it as a different parameter or you misspelt it. ",
                "It is recommended to use singleton values instead of literal strings for fields."
            )
        };
    }
    let param_name = param.name.to_string();
    match &param.kind {
        CallBlockParamKind::Input { default } => {
            if let Some(input_value) = create_input_value(
                known_block_to_input_repr_no_menu(value, known_block_source_span, context)?,
                default.as_ref(),
            ) {
                inputs.insert(param_name, input_value);
            }
        }
        CallBlockParamKind::Field { category, .. } => {
            fields.insert(param_name, {
                let (field_value, categories) =
                    value.resolve_for_field(known_block_source_span, context);
                if !categories.contains(category) {
                    emit_message(
                        context,
                        GrazeMessage::Warning(
                            GrazeWarning::Specific(
                                GrazeWarningKind::LiteralFieldValueIncorrect,
                                format!(LITERAL_FIELD_VALUE_INCORRECT_MSG!(), value).into(),
                                known_block_source_span,
                            ),
                            None,
                        ),
                        GrazeMessageSetting::Warnings,
                    );
                }
                field_value
            });
        }
        CallBlockParamKind::MenuInput {
            opcode,
            field_name,
            default,
            category,
        } => {
            let (input_repr, is_shadow) = match value
                .resolve_for_input(known_block_source_span, context)
            {
                grazelang_library::KnownBlockInput::PrimitiveInput(sb3_primitive_block) => {
                    let is_shadow = sb3_primitive_block.is_shadow();
                    match &sb3_primitive_block {
                        Sb3PrimitiveBlock::Number(sb3_primitive)
                        | Sb3PrimitiveBlock::PositiveNumber(sb3_primitive)
                        | Sb3PrimitiveBlock::PositiveInteger(sb3_primitive)
                        | Sb3PrimitiveBlock::Integer(sb3_primitive)
                        | Sb3PrimitiveBlock::Angle(sb3_primitive)
                        | Sb3PrimitiveBlock::Color(sb3_primitive)
                        | Sb3PrimitiveBlock::String(sb3_primitive) => {
                            let cow_str = sb3_primitive.as_cow_str();
                            let categories = context
                                .field_entry_categories
                                .get(&*cow_str)
                                .unwrap_or_else(|| &*NO_CATEGORIES);
                            if !categories.contains(category) {
                                emit_message(
                                    context,
                                    GrazeMessage::Warning(
                                        GrazeWarning::Specific(
                                            GrazeWarningKind::LiteralFieldValueIncorrect,
                                            format!(LITERAL_FIELD_VALUE_INCORRECT_MSG!(), value)
                                                .into(),
                                            known_block_source_span,
                                        ),
                                        None,
                                    ),
                                    GrazeMessageSetting::Warnings,
                                );
                            }
                        }
                        Sb3PrimitiveBlock::Broadcast { .. } => {
                            if !BROADCAST_CATEGORIES.contains(category) {
                                emit_message(
                                    context,
                                    GrazeMessage::Warning(
                                        GrazeWarning::Specific(
                                            GrazeWarningKind::LiteralFieldValueIncorrect,
                                            format!(LITERAL_FIELD_VALUE_INCORRECT_MSG!(), value)
                                                .into(),
                                            known_block_source_span,
                                        ),
                                        None,
                                    ),
                                    GrazeMessageSetting::Warnings,
                                );
                            }
                        }
                        _ => (),
                    }
                    (Sb3InputRepr::PrimitiveBlock(sb3_primitive_block), is_shadow)
                }
                grazelang_library::KnownBlockInput::BlockRef(id) => {
                    (Sb3InputRepr::Reference(id.to_string()), IsShadow::No)
                }
                grazelang_library::KnownBlockInput::SimpleBlock(opcode, params) => (
                    Sb3InputRepr::Reference(
                        introduce_input_simple_block(opcode, params.iter(), context)?.to_string(),
                    ),
                    IsShadow::No,
                ),
                grazelang_library::KnownBlockInput::Menu(input_menu_value, categories) => {
                    if !categories.contains(category) {
                        emit_message(
                            context,
                            GrazeMessage::Warning(
                                GrazeWarning::Specific(
                                    GrazeWarningKind::LiteralFieldValueIncorrect,
                                    format!(LITERAL_FIELD_VALUE_INCORRECT_MSG!(), value).into(),
                                    known_block_source_span,
                                ),
                                None,
                            ),
                            GrazeMessageSetting::Warnings,
                        );
                    }
                    (
                        Sb3InputRepr::Reference(
                            introduce_input_menu(opcode, field_name, input_menu_value, context)
                                .to_string(),
                        ),
                        IsShadow::Yes,
                    )
                }
                grazelang_library::KnownBlockInput::Empty => (
                    Sb3InputRepr::Reference(
                        introduce_input_menu(opcode, field_name, default.clone(), context)
                            .to_string(),
                    ),
                    IsShadow::Yes,
                ),
            };
            inputs.insert(
                param_name,
                if is_shadow == IsShadow::Yes {
                    Sb3InputValue::Shadow(input_repr)
                } else {
                    Sb3InputValue::ObscuredShadow {
                        value: input_repr,
                        shadow: Sb3InputRepr::Reference(
                            introduce_input_menu(opcode, field_name, default.clone(), context)
                                .to_string(),
                        ),
                    }
                },
            );
        }
        CallBlockParamKind::BlockStack => {
            return Err(GrazeSb3GeneratorError::TriedGetKnownBlockOfBlockStack {
                source_span: known_block_source_span,
            });
        }
    }
    Ok(())
}

pub fn add_params<'a, I>(
    context: &mut GrazeSb3GeneratorContext,
    params: I,
    inputs: &mut HashMap<String, Sb3InputValue>,
    fields: &mut HashMap<String, Sb3FieldValue>,
) -> Result<(), GrazeSb3GeneratorError>
where
    I: Iterator<Item = &'a (CallBlockParam, KnownBlock)>,
{
    for (param, value) in params {
        add_known_block_to_params(context, param, value, Default::default(), inputs, fields)?;
    }
    Ok(())
}

pub fn introduce_input_simple_block<'a, I>(
    opcode: &IString,
    params: I,
    context: &mut GrazeSb3GeneratorContext,
) -> Result<IdString, GrazeSb3GeneratorError>
where
    I: Iterator<Item = &'a (CallBlockParam, KnownBlock)>,
{
    wrap_in_reporter(context, |context, parent, this_id| {
        let mut fields = HashMap::new();
        let mut inputs = HashMap::new();
        add_params(context, params, &mut inputs, &mut fields)?;
        add_block(
            context,
            &this_id,
            make_block(parent, opcode.to_string(), inputs, fields, false, None),
        );
        Ok(this_id)
    })
}

pub fn known_block_to_input_repr_no_menu(
    known_block: &KnownBlock,
    known_block_source_span: SourceSpan,
    context: &mut GrazeSb3GeneratorContext,
) -> Result<Option<(Sb3InputRepr, IsShadow)>, GrazeSb3GeneratorError> {
    let known_block_input = known_block.resolve_for_input(known_block_source_span, context);
    match known_block_input {
        grazelang_library::KnownBlockInput::PrimitiveInput(sb3_primitive_block) => {
            let is_shadow = sb3_primitive_block.is_shadow();
            Ok(Some((
                Sb3InputRepr::PrimitiveBlock(sb3_primitive_block),
                is_shadow,
            )))
        }
        grazelang_library::KnownBlockInput::BlockRef(id) => Ok(Some((
            Sb3InputRepr::Reference(id.to_string()),
            IsShadow::No,
        ))),
        grazelang_library::KnownBlockInput::SimpleBlock(opcode, params) => Ok(Some((
            Sb3InputRepr::Reference(
                introduce_input_simple_block(opcode, params.iter(), context)?.to_string(),
            ),
            IsShadow::No,
        ))),
        grazelang_library::KnownBlockInput::Menu(input_menu_value, _) => {
            Err(GrazeSb3GeneratorError::UnexpectedInputMenu {
                input_menu_value,
                source_span: known_block_source_span,
            })
        }
        grazelang_library::KnownBlockInput::Empty => Ok(None),
    }
}

pub fn param_to_input_repr_no_menu(
    param: Param,
    param_source_span: SourceSpan,
    context: &mut GrazeSb3GeneratorContext,
) -> Result<Option<(Sb3InputRepr, IsShadow)>, GrazeSb3GeneratorError> {
    with_known_block!(context, param, param_source_span, value => {
        known_block_to_input_repr_no_menu(value, param_source_span, context)
    })
}

pub fn add_param_to_params(
    context: &mut GrazeSb3GeneratorContext,
    param: &CallBlockParam,
    value: Param,
    known_block_source_span: SourceSpan,
    inputs: &mut HashMap<String, Sb3InputValue>,
    fields: &mut HashMap<String, Sb3FieldValue>,
) -> Result<(), GrazeSb3GeneratorError> {
    with_known_block!(context, value, known_block_source_span, value => {
        add_known_block_to_params(context, param, value, known_block_source_span, inputs, fields)?;
    });
    Ok(())
}

pub fn make_proc_call_mutation(
    mutation: (&IString, &[(IString, HasShadow)], &bool),
) -> Sb3BlockMutation {
    Sb3BlockMutation::ProceduresCall {
        procedure_code: mutation.0.to_string(),
        argument_ids: mutation.1.iter().map(|value| value.0.to_string()).collect(),
        warp: *mutation.2,
    }
}

pub fn add_block(context: &mut GrazeSb3GeneratorContext, id: &IdString, block: Sb3Block) {
    context
        .current_sb3_target
        .as_mut()
        .unwrap() // the visitor should always guarantee there is a target when blocks are added
        .blocks
        .insert(id.to_string(), block);
}

pub fn create_control_block<I>(
    context: &mut GrazeSb3GeneratorContext,
    identifier: &Identifier,
    args: (I, usize),
    args_source_span: SourceSpan,
    substack: (Param, SourceSpan),
    parent: Option<String>,
    this_id: IString,
) -> Result<(), GrazeSb3GeneratorError>
where
    I: Iterator<Item = (Param, SourceSpan)>,
{
    let (args, arg_count) = args;
    let (substack, substack_source_span) = substack;
    let symbol_id = get_symbol_id(context, identifier)?;
    let symbol = &context.symbol_table[symbol_id];
    let known_block = get_known_block(symbol, identifier)?.clone();
    let CallableKnownBlockSignature(opcode, params, known_params, mutation) = known_block
        .resolve_for_call_block(context)
        .ok_or_else(|| GrazeSb3GeneratorError::IdentifierNotCallable {
            identifier: identifier.clone(),
        })?;
    let mut fields = HashMap::new();
    let mut inputs = HashMap::new();
    add_params(context, known_params.iter(), &mut inputs, &mut fields)?;
    if params.len() != arg_count + 1 {
        return Err(GrazeSb3GeneratorError::IncorrectParamCount {
            unexpected: arg_count,
            expected: params.len() - 1,
            source_span: args_source_span,
        });
    }
    let substack_input_name = if let CallBlockParam {
        kind: CallBlockParamKind::BlockStack,
        name,
    } = params.last().unwrap()
    {
        name
    } else {
        return Err(GrazeSb3GeneratorError::BlockIsNotCBlock {
            identifier: identifier.clone(),
        });
    };
    let substack = if let Param::BlockStack(block_ref) = substack {
        block_ref
    } else {
        return Err(GrazeSb3GeneratorError::PassedNormalParamAsBlockStack {
            param: Box::new(substack),
            source_span: substack_source_span,
        });
    };
    for (param, (value, source_span)) in zip(params.iter(), args) {
        add_param_to_params(context, param, value, source_span, &mut inputs, &mut fields)?;
    }
    if let Some(substack) = substack {
        inputs.insert(
            substack_input_name.to_string(),
            Sb3InputValue::NoShadow(Sb3InputRepr::Reference(substack)),
        );
    }
    add_block(
        context,
        &this_id,
        make_block(
            parent,
            opcode.to_string(),
            inputs,
            fields,
            false,
            mutation.map(make_proc_call_mutation),
        ),
    );
    Ok(())
}

pub fn emit_message(
    context: &mut GrazeSb3GeneratorContext,
    message: GrazeMessage,
    message_type: GrazeMessageSetting,
) {
    if context.settings.message_setting >= message_type {
        context.messages.push(message);
    };
}

impl GrazeVisitor<GrazeSb3GeneratorContext, GrazeSb3GeneratorError> for GrazeSb3Generator {
    // Expressions:

    fn visit_expression_binary_operation(
        &self,
        value: (
            &crate::parser::cst::Expression,
            &crate::parser::cst::BinOp,
            &crate::parser::cst::Expression,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_reporter(context, |context, parent, this_id| {
            default_visit_expression_binary_operation(self, value, context)?;
            let (op_b_param, op_a_param) =
                (context.pop_param().unwrap(), context.pop_param().unwrap());
            let BinOpDescriptor {
                opcode,
                operand_a_input_name,
                operand_b_input_name,
                operand_a_default,
                operand_b_default,
                is_negated,
            } = value.1.get_descriptor();
            let mut inputs = HashMap::with_capacity(2);
            if let Some(input_value) = create_input_value(
                param_to_input_repr_no_menu(op_a_param, *value.0.get_source_span(), context)?,
                operand_a_default.as_ref(),
            ) {
                inputs.insert(operand_a_input_name, input_value);
            }
            if let Some(input_value) = create_input_value(
                param_to_input_repr_no_menu(op_b_param, *value.2.get_source_span(), context)?,
                operand_b_default.as_ref(),
            ) {
                inputs.insert(operand_b_input_name, input_value);
            }
            if is_negated {
                let inner_reporter_id = wrap_in_reporter(context, |context, parent, this_id| {
                    add_block(
                        context,
                        &this_id,
                        make_block(
                            parent,
                            opcode.to_string(),
                            inputs,
                            HashMap::new(),
                            false,
                            None,
                        ),
                    );
                    Ok(this_id)
                })?;
                add_block(
                    context,
                    &this_id,
                    make_block(
                        parent,
                        "operator_not".to_string(),
                        HashMap::from([(
                            "OPERAND".to_string(),
                            Sb3InputValue::NoShadow(Sb3InputRepr::Reference(
                                inner_reporter_id.to_string(),
                            )),
                        )]),
                        HashMap::new(),
                        false,
                        None,
                    ),
                );
                context.push_param(Param::Owned(Box::new(this_id.into())));
                return Ok(());
            }

            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    opcode.to_string(),
                    inputs,
                    HashMap::new(),
                    false,
                    None,
                ),
            );
            context.push_param(Param::Owned(Box::new(this_id.into())));
            Ok(())
        })
    }

    fn visit_expression_call(
        &self,
        value: (
            &crate::parser::cst::Identifier,
            &crate::parser::cst::LeftParens,
            &[(
                crate::parser::cst::Expression,
                Option<crate::parser::cst::Comma>,
            )],
            &crate::parser::cst::RightParens,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_reporter(context, |context, parent, this_id| {
            default_visit_expression_call(self, value, context)?;
            let reversed_args = iter::repeat_with(|| context.pop_param().unwrap())
                .take(value.2.len())
                .collect::<Vec<_>>();
            let symbol_id = get_symbol_id(context, value.0)?;
            let symbol = &context.symbol_table[symbol_id];
            let known_block = get_known_block(symbol, value.0)?.clone();
            let CallableKnownBlockSignature(opcode, params, known_params, mutation) = known_block
                .resolve_for_call_block(context)
                .ok_or_else(|| GrazeSb3GeneratorError::IdentifierNotCallable {
                    identifier: value.0.clone(),
                })?;
            let mut fields = HashMap::new();
            let mut inputs = HashMap::new();
            add_params(context, known_params.iter(), &mut inputs, &mut fields)?;
            if params.len() != reversed_args.len() {
                return Err(GrazeSb3GeneratorError::IncorrectParamCount {
                    unexpected: reversed_args.len(),
                    expected: params.len(),
                    source_span: value.0.range_to(value.3),
                });
            }
            for (param, (value, (expr, _))) in
                zip(params.iter(), reversed_args.into_iter().rev().zip(value.2))
            {
                add_param_to_params(
                    context,
                    param,
                    value,
                    *expr.get_source_span(),
                    &mut inputs,
                    &mut fields,
                )?;
            }
            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    opcode.to_string(),
                    inputs,
                    fields,
                    false,
                    mutation.map(make_proc_call_mutation),
                ),
            );
            context.push_param(Param::Owned(Box::new(this_id.into())));
            Ok(())
        })
    }

    fn visit_expression_formatted_string(
        &self,
        value: (
            &[crate::parser::cst::FormattedStringContent],
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        enum FormattedStringTree {
            Expression(SourceSpan),
            String(String),
            EmptyString,
            Joined(
                Box<FormattedStringTree>,
                Box<FormattedStringTree>,
                Option<String>,
                IdString,
            ),
        }
        fn join_recursively(
            context: &mut GrazeSb3GeneratorContext,
            value: &[FormattedStringContent],
        ) -> FormattedStringTree {
            match value.len() {
                0 => FormattedStringTree::EmptyString,
                1 => match &value[0] {
                    FormattedStringContent::Expression(expr) => {
                        FormattedStringTree::Expression(*expr.get_source_span())
                    }
                    FormattedStringContent::String(arc_str, _) => {
                        FormattedStringTree::String(arc_str.to_string())
                    }
                },
                _ => {
                    let mid = value.len() / 2;
                    wrap_in_reporter(context, |context, parent, this_id| {
                        let parent = context
                            .formatted_string_context
                            .ids
                            .pop()
                            .flatten()
                            .or(parent);
                        context
                            .formatted_string_context
                            .ids
                            .push(Some(this_id.to_string()));
                        let left = Box::new(join_recursively(context, &value[..mid]));
                        context
                            .formatted_string_context
                            .ids
                            .push(Some(this_id.to_string()));
                        let right = Box::new(join_recursively(context, &value[mid..]));
                        FormattedStringTree::Joined(left, right, parent, this_id)
                    })
                }
            }
        }
        fn make_join(
            parent: Option<String>,
            left: Option<Sb3InputValue>,
            right: Option<Sb3InputValue>,
        ) -> Sb3Block {
            let mut inputs = HashMap::with_capacity(2);
            if let Some(left) = left {
                inputs.insert("STRING1".to_string(), left);
            }
            if let Some(right) = right {
                inputs.insert("STRING2".to_string(), right);
            }
            make_block(
                parent,
                "operator_join".to_string(),
                inputs,
                HashMap::new(),
                false,
                None,
            )
        }
        fn convert_into_block_tree(
            context: &mut GrazeSb3GeneratorContext,
            value: FormattedStringTree,
        ) -> Result<(Param, SourceSpan), GrazeSb3GeneratorError> {
            match value {
                FormattedStringTree::Expression(source_span) => {
                    Ok((context.pop_param().unwrap(), source_span))
                }
                FormattedStringTree::String(string) => Ok((
                    Param::Owned(Box::new(KnownBlock::PrimitiveBlock {
                        value: string.into(),
                    })),
                    Default::default(),
                )),
                FormattedStringTree::EmptyString => Ok((
                    Param::Owned(Box::new(KnownBlock::PrimitiveBlock { value: "".into() })),
                    Default::default(),
                )),
                FormattedStringTree::Joined(left, right, parent, this_id) => {
                    // right comes first because a stack is LIFO
                    let (right, right_pr) = convert_into_block_tree(context, *right)?;
                    let (left, left_pr) = convert_into_block_tree(context, *left)?;
                    let left = param_to_input_repr_no_menu(left, left_pr, context)?;
                    let right = param_to_input_repr_no_menu(right, right_pr, context)?;
                    let left = create_input_value(
                        left,
                        if context.settings.use_shadows == UseShadows::NotEverywhere {
                            None::<Sb3PrimitiveBlock>
                        } else {
                            Some(Sb3PrimitiveBlock::String(
                                if context.settings.use_shadows
                                    == UseShadows::CorrectShadowsEverywhere
                                {
                                    "apple "
                                } else {
                                    ""
                                }
                                .into(),
                            ))
                        },
                    );
                    let right = create_input_value(
                        right,
                        if context.settings.use_shadows == UseShadows::NotEverywhere {
                            None::<Sb3PrimitiveBlock>
                        } else {
                            Some(Sb3PrimitiveBlock::String(
                                if context.settings.use_shadows
                                    == UseShadows::CorrectShadowsEverywhere
                                {
                                    "banana"
                                } else {
                                    ""
                                }
                                .into(),
                            ))
                        },
                    );
                    add_block(context, &this_id, make_join(parent, left, right));
                    Ok((
                        Param::Owned(Box::new(KnownBlock::BlockRef { id: this_id })),
                        Default::default(),
                    ))
                }
            }
        }
        let section_count = value.0.len();
        let prev_formatted_string_context = std::mem::replace(
            &mut context.formatted_string_context,
            FormattedStringContext {
                ids: if section_count == 1 {
                    vec![context.current_parent.clone()]
                } else {
                    Vec::with_capacity(section_count)
                },
                current_idx: 0,
            },
        );
        let formatted_string = join_recursively(context, value.0);
        let formatted_string = match formatted_string {
            FormattedStringTree::Expression(_) => {
                default_visit_expression_formatted_string(self, value, context)?;
                return Ok(());
            }
            FormattedStringTree::String(string) => {
                default_visit_expression_formatted_string(self, value, context)?;
                context.push_param(Param::Owned(Box::new(KnownBlock::PrimitiveBlock {
                    value: string.into(),
                })));
                return Ok(());
            }
            FormattedStringTree::EmptyString => {
                default_visit_expression_formatted_string(self, value, context)?;
                context.push_param(Param::Owned(Box::new(KnownBlock::PrimitiveBlock {
                    value: "".into(),
                })));
                return Ok(());
            }
            FormattedStringTree::Joined(left, right, parent, this_id) => {
                FormattedStringTree::Joined(left, right, parent, this_id)
            }
        };
        default_visit_expression_formatted_string(self, value, context)?;
        context.formatted_string_context = prev_formatted_string_context;
        let (param, _) = convert_into_block_tree(context, formatted_string)?;
        context.push_param(param);
        Ok(())
    }

    fn visit_formatted_string_content(
        &self,
        value: &FormattedStringContent,
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        context.current_parent = context.formatted_string_context.ids
            [context.formatted_string_context.current_idx]
            .clone();
        context.formatted_string_context.current_idx += 1;
        default_visit_formatted_string_content(self, value, context)
    }

    fn visit_expression_get_item(
        &self,
        value: (
            &crate::parser::cst::Identifier,
            &crate::parser::cst::LeftBracket,
            &crate::parser::cst::Expression,
            &crate::parser::cst::RightBracket,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_reporter(context, |context, parent, this_id| {
            default_visit_expression_get_item(self, value, context)?;
            let index = context.pop_param().unwrap();

            let inputs = if let Some(index_input_value) = create_input_value(
                param_to_input_repr_no_menu(index, *value.2.get_source_span(), context)?,
                Some(Sb3PrimitiveBlock::Integer(Sb3Primitive::Int128(1))),
            ) {
                HashMap::from([("INDEX".to_string(), index_input_value)])
            } else {
                HashMap::new()
            };
            let symbol_id = get_symbol_id(context, value.0)?;
            let symbol = &context.symbol_table[symbol_id];
            let known_block = get_known_block(symbol, value.0)?.clone();
            let (canonical_name, id) = match known_block.as_ref() {
                KnownBlock::List { canonical_name, id } => (canonical_name, id),
                _ => {
                    return Err(GrazeSb3GeneratorError::ListAccessForNonLists {
                        identifier: value.0.clone(),
                    });
                }
            };
            let list_field_value = Sb3FieldValue::WithId {
                value: canonical_name.as_str().into(),
                id: id.to_string(),
            };
            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    "data_itemoflist".to_string(),
                    inputs,
                    HashMap::from([("LIST".to_string(), list_field_value)]),
                    false,
                    None,
                ),
            );
            context.push_param(Param::Owned(Box::new(this_id.into())));
            Ok(())
        })
    }

    fn visit_expression_get_letter(
        &self,
        value: (
            &Expression,
            &crate::parser::cst::LetterAccessLeftBracket,
            &Expression,
            &crate::parser::cst::RightBracket,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_reporter(context, |context, parent, this_id| {
            default_visit_expression_get_letter(self, value, context)?;
            let index = context.pop_param().unwrap();
            let index_input_value = create_input_value(
                param_to_input_repr_no_menu(index, *value.2.get_source_span(), context)?,
                Some(Sb3PrimitiveBlock::PositiveInteger(Sb3Primitive::Int128(1))),
            );
            let string = context.pop_param().unwrap();
            let string_input_value = create_input_value::<Sb3PrimitiveBlock>(
                param_to_input_repr_no_menu(string, *value.0.get_source_span(), context)?,
                Some("apple".into()),
            );
            let mut inputs = HashMap::with_capacity(2);
            if let Some(string_input_value) = string_input_value {
                inputs.insert("STRING".to_string(), string_input_value);
            }
            if let Some(index_input_value) = index_input_value {
                inputs.insert("LETTER".to_string(), index_input_value);
            }
            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    "operator_letter_of".to_string(),
                    inputs,
                    HashMap::new(),
                    false,
                    None,
                ),
            );
            context.push_param(Param::Owned(Box::new(this_id.into())));
            Ok(())
        })
    }

    fn visit_expression_unary_operation(
        &self,
        value: (
            &crate::parser::cst::UnOp,
            &crate::parser::cst::Expression,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_reporter(context, |context, parent, this_id| {
            default_visit_expression_unary_operation(self, value, context)?;
            let operand = context.pop_param().unwrap();
            let UnOpDescriptor {
                opcode,
                operand_input_name,
                extra_inputs,
                field_values,
                default,
            } = value.0.get_descriptor();
            let operand_input_value = create_input_value(
                param_to_input_repr_no_menu(operand, *value.1.get_source_span(), context)?,
                default,
            );
            let mut inputs = HashMap::with_capacity(extra_inputs.len() + 1);
            if let Some(operand_input_value) = operand_input_value {
                inputs.insert(operand_input_name, operand_input_value);
            }
            for (key, value) in extra_inputs {
                inputs.insert(
                    key,
                    Sb3InputValue::Shadow(Sb3InputRepr::PrimitiveBlock(value)),
                );
            }
            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    opcode.to_string(),
                    inputs,
                    field_values,
                    false,
                    None,
                ),
            );
            context.push_param(Param::Owned(Box::new(this_id.into())));
            Ok(())
        })
    }

    fn visit_expression_identifier(
        &self,
        value: &crate::parser::cst::Identifier,
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        default_visit_expression_identifier(self, value, context)?;
        context.push_param(Param::LazyIdentifier(value.clone()));
        Ok(())
    }

    fn visit_expression_literal(
        &self,
        value: &crate::parser::cst::Literal,
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        default_visit_expression_literal(self, value, context)?;
        context.push_param(match value {
            Literal::EmptyExpression(..) => Param::Owned(Box::new(KnownBlock::Empty)),
            _ => Param::Owned(Box::new(KnownBlock::PrimitiveBlock {
                value: value.into(),
            })),
        });
        Ok(())
    }

    // Statements:

    fn visit_statement_assignment(
        &self,
        value: (
            &Identifier,
            &crate::parser::cst::NormalAssignmentOperator,
            &Expression,
            &crate::parser::cst::Semicolon,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_statement(context, |context, parent, this_id| {
            default_visit_statement_assignment(self, value, context)?;
            let symbol_id = get_symbol_id(context, value.0)?;
            let symbol = &context.symbol_table[symbol_id];
            let known_block = get_known_block(symbol, value.0)?.clone();
            let SimpleCallableKnownBlockSignature(opcode, param, known_params) =
                known_block.resolve_for_assignment(context);
            let assignment_value = context.pop_param().unwrap();
            let mut fields = HashMap::new();
            let mut inputs = HashMap::new();
            add_params(context, known_params.iter(), &mut inputs, &mut fields)?;
            add_param_to_params(
                context,
                param,
                assignment_value,
                *value.2.get_source_span(),
                &mut inputs,
                &mut fields,
            )?;
            add_block(
                context,
                &this_id,
                make_block(parent, opcode.to_string(), inputs, fields, false, None),
            );
            Ok(())
        })
    }

    fn visit_statement_multi_input_control(
        &self,
        value: (
            &Identifier,
            &crate::parser::cst::LeftParens,
            &[(Expression, Option<crate::parser::cst::Comma>)],
            &crate::parser::cst::RightParens,
            &crate::parser::cst::CodeBlock,
            Option<&crate::parser::cst::Semicolon>,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_statement(context, |context, parent, this_id| {
            default_visit_statement_multi_input_control(self, value, context)?;
            let substack = context.pop_param().unwrap();
            let reversed_args = iter::repeat_with(|| context.pop_param().unwrap())
                .take(value.2.len())
                .collect::<Vec<_>>();
            let arg_count = reversed_args.len();
            create_control_block(
                context,
                value.0,
                (
                    reversed_args
                        .into_iter()
                        .rev()
                        .zip(value.2.iter().map(|(expr, _)| *expr.get_source_span())),
                    arg_count,
                ),
                value.0.range_to(value.3),
                (substack, *value.4.get_source_span()),
                parent,
                this_id,
            )
        })
    }

    fn visit_statement_single_input_control(
        &self,
        value: (
            &Identifier,
            &Expression,
            &crate::parser::cst::CodeBlock,
            Option<&crate::parser::cst::Semicolon>,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_statement(context, |context, parent, this_id| {
            default_visit_statement_single_input_control(self, value, context)?;
            let substack = context.pop_param().unwrap();
            let arg = context.pop_param().unwrap();
            create_control_block(
                context,
                value.0,
                (iter::once((arg, *value.1.get_source_span())), 1),
                value.0.range_to(value.1),
                (substack, *value.2.get_source_span()),
                parent,
                this_id,
            )
        })
    }

    fn visit_statement_call(
        &self,
        value: (
            &Identifier,
            &crate::parser::cst::LeftParens,
            &[(Expression, Option<crate::parser::cst::Comma>)],
            &crate::parser::cst::RightParens,
            &crate::parser::cst::Semicolon,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_statement(context, |context, parent, this_id| {
            default_visit_statement_call(self, value, context)?;
            let reversed_args = iter::repeat_with(|| context.pop_param().unwrap())
                .take(value.2.len())
                .collect::<Vec<_>>();
            let symbol_id = get_symbol_id(context, value.0)?;
            let symbol = &context.symbol_table[symbol_id];
            let known_block = get_known_block(symbol, value.0)?.clone();
            let CallableKnownBlockSignature(opcode, params, known_params, mutation) = known_block
                .resolve_for_call_block(context)
                .ok_or_else(|| GrazeSb3GeneratorError::IdentifierNotCallable {
                    identifier: value.0.clone(),
                })?;
            let mut fields = HashMap::new();
            let mut inputs = HashMap::new();
            add_params(context, known_params.iter(), &mut inputs, &mut fields)?;
            if params.len() != reversed_args.len() {
                return Err(GrazeSb3GeneratorError::IncorrectParamCount {
                    unexpected: reversed_args.len(),
                    expected: params.len(),
                    source_span: value.0.range_to(value.3),
                });
            }
            for (param, (value, source_span)) in zip(
                params.iter(),
                reversed_args
                    .into_iter()
                    .rev()
                    .zip(value.2.iter().map(|(expr, _)| *expr.get_source_span())),
            ) {
                add_param_to_params(context, param, value, source_span, &mut inputs, &mut fields)?;
            }
            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    opcode.to_string(),
                    inputs,
                    fields,
                    false,
                    mutation.map(make_proc_call_mutation),
                ),
            );
            Ok(())
        })
    }

    fn visit_statement_forever(
        &self,
        value: (
            &Identifier,
            &crate::parser::cst::CodeBlock,
            Option<&crate::parser::cst::Semicolon>,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_statement(context, |context, parent, this_id| {
            default_visit_statement_forever(self, value, context)?;
            let substack = context.pop_param().unwrap();
            create_control_block(
                context,
                value.0,
                (iter::empty(), 0),
                *value.0.get_source_span(),
                (substack, *value.1.get_source_span()),
                parent,
                this_id,
            )
        })
    }

    fn visit_statement_set_item(
        &self,
        value: (
            &Identifier,
            &crate::parser::cst::LeftBracket,
            &Expression,
            &crate::parser::cst::RightBracket,
            &crate::parser::cst::NormalAssignmentOperator,
            &Expression,
            &crate::parser::cst::Semicolon,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        wrap_in_statement(context, |context, parent, this_id| {
            default_visit_statement_set_item(self, value, context)?;
            let item_value = context.pop_param().unwrap();
            let item_value_input_value = create_input_value::<Sb3PrimitiveBlock>(
                param_to_input_repr_no_menu(item_value, *value.5.get_source_span(), context)?,
                Some("thing".into()),
            );
            let index = context.pop_param().unwrap();
            let index_input_value = create_input_value(
                param_to_input_repr_no_menu(index, *value.2.get_source_span(), context)?,
                Some(Sb3PrimitiveBlock::Integer(Sb3Primitive::Int128(1))),
            );
            let symbol_id = get_symbol_id(context, value.0)?;
            let symbol = &context.symbol_table[symbol_id];
            let known_block = get_known_block(symbol, value.0)?.clone();
            let (canonical_name, id) = match known_block.as_ref() {
                KnownBlock::List { canonical_name, id } => (canonical_name, id),
                _ => {
                    return Err(GrazeSb3GeneratorError::ListAccessForNonLists {
                        identifier: value.0.clone(),
                    });
                }
            };
            let list_field_value = Sb3FieldValue::WithId {
                value: canonical_name.as_str().into(),
                id: id.to_string(),
            };
            let mut inputs = HashMap::with_capacity(2);
            if let Some(index_input_value) = index_input_value {
                inputs.insert("INDEX".to_string(), index_input_value);
            }
            if let Some(item_value_input_value) = item_value_input_value {
                inputs.insert("ITEM".to_string(), item_value_input_value);
            }
            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    "data_replaceitemoflist".to_string(),
                    inputs,
                    HashMap::from([("LIST".to_string(), list_field_value)]),
                    false,
                    None,
                ),
            );
            Ok(())
        })
    }

    fn visit_statement_list_assignment(
        &self,
        value: (
            &Identifier,
            &crate::parser::cst::NormalAssignmentOperator,
            &crate::parser::cst::LeftBracket,
            &[(
                crate::parser::cst::ListEntry,
                Option<crate::parser::cst::Comma>,
            )],
            &crate::parser::cst::RightBracket,
            &crate::parser::cst::Semicolon,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        let symbol_id = get_symbol_id(context, value.0)?;
        let symbol = &context.symbol_table[symbol_id];
        let known_block = get_known_block(symbol, value.0)?.clone();
        let (canonical_name, id) = match known_block.as_ref() {
            KnownBlock::List { canonical_name, id } => (canonical_name, id),
            _ => {
                return Err(GrazeSb3GeneratorError::ListAccessForNonLists {
                    identifier: value.0.clone(),
                });
            }
        };
        let list_field_value = Sb3FieldValue::WithId {
            value: canonical_name.as_str().into(),
            id: id.to_string(),
        };
        wrap_in_statement(context, |context, parent, this_id| {
            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    "data_deletealloflist".to_string(),
                    HashMap::new(),
                    HashMap::from([("LIST".to_string(), list_field_value.clone())]),
                    false,
                    None,
                ),
            );
        });
        for item in value.3 {
            match &item.0 {
                ListEntry::Expression(expression) => {
                    wrap_in_statement(context, |context, parent, this_id| {
                        self.visit_expression(expression, context)?;
                        let param = context.pop_param().unwrap();
                        let value = param_to_input_repr_no_menu(
                            param,
                            *expression.get_source_span(),
                            context,
                        )?;
                        let value = create_input_value(
                            value,
                            if context.settings.use_shadows == UseShadows::NotEverywhere {
                                None::<Sb3PrimitiveBlock>
                            } else {
                                Some(Sb3PrimitiveBlock::String(
                                    if context.settings.use_shadows
                                        == UseShadows::CorrectShadowsEverywhere
                                    {
                                        "thing"
                                    } else {
                                        ""
                                    }
                                    .into(),
                                ))
                            },
                        );
                        let inputs = if let Some(value) = value {
                            HashMap::from([("ITEM".to_string(), value)])
                        } else {
                            HashMap::new()
                        };
                        add_block(
                            context,
                            &this_id,
                            make_block(
                                parent,
                                "data_addtolist".to_string(),
                                inputs,
                                HashMap::from([("LIST".to_string(), list_field_value.clone())]),
                                false,
                                None,
                            ),
                        );
                        Ok(())
                    })?;
                }
                ListEntry::Unwrap(literal, _) => {
                    for c in literal.get_string_value().chars() {
                        wrap_in_statement(context, |context, parent, this_id| {
                            add_block(
                                context,
                                &this_id,
                                make_block(
                                    parent,
                                    "data_addtolist".to_string(),
                                    HashMap::from([(
                                        "ITEM".to_string(),
                                        Sb3InputValue::Shadow(Sb3InputRepr::PrimitiveBlock(
                                            Sb3PrimitiveBlock::String(c.to_string().into()),
                                        )),
                                    )]),
                                    HashMap::from([("LIST".to_string(), list_field_value.clone())]),
                                    false,
                                    None,
                                ),
                            );
                            Ok(())
                        })?;
                    }
                }
            }
        }
        Ok(())
    }

    fn visit_statement_data_declaration(
        &self,
        value: (
            &crate::parser::cst::LetKeyword,
            &crate::parser::cst::DataDeclaration,
            &crate::parser::cst::Semicolon,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        let stage_var_symbol = [literal!("vars")];
        let stage_list_symbol = [literal!("lists")];
        let (this_target_var_symbol, this_target_list_symbol) = {
            let current_target = context.current_sb3_target.as_ref().unwrap();
            if current_target.is_stage {
                (
                    (stage_var_symbol[0].clone(), None),
                    (stage_list_symbol[0].clone(), None),
                )
            } else {
                let value: (_, Option<IString>) = (
                    SPRITES_ISTRING.clone(),
                    Some(current_target.name.as_str().into()),
                );
                (value.clone(), value)
            }
        };
        #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
        pub enum SingleAssignment {
            List([Option<IString>; 3], Vec<Sb3Primitive>),
            Var([Option<IString>; 3], Sb3Primitive),
        }
        let assignments: Vec<SingleAssignment> = match value.1 {
            crate::parser::cst::DataDeclaration::Mixed(parent_scope, _, items, _, _)
            | crate::parser::cst::DataDeclaration::Vars(parent_scope, _, _, items, _, _)
            | crate::parser::cst::DataDeclaration::Lists(parent_scope, _, _, items, _, _) => items
                .iter()
                .map(|(value, _)| match value {
                    crate::parser::cst::SingleDataDeclaration::Variable(
                        _,
                        my_scope,
                        _,
                        identifier,
                        _,
                        expression,
                        _,
                    ) => {
                        let var = identifier.to_single().unwrap().0.clone();
                        let path = if matches!(
                            (parent_scope, my_scope),
                            (
                                DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_),
                                DataDeclarationScope::Unset
                            ) | (
                                _,
                                DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_)
                            )
                        ) {
                            [Some(stage_var_symbol[0].clone()), Some(var), None]
                        } else {
                            let (a, b) = this_target_var_symbol.clone();
                            match b {
                                Some(b) => [Some(a), Some(b), Some(var)],
                                None => [Some(a), Some(var), None],
                            }
                        };
                        SingleAssignment::Var(path, expression.calculate_value())
                    }
                    crate::parser::cst::SingleDataDeclaration::EmptyVariable(
                        _,
                        my_scope,
                        _,
                        identifier,
                        _,
                    ) => {
                        let var = identifier.to_single().unwrap().0.clone();
                        let path = if matches!(
                            (parent_scope, my_scope),
                            (
                                DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_),
                                DataDeclarationScope::Unset
                            ) | (
                                _,
                                DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_)
                            )
                        ) {
                            [Some(stage_var_symbol[0].clone()), Some(var), None]
                        } else {
                            let (a, b) = this_target_var_symbol.clone();
                            match b {
                                Some(b) => [Some(a), Some(b), Some(var)],
                                None => [Some(a), Some(var), None],
                            }
                        };
                        SingleAssignment::Var(path, "".into())
                    }
                    crate::parser::cst::SingleDataDeclaration::List(
                        _,
                        my_scope,
                        _,
                        identifier,
                        _,
                        _,
                        items,
                        _,
                        _,
                    ) => {
                        let list = identifier.to_single().unwrap().0.clone();
                        let path = if matches!(
                            (parent_scope, my_scope),
                            (
                                DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_),
                                DataDeclarationScope::Unset
                            ) | (
                                _,
                                DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_)
                            )
                        ) {
                            [Some(stage_list_symbol[0].clone()), Some(list), None]
                        } else {
                            let (a, b) = this_target_list_symbol.clone();
                            match b {
                                Some(b) => [Some(a), Some(b), Some(list)],
                                None => [Some(a), Some(list), None],
                            }
                        };
                        SingleAssignment::List(path, {
                            let mut values = Vec::with_capacity(items.len());
                            for (value, _) in items {
                                match value {
                                    ListEntry::Expression(expression) => {
                                        values.push(expression.calculate_value());
                                    }
                                    ListEntry::Unwrap(literal, _) => {
                                        for c in literal.get_string_value().chars() {
                                            values.push(c.to_string().into());
                                        }
                                    }
                                }
                            }
                            values
                        })
                    }
                    crate::parser::cst::SingleDataDeclaration::EmptyList(
                        _,
                        my_scope,
                        _,
                        identifier,
                        _,
                    ) => {
                        let list = identifier.to_single().unwrap().0.clone();
                        let path = if matches!(
                            (parent_scope, my_scope),
                            (
                                DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_),
                                DataDeclarationScope::Unset
                            ) | (
                                _,
                                DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_)
                            )
                        ) {
                            [Some(stage_list_symbol[0].clone()), Some(list), None]
                        } else {
                            let (a, b) = this_target_list_symbol.clone();
                            match b {
                                Some(b) => [Some(a), Some(b), Some(list)],
                                None => [Some(a), Some(list), None],
                            }
                        };
                        SingleAssignment::List(path, Vec::new())
                    }
                })
                .collect(),
            crate::parser::cst::DataDeclaration::Single(single_data_declaration) => {
                let single_data_declaration = single_data_declaration.as_ref();
                let single_assignment = match single_data_declaration {
                    crate::parser::cst::SingleDataDeclaration::Variable(
                        _,
                        my_scope,
                        _,
                        identifier,
                        _,
                        expression,
                        _,
                    ) => {
                        let var = identifier.to_single().unwrap().0.clone();
                        let path = if matches!(
                            my_scope,
                            DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_)
                        ) {
                            [Some(stage_var_symbol[0].clone()), Some(var), None]
                        } else {
                            let (a, b) = this_target_var_symbol.clone();
                            match b {
                                Some(b) => [Some(a), Some(b), Some(var)],
                                None => [Some(a), Some(var), None],
                            }
                        };
                        SingleAssignment::Var(path, expression.calculate_value())
                    }
                    crate::parser::cst::SingleDataDeclaration::EmptyVariable(
                        _,
                        my_scope,
                        _,
                        identifier,
                        _,
                    ) => {
                        let var = identifier.to_single().unwrap().0.clone();
                        let path = if matches!(
                            my_scope,
                            DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_)
                        ) {
                            [Some(stage_var_symbol[0].clone()), Some(var), None]
                        } else {
                            let (a, b) = this_target_var_symbol.clone();
                            match b {
                                Some(b) => [Some(a), Some(b), Some(var)],
                                None => [Some(a), Some(var), None],
                            }
                        };
                        SingleAssignment::Var(path, "".into())
                    }
                    crate::parser::cst::SingleDataDeclaration::List(
                        _,
                        my_scope,
                        _,
                        identifier,
                        _,
                        _,
                        items,
                        _,
                        _,
                    ) => {
                        let list = identifier.to_single().unwrap().0.clone();
                        let path = if matches!(
                            my_scope,
                            DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_)
                        ) {
                            [Some(stage_list_symbol[0].clone()), Some(list), None]
                        } else {
                            let (a, b) = this_target_list_symbol.clone();
                            match b {
                                Some(b) => [Some(a), Some(b), Some(list)],
                                None => [Some(a), Some(list), None],
                            }
                        };
                        SingleAssignment::List(path, {
                            let mut values = Vec::with_capacity(items.len());
                            for (value, _) in items {
                                match value {
                                    ListEntry::Expression(expression) => {
                                        values.push(expression.calculate_value());
                                    }
                                    ListEntry::Unwrap(literal, _) => {
                                        for c in literal.get_string_value().chars() {
                                            values.push(c.to_string().into());
                                        }
                                    }
                                }
                            }
                            values
                        })
                    }
                    crate::parser::cst::SingleDataDeclaration::EmptyList(
                        _,
                        my_scope,
                        _,
                        identifier,
                        _,
                    ) => {
                        let list = identifier.to_single().unwrap().0.clone();
                        let path = if matches!(
                            my_scope,
                            DataDeclarationScope::Cloud(_) | DataDeclarationScope::Global(_)
                        ) {
                            [Some(stage_list_symbol[0].clone()), Some(list), None]
                        } else {
                            let (a, b) = this_target_list_symbol.clone();
                            match b {
                                Some(b) => [Some(a), Some(b), Some(list)],
                                None => [Some(a), Some(list), None],
                            }
                        };
                        SingleAssignment::List(path, Vec::new())
                    }
                };
                vec![single_assignment]
            }
        };
        for assignment in assignments {
            match assignment {
                SingleAssignment::List(identifier, sb3_primitives) => {
                    let symbol_id = context
                        .resolve_path(identifier.iter().map_while(|value| value.as_ref()))
                        .unwrap();
                    let symbol = &context.symbol_table[symbol_id];
                    let known_block = symbol.known_block.as_ref().unwrap().clone();
                    let (canonical_name, id) = match known_block.as_ref() {
                        KnownBlock::List { canonical_name, id } => (canonical_name, id),
                        _ => unreachable!(),
                    };
                    let list_field_value = Sb3FieldValue::WithId {
                        value: canonical_name.as_str().into(),
                        id: id.to_string(),
                    };
                    wrap_in_statement(context, |context, parent, this_id| {
                        add_block(
                            context,
                            &this_id,
                            make_block(
                                parent,
                                "data_deletealloflist".to_string(),
                                HashMap::new(),
                                HashMap::from([("LIST".to_string(), list_field_value.clone())]),
                                false,
                                None,
                            ),
                        );
                    });
                    for item in sb3_primitives {
                        wrap_in_statement(context, |context, parent, this_id| {
                            let value = Sb3InputValue::Shadow(Sb3InputRepr::PrimitiveBlock(
                                Sb3PrimitiveBlock::String(item),
                            ));
                            add_block(
                                context,
                                &this_id,
                                make_block(
                                    parent,
                                    "data_addtolist".to_string(),
                                    HashMap::from([("ITEM".to_string(), value)]),
                                    HashMap::from([("LIST".to_string(), list_field_value.clone())]),
                                    false,
                                    None,
                                ),
                            );
                            Ok(())
                        })?;
                    }
                }
                SingleAssignment::Var(identifier, sb3_primitive) => {
                    wrap_in_statement(context, |context, parent, this_id| {
                        let symbol_id = context
                            .resolve_path(identifier.iter().map_while(|value| value.as_ref()))
                            .unwrap();
                        let symbol = &context.symbol_table[symbol_id];
                        let known_block = symbol.known_block.as_ref().unwrap().clone();
                        let SimpleCallableKnownBlockSignature(opcode, param, known_params) =
                            known_block.resolve_for_assignment(context);
                        let mut fields = HashMap::new();
                        let mut inputs = HashMap::new();
                        add_params(context, known_params.iter(), &mut inputs, &mut fields)?;
                        add_known_block_to_params(
                            context,
                            param,
                            &KnownBlock::PrimitiveBlock {
                                value: Sb3PrimitiveBlock::String(sb3_primitive),
                            },
                            Default::default(),
                            &mut inputs,
                            &mut fields,
                        )?;
                        add_block(
                            context,
                            &this_id,
                            make_block(parent, opcode.to_string(), inputs, fields, false, None),
                        );
                        Ok(())
                    })?;
                }
            }
        }
        Ok(())
    }

    fn visit_statement_if_else(
        &self,
        value: (
            &(
                crate::parser::cst::SyntacticIf,
                Expression,
                crate::parser::cst::CodeBlock,
            ),
            &[(
                crate::parser::cst::SyntacticElse,
                crate::parser::cst::SyntacticIf,
                Expression,
                crate::parser::cst::CodeBlock,
            )],
            Option<&(
                crate::parser::cst::SyntacticElse,
                crate::parser::cst::CodeBlock,
            )>,
            Option<&crate::parser::cst::Semicolon>,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        pub fn make_if_else_recursively(
            visitor: &GrazeSb3Generator,
            context: &mut GrazeSb3GeneratorContext,
            else_ifs: &[(
                crate::parser::cst::SyntacticElse,
                crate::parser::cst::SyntacticIf,
                Expression,
                crate::parser::cst::CodeBlock,
            )],
            else_value: Option<&(
                crate::parser::cst::SyntacticElse,
                crate::parser::cst::CodeBlock,
            )>,
        ) -> Result<(), GrazeSb3GeneratorError> {
            wrap_in_statement(context, |context, parent, this_id| {
                let use_if_else = else_ifs.len() > 1 || else_value.is_some();
                let opcode = if use_if_else {
                    "control_if_else"
                } else {
                    "control_if"
                };
                let mut inputs = HashMap::with_capacity(if use_if_else { 3 } else { 2 });
                visitor.visit_expression(&else_ifs[0].2, context)?;
                let condition_value = context.pop_param().unwrap();
                if let Some(condition) = create_input_value(
                    param_to_input_repr_no_menu(
                        condition_value,
                        *else_ifs[0].2.get_source_span(),
                        context,
                    )?,
                    None::<Sb3PrimitiveBlock>,
                ) {
                    inputs.insert("CONDITION".to_string(), condition);
                }
                visitor.visit_code_block(&else_ifs[0].3, context)?;
                let first_branch = context.pop_param().unwrap();
                let first_branch = if let Param::BlockStack(block_ref) = first_branch {
                    block_ref
                } else {
                    return Err(GrazeSb3GeneratorError::PassedNormalParamAsBlockStack {
                        param: Box::new(first_branch),
                        source_span: *else_ifs[0].3.get_source_span(),
                    });
                };
                if let Some(first_branch) = first_branch {
                    inputs.insert(
                        "SUBSTACK".to_string(),
                        Sb3InputValue::NoShadow(Sb3InputRepr::Reference(first_branch)),
                    );
                }
                match (else_ifs.len() > 1, else_value) {
                    (true, _) => {
                        context.current_previous_block = None;
                        make_if_else_recursively(visitor, context, &else_ifs[1..], else_value)?;
                        if let Some(Param::BlockStack(Some(inner_id))) = context.pop_param() {
                            inputs.insert(
                                "SUBSTACK2".to_string(),
                                Sb3InputValue::NoShadow(Sb3InputRepr::Reference(inner_id)),
                            );
                        };
                    }
                    (false, Some(else_value)) => {
                        context.current_previous_block = None;
                        visitor.visit_code_block(&else_value.1, context)?;
                        let else_branch = context.pop_param().unwrap();
                        let else_branch = if let Param::BlockStack(block_ref) = else_branch {
                            block_ref
                        } else {
                            return Err(GrazeSb3GeneratorError::PassedNormalParamAsBlockStack {
                                param: Box::new(else_branch),
                                source_span: *else_value.1.get_source_span(),
                            });
                        };
                        if let Some(else_branch) = else_branch {
                            inputs.insert(
                                "SUBSTACK2".to_string(),
                                Sb3InputValue::NoShadow(Sb3InputRepr::Reference(else_branch)),
                            );
                        }
                    }
                    (false, None) => (),
                }
                add_block(
                    context,
                    &this_id,
                    make_block(
                        parent,
                        opcode.to_string(),
                        inputs,
                        HashMap::new(),
                        false,
                        None,
                    ),
                );
                Ok(())
            })
        }
        wrap_in_statement(context, |context, parent, this_id| {
            let use_if_else = !value.1.is_empty() || value.2.is_some();
            let opcode = if use_if_else {
                "control_if_else"
            } else {
                "control_if"
            };
            let mut inputs = HashMap::with_capacity(if use_if_else { 3 } else { 2 });
            self.visit_expression(&value.0.1, context)?;
            let condition_value = context.pop_param().unwrap();
            if let Some(condition) = create_input_value(
                param_to_input_repr_no_menu(
                    condition_value,
                    *value.0.1.get_source_span(),
                    context,
                )?,
                None::<Sb3PrimitiveBlock>,
            ) {
                inputs.insert("CONDITION".to_string(), condition);
            }
            self.visit_code_block(&value.0.2, context)?;
            let first_branch = context.pop_param().unwrap();
            let first_branch = if let Param::BlockStack(block_ref) = first_branch {
                block_ref
            } else {
                return Err(GrazeSb3GeneratorError::PassedNormalParamAsBlockStack {
                    param: Box::new(first_branch),
                    source_span: *value.0.2.get_source_span(),
                });
            };
            if let Some(first_branch) = first_branch {
                inputs.insert(
                    "SUBSTACK".to_string(),
                    Sb3InputValue::NoShadow(Sb3InputRepr::Reference(first_branch)),
                );
            }
            if use_if_else {
                context.current_previous_block = None;
                make_if_else_recursively(self, context, value.1, value.2)?;
                if let Some(Param::BlockStack(Some(inner_id))) = context.pop_param() {
                    inputs.insert(
                        "SUBSTACK2".to_string(),
                        Sb3InputValue::NoShadow(Sb3InputRepr::Reference(inner_id.to_string())),
                    );
                };
            }
            add_block(
                context,
                &this_id,
                make_block(
                    parent,
                    opcode.to_string(),
                    inputs,
                    HashMap::new(),
                    false,
                    None,
                ),
            );
            Ok(())
        })
    }

    // Helpers:

    fn visit_code_block(
        &self,
        value: &crate::parser::cst::CodeBlock,
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        default_visit_code_block(self, value, context)?;
        if value.statements.is_empty() {
            context.push_param(Param::BlockStack(None));
        }
        Ok(())
    }

    // Target statements:

    fn visit_no_input_hat_statement(
        &self,
        value: (
            &Identifier,
            &crate::parser::cst::CodeBlock,
            Option<&crate::parser::cst::Semicolon>,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        context.current_previous_block = None;
        context.current_parent = None;
        context.arg_stack.clear();
        let (parent, this_id) = wrap_in_statement(context, |_, parent, this_id| (parent, this_id));
        let symbol_id = get_symbol_id(context, value.0)?;
        let symbol = &context.symbol_table[symbol_id];
        let known_block = get_known_block(symbol, value.0)?.clone();
        let CallableKnownBlockSignature(opcode, params, known_params, mutation) = known_block
            .resolve_for_call_block(context)
            .ok_or_else(|| GrazeSb3GeneratorError::IdentifierNotCallable {
                identifier: value.0.clone(),
            })?;
        add_block(
            context,
            &this_id,
            make_block(
                parent.clone(),
                opcode.to_string(),
                HashMap::new(), // both of these will be replaced when the argument is available
                HashMap::new(),
                false,
                mutation.map(make_proc_call_mutation),
            ),
        );
        default_visit_no_input_hat_statement(self, value, context)?;
        let mut inputs = HashMap::new();
        let mut fields = HashMap::new();
        add_params(context, known_params.iter(), &mut inputs, &mut fields)?;
        if !params.is_empty() {
            return Err(GrazeSb3GeneratorError::IncorrectParamCount {
                unexpected: 0,
                expected: params.len(),
                source_span: *value.0.get_source_span(),
            });
        }
        let block = context
            .current_sb3_target
            .as_mut()
            .unwrap() // the visitor should always guarantee there is a target when blocks are added
            .blocks
            .get_mut(this_id.as_str());
        set_params_or_unreachable(block, inputs, fields);
        Ok(())
    }

    fn visit_single_input_hat_statement(
        &self,
        value: (
            &Identifier,
            &Expression,
            &crate::parser::cst::CodeBlock,
            Option<&crate::parser::cst::Semicolon>,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        context.current_previous_block = None;
        context.current_parent = None;
        context.arg_stack.clear();
        let (parent, this_id) = wrap_in_statement(context, |_, parent, this_id| (parent, this_id));
        let symbol_id = get_symbol_id(context, value.0)?;
        let symbol = &context.symbol_table[symbol_id];
        let known_block = get_known_block(symbol, value.0)?.clone();
        let CallableKnownBlockSignature(opcode, params, known_params, mutation) = known_block
            .resolve_for_call_block(context)
            .ok_or_else(|| GrazeSb3GeneratorError::IdentifierNotCallable {
                identifier: value.0.clone(),
            })?;
        add_block(
            context,
            &this_id,
            make_block(
                parent.clone(),
                opcode.to_string(),
                HashMap::new(), // both of these will be replaced when the argument is available
                HashMap::new(),
                false,
                mutation.map(make_proc_call_mutation),
            ),
        );
        default_visit_single_input_hat_statement(self, value, context)?;
        let arg = context.pop_param().unwrap();
        let mut fields = HashMap::new();
        let mut inputs = HashMap::new();
        add_params(context, known_params.iter(), &mut inputs, &mut fields)?;
        let skip_param = if params.len() != 1 {
            if params.is_empty() && value.1.is_empty() {
                true
            } else {
                return Err(GrazeSb3GeneratorError::IncorrectParamCount {
                    unexpected: 1,
                    expected: params.len(),
                    source_span: value.0.range_to(value.1),
                });
            }
        } else {
            false
        };
        if !skip_param {
            let param = params.first().unwrap();
            let prev_parent = if let Some(parent) = parent {
                context.current_parent.replace(parent)
            } else {
                None
            };
            add_param_to_params(
                context,
                param,
                arg,
                *value.1.get_source_span(),
                &mut inputs,
                &mut fields,
            )?;

            context.current_parent = prev_parent;
        }
        let block = context
            .current_sb3_target
            .as_mut()
            .unwrap() // the visitor should always guarantee there is a target when blocks are added
            .blocks
            .get_mut(this_id.as_str());
        set_params_or_unreachable(block, inputs, fields);
        Ok(())
    }

    fn visit_multi_input_hat_statement(
        &self,
        value: (
            &Identifier,
            &crate::parser::cst::LeftParens,
            &[(Expression, Option<crate::parser::cst::Comma>)],
            &crate::parser::cst::RightParens,
            &crate::parser::cst::CodeBlock,
            Option<&crate::parser::cst::Semicolon>,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        context.current_previous_block = None;
        context.current_parent = None;
        context.arg_stack.clear();
        let (parent, this_id) = wrap_in_statement(context, |_, parent, this_id| (parent, this_id));
        let symbol_id = get_symbol_id(context, value.0)?;
        let symbol = &context.symbol_table[symbol_id];
        let known_block = get_known_block(symbol, value.0)?.clone();
        let CallableKnownBlockSignature(opcode, params, known_params, mutation) = known_block
            .resolve_for_call_block(context)
            .ok_or_else(|| GrazeSb3GeneratorError::IdentifierNotCallable {
                identifier: value.0.clone(),
            })?;
        add_block(
            context,
            &this_id,
            make_block(
                parent.clone(),
                opcode.to_string(),
                HashMap::new(), // both of these will be replaced when the argument is available
                HashMap::new(),
                false,
                mutation.map(make_proc_call_mutation),
            ),
        );
        default_visit_multi_input_hat_statement(self, value, context)?;
        let reversed_args = iter::repeat_with(|| context.pop_param().unwrap())
            .take(value.2.len())
            .collect::<Vec<_>>();
        let mut fields = HashMap::new();
        let mut inputs = HashMap::new();
        add_params(context, known_params.iter(), &mut inputs, &mut fields)?;
        if params.len() != reversed_args.len() {
            return Err(GrazeSb3GeneratorError::IncorrectParamCount {
                unexpected: reversed_args.len(),
                expected: params.len(),
                source_span: value.0.range_to(value.3),
            });
        }
        let prev_parent = if let Some(parent) = parent {
            context.current_parent.replace(parent)
        } else {
            None
        };
        for (param, (value, source_span)) in zip(
            params.iter(),
            reversed_args
                .into_iter()
                .rev()
                .zip(value.2.iter().map(|(expr, _)| *expr.get_source_span())),
        ) {
            add_param_to_params(context, param, value, source_span, &mut inputs, &mut fields)?;
        }
        context.current_parent = prev_parent;
        let block = context
            .current_sb3_target
            .as_mut()
            .unwrap() // the visitor should always guarantee there is a target when blocks are added
            .blocks
            .get_mut(this_id.as_str());
        set_params_or_unreachable(block, inputs, fields);
        Ok(())
    }

    fn visit_custom_block_definition(
        &self,
        value: (
            Option<&crate::parser::cst::WarpSpecifier>,
            &crate::parser::cst::ProcKeyword,
            Option<&crate::parser::cst::CanonicalIdentifier>,
            &Identifier,
            &crate::parser::cst::LeftParens,
            &[(
                Option<crate::parser::cst::CustomBlockParamKind>,
                Option<crate::parser::cst::CanonicalIdentifier>,
                Identifier,
                Option<crate::parser::cst::Comma>,
            )],
            &crate::parser::cst::RightParens,
            &crate::parser::cst::CodeBlock,
            Option<&crate::parser::cst::Semicolon>,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        context.current_previous_block = None;
        context.current_parent = None;
        context.arg_stack.clear();
        let current_target = context.current_sb3_target.as_ref().unwrap();

        let proc_name = value.3.to_single().unwrap().0.clone();
        let proc_symbol_id = context
            .resolve_path(
                if current_target.is_stage {
                    [SPRITES_ISTRING.clone(), STAGE_ISTRING.clone(), proc_name]
                } else {
                    [
                        SPRITES_ISTRING.clone(),
                        context
                            .current_target_symbol_name
                            .as_ref()
                            .unwrap()
                            .as_str()
                            .into(),
                        proc_name,
                    ]
                }
                .iter(),
            )
            .unwrap();
        let proc_symbol = &context.symbol_table[proc_symbol_id];
        let proc_known_block = proc_symbol.known_block.as_ref().unwrap().clone();
        let (proccode, params, is_warp) = if let KnownBlock::CustomBlock {
            proccode,
            call_params: _,
            params,
            is_warp,
        } = proc_known_block.as_ref()
        {
            (proccode, params, is_warp)
        } else {
            unreachable!()
        };
        wrap_in_statement(context, |context, parent, this_id| {
            let prototype_id = wrap_in_statement(context, |context, parent, this_id| {
                // let mut inputs = HashMap::<String, Sb3InputValue>::with_capacity(value.5.len());
                // for ((param_id, has_shadow), (_, canonical_identifier, identifier, _)) in
                //     zip(params, value.5)
                // {
                //     let block_ref = Sb3InputRepr::Reference(
                //         wrap_in_statement(context, |context, parent, this_id| {
                //             add_block(
                //                 context,
                //                 &this_id,
                //                 make_block(
                //                     parent.clone(),
                //                     if has_shadow == &HasShadow::No {
                //                         "argument_reporter_string_number"
                //                     } else {
                //                         "argument_reporter_boolean"
                //                     }
                //                     .to_string(),
                //                     HashMap::new(),
                //                     HashMap::from([(
                //                         "VALUE".to_string(),
                //                         Sb3FieldValue::Normal(
                //                             canonical_identifier
                //                                 .as_ref()
                //                                 .map(|value| value.name.to_string())
                //                                 .unwrap_or_else(|| {
                //                                     identifier.to_single().unwrap().0.to_string()
                //                                 })
                //                                 .into(),
                //                         ),
                //                     )]),
                //                     false,
                //                     None,
                //                 ),
                //             );
                //             this_id
                //         })
                //         .to_string(),
                //     );
                //     inputs.insert(param_id.to_string(), Sb3InputValue::NoShadow(block_ref));
                // }
                add_block(
                    context,
                    &this_id,
                    make_block(
                        parent.clone(),
                        "procedures_prototype".to_string(),
                        HashMap::new(),
                        HashMap::new(),
                        false,
                        Some(Sb3BlockMutation::ProceduresPrototype {
                            procedure_code: proccode.to_string(),
                            argument_ids: params.iter().map(|value| value.0.to_string()).collect(),
                            warp: *is_warp,
                            argument_names: value
                                .5
                                .iter()
                                .map(|value| {
                                    value
                                        .1
                                        .as_ref()
                                        .map(|value| value.name.to_string())
                                        .unwrap_or_else(|| {
                                            value.2.to_single().unwrap().0.to_string()
                                        })
                                })
                                .collect(),
                            argument_defaults: params
                                .iter()
                                .map(|value| {
                                    serde_json::Value::String(if value.1 == HasShadow::No {
                                        "false".to_string()
                                    } else {
                                        "".to_string()
                                    })
                                })
                                .collect(),
                        }),
                    ),
                );
                Ok(this_id)
            })?;
            add_block(
                context,
                &this_id,
                make_block(
                    parent.clone(),
                    "procedures_definition".to_string(),
                    HashMap::from([(
                        "custom_block".to_string(),
                        Sb3InputValue::NoShadow(Sb3InputRepr::Reference(prototype_id.to_string())),
                    )]),
                    HashMap::new(),
                    false,
                    None,
                ),
            );
            Ok(())
        })?;
        let arguments = value
            .5
            .iter()
            .map(|value| {
                let name = value
                    .1
                    .as_ref()
                    .map(|value| value.name.to_string())
                    .unwrap_or_else(|| value.2.to_single().unwrap().0.to_string());
                (
                    name.as_str().into(),
                    Symbol {
                        known_block: Some(Rc::new(KnownBlock::SingletonReporter {
                            opcode: if matches!(
                                value.0,
                                Some(CustomBlockParamKind {
                                    kind: CustomBlockParamKindValue::Boolean,
                                    source_span: _,
                                })
                            ) {
                                literal!("argument_reporter_boolean")
                            } else {
                                literal!("argument_reporter_string_number")
                            },
                            params: vec![(
                                CallBlockParam {
                                    kind: CallBlockParamKind::Field {
                                        default: None,
                                        category: NO_CATEGORY_ID,
                                    },
                                    name: literal!("VALUE"),
                                },
                                KnownBlock::FieldValue {
                                    value: Sb3FieldValue::Normal(Sb3Primitive::String(name)),
                                    categories: HashSet::from([NO_CATEGORY_ID]),
                                },
                            )],
                            field: None,
                            assign: None,
                            bind_info: None,
                        })),
                        namespace: HashMap::new(),
                        parent: Default::default(),
                    },
                )
            })
            .collect::<HashMap<_, _>>();
        const ARGUMENTS_ISTRING: &IString = &literal!("args");
        let arguments_symbol_id = context.symbol_table.new_child_symbol(
            Default::default(),
            ARGUMENTS_ISTRING.clone(),
            None,
            arguments.len(),
        );
        for (key, arg) in arguments {
            let arg = context.symbol_table.new_symbol(arg);
            context
                .symbol_table
                .insert_child(arguments_symbol_id, key, arg);
        }
        default_visit_custom_block_definition(self, value, context)?;
        context.symbol_table[Default::default()]
            .namespace
            .remove(ARGUMENTS_ISTRING);
        Ok(())
    }

    fn visit_isolated_block(
        &self,
        value: (
            &crate::parser::cst::CodeBlock,
            Option<&crate::parser::cst::Semicolon>,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        context.current_previous_block = None;
        context.current_parent = None;
        context.arg_stack.clear();
        default_visit_isolated_block(self, value, context)
    }

    fn visit_isolated_expression(
        &self,
        value: (
            &crate::parser::cst::LeftParens,
            &Expression,
            &crate::parser::cst::RightParens,
            Option<&crate::parser::cst::Semicolon>,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        context.current_previous_block = None;
        context.current_parent = None;
        context.arg_stack.clear();
        default_visit_isolated_expression(self, value, context)?;
        let param_source_span = *value.1.get_source_span();
        with_known_block!(context, context.pop_param().unwrap(), param_source_span, value => {
            match value.resolve_for_input(param_source_span, context) {
                grazelang_library::KnownBlockInput::PrimitiveInput(mut sb3_primitive_block) => {
                    match &mut sb3_primitive_block {
                        Sb3PrimitiveBlock::Variable { name: _, id: _, x, y } |
                        Sb3PrimitiveBlock::List { name: _, id: _, x, y } => {
                            x.replace(0.0);
                            y.replace(0.0);
                        },
                        // TODO: Warn user when trying to create a top level shadow expression
                        // Issue: #47
                        _ => return Ok(())
                    }
                    wrap_in_reporter(context, |context, _, this_id| {
                        add_block(
                            context,
                            &this_id,
                            Sb3Block::Primitive(sb3_primitive_block)
                        )
                    })
                },
                grazelang_library::KnownBlockInput::BlockRef(_) => (),
                grazelang_library::KnownBlockInput::SimpleBlock(opcode, params) => {
                    introduce_input_simple_block(opcode, params.iter(), context)?;
                },
                grazelang_library::KnownBlockInput::Menu(input_menu_value, _) => return Err(GrazeSb3GeneratorError::UnexpectedInputMenu { input_menu_value, source_span: param_source_span }),
                grazelang_library::KnownBlockInput::Empty => (),
            }
        });
        Ok(())
    }

    // Assets:

    fn visit_top_level_statement_stage(
        &self,
        value: (
            &crate::parser::cst::StageKeyword,
            &crate::parser::cst::StageCodeBlock,
            Option<&crate::parser::cst::Semicolon>,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        let mut stage = context.uninitialized_stage.take().ok_or(
            GrazeSb3GeneratorError::RepeatedStageInitialization {
                stage_keyword: value.0.clone(),
            },
        )?;
        let assets = context.target_attachments.remove("stage").unwrap();
        let my_blocks_symbol_id = static_resolve_identifier!(context, [MY_BLOCKS_ISTRING]);
        for asset in assets {
            match asset {
                TargetAttachment::Costume(costume) => stage.costumes.push(costume),
                TargetAttachment::Sound(sound) => stage.sounds.push(sound),
                TargetAttachment::Var(name, sb3_variable_declaration) => {
                    stage.variables.insert(name, sb3_variable_declaration);
                }
                TargetAttachment::List(name, sb3_list_declaration) => {
                    stage.lists.insert(name, sb3_list_declaration);
                }
                TargetAttachment::CustomBlock(name, custom_block) => {
                    context.symbol_table.new_child_symbol(
                        my_blocks_symbol_id,
                        name,
                        Some(Rc::new(custom_block)),
                        0,
                    );
                }
                TargetAttachment::Broadcast { name, id } => {
                    stage.broadcasts.insert(id, name);
                }
            }
        }
        context.current_sb3_target = Some(stage);
        context.current_target_symbol_name = Some(STAGE_ISTRING.clone());
        default_visit_top_level_statement_stage(self, value, context)?;
        context
            .sb3
            .targets
            .push(context.current_sb3_target.take().unwrap());
        context.symbol_table[my_blocks_symbol_id].namespace.clear();
        Ok(())
    }

    fn visit_top_level_statement_sprite(
        &self,
        value: (
            &crate::parser::cst::SpriteKeyword,
            Option<&crate::parser::cst::CanonicalIdentifier>,
            &Identifier,
            &crate::parser::cst::SpriteCodeBlock,
            Option<&crate::parser::cst::Semicolon>,
            &crate::lexer::SourceSpan,
        ),
        context: &mut GrazeSb3GeneratorContext,
    ) -> Result<(), GrazeSb3GeneratorError> {
        let target_name = value
            .1
            .as_ref()
            .map(|value| value.name.clone())
            .unwrap_or_else(|| value.2.fields.last().unwrap().0.clone());
        let assets = context
            .target_attachments
            .remove(&value.2.to_single().unwrap().0)
            .ok_or_else(|| GrazeSb3GeneratorError::ShadowedSprite {
                identifier: value.2.clone(),
            })?;
        let mut new_sprite = Sb3Target::new_sprite(target_name.to_string());
        let my_blocks_symbol_id = static_resolve_identifier!(context, [MY_BLOCKS_ISTRING]);
        let sprite_symbol_id =
            static_resolve_identifier!(context, [SPRITES_ISTRING, &value.2.to_single().unwrap().0]);
        let inserted_symbols = {
            let mut insert_symbols = Vec::new();
            let root_symbol_id = Default::default();
            for (key, symbol_id) in &context.symbol_table[sprite_symbol_id].namespace {
                if context
                    .symbol_table
                    .get_child(root_symbol_id, key)
                    .is_some()
                {
                    continue;
                }
                insert_symbols.push((key.clone(), *symbol_id));
            }
            for (key, symbol_id) in &insert_symbols {
                context
                    .symbol_table
                    .insert_alias(root_symbol_id, key.clone(), *symbol_id);
            }
            insert_symbols
        };
        for asset in assets {
            match asset {
                TargetAttachment::Costume(costume) => new_sprite.costumes.push(costume),
                TargetAttachment::Sound(sound) => new_sprite.sounds.push(sound),
                TargetAttachment::Var(name, sb3_variable_declaration) => {
                    new_sprite.variables.insert(name, sb3_variable_declaration);
                }
                TargetAttachment::List(name, sb3_list_declaration) => {
                    new_sprite.lists.insert(name, sb3_list_declaration);
                }
                TargetAttachment::CustomBlock(name, custom_block) => {
                    context.symbol_table.new_child_symbol(
                        my_blocks_symbol_id,
                        name,
                        Some(Rc::new(custom_block)),
                        0,
                    );
                }
                TargetAttachment::Broadcast { name, id } => {
                    new_sprite.broadcasts.insert(id, name);
                }
            }
        }
        new_sprite.layer_order = context.sb3.targets.len()
            + if context.uninitialized_stage.is_some() {
                1
            } else {
                0
            };
        context.current_sb3_target = Some(new_sprite);
        context.current_target_symbol_name = Some(value.2.to_single().unwrap().0.clone());
        default_visit_top_level_statement_sprite(self, value, context)?;
        context
            .sb3
            .targets
            .push(context.current_sb3_target.take().unwrap());
        {
            let root_symbol_id = Default::default();
            for (key, _) in inserted_symbols {
                context.symbol_table[root_symbol_id].namespace.remove(&key);
            }
        }
        context.symbol_table[my_blocks_symbol_id].namespace.clear();
        Ok(())
    }
}
