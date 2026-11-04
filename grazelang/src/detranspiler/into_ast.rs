use std::fmt::Write;

use arcstr::{ArcStr as IString, format as format_istring, literal};
use grazelang_types::project_json;
use serde::{Serialize, de::DeserializeOwned};

use super::core::{
    DetranspilerAsset, DetranspilerBroadcast, DetranspilerCostumeUncommonData,
    DetranspilerSoundUncommonData, DetranspilerVarOrList, DetranspilerVarOrListKind,
};
use crate::{
    ast::types::{self as ast_types},
    detranspiler::core::{DetranspilerMonitor, DetranspilerTargetBlockStack},
    names::DetranspilerAssetNamespace,
};

pub trait IntoAST<T> {
    fn into_ast(self) -> T;
}

impl IntoAST<ast_types::TopLevelStatement> for &DetranspilerBroadcast {
    fn into_ast(self) -> ast_types::TopLevelStatement {
        ast_types::TopLevelStatement::BroadcastDeclaration {
            canonical_identifier: self
                .canonical_name
                .as_ref()
                .cloned()
                .map(ast_types::CanonicalIdentifier::new),
            identifier: ast_types::SingleIdentifier::new(self.name.clone()),
        }
    }
}

impl IntoAST<ast_types::StageStatement> for &DetranspilerAsset<DetranspilerCostumeUncommonData> {
    #[inline]
    fn into_ast(self) -> ast_types::StageStatement {
        ast_types::StageStatement::BackdropDeclaration(self.into_ast())
    }
}

impl IntoAST<ast_types::SpriteStatement> for &DetranspilerAsset<DetranspilerCostumeUncommonData> {
    #[inline]
    fn into_ast(self) -> ast_types::SpriteStatement {
        ast_types::SpriteStatement::CostumeDeclaration(self.into_ast())
    }
}

impl IntoAST<ast_types::StageStatement> for &DetranspilerAsset<DetranspilerSoundUncommonData> {
    #[inline]
    fn into_ast(self) -> ast_types::StageStatement {
        ast_types::StageStatement::SoundDeclaration(self.into_ast())
    }
}

impl IntoAST<ast_types::SpriteStatement> for &DetranspilerAsset<DetranspilerSoundUncommonData> {
    #[inline]
    fn into_ast(self) -> ast_types::SpriteStatement {
        ast_types::SpriteStatement::SoundDeclaration(self.into_ast())
    }
}

pub fn assets_to_asset_declaration<'a, I, D>(assets: I) -> ast_types::AssetDeclaration
where
    I: Iterator<Item = &'a DetranspilerAsset<D>>,
    D: std::fmt::Debug + Clone + PartialEq + Serialize + DeserializeOwned + 'a,
    &'a DetranspilerAsset<D>: IntoAST<ast_types::SingleAssetDeclaration>,
{
    ast_types::AssetDeclaration::Multiple(
        assets
            .map(IntoAST::<ast_types::SingleAssetDeclaration>::into_ast)
            .collect::<Vec<_>>(),
    )
}

impl<D> IntoAST<ast_types::AssetDeclaration> for &DetranspilerAsset<D>
where
    D: std::fmt::Debug + Clone + PartialEq + Serialize + DeserializeOwned,
    for<'a> &'a DetranspilerAsset<D>: IntoAST<ast_types::SingleAssetDeclaration>,
{
    #[inline]
    fn into_ast(self) -> ast_types::AssetDeclaration {
        ast_types::AssetDeclaration::Single(self.into_ast())
    }
}

impl<D> IntoAST<ast_types::SingleAssetDeclaration> for &DetranspilerAsset<D>
where
    D: std::fmt::Debug + Clone + PartialEq + Serialize + DeserializeOwned,
    for<'a> &'a DetranspilerAsset<D>: IntoAST<ast_types::SingleAssetDeclarationValue>,
{
    fn into_ast(self) -> ast_types::SingleAssetDeclaration {
        ast_types::SingleAssetDeclaration {
            canonical_identifier: self
                .canonical_name
                .as_ref()
                .cloned()
                .map(ast_types::CanonicalIdentifier::new),
            identifier: ast_types::SingleIdentifier::new(self.name.clone()),
            value: self.into_ast(),
        }
    }
}

impl IntoAST<ast_types::SingleAssetDeclarationValue>
    for &DetranspilerAsset<DetranspilerCostumeUncommonData>
{
    fn into_ast(self) -> ast_types::SingleAssetDeclarationValue {
        let rotation_center_x = self.uncommon_data.rotation_center_x;
        let rotation_center_y = self.uncommon_data.rotation_center_y;
        if rotation_center_x == 0.0 && rotation_center_y == 0.0 {
            return ast_types::SingleAssetDeclarationValue::Simple(self.asset_path.clone());
        }
        let mut entries = Vec::with_capacity(
            1 + (rotation_center_x != 0.0) as usize + (rotation_center_y != 0.0) as usize,
        );
        entries.push(ast_types::DictionaryEntry {
            identifier: ast_types::SingleIdentifier::new(literal!("path")),
            value: ast_types::DictionaryValue::Primitive(ast_types::Literal::String(
                self.asset_path.clone(),
            )),
        });
        if rotation_center_x != 0.0 {
            entries.push(ast_types::DictionaryEntry {
                identifier: ast_types::SingleIdentifier::new(literal!("rotation_center_x")),
                value: ast_types::DictionaryValue::Primitive(ast_types::Literal::DecimalFloat(
                    format_istring!("{rotation_center_x}"),
                )),
            });
        }
        if rotation_center_y != 0.0 {
            entries.push(ast_types::DictionaryEntry {
                identifier: ast_types::SingleIdentifier::new(literal!("rotation_center_y")),
                value: ast_types::DictionaryValue::Primitive(ast_types::Literal::DecimalFloat(
                    format_istring!("{rotation_center_y}"),
                )),
            });
        }
        ast_types::SingleAssetDeclarationValue::Dictionary(entries)
    }
}

impl IntoAST<ast_types::SingleAssetDeclarationValue>
    for &DetranspilerAsset<DetranspilerSoundUncommonData>
{
    #[inline]
    fn into_ast(self) -> ast_types::SingleAssetDeclarationValue {
        ast_types::SingleAssetDeclarationValue::Simple(self.asset_path.clone())
    }
}

pub fn convert_var_or_list_to_stage_statement(
    var_or_list: &DetranspilerVarOrList,
    list_files: &mut Vec<(IString, String)>,
    namespace: &mut DetranspilerAssetNamespace,
    len_threshold: usize,
    item_size_threshold: usize,
) -> ast_types::StageStatement {
    ast_types::StageStatement::DataDeclaration(convert_var_or_list_to_data_declaration(
        var_or_list,
        list_files,
        namespace,
        len_threshold,
        item_size_threshold,
    ))
}

pub fn convert_var_or_list_to_sprite_statement(
    var_or_list: &DetranspilerVarOrList,
    list_files: &mut Vec<(IString, String)>,
    namespace: &mut DetranspilerAssetNamespace,
    len_threshold: usize,
    item_size_threshold: usize,
) -> ast_types::SpriteStatement {
    ast_types::SpriteStatement::DataDeclaration(convert_var_or_list_to_data_declaration(
        var_or_list,
        list_files,
        namespace,
        len_threshold,
        item_size_threshold,
    ))
}

pub fn data_to_data_declaration<'a, I>(
    data: I,
    list_files: &mut Vec<(IString, String)>,
    namespace: &mut DetranspilerAssetNamespace,
    len_threshold: usize,
    item_size_threshold: usize,
) -> Vec<ast_types::SingleDataDeclaration>
where
    I: Iterator<Item = &'a DetranspilerVarOrList>,
{
    data.map(|value| {
        convert_var_or_list(
            value,
            list_files,
            namespace,
            len_threshold,
            item_size_threshold,
        )
    })
    .collect()
}

pub fn data_to_split_data_declaration<'a, I>(
    data: I,
    list_files: &mut Vec<(IString, String)>,
    namespace: &mut DetranspilerAssetNamespace,
    len_threshold: usize,
    item_size_threshold: usize,
) -> (
    Vec<ast_types::SingleDataDeclaration>,
    Vec<ast_types::SingleDataDeclaration>,
)
where
    I: Iterator<Item = &'a DetranspilerVarOrList>,
{
    let cap = data.size_hint().0 / 2 + 1;
    data.fold(
        (Vec::with_capacity(cap), Vec::with_capacity(cap)),
        |(mut vars, mut lists), item| {
            let declaration = convert_var_or_list(
                item,
                list_files,
                namespace,
                len_threshold,
                item_size_threshold,
            );
            if matches!(&item.kind, DetranspilerVarOrListKind::Variable { .. }) {
                vars.push(declaration);
            } else {
                lists.push(declaration);
            }
            (vars, lists)
        },
    )
}

pub fn convert_var_or_list_to_data_declaration(
    var_or_list: &DetranspilerVarOrList,
    list_files: &mut Vec<(IString, String)>,
    namespace: &mut DetranspilerAssetNamespace,
    len_threshold: usize,
    item_size_threshold: usize,
) -> ast_types::DataDeclaration {
    ast_types::DataDeclaration::Single(Box::new(convert_var_or_list(
        var_or_list,
        list_files,
        namespace,
        len_threshold,
        item_size_threshold,
    )))
}

fn random_str(buf: &mut [u8; 64]) -> &str {
    use rand::{
        Rng, SeedableRng,
        rngs::{StdRng, SysRng},
    };
    use std::sync::{LazyLock, Mutex};
    static RNG: LazyLock<Mutex<StdRng>> =
        LazyLock::new(|| Mutex::new(StdRng::try_from_rng(&mut SysRng).unwrap()));
    let mut data = [0_u8; 32];
    RNG.lock()
        .unwrap_or_else(|err| err.into_inner())
        .fill_bytes(&mut data);
    base16ct::lower::encode_str(&data, buf).unwrap()
}

pub fn convert_var_or_list(
    var_or_list: &DetranspilerVarOrList,
    list_files: &mut Vec<(IString, String)>,
    namespace: &mut DetranspilerAssetNamespace,
    len_threshold: usize,
    item_size_threshold: usize,
) -> ast_types::SingleDataDeclaration {
    match &var_or_list.kind {
        DetranspilerVarOrListKind::Variable { value, is_cloud } =>
            ast_types::SingleDataDeclaration::Variable {
                scope: is_cloud.then_some(ast_types::DataDeclarationScope::Cloud).unwrap_or_default(),
                canonical_identifier: var_or_list
                    .canonical_name
                    .as_ref()
                    .cloned()
                    .map(ast_types::CanonicalIdentifier::new),
                identifier: ast_types::SingleIdentifier::new(var_or_list.name.clone()),
                value: (!matches!(value, project_json::Sb3PrimitiveOrBool::String(value) if value.is_empty())).then(|| ast_types::Expression::Literal(value.into())),
            },
        DetranspilerVarOrListKind::List { value } => {
            let (list_declaration_len, max_unwrap_len) = get_converted_list_lengths(value.iter());
            if list_declaration_len >= len_threshold || max_unwrap_len >= item_size_threshold || value.iter().any(|value| match value {
                project_json::Sb3PrimitiveOrBool::String(value) => value.len() >= item_size_threshold,
                _ => false,
            }) {
                let mut buf = [0_u8; 64];
                let identifier = random_str(&mut buf);
                let asset_name = namespace.get_symbol(var_or_list.get_original_name(), identifier);
                let mut list_value = String::new();
                let mut value = value.iter();
                if let Some(i) = value.next() {
                    list_value.write_fmt(format_args!("{}", i)).unwrap();
                }
                for i in value {
                    list_value.write_fmt(format_args!("\n{}", i)).unwrap();
                }
                list_files.push((asset_name.clone(), list_value));
                return ast_types::SingleDataDeclaration::FileList {
                    scope: Default::default(),
                    canonical_identifier: var_or_list
                        .canonical_name
                        .as_ref()
                        .cloned()
                        .map(ast_types::CanonicalIdentifier::new),
                    identifier: ast_types::SingleIdentifier::new(var_or_list.name.clone()),
                    source: ast_types::Expression::Literal(ast_types::Literal::String(asset_name)),
                };
            }
            ast_types::SingleDataDeclaration::List {
                scope: Default::default(),
                canonical_identifier: var_or_list
                    .canonical_name
                    .as_ref()
                    .cloned()
                    .map(ast_types::CanonicalIdentifier::new),
                identifier: ast_types::SingleIdentifier::new(var_or_list.name.clone()),
                value: convert_list_value(value.iter()),
            }
        },
    }
}

impl IntoAST<ast_types::StageStatement> for DetranspilerMonitor {
    #[inline]
    fn into_ast(self) -> ast_types::StageStatement {
        let (value, configuration) = self.into_ast();
        ast_types::StageStatement::MonitorDeclaration {
            value,
            configuration,
        }
    }
}

impl IntoAST<ast_types::SpriteStatement> for DetranspilerMonitor {
    #[inline]
    fn into_ast(self) -> ast_types::SpriteStatement {
        let (value, configuration) = self.into_ast();
        ast_types::SpriteStatement::MonitorDeclaration {
            value,
            configuration,
        }
    }
}

impl IntoAST<(ast_types::MonitorValue, Vec<ast_types::DictionaryEntry>)> for DetranspilerMonitor {
    #[inline]
    fn into_ast(self) -> (ast_types::MonitorValue, Vec<ast_types::DictionaryEntry>) {
        (self.value, self.config)
    }
}

impl IntoAST<ast_types::StageStatement> for DetranspilerTargetBlockStack {
    fn into_ast(self) -> ast_types::StageStatement {
        match self {
            DetranspilerTargetBlockStack::HatBlock {
                hat_function,
                arguments,
                code_block,
            } => ast_types::StageStatement::HatStatement {
                hat_function,
                arguments,
                code_block,
            },
            DetranspilerTargetBlockStack::CustomBlock {
                is_warp,
                canonical_identifier,
                identifier,
                parameters,
                code_block,
            } => ast_types::StageStatement::CustomBlockDefinition {
                is_warp,
                canonical_identifier,
                identifier,
                parameters,
                code_block,
            },
            DetranspilerTargetBlockStack::IsolatedStack { code_block } => {
                ast_types::StageStatement::IsolatedBlock(code_block)
            }
            DetranspilerTargetBlockStack::IsolatedExpression { expression } => {
                ast_types::StageStatement::IsolatedExpression(expression)
            }
        }
    }
}

impl IntoAST<ast_types::SpriteStatement> for DetranspilerTargetBlockStack {
    fn into_ast(self) -> ast_types::SpriteStatement {
        match self {
            DetranspilerTargetBlockStack::HatBlock {
                hat_function,
                arguments,
                code_block,
            } => ast_types::SpriteStatement::HatStatement {
                hat_function,
                arguments,
                code_block,
            },
            DetranspilerTargetBlockStack::CustomBlock {
                is_warp,
                canonical_identifier,
                identifier,
                parameters,
                code_block,
            } => ast_types::SpriteStatement::CustomBlockDefinition {
                is_warp,
                canonical_identifier,
                identifier,
                parameters,
                code_block,
            },
            DetranspilerTargetBlockStack::IsolatedStack { code_block } => {
                ast_types::SpriteStatement::IsolatedBlock(code_block)
            }
            DetranspilerTargetBlockStack::IsolatedExpression { expression } => {
                ast_types::SpriteStatement::IsolatedExpression(expression)
            }
        }
    }
}

pub fn convert_list_value<'a, I>(iter: I) -> Vec<ast_types::ListEntry>
where
    I: Iterator<Item = &'a project_json::Sb3PrimitiveOrBool>,
{
    const LENGTH_THRESHOLD: usize = 3;
    let mut entries = Vec::new();
    let mut current_string = String::new();
    let mut current_string_chars = 0;
    for value in iter {
        if let project_json::Sb3PrimitiveOrBool::String(value) = value
            && value
                .chars()
                .try_fold(0, |state, _| (state == 0).then_some(1))
                == Some(1)
        {
            current_string.push_str(value);
            current_string_chars += 1;
        } else {
            if current_string_chars >= LENGTH_THRESHOLD {
                entries.reserve(2);
                entries.push(ast_types::ListEntry::Unwrap(ast_types::Literal::String(
                    current_string.as_str().into(),
                )));
            } else if current_string_chars > 0 {
                entries.reserve(current_string_chars + 1);
                for c in current_string.chars() {
                    entries.push(ast_types::ListEntry::Expression(
                        ast_types::Expression::Literal(ast_types::Literal::String(
                            format_istring!("{c}"),
                        )),
                    ));
                }
            }
            current_string.clear();
            current_string_chars = 0;
            entries.push(ast_types::ListEntry::Expression(
                ast_types::Expression::Literal(value.into()),
            ));
        }
    }
    if current_string_chars >= LENGTH_THRESHOLD {
        entries.push(ast_types::ListEntry::Unwrap(ast_types::Literal::String(
            current_string.as_str().into(),
        )));
    } else if current_string_chars > 0 {
        entries.reserve(current_string_chars);
        for c in current_string.chars() {
            entries.push(ast_types::ListEntry::Expression(
                ast_types::Expression::Literal(ast_types::Literal::String(format_istring!("{c}"))),
            ));
        }
    }
    entries
}

fn get_converted_list_lengths<'a, I>(iter: I) -> (usize, usize)
where
    I: Iterator<Item = &'a project_json::Sb3PrimitiveOrBool>,
{
    const LENGTH_THRESHOLD: usize = 3;
    let mut max_unwrap_len = 0_usize;
    let mut entries = 0;
    let mut current_string_chars = 0;
    for value in iter {
        if let project_json::Sb3PrimitiveOrBool::String(value) = value
            && value
                .chars()
                .try_fold(0, |state, _| (state == 0).then_some(1))
                == Some(1)
        {
            current_string_chars += 1;
        } else {
            if current_string_chars >= LENGTH_THRESHOLD {
                entries += 1;
                if current_string_chars > max_unwrap_len {
                    max_unwrap_len = current_string_chars;
                }
            } else if current_string_chars > 0 {
                entries += current_string_chars;
            }
            current_string_chars = 0;
            entries += 1;
        }
    }
    if current_string_chars >= LENGTH_THRESHOLD {
        entries += 1;
        if current_string_chars > max_unwrap_len {
            max_unwrap_len = current_string_chars;
        }
    } else if current_string_chars > 0 {
        entries += current_string_chars;
    }
    (entries, max_unwrap_len)
}
