use arcstr::{format as format_istring, literal};
use grazelang_types::project_json;
use serde::{Serialize, de::DeserializeOwned};

use super::core::{
    DetranspilerAsset, DetranspilerBroadcast, DetranspilerCostumeUncommonData,
    DetranspilerSoundUncommonData, DetranspilerVarOrList, DetranspilerVarOrListKind,
};
use crate::{
    ast::types::{self as ast_types},
    detranspiler::core::{DetranspilerMonitor, DetranspilerTargetBlockStack},
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

impl IntoAST<ast_types::StageStatement> for &DetranspilerVarOrList {
    #[inline]
    fn into_ast(self) -> ast_types::StageStatement {
        ast_types::StageStatement::DataDeclaration(self.into_ast())
    }
}

impl IntoAST<ast_types::SpriteStatement> for &DetranspilerVarOrList {
    #[inline]
    fn into_ast(self) -> ast_types::SpriteStatement {
        ast_types::SpriteStatement::DataDeclaration(self.into_ast())
    }
}

pub fn data_to_data_declaration<'a, I>(data: I) -> Vec<ast_types::SingleDataDeclaration>
where
    I: Iterator<Item = &'a DetranspilerVarOrList>,
{
    data.map(IntoAST::<ast_types::SingleDataDeclaration>::into_ast)
        .collect()
}

pub fn data_to_split_data_declaration<'a, I>(
    data: I,
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
            if matches!(&item.kind, DetranspilerVarOrListKind::Variable { .. }) {
                vars.push(item.into_ast());
            } else {
                lists.push(item.into_ast());
            }
            (vars, lists)
        },
    )
}

impl IntoAST<ast_types::DataDeclaration> for &DetranspilerVarOrList {
    #[inline]
    fn into_ast(self) -> ast_types::DataDeclaration {
        ast_types::DataDeclaration::Single(Box::new(self.into_ast()))
    }
}

impl IntoAST<ast_types::SingleDataDeclaration> for &DetranspilerVarOrList {
    fn into_ast(self) -> ast_types::SingleDataDeclaration {
        match &self.kind {
            DetranspilerVarOrListKind::Variable { value } =>
                ast_types::SingleDataDeclaration::Variable {
                    scope: Default::default(),
                    canonical_identifier: self
                        .canonical_name
                        .as_ref()
                        .cloned()
                        .map(ast_types::CanonicalIdentifier::new),
                    identifier: ast_types::SingleIdentifier::new(self.name.clone()),
                    value: (!matches!(value, project_json::Sb3PrimitiveOrBool::String(value) if value.is_empty())).then(|| ast_types::Expression::Literal(value.into())),
                },
            DetranspilerVarOrListKind::List { value } => {
                ast_types::SingleDataDeclaration::List {
                    scope: Default::default(),
                    canonical_identifier: self
                        .canonical_name
                        .as_ref()
                        .cloned()
                        .map(ast_types::CanonicalIdentifier::new),
                    identifier: ast_types::SingleIdentifier::new(self.name.clone()),
                    value: value.iter().map(|value| ast_types::ListEntry::Expression(ast_types::Expression::Literal(value.into()))).collect(),
                }
            },
        }
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
