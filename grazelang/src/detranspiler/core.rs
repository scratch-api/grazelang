use std::collections::HashMap;

use arcstr::ArcStr as IString;
use grazelang_types::project_json;
use serde::{Deserialize, Serialize, de::DeserializeOwned};

use crate::names::BidirectionalNamespace;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DetranspilerContext {
    pub targets: Vec<DetranspilerTarget>,
    pub asset_namespace: BidirectionalNamespace,
    pub assets: HashMap<AssetPath, OutAssetPath>,
}

type OutAssetPath = String;
type AssetPath = String;
type AssetId = String;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DetranspilerTarget {
    pub costumes: HashMap<AssetId, DetranspilerAsset<DetranspilerCostumeUncommonData>>,
    pub sounds: HashMap<AssetId, DetranspilerAsset<DetranspilerSoundUncommonData>>,
    pub is_stage: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(bound(deserialize = "D: DeserializeOwned"))]
pub struct DetranspilerAsset<D>
where
    D: std::fmt::Debug + Clone + PartialEq + Serialize + DeserializeOwned,
{
    pub name: String,
    pub file_extension: String,
    pub uncommon_data: D,
    pub asset_name: IString,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DetranspilerCostumeUncommonData {
    rotation_center_x: f64,
    rotation_center_y: f64,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DetranspilerSoundUncommonData;

pub fn visit_target(
    target: &project_json::Sb3Target,
    context: &mut DetranspilerContext,
) -> DetranspilerTarget {
    fn get_asset_name_and_register_asset(
        context: &mut DetranspilerContext,
        name: &str,
        asset_id: &str,
        data_format: &str,
        md5ext: &str,
    ) -> IString {
        use std::fmt::Write;
        let asset_name = context.asset_namespace.get_symbol(name, asset_id);
        let mut out_asset_path = String::with_capacity(asset_name.len() + data_format.len() + 1);
        write!(&mut out_asset_path, "{asset_name}.{}", data_format).unwrap();
        if !context.assets.contains_key(md5ext) {
            context.assets.insert(md5ext.to_string(), out_asset_path);
        }
        asset_name
    }
    let costumes = target
        .costumes
        .iter()
        .map(|value| {
            (value.asset_id.clone(), {
                DetranspilerAsset {
                    name: value.name.clone(),
                    file_extension: value.data_format.clone(),
                    uncommon_data: DetranspilerCostumeUncommonData {
                        rotation_center_x: value.rotation_center_x,
                        rotation_center_y: value.rotation_center_y,
                    },
                    asset_name: get_asset_name_and_register_asset(
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
                DetranspilerAsset {
                    name: value.name.clone(),
                    file_extension: value.data_format.clone(),
                    uncommon_data: DetranspilerSoundUncommonData,
                    asset_name: get_asset_name_and_register_asset(
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
    DetranspilerTarget {
        costumes,
        sounds,
        is_stage: target.is_stage,
    }
}
