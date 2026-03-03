use std::collections::HashMap;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Sb3Root {
    pub targets: Vec<Sb3Target>,
    pub monitors: Vec<Sb3Monitor>,
    pub extensions: Vec<String>,
    pub meta: Sb3Meta,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Sb3Target {
    pub is_stage: bool,
    pub name: String,
    pub variables: HashMap<String, Sb3Variable>,
    pub lists: HashMap<String, Sb3List>,
    pub broadcasts: HashMap<String, String>,
    pub blocks: HashMap<String, Sb3Block>,
    pub comments: HashMap<String, Sb3Comment>,
    pub current_costume: i32,
    pub costumes: Vec<Sb3Costume>,
    pub sounds: Vec<Sb3Sound>,
    #[serde(default)]
    pub volume: f64,
    #[serde(default)]
    pub layer_order: i32,
    
    // Stage
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tempo: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub video_transparency: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub video_state: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text_to_speech_language: Option<String>,

    // Sprite
    #[serde(skip_serializing_if = "Option::is_none")]
    pub visible: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub x: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub y: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub size: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub direction: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub draggable: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rotation_style: Option<String>,
}

type Sb3Variable = ();
type Sb3List = ();
type Sb3Block = ();
type Sb3Comment = ();
type Sb3Costume = ();
type Sb3Sound = ();

type Sb3Monitor = ();

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Sb3Meta {
    pub semver: String,
    pub vm: String,
    pub agent: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub origin: Option<String>,
}