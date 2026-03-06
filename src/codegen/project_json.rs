use serde::{
    Deserialize, Serialize,
    de::{self, Expected, Visitor},
    ser::{SerializeSeq, SerializeTuple},
};
use std::collections::HashMap;

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
    pub variables: HashMap<String, Sb3VariableDeclaration>,
    pub lists: HashMap<String, Sb3ListDeclaration>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Sb3VariableDeclaration {
    pub name: String,
    pub value: Sb3Primitive,
    pub is_cloud: bool,
}

impl Serialize for Sb3VariableDeclaration {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(if self.is_cloud { 3 } else { 2 }))?;
        seq.serialize_element(&self.name)?;
        seq.serialize_element(&self.value)?;
        if self.is_cloud {
            seq.serialize_element(&true)?;
        }
        seq.end()
    }
}

struct Sb3VariableDeclarationVisitor;

impl<'de> Visitor<'de> for Sb3VariableDeclarationVisitor {
    type Value = Sb3VariableDeclaration;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("an array representing a variable declaration")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let name = seq
            .next_element::<String>()?
            .ok_or_else(|| de::Error::invalid_length(0, &"2 to 3"))?;
        let value = seq
            .next_element::<Sb3Primitive>()?
            .ok_or_else(|| de::Error::invalid_length(1, &"2 to 3"))?;
        let is_cloud = seq.next_element::<bool>()?.unwrap_or(false);
        Ok(Sb3VariableDeclaration {
            name: name,
            value: value,
            is_cloud: is_cloud,
        })
    }
}

impl<'de> Deserialize<'de> for Sb3VariableDeclaration {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_seq(Sb3VariableDeclarationVisitor)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sb3Primitive {
    String(String),
    Int128(i128),
    Int(i64),
    Float(f64), // f128 is not stable i guess
    Bool(bool),
    Null,
}

impl Serialize for Sb3Primitive {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Sb3Primitive::String(s) => serializer.serialize_str(s),
            Sb3Primitive::Int128(i) => serializer.serialize_i128(*i),
            Sb3Primitive::Int(i) => serializer.serialize_i64(*i),
            Sb3Primitive::Float(f) => serializer.serialize_f64(*f),
            Sb3Primitive::Bool(b) => serializer.serialize_bool(*b),
            Sb3Primitive::Null => serializer.serialize_none(),
        }
    }
}

struct Sb3PrimitiveVisitor;

impl<'de> Visitor<'de> for Sb3PrimitiveVisitor {
    type Value = Sb3Primitive;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a json primitive")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error, {
        Ok(Sb3Primitive::String(v.to_string()))
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(Sb3Primitive::String(v))
    }

    fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(Sb3Primitive::Int(v))
    }

    fn visit_i128<E>(self, v: i128) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(Sb3Primitive::Int128(v))
    }

    fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(match v.try_into() {
            Ok(value) => Sb3Primitive::Int(value),
            Err(_) => Sb3Primitive::Int128(v as i128)
        })
    }

    fn visit_u128<E>(self, v: u128) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(Sb3Primitive::Int128(v.try_into().map_err(|_| {
            de::Error::invalid_value(de::Unexpected::Other("integer too big for i128"), &"i128")
        })?))
    }

    fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
        where
            E: de::Error, {
        Ok(Sb3Primitive::Float(v))
    }

    fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
        where
            E: de::Error, {
        Ok(Sb3Primitive::Bool(v))
    }

    fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
        where
            D: de::Deserializer<'de>, {
        deserializer.deserialize_any(self)
    }

    fn visit_none<E>(self) -> Result<Self::Value, E>
        where
            E: de::Error, {
        Ok(Sb3Primitive::Null)
    }
}

impl<'de> Deserialize<'de> for Sb3Primitive {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: de::Deserializer<'de> {
        deserializer.deserialize_any(Sb3PrimitiveVisitor)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Sb3ListDeclaration(pub String, pub Vec<Sb3Primitive>);

type Sb3Block = ();

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Sb3Comment {
    pub block_id: String,
    pub x: f64,
    pub y: f64,
    pub width: f64,
    pub height: f64,
    pub minimized: bool,
    pub text: String,
}

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
