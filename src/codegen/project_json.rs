use serde::{
    Deserialize, Serialize,
    de::{self, Expected, Visitor},
    ser::{SerializeSeq, SerializeTuple},
};
use serde_json::Value;
use serde_repr::{Deserialize_repr, Serialize_repr};
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
        E: de::Error,
    {
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
            Err(_) => Sb3Primitive::Int128(v as i128),
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
        E: de::Error,
    {
        Ok(Sb3Primitive::Float(v))
    }

    fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(Sb3Primitive::Bool(v))
    }

    fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer.deserialize_any(self)
    }

    fn visit_none<E>(self) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(Sb3Primitive::Null)
    }
}

impl<'de> Deserialize<'de> for Sb3Primitive {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer.deserialize_any(Sb3PrimitiveVisitor)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Sb3ListDeclaration(pub String, pub Vec<Sb3Primitive>);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Sb3Block {
    pub opcode: String,
    pub next: Option<String>,
    pub parent: Option<String>,
    pub inputs: HashMap<String, Sb3InputValue>,
    pub fields: HashMap<String, Sb3FieldValue>,
    pub shadow: bool,
    pub top_level: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mutation: Option<Sb3BlockMutation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub x: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub y: Option<f64>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sb3InputValue {
    Shadow(Sb3InputRepr),   // tag: 1
    NoShadow(Sb3InputRepr), // tag: 2
    ObscuredShadow {
        // tag: 3
        value: Sb3InputRepr,
        shadow: Sb3InputRepr,
    },
}

impl Serialize for Sb3InputValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Sb3InputValue::Shadow(sb3_input_repr) => {
                let mut seq = serializer.serialize_seq(Some(2))?;
                seq.serialize_element(&1_u8)?;
                seq.serialize_element(sb3_input_repr)?;
                seq.end()
            }
            Sb3InputValue::NoShadow(sb3_input_repr) => {
                let mut seq = serializer.serialize_seq(Some(2))?;
                seq.serialize_element(&2_u8)?;
                seq.serialize_element(sb3_input_repr)?;
                seq.end()
            }
            Sb3InputValue::ObscuredShadow { value, shadow } => {
                let mut seq = serializer.serialize_seq(Some(3))?;
                seq.serialize_element(&3_u8)?;
                seq.serialize_element(value)?;
                seq.serialize_element(shadow)?;
                seq.end()
            }
        }
    }
}

struct Sb3InputValueVisitor;

impl<'de> Visitor<'de> for Sb3InputValueVisitor {
    type Value = Sb3InputValue;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("an input value")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: de::SeqAccess<'de>,
    {
        let kind = seq
            .next_element::<u8>()?
            .ok_or_else(|| de::Error::invalid_length(0, &"2 to 3"))?;
        Ok(match kind {
            1 => Sb3InputValue::Shadow(
                seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(1, &"2"))?,
            ),
            2 => Sb3InputValue::NoShadow(
                seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(1, &"2"))?,
            ),
            3 => Sb3InputValue::ObscuredShadow {
                value: seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(1, &"3"))?,
                shadow: seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(2, &"3"))?,
            },
            _ => {
                return Err(de::Error::invalid_value(
                    de::Unexpected::Signed(kind as i64),
                    &"1 to 3",
                ));
            }
        })
    }
}

impl<'de> Deserialize<'de> for Sb3InputValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer.deserialize_seq(Sb3InputValueVisitor)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sb3InputRepr {
    Reference(String),
    PrimitiveBlock(Sb3PrimitiveBlock),
}

impl Serialize for Sb3InputRepr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Sb3InputRepr::Reference(id) => serializer.serialize_str(id),
            Sb3InputRepr::PrimitiveBlock(primitive_block) => {
                serializer.serialize_some(primitive_block)
            }
        }
    }
}

struct Sb3InputReprVisitor;

impl<'de> Visitor<'de> for Sb3InputReprVisitor {
    type Value = Sb3InputRepr;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("an input representation")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(Sb3InputRepr::Reference(v.to_string()))
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(Sb3InputRepr::Reference(v))
    }

    fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
    where
        A: de::SeqAccess<'de>,
    {
        Ok(Sb3InputRepr::PrimitiveBlock(
            Sb3PrimitiveBlockVisitor.visit_seq(seq)?,
        ))
    }

    fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer.deserialize_any(self)
    }
}

impl<'de> Deserialize<'de> for Sb3InputRepr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer.deserialize_any(Sb3InputReprVisitor)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sb3PrimitiveBlock {
    Number(Sb3Primitive),          // tag: 4
    PositiveNumber(Sb3Primitive),  // tag: 5
    PositiveInteger(Sb3Primitive), // tag: 6
    Integer(Sb3Primitive),         // tag: 7
    Angle(Sb3Primitive),           // tag: 8
    Color(Sb3Primitive),           // tag: 9
    String(Sb3Primitive),          // tag: 10
    Broadcast {
        name: String,
        id: String,
    }, // tag: 11
    Variable {
        name: String,
        id: String,
        x: Option<f64>,
        y: Option<f64>,
    }, // tag: 12
    List {
        name: String,
        id: String,
        x: Option<f64>,
        y: Option<f64>,
    }, // tag: 13
}

impl Serialize for Sb3PrimitiveBlock {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Sb3PrimitiveBlock::Number(sb3_primitive) => {
                let mut seq = serializer.serialize_seq(Some(2))?;
                seq.serialize_element(&4_u8)?;
                seq.serialize_element(sb3_primitive)?;
                seq.end()
            } // tag: 4
            Sb3PrimitiveBlock::PositiveNumber(sb3_primitive) => {
                let mut seq = serializer.serialize_seq(Some(2))?;
                seq.serialize_element(&5_u8)?;
                seq.serialize_element(sb3_primitive)?;
                seq.end()
            } // tag: 5
            Sb3PrimitiveBlock::PositiveInteger(sb3_primitive) => {
                let mut seq = serializer.serialize_seq(Some(2))?;
                seq.serialize_element(&6_u8)?;
                seq.serialize_element(sb3_primitive)?;
                seq.end()
            } // tag: 6
            Sb3PrimitiveBlock::Integer(sb3_primitive) => {
                let mut seq = serializer.serialize_seq(Some(2))?;
                seq.serialize_element(&7_u8)?;
                seq.serialize_element(sb3_primitive)?;
                seq.end()
            } // tag: 7
            Sb3PrimitiveBlock::Angle(sb3_primitive) => {
                let mut seq = serializer.serialize_seq(Some(2))?;
                seq.serialize_element(&8_u8)?;
                seq.serialize_element(sb3_primitive)?;
                seq.end()
            } // tag: 8
            Sb3PrimitiveBlock::Color(sb3_primitive) => {
                let mut seq = serializer.serialize_seq(Some(2))?;
                seq.serialize_element(&9_u8)?;
                seq.serialize_element(sb3_primitive)?;
                seq.end()
            } // tag: 9
            Sb3PrimitiveBlock::String(sb3_primitive) => {
                let mut seq = serializer.serialize_seq(Some(2))?;
                seq.serialize_element(&10_u8)?;
                seq.serialize_element(sb3_primitive)?;
                seq.end()
            } // tag: 10
            Sb3PrimitiveBlock::Broadcast { name, id } => {
                let mut seq = serializer.serialize_seq(Some(3))?;
                seq.serialize_element(&11_u8)?;
                seq.serialize_element(name)?;
                seq.serialize_element(id)?;
                seq.end()
            } // tag: 11
            Sb3PrimitiveBlock::Variable { name, id, x, y } => {
                let mut seq = serializer.serialize_seq(Some(if x == &None { 3 } else { 5 }))?;
                seq.serialize_element(&12_u8)?;
                seq.serialize_element(name)?;
                seq.serialize_element(id)?;
                if x != &None {
                    seq.serialize_element(x)?;
                    seq.serialize_element(y)?;
                }
                seq.end()
            } // tag: 12
            Sb3PrimitiveBlock::List { name, id, x, y } => {
                let mut seq = serializer.serialize_seq(Some(if x == &None { 3 } else { 5 }))?;
                seq.serialize_element(&13_u8)?;
                seq.serialize_element(name)?;
                seq.serialize_element(id)?;
                if x != &None {
                    seq.serialize_element(x)?;
                    seq.serialize_element(y)?;
                }
                seq.end()
            } // tag: 13
        }
    }
}

struct Sb3PrimitiveBlockVisitor;

impl<'de> Visitor<'de> for Sb3PrimitiveBlockVisitor {
    type Value = Sb3PrimitiveBlock;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a primitive block")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: de::SeqAccess<'de>,
    {
        let tag = seq
            .next_element::<u8>()?
            .ok_or_else(|| de::Error::invalid_length(0, &"2 to 5"))?;
        Ok(match tag {
            4 => {
                let value = seq
                    .next_element::<Sb3Primitive>()?
                    .ok_or_else(|| de::Error::invalid_length(1, &"2"))?;
                Sb3PrimitiveBlock::Number(value)
            } // tag: 4
            5 => {
                let value = seq
                    .next_element::<Sb3Primitive>()?
                    .ok_or_else(|| de::Error::invalid_length(1, &"2"))?;
                Sb3PrimitiveBlock::PositiveNumber(value)
            } // tag: 5
            6 => {
                let value = seq
                    .next_element::<Sb3Primitive>()?
                    .ok_or_else(|| de::Error::invalid_length(1, &"2"))?;
                Sb3PrimitiveBlock::PositiveInteger(value)
            } // tag: 6
            7 => {
                let value = seq
                    .next_element::<Sb3Primitive>()?
                    .ok_or_else(|| de::Error::invalid_length(1, &"2"))?;
                Sb3PrimitiveBlock::Integer(value)
            } // tag: 7
            8 => {
                let value = seq
                    .next_element::<Sb3Primitive>()?
                    .ok_or_else(|| de::Error::invalid_length(1, &"2"))?;
                Sb3PrimitiveBlock::Angle(value)
            } // tag: 8
            9 => {
                let value = seq
                    .next_element::<Sb3Primitive>()?
                    .ok_or_else(|| de::Error::invalid_length(1, &"2"))?;
                Sb3PrimitiveBlock::Color(value)
            } // tag: 9
            10 => {
                let value = seq
                    .next_element::<Sb3Primitive>()?
                    .ok_or_else(|| de::Error::invalid_length(1, &"2"))?;
                Sb3PrimitiveBlock::String(value)
            } // tag: 10
            11 => {
                let name = seq
                    .next_element::<String>()?
                    .ok_or_else(|| de::Error::invalid_length(1, &"3"))?;
                let id = seq
                    .next_element::<String>()?
                    .ok_or_else(|| de::Error::invalid_length(2, &"3"))?;
                Sb3PrimitiveBlock::Broadcast { name, id }
            } // tag: 11
            12 => {
                let name = seq
                    .next_element::<String>()?
                    .ok_or_else(|| de::Error::invalid_length(1, &"3 or 5"))?;
                let id = seq
                    .next_element::<String>()?
                    .ok_or_else(|| de::Error::invalid_length(2, &"3 or 5"))?;
                let x = seq.next_element::<f64>()?;
                let y = x.map_or(Ok(None), |_| seq.next_element::<f64>())?;
                Sb3PrimitiveBlock::Variable { name, id, x, y }
            } // tag: 12
            13 => {
                let name = seq
                    .next_element::<String>()?
                    .ok_or_else(|| de::Error::invalid_length(1, &"3 or 5"))?;
                let id = seq
                    .next_element::<String>()?
                    .ok_or_else(|| de::Error::invalid_length(2, &"3 or 5"))?;
                let x = seq.next_element::<f64>()?;
                let y = x.map_or(Ok(None), |_| seq.next_element::<f64>())?;
                Sb3PrimitiveBlock::List { name, id, x, y }
            } // tag: 13
            _ => {
                return Err(de::Error::invalid_value(
                    de::Unexpected::Signed(tag as i64),
                    &"4 to 13",
                ));
            }
        })
    }
}

impl<'de> Deserialize<'de> for Sb3PrimitiveBlock {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer.deserialize_seq(Sb3PrimitiveBlockVisitor)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sb3FieldValue {
    Normal(Sb3Primitive),
    WithId { value: Sb3Primitive, id: String },
}

impl Serialize for Sb3FieldValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Sb3FieldValue::Normal(sb3_primitive) => {
                let mut seq = serializer.serialize_seq(Some(1))?;
                seq.serialize_element(sb3_primitive)?;
                seq.end()
            }
            Sb3FieldValue::WithId { value, id } => {
                let mut seq = serializer.serialize_seq(Some(2))?;
                seq.serialize_element(value)?;
                seq.serialize_element(id)?;
                seq.end()
            }
        }
    }
}

struct Sb3FieldValueVisitor;

impl<'de> Visitor<'de> for Sb3FieldValueVisitor {
    type Value = Sb3FieldValue;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a field value")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: de::SeqAccess<'de>,
    {
        let value = seq
            .next_element::<Sb3Primitive>()?
            .ok_or_else(|| de::Error::invalid_length(0, &"1 to 2"))?;
        Ok(match seq.next_element::<String>()? {
            None => Sb3FieldValue::Normal(value),
            Some(id) => Sb3FieldValue::WithId { value, id },
        })
    }
}

impl<'de> Deserialize<'de> for Sb3FieldValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer.deserialize_seq(Sb3FieldValueVisitor)
    }
}

type Sb3BlockMutation = ();

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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Sb3Costume {
    pub asset_id: String,
    pub name: String,
    pub md5ext: String,
    pub data_format: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bitmap_resolution: Option<f64>,
    pub rotation_center_x: f64,
    pub rotation_center_y: f64,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Sb3Sound {
    pub asset_id: String,
    pub name: String,
    pub md5ext: String,
    pub data_format: String,
    pub rate: f64,
    pub sample_count: i64,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Sb3Monitor {
    pub id: String,
    pub mode: Sb3MonitorMode,
    pub opcode: String,
    pub params: HashMap<String, Value>,
    pub sprite_name: String,
    pub value: Sb3Primitive,
    pub width: f64,
    pub height: f64,
    pub x: f64,
    pub y: f64,
    pub visible: bool,

    // Non lists
    #[serde(skip_serializing_if = "Option::is_none")]
    pub slider_min: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub slider_max: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_discrete: Option<f64>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Sb3MonitorMode {
    Default,
    Large,
    Slider,
    List,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Sb3Meta {
    pub semver: String,
    pub vm: String,
    pub agent: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub origin: Option<String>,
}
