use serde::{
    Deserialize, Serialize,
    de::{self, Visitor},
    ser::{SerializeMap, SerializeSeq},
};
use serde_json::Value;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Default, Serialize, Deserialize)]
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
    pub text_to_speech_language: Option<String>, // Can seemingly be absent or null

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

impl Sb3Target {
    pub fn new_stage() -> Self {
        Sb3Target {
            is_stage: true,
            name: "Stage".to_string(),
            variables: HashMap::new(),
            lists: HashMap::new(),
            broadcasts: HashMap::new(),
            blocks: HashMap::new(),
            comments: HashMap::new(),
            current_costume: 0,
            costumes: Vec::new(),
            sounds: Vec::new(),
            volume: 100.0,
            layer_order: 0,
            tempo: Some(60.0),
            video_transparency: Some(50.0),
            video_state: Some("on".to_string()),
            text_to_speech_language: None,
            visible: None,
            x: None,
            y: None,
            size: None,
            direction: None,
            draggable: None,
            rotation_style: None,
        }
    }
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

impl<'de> Deserialize<'de> for Sb3VariableDeclaration {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
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
                    name,
                    value,
                    is_cloud,
                })
            }
        }
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

impl From<String> for Sb3Primitive {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<&arcstr::ArcStr> for Sb3Primitive {
    fn from(value: &arcstr::ArcStr) -> Self {
        value.to_string().into()
    }
}

impl From<&str> for Sb3Primitive {
    fn from(value: &str) -> Self {
        value.to_string().into()
    }
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

mod sb3_primitive {
    use super::Sb3Primitive;
    use serde::de::{self, Visitor};

    pub(super) struct Sb3PrimitiveVisitor;

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
}

impl<'de> Deserialize<'de> for Sb3Primitive {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer.deserialize_any(sb3_primitive::Sb3PrimitiveVisitor)
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

impl<'de> Deserialize<'de> for Sb3InputValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
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

impl<'de> Deserialize<'de> for Sb3InputRepr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
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
                    sb3_primitive_block::Sb3PrimitiveBlockVisitor.visit_seq(seq)?,
                ))
            }

            fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
            where
                D: de::Deserializer<'de>,
            {
                deserializer.deserialize_any(self)
            }
        }
        deserializer.deserialize_any(Sb3InputReprVisitor)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sb3PrimitiveBlock {
    /// tag: 4
    Number(Sb3Primitive),
    /// tag: 5
    PositiveNumber(Sb3Primitive),
    /// tag: 6
    PositiveInteger(Sb3Primitive),
    /// tag: 7
    Integer(Sb3Primitive),
    /// tag: 8
    Angle(Sb3Primitive),
    /// tag: 9
    Color(Sb3Primitive),
    /// tag: 10
    String(Sb3Primitive),
    /// tag: 11
    Broadcast { name: String, id: String },
    /// tag: 12
    Variable {
        name: String,
        id: String,
        x: Option<f64>,
        y: Option<f64>,
    },
    /// tag: 13
    List {
        name: String,
        id: String,
        x: Option<f64>,
        y: Option<f64>,
    },
}

impl From<String> for Sb3PrimitiveBlock {
    fn from(value: String) -> Self {
        Self::String(value.into())
    }
}

impl From<&arcstr::ArcStr> for Sb3PrimitiveBlock {
    fn from(value: &arcstr::ArcStr) -> Self {
        value.to_string().into()
    }
}

impl From<&str> for Sb3PrimitiveBlock {
    fn from(value: &str) -> Self {
        value.to_string().into()
    }
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
                let mut seq = serializer.serialize_seq(Some(if x.is_none() { 3 } else { 5 }))?;
                seq.serialize_element(&12_u8)?;
                seq.serialize_element(name)?;
                seq.serialize_element(id)?;
                if x.is_some() {
                    seq.serialize_element(x)?;
                    seq.serialize_element(y)?;
                }
                seq.end()
            } // tag: 12
            Sb3PrimitiveBlock::List { name, id, x, y } => {
                let mut seq = serializer.serialize_seq(Some(if x.is_none() { 3 } else { 5 }))?;
                seq.serialize_element(&13_u8)?;
                seq.serialize_element(name)?;
                seq.serialize_element(id)?;
                if x.is_some() {
                    seq.serialize_element(x)?;
                    seq.serialize_element(y)?;
                }
                seq.end()
            } // tag: 13
        }
    }
}

mod sb3_primitive_block {
    use super::{Sb3Primitive, Sb3PrimitiveBlock};
    use serde::de::{self, Visitor};

    pub(super) struct Sb3PrimitiveBlockVisitor;

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
}
impl<'de> Deserialize<'de> for Sb3PrimitiveBlock {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer.deserialize_seq(sb3_primitive_block::Sb3PrimitiveBlockVisitor)
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
                let mut seq = serializer.serialize_seq(Some(2))?;
                seq.serialize_element(sb3_primitive)?;
                seq.serialize_element(&None::<String>)?;
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

impl<'de> Deserialize<'de> for Sb3FieldValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
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
                Ok(match seq.next_element()? {
                    None => Sb3FieldValue::Normal(value),
                    Some(None) => Sb3FieldValue::Normal(value),
                    Some(Some(id)) => Sb3FieldValue::WithId { value, id },
                })
            }
        }
        deserializer.deserialize_seq(Sb3FieldValueVisitor)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sb3BlockMutation {
    ProceduresCall {
        procedure_code: String,
        argument_ids: Vec<String>,
        warp: bool,
    },
    ProceduresPrototype {
        procedure_code: String,
        argument_ids: Vec<String>,
        warp: bool,
        argument_names: Vec<String>,
        argument_defaults: Vec<Value>,
    },
    ControlStop {
        has_next: bool,
    },
}

impl Serialize for Sb3BlockMutation {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        fn json_encode<S, T>(data: T) -> Result<String, S::Error>
        where
            S: serde::Serializer,
            T: Serialize,
        {
            serde_json::to_string(&data)
                .map_err(|_| serde::ser::Error::custom("could not encode to string"))
        }
        match self {
            Sb3BlockMutation::ProceduresCall {
                procedure_code,
                argument_ids,
                warp,
            } => {
                let mut map = serializer.serialize_map(Some(5))?;
                map.serialize_entry("tagName", "mutation")?;
                map.serialize_entry::<str, [(); 0]>("children", &[])?;
                map.serialize_entry("proccode", procedure_code)?;
                map.serialize_entry("argumentids", &json_encode::<S, _>(argument_ids)?)?;
                map.serialize_entry("warp", warp)?;
                map.end()
            }
            Sb3BlockMutation::ProceduresPrototype {
                procedure_code,
                argument_ids,
                warp,
                argument_names,
                argument_defaults,
            } => {
                let mut map = serializer.serialize_map(Some(7))?;
                map.serialize_entry("tagName", "mutation")?;
                map.serialize_entry::<str, [()]>("children", &[])?;
                map.serialize_entry("proccode", procedure_code)?;
                map.serialize_entry("argumentids", &json_encode::<S, _>(argument_ids)?)?;
                map.serialize_entry("warp", warp)?;
                map.serialize_entry("argumentnames", &json_encode::<S, _>(argument_names)?)?;
                map.serialize_entry("argumentdefaults", &json_encode::<S, _>(argument_defaults)?)?;
                map.end()
            }
            Sb3BlockMutation::ControlStop { has_next } => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("tagName", "mutation")?;
                map.serialize_entry::<str, [()]>("children", &[])?;
                map.serialize_entry("hasnext", &json_encode::<S, _>(has_next)?)?;
                map.end()
            }
        }
    }
}

impl<'de> Deserialize<'de> for Sb3BlockMutation {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        struct Sb3BlockMutationVisitor;

        impl<'de> Visitor<'de> for Sb3BlockMutationVisitor {
            type Value = Sb3BlockMutation;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a block mutation")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut tag_name = None::<&str>;
                let mut children = None::<[(); 0]>;
                let mut procedure_code = None::<String>;
                let mut argument_ids = None::<Vec<String>>;
                let mut warp = None::<bool>;
                let mut argument_names = None::<Vec<String>>;
                let mut argument_defaults = None::<Vec<Value>>;
                let mut has_next = None::<bool>;
                let mut entry_count = 0;
                loop {
                    if entry_count > 7 {
                        return Err(de::Error::invalid_length(entry_count, &"3, 5 or 7"));
                    }
                    if let Some(key) = map.next_key::<&str>()? {
                        entry_count += 1;
                        match key {
                            "tagName" => {
                                tag_name = map.next_value()?;
                            }
                            "children" => {
                                children = map.next_value()?;
                            }
                            "proccode" => {
                                procedure_code = map.next_value()?;
                            }
                            "argumentids" => {
                                argument_ids = serde_json::from_str(&map.next_value::<String>()?)
                                    .map_err(|_| {
                                    de::Error::custom("argumentids was not a list of strings")
                                })?;
                            }
                            "warp" => {
                                warp = serde_json::from_str(&map.next_value::<String>()?)
                                    .map_err(|_| de::Error::custom("warp was not a bool"))?;
                            }
                            "argumentnames" => {
                                argument_names = serde_json::from_str(&map.next_value::<String>()?)
                                    .map_err(|_| {
                                        de::Error::custom("argumentnames was not a list of strings")
                                    })?;
                            }
                            "argumentdefaults" => {
                                argument_defaults = serde_json::from_str(
                                    &map.next_value::<String>()?,
                                )
                                .map_err(|_| {
                                    de::Error::custom("argumentdefaults was not a list of strings")
                                })?;
                            }
                            "hasnext" => {
                                has_next = serde_json::from_str(&map.next_value::<String>()?)
                                    .map_err(|_| de::Error::custom("hasnext was not a bool"))?;
                            }
                            _ => {
                                return Err(de::Error::invalid_value(
                                    de::Unexpected::Map,
                                    &"a block mutation key",
                                ));
                            }
                        }
                    } else {
                        if !matches!(entry_count, 3 | 5 | 7) {
                            return Err(de::Error::invalid_length(entry_count, &"3, 5 or 7"));
                        }
                        match (tag_name, children) {
                            (Some("mutation"), Some([])) => (),
                            _ => {
                                return Err(de::Error::invalid_value(
                                    de::Unexpected::Other(
                                        "block mutation without tagName or children set to the correct values",
                                    ),
                                    &"a valid block mutation",
                                ));
                            }
                        }
                        if let Some(has_next) = has_next {
                            if entry_count != 3 {
                                return Err(de::Error::invalid_value(
                                    de::Unexpected::Other(
                                        "control stop block mutation with more than three entries",
                                    ),
                                    &"a valid control stop block mutation",
                                ));
                            }
                            return Ok(Sb3BlockMutation::ControlStop { has_next });
                        }
                        if let (Some(argument_names), Some(argument_defaults)) =
                            (argument_names, argument_defaults)
                        {
                            if entry_count != 7 {
                                return Err(de::Error::invalid_value(
                                    de::Unexpected::Other(
                                        "procedures prototype block mutation with less than 7 entries",
                                    ),
                                    &"a valid procedures prototype block mutation",
                                ));
                            }
                            let (procedure_code, argument_ids, warp) = match (
                                procedure_code,
                                argument_ids,
                                warp,
                            ) {
                                (Some(val_1), Some(val_2), Some(val_3)) => (val_1, val_2, val_3),
                                _ => {
                                    return Err(de::Error::invalid_value(
                                        de::Unexpected::Other(
                                            "procedures prototype block mutation without proccode, argumentids or warp set",
                                        ),
                                        &"a valid procedures prototype block mutation",
                                    ));
                                }
                            };
                            return Ok(Sb3BlockMutation::ProceduresPrototype {
                                procedure_code,
                                argument_ids,
                                warp,
                                argument_names,
                                argument_defaults,
                            });
                        }
                        if entry_count != 5 {
                            return Err(de::Error::invalid_value(
                                de::Unexpected::Other(
                                    "procedures call block mutation with anything other than 5 entries",
                                ),
                                &"a valid procedures call block mutation",
                            ));
                        }
                        let (procedure_code, argument_ids, warp) = match (
                            procedure_code,
                            argument_ids,
                            warp,
                        ) {
                            (Some(val_1), Some(val_2), Some(val_3)) => (val_1, val_2, val_3),
                            _ => {
                                return Err(de::Error::invalid_value(
                                    de::Unexpected::Other(
                                        "procedures call block mutation without proccode, argumentids or warp set",
                                    ),
                                    &"a valid procedures call block mutation",
                                ));
                            }
                        };
                        return Ok(Sb3BlockMutation::ProceduresCall {
                            procedure_code,
                            argument_ids,
                            warp,
                        });
                    }
                }
            }
        }
        deserializer.deserialize_map(Sb3BlockMutationVisitor)
    }
}

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
    pub sprite_name: Option<String>,
    pub value: Sb3MonitorValue,
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
    pub is_discrete: Option<bool>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sb3MonitorValue {
    List(Vec<Sb3Primitive>),
    Primitive(Sb3Primitive),
}

impl Serialize for Sb3MonitorValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Sb3MonitorValue::List(sb3_primitives) => sb3_primitives.serialize(serializer),
            Sb3MonitorValue::Primitive(sb3_primitive) => sb3_primitive.serialize(serializer),
        }
    }
}

impl<'de> Deserialize<'de> for Sb3MonitorValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        struct Sb3MonitorValueVisitor;

        impl<'de> Visitor<'de> for Sb3MonitorValueVisitor {
            type Value = Sb3MonitorValue;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a monitor value")
            }

            fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'de>,
            {
                Ok(Sb3MonitorValue::List(Vec::<Sb3Primitive>::deserialize(
                    serde::de::value::SeqAccessDeserializer::new(seq),
                )?))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Sb3MonitorValue::Primitive(
                    sb3_primitive::Sb3PrimitiveVisitor.visit_str(v)?,
                ))
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Sb3MonitorValue::Primitive(
                    sb3_primitive::Sb3PrimitiveVisitor.visit_string(v)?,
                ))
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Sb3MonitorValue::Primitive(
                    sb3_primitive::Sb3PrimitiveVisitor.visit_i64(v)?,
                ))
            }

            fn visit_i128<E>(self, v: i128) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Sb3MonitorValue::Primitive(
                    sb3_primitive::Sb3PrimitiveVisitor.visit_i128(v)?,
                ))
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Sb3MonitorValue::Primitive(
                    sb3_primitive::Sb3PrimitiveVisitor.visit_u64(v)?,
                ))
            }

            fn visit_u128<E>(self, v: u128) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Sb3MonitorValue::Primitive(
                    sb3_primitive::Sb3PrimitiveVisitor.visit_u128(v)?,
                ))
            }

            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Sb3MonitorValue::Primitive(
                    sb3_primitive::Sb3PrimitiveVisitor.visit_f64(v)?,
                ))
            }

            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Sb3MonitorValue::Primitive(
                    sb3_primitive::Sb3PrimitiveVisitor.visit_bool(v)?,
                ))
            }

            fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
            where
                D: de::Deserializer<'de>,
            {
                Ok(Sb3MonitorValue::Primitive(
                    sb3_primitive::Sb3PrimitiveVisitor.visit_some(deserializer)?,
                ))
            }

            fn visit_none<E>(self) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Sb3MonitorValue::Primitive(
                    sb3_primitive::Sb3PrimitiveVisitor.visit_none()?,
                ))
            }
        }
        deserializer.deserialize_any(Sb3MonitorValueVisitor)
    }
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

impl Default for Sb3Meta {
    fn default() -> Self {
        Self {
            semver: "3.0.0".into(),
            vm: "12.6.4".into(),
            agent: "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/145.0.0.0 Safari/537.36".into(),
            // ^ not my user agent
            origin: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
#[serde(from = "bool", into = "bool")]
pub enum IsShadow {
    Yes,
    No,
}

impl From<bool> for IsShadow {
    fn from(value: bool) -> Self {
        if value { Self::Yes } else { Self::No }
    }
}

impl From<IsShadow> for bool {
    fn from(value: IsShadow) -> Self {
        match value {
            IsShadow::Yes => true,
            IsShadow::No => false,
        }
    }
}
