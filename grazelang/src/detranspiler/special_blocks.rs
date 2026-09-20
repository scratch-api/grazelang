use std::collections::{HashMap, HashSet};

use arcstr::{ArcStr as IString, literal};
use grazelang_types::project_json;

use super::{
    core::{
        DetranspilerContext, DetranspilerResult, DetranspilerVarOrListKind, INDEX_STR, LIST_STR,
        NextBlockId, convert_block_stack, convert_primitive_reporter_block, convert_reporter_block,
        create_simple_identifier, emit_error, emit_message, emit_message_eager,
        get_primary_input_repr, lookup_broadcast, lookup_var_or_list, unwrap_or_emit_message,
    },
    get_info::{SpecialReporterInfo, SpecialStackBlockInfo, check_special_stack_block},
};
use crate::{
    ast::types::{self as ast_types},
    messages::types::{GrazeDetranspilerError, GrazeDetranspilerWarning},
    settings::GrazeMessageSetting,
};

/// Result is bubbled
pub fn convert_special_reporter_block(
    reporter: SpecialReporterInfo,
    block: &project_json::Sb3NormalBlock,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_index: usize,
) -> DetranspilerResult<ast_types::Expression> {
    /// Result is bubbled
    fn convert_operand_input_value(
        operand: &project_json::Sb3InputValue,
        blocks: &HashMap<String, project_json::Sb3Block>,
        context: &mut DetranspilerContext,
        target_index: usize,
    ) -> DetranspilerResult<ast_types::Expression> {
        let input_repr = get_primary_input_repr(operand);
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
                        convert_reporter_block(block, block_id, blocks, context, target_index)
                    }),
                context,
                Ok(ast_types::Expression::default())
            )?,
            project_json::Sb3InputRepr::PrimitiveBlock(block) => {
                unwrap_or_emit_message!(
                    convert_primitive_reporter_block(block, context, target_index),
                    context,
                    ast_types::Expression::default()
                )
            }
            project_json::Sb3InputRepr::Missing => ast_types::Expression::default(),
        })
    }
    Ok(match reporter {
        SpecialReporterInfo::BinOp {
            binop,
            left_operand,
            right_operand,
        } => {
            let mut operands_present = 0;
            let left_operand_expression =
                if let Some(operand) = block.inputs.get(left_operand.as_str()) {
                    operands_present += 1;
                    convert_operand_input_value(operand, blocks, context, target_index)?
                } else {
                    ast_types::Expression::default()
                };
            let right_operand_expression =
                if let Some(operand) = block.inputs.get(right_operand.as_str()) {
                    operands_present += 1;
                    convert_operand_input_value(operand, blocks, context, target_index)?
                } else {
                    ast_types::Expression::default()
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
        SpecialReporterInfo::NegatedBinOp {
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
                    convert_operand_input_value(operand, blocks, context, target_index)?
                } else {
                    ast_types::Expression::default()
                };
            let right_operand_expression =
                if let Some(operand) = inner_block.inputs.get(inner_right_operand.as_str()) {
                    inner_operands_present += 1;
                    convert_operand_input_value(operand, blocks, context, target_index)?
                } else {
                    ast_types::Expression::default()
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
        SpecialReporterInfo::UnOp {
            unop,
            operand,
            unused_operand,
            unused_field,
        } => {
            let mut operands_present = 0;
            let mut unused_field_present = 0;
            let operand_expression = if let Some(operand) = block.inputs.get(operand.as_str()) {
                convert_operand_input_value(operand, blocks, context, target_index)?
            } else {
                ast_types::Expression::default()
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
        SpecialReporterInfo::ProcedureArgument { is_bool } => {
            let Some(project_json::Sb3FieldValue::Normal(name)) = block.fields.get("VALUE") else {
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
        SpecialReporterInfo::PrimitiveValueBlock { field } => {
            let Some(
                project_json::Sb3FieldValue::Normal(value)
                | project_json::Sb3FieldValue::WithId { value, id: _ },
            ) = block.fields.get(field.as_str())
            else {
                emit_error!(
                    GrazeDetranspilerError::MissingField {
                        field: field.to_string(),
                        block_id: block_id.to_string()
                    },
                    context
                );
                return Ok(ast_types::Expression::Literal(
                    ast_types::Literal::EmptyExpression,
                ));
            };
            ast_types::Expression::Literal(value.into())
        }
        SpecialReporterInfo::Variable => {
            const FIELD: &str = "VARIABLE";
            let Some(value) = block.fields.get(FIELD) else {
                emit_error!(
                    GrazeDetranspilerError::MissingField {
                        field: FIELD.to_string(),
                        block_id: block_id.to_string()
                    },
                    context
                );
                return Ok(ast_types::Expression::Literal(
                    ast_types::Literal::EmptyExpression,
                ));
            };
            let (name, id) = match value {
                project_json::Sb3FieldValue::Normal(name) => {
                    emit_error!(
                        GrazeDetranspilerError::UnknownVariable {
                            id: "null".to_string(),
                            name: name.to_string()
                        },
                        context
                    );
                    return Ok(ast_types::Expression::Literal(
                        ast_types::Literal::EmptyExpression,
                    ));
                }
                project_json::Sb3FieldValue::WithId { value, id } => (value, id),
            };
            let name = name.as_cow_str();
            let variable =
                lookup_var_or_list(&name, id, target_index, context)?.ok_or_else(|| {
                    GrazeDetranspilerError::UnknownVariable {
                        id: id.clone(),
                        name: name.to_string(),
                    }
                })?;
            ast_types::Expression::Identifier(create_simple_identifier(variable.name.clone()))
        }
        SpecialReporterInfo::List => {
            const FIELD: &str = "LIST";
            let Some(value) = block.fields.get(FIELD) else {
                emit_error!(
                    GrazeDetranspilerError::MissingField {
                        field: FIELD.to_string(),
                        block_id: block_id.to_string()
                    },
                    context
                );
                return Ok(ast_types::Expression::Literal(
                    ast_types::Literal::EmptyExpression,
                ));
            };
            let (name, id) = match value {
                project_json::Sb3FieldValue::Normal(name) => {
                    emit_error!(
                        GrazeDetranspilerError::UnknownList {
                            id: "null".to_string(),
                            name: name.to_string()
                        },
                        context
                    );
                    return Ok(ast_types::Expression::Literal(
                        ast_types::Literal::EmptyExpression,
                    ));
                }
                project_json::Sb3FieldValue::WithId { value, id } => (value, id),
            };
            let name = name.as_cow_str();
            let list = lookup_var_or_list(&name, id, target_index, context)?.ok_or_else(|| {
                GrazeDetranspilerError::UnknownList {
                    id: id.clone(),
                    name: name.to_string(),
                }
            })?;
            ast_types::Expression::Identifier(create_simple_identifier(list.name.clone()))
        }
        SpecialReporterInfo::Broadcast => {
            const FIELD: &str = "BROADCAST_OPTION";
            let Some(value) = block.fields.get(FIELD) else {
                emit_error!(
                    GrazeDetranspilerError::MissingField {
                        field: FIELD.to_string(),
                        block_id: block_id.to_string()
                    },
                    context
                );
                return Ok(ast_types::Expression::Literal(
                    ast_types::Literal::EmptyExpression,
                ));
            };
            let (name, id) = match value {
                project_json::Sb3FieldValue::Normal(name) => {
                    emit_error!(
                        GrazeDetranspilerError::UnknownBroadcast {
                            id: "null".to_string(),
                            name: name.to_string()
                        },
                        context
                    );
                    return Ok(ast_types::Expression::Literal(
                        ast_types::Literal::EmptyExpression,
                    ));
                }
                project_json::Sb3FieldValue::WithId { value, id } => (value, id),
            };
            let name = name.as_cow_str();
            let broadcast = lookup_broadcast(&name, id, context)?.ok_or_else(|| {
                GrazeDetranspilerError::UnknownBroadcast {
                    id: id.clone(),
                    name: name.to_string(),
                }
            })?;
            ast_types::Expression::Identifier(create_simple_identifier(broadcast.name.clone()))
        }
        SpecialReporterInfo::Join => {
            const JOIN_OPCODE: &str = "operator_join";
            const LEFT_OPERAND: &str = "STRING1";
            const RIGHT_OPERAND: &str = "STRING2";
            let mut operands_present = 0;
            /// Result is bubbled
            fn recursively_convert_formatted_string(
                block: &project_json::Sb3NormalBlock,
                block_id: &str,
                blocks: &HashMap<String, project_json::Sb3Block>,
                context: &mut DetranspilerContext,
                target_index: usize,
                formatted_string_content: &mut Vec<ast_types::FormattedStringContent>,
            ) -> DetranspilerResult<()> {
                let mut operands_present = 0;
                if let Some(left_operand) = block.inputs.get(LEFT_OPERAND) {
                    operands_present += 1;
                    if let project_json::Sb3InputRepr::Reference(left_operand_block_id) =
                        get_primary_input_repr(left_operand)
                        && let Some(project_json::Sb3Block::Normal(left_operand_block)) =
                            blocks.get(left_operand_block_id)
                        && left_operand_block.opcode.as_str() == JOIN_OPCODE
                    {
                        recursively_convert_formatted_string(
                            left_operand_block,
                            left_operand_block_id,
                            blocks,
                            context,
                            target_index,
                            formatted_string_content,
                        )?;
                    } else {
                        let value = convert_operand_input_value(
                            left_operand,
                            blocks,
                            context,
                            target_index,
                        )?;
                        formatted_string_content.push(
                            if let ast_types::Expression::Literal(ast_types::Literal::String(
                                value,
                            )) = value
                            {
                                ast_types::FormattedStringContent::String(value)
                            } else {
                                ast_types::FormattedStringContent::Expression(Box::new(value))
                            },
                        );
                    }
                }
                if let Some(right_operand) = block.inputs.get(RIGHT_OPERAND) {
                    operands_present += 1;
                    if let project_json::Sb3InputRepr::Reference(right_operand_block_id) =
                        get_primary_input_repr(right_operand)
                        && let Some(project_json::Sb3Block::Normal(right_operand_block)) =
                            blocks.get(right_operand_block_id)
                        && right_operand_block.opcode.as_str() == JOIN_OPCODE
                    {
                        recursively_convert_formatted_string(
                            right_operand_block,
                            right_operand_block_id,
                            blocks,
                            context,
                            target_index,
                            formatted_string_content,
                        )?;
                    } else {
                        let value = convert_operand_input_value(
                            right_operand,
                            blocks,
                            context,
                            target_index,
                        )?;
                        formatted_string_content.push(
                            if let ast_types::Expression::Literal(ast_types::Literal::String(
                                value,
                            )) = value
                            {
                                ast_types::FormattedStringContent::String(value)
                            } else {
                                ast_types::FormattedStringContent::Expression(Box::new(value))
                            },
                        );
                    }
                }
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
                        if key.as_str() == LEFT_OPERAND || key.as_str() == RIGHT_OPERAND {
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
                Ok(())
            }
            if if let Some(left_operand) = block.inputs.get(LEFT_OPERAND)
                && let project_json::Sb3InputRepr::Reference(left_operand_block_id) =
                    get_primary_input_repr(left_operand)
                && let Some(project_json::Sb3Block::Normal(left_operand_block)) =
                    blocks.get(left_operand_block_id)
                && left_operand_block.opcode.as_str() == JOIN_OPCODE
            {
                true
            } else if let Some(right_operand) = block.inputs.get(RIGHT_OPERAND)
                && let project_json::Sb3InputRepr::Reference(right_operand_block_id) =
                    get_primary_input_repr(right_operand)
                && let Some(project_json::Sb3Block::Normal(right_operand_block)) =
                    blocks.get(right_operand_block_id)
                && right_operand_block.opcode.as_str() == JOIN_OPCODE
            {
                true
            } else {
                false
            } {
                let mut formatted_string_content = Vec::with_capacity(3);
                recursively_convert_formatted_string(
                    block,
                    block_id,
                    blocks,
                    context,
                    target_index,
                    &mut formatted_string_content,
                )?;
                return Ok(ast_types::Expression::FormattedString(
                    formatted_string_content,
                ));
            }
            let left_operand_expression = if let Some(operand) = block.inputs.get(LEFT_OPERAND) {
                operands_present += 1;
                convert_operand_input_value(operand, blocks, context, target_index)?
            } else {
                ast_types::Expression::default()
            };
            let right_operand_expression = if let Some(operand) = block.inputs.get(RIGHT_OPERAND) {
                operands_present += 1;
                convert_operand_input_value(operand, blocks, context, target_index)?
            } else {
                ast_types::Expression::default()
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
                    if key.as_str() == LEFT_OPERAND || key.as_str() == RIGHT_OPERAND {
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
                operator: ast_types::BinOp::Join,
                left_operand: Box::new(left_operand_expression),
                right_operand: Box::new(right_operand_expression),
            }
        }
        SpecialReporterInfo::GetItem => {
            let Some(field_value) = block.fields.get(LIST_STR) else {
                return Err(GrazeDetranspilerError::MissingField {
                    field: LIST_STR.to_string(),
                    block_id: block_id.to_string(),
                });
            };
            let (name, id) = match field_value {
                project_json::Sb3FieldValue::Normal(name) => {
                    return Err(GrazeDetranspilerError::UnknownList {
                        id: "null".to_string(),
                        name: name.to_string(),
                    });
                }
                project_json::Sb3FieldValue::WithId { value, id } => (value, id),
            };
            let (list, is_list) =
                lookup_var_or_list(&name.as_cow_str(), id, target_index, context)?
                    .map(|value| {
                        (
                            Some(value.name.clone()),
                            matches!(value.kind, DetranspilerVarOrListKind::List { .. }),
                        )
                    })
                    .unwrap_or_else(|| {
                        emit_message(
                            context,
                            || {
                                GrazeDetranspilerWarning::UnknownVLBValue {
                                    field: LIST_STR.to_string(),
                                    block_id: block_id.to_string(),
                                }
                                .into()
                            },
                            GrazeMessageSetting::Warnings,
                        );
                        (None, false)
                    });
            let Some(index_input) = block.inputs.get(INDEX_STR) else {
                return Err(GrazeDetranspilerError::MissingInput {
                    input: INDEX_STR.to_string(),
                    block_id: block_id.to_string(),
                });
            };
            let index_input_repr = get_primary_input_repr(index_input);
            let index = match index_input_repr {
                project_json::Sb3InputRepr::Reference(block_id) => 'a: {
                    let inner_block = unwrap_or_emit_message!(
                        blocks.get(block_id).ok_or_else(|| {
                            GrazeDetranspilerError::InvalidBlockReference {
                                block_id: block_id.clone(),
                            }
                        }),
                        context,
                        break 'a ast_types::Expression::default()
                    );
                    convert_reporter_block(inner_block, block_id, blocks, context, target_index)?
                }
                project_json::Sb3InputRepr::PrimitiveBlock(block) => unwrap_or_emit_message!(
                    convert_primitive_reporter_block(block, context, target_index),
                    context,
                    ast_types::Expression::default()
                ),
                project_json::Sb3InputRepr::Missing => {
                    emit_error!(
                        GrazeDetranspilerError::MissingInput {
                            input: INDEX_STR.to_string(),
                            block_id: block_id.to_string(),
                        },
                        context
                    );
                    ast_types::Expression::default()
                }
            };
            if block.inputs.len() != 1 {
                for input in block.inputs.keys() {
                    if input.as_str() == INDEX_STR {
                        continue;
                    }
                    emit_message(
                        context,
                        || {
                            GrazeDetranspilerWarning::UnusedInput {
                                input: input.clone(),
                                block_id: block_id.to_string(),
                            }
                            .into()
                        },
                        GrazeMessageSetting::Warnings,
                    );
                }
            }
            if block.fields.len() != 1 {
                for field in block.fields.keys() {
                    if field.as_str() == LIST_STR {
                        continue;
                    }
                    emit_message(
                        context,
                        || {
                            GrazeDetranspilerWarning::UnusedField {
                                field: field.clone(),
                                block_id: block_id.to_string(),
                            }
                            .into()
                        },
                        GrazeMessageSetting::Warnings,
                    );
                }
            }
            if is_list && let Some(list) = list {
                ast_types::Expression::GetItem {
                    list: create_simple_identifier(list),
                    item: Box::new(index),
                }
            } else {
                ast_types::Expression::Call {
                    function: create_simple_identifier(literal!("get_item_of_list")),
                    arguments: vec![
                        list.map(|value| {
                            ast_types::Expression::Identifier(create_simple_identifier(value))
                        })
                        .unwrap_or_default(),
                        index,
                    ],
                }
            }
        }
    })
}

/// Result is unbubbled
pub fn convert_special_stack_block(
    stack_block: SpecialStackBlockInfo,
    block: &project_json::Sb3NormalBlock,
    block_id: &str,
    blocks: &HashMap<String, project_json::Sb3Block>,
    context: &mut DetranspilerContext,
    target_index: usize,
) -> DetranspilerResult<(ast_types::Statement, Option<NextBlockId>)> {
    const ITEM_STR: &str = "ITEM";
    match stack_block {
        SpecialStackBlockInfo::ProcedureCall => {
            let Some(mutation) = &block.mutation else {
                return Err(GrazeDetranspilerError::MissingMutation {
                    block_id: block_id.to_string(),
                });
            };
            let project_json::Sb3BlockMutation::ProceduresCall {
                procedure_code,
                argument_ids,
                warp: _,
            } = mutation
            else {
                return Err(GrazeDetranspilerError::IncorrectMutationType {
                    block_id: block_id.to_string(),
                });
            };
            let Some(procedure_info) = context
                .targets
                .get(target_index)
                .unwrap()
                .procedures
                .get(procedure_code.as_str())
            else {
                return Err(GrazeDetranspilerError::UnknownProccode {
                    block_id: block_id.to_string(),
                    proccode: procedure_code.clone(),
                });
            };
            let procedure_identifier = create_simple_identifier(procedure_info.name.clone());
            let mut arguments = Vec::with_capacity(argument_ids.len());
            let mut tracked_args = 0_usize;
            for argument_id in argument_ids {
                let Some(input) = block.inputs.get(argument_id) else {
                    arguments.push(ast_types::Expression::default());
                    continue;
                };
                tracked_args += 1;
                let input_repr = get_primary_input_repr(input);
                arguments.push(match input_repr {
                    project_json::Sb3InputRepr::Reference(block_id) => convert_reporter_block(
                        unwrap_or_emit_message!(
                            blocks.get(block_id).ok_or_else(|| {
                                GrazeDetranspilerError::InvalidBlockReference {
                                    block_id: block_id.clone(),
                                }
                            }),
                            context,
                            {
                                arguments.push(ast_types::Expression::default());
                                continue;
                            }
                        ),
                        block_id,
                        blocks,
                        context,
                        target_index,
                    )?,
                    project_json::Sb3InputRepr::PrimitiveBlock(block) => {
                        unwrap_or_emit_message!(
                            convert_primitive_reporter_block(block, context, target_index),
                            context,
                            ast_types::Expression::default()
                        )
                    }
                    project_json::Sb3InputRepr::Missing => ast_types::Expression::default(),
                });
            }
            if tracked_args != block.inputs.len() {
                let mut tracked_args = HashSet::<String>::with_capacity(block.inputs.len());
                for key in block.inputs.keys() {
                    tracked_args.insert(key.clone());
                }
                for argument_id in argument_ids {
                    tracked_args.remove(argument_id);
                }
                for arg in tracked_args {
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
            for key in block.fields.keys() {
                emit_message(
                    context,
                    || {
                        GrazeDetranspilerWarning::UnusedField {
                            field: key.clone(),
                            block_id: block_id.to_string(),
                        }
                        .into()
                    },
                    GrazeMessageSetting::Warnings,
                );
            }
            Ok((
                ast_types::Statement::Call {
                    function: procedure_identifier,
                    arguments,
                },
                block.next.as_deref().map(Into::into),
            ))
        }
        SpecialStackBlockInfo::IfElse => {
            type ElseBranch = Option<ast_types::CodeBlock>;
            type IfBranch = (ast_types::Expression, ast_types::CodeBlock);
            type OptionalIfBranch = Option<IfBranch>;
            macro_rules! get_input {
                (($block:expr, $block_id:expr, $blocks:expr, $context:expr, $target_index:expr, $tracked_args:ident, $name:expr) as Input) => {
                    if let Some(input) = $block.inputs.get($name) {
                        $tracked_args += 1;
                        let input_repr = get_primary_input_repr(input);
                        match input_repr {
                            project_json::Sb3InputRepr::Reference(block_id) => {
                                unwrap_or_emit_message!(
                                    $blocks
                                        .get(block_id)
                                        .ok_or_else(|| {
                                            GrazeDetranspilerError::InvalidBlockReference {
                                                block_id: block_id.clone(),
                                            }
                                        })
                                        .map(Some),
                                    $context,
                                    None
                                )
                                .map(|block| {
                                    convert_reporter_block(
                                        block,
                                        block_id,
                                        $blocks,
                                        $context,
                                        $target_index,
                                    )
                                })
                                .transpose()?
                            }
                            project_json::Sb3InputRepr::PrimitiveBlock(block) => {
                                Some(unwrap_or_emit_message!(
                                    convert_primitive_reporter_block(
                                        block,
                                        $context,
                                        $target_index
                                    ),
                                    $context,
                                    ast_types::Expression::default()
                                ))
                            }
                            project_json::Sb3InputRepr::Missing => {
                                Some(ast_types::Expression::default())
                            }
                        }
                    } else {
                        None
                    }
                };
                (($block:expr, $block_id:expr, $blocks:expr, $context:expr, $target_index:expr, $tracked_args:ident, $name:literal) as Stack) => {
                    if let Some(input) = $block.inputs.get($name) {
                        $tracked_args += 1;
                        let input_repr = get_primary_input_repr(input);
                        match input_repr {
                            project_json::Sb3InputRepr::Reference(block_id) => {
                                unwrap_or_emit_message!(
                                    $blocks
                                        .get(block_id)
                                        .ok_or_else(|| {
                                            GrazeDetranspilerError::InvalidBlockReference {
                                                block_id: block_id.clone(),
                                            }
                                        })
                                        .map(Some),
                                    $context,
                                    None
                                )
                                .map(|block| {
                                    convert_block_stack(
                                        block,
                                        block_id,
                                        $blocks,
                                        $context,
                                        $target_index,
                                    )
                                })
                                .transpose()?
                            }
                            project_json::Sb3InputRepr::PrimitiveBlock(_) => {
                                emit_error!(
                                    GrazeDetranspilerError::PrimitiveBlockAsSubstack {
                                        block_id: $block_id.to_string(),
                                        input_name: $name.to_string()
                                    },
                                    $context
                                );
                                None
                            }
                            project_json::Sb3InputRepr::Missing => None,
                        }
                    } else {
                        None
                    }
                };
            }
            fn convert_if(
                block: &project_json::Sb3NormalBlock,
                block_id: &str,
                blocks: &HashMap<String, project_json::Sb3Block>,
                context: &mut DetranspilerContext,
                target_index: usize,
            ) -> DetranspilerResult<IfBranch> {
                let mut tracked_args = 0_usize;
                let condition = get_input!((
                    block,
                    block_id,
                    blocks,
                    context,
                    target_index,
                    tracked_args,
                    "CONDITION"
                ) as Input);
                let substack = get_input!((
                    block,
                    block_id,
                    blocks,
                    context,
                    target_index,
                    tracked_args,
                    "SUBSTACK"
                ) as Stack);
                if tracked_args != block.inputs.len() {
                    for key in block.inputs.keys() {
                        if matches!(key.as_str(), "CONDITION" | "SUBSTACK") {
                            continue;
                        }
                        emit_message(
                            context,
                            || {
                                GrazeDetranspilerWarning::UnusedInput {
                                    input: key.clone(),
                                    block_id: block_id.to_string(),
                                }
                                .into()
                            },
                            GrazeMessageSetting::Warnings,
                        );
                    }
                }
                for key in block.fields.keys() {
                    emit_message(
                        context,
                        || {
                            GrazeDetranspilerWarning::UnusedField {
                                field: key.clone(),
                                block_id: block_id.to_string(),
                            }
                            .into()
                        },
                        GrazeMessageSetting::Warnings,
                    );
                }
                Ok((condition.unwrap_or_default(), substack.unwrap_or_default()))
            }
            /// Result is bubbled
            fn convert_if_else(
                block: &project_json::Sb3NormalBlock,
                block_id: &str,
                blocks: &HashMap<String, project_json::Sb3Block>,
                context: &mut DetranspilerContext,
                target_index: usize,
                alternative_branches: &mut Vec<(ast_types::Expression, ast_types::CodeBlock)>,
                first_if_branch: bool,
            ) -> DetranspilerResult<(OptionalIfBranch, ElseBranch)> {
                let mut tracked_args = 0_usize;
                let condition = get_input!((
                    block,
                    block_id,
                    blocks,
                    context,
                    target_index,
                    tracked_args,
                    "CONDITION"
                ) as Input);
                let substack_1 = get_input!((
                    block,
                    block_id,
                    blocks,
                    context,
                    target_index,
                    tracked_args,
                    "SUBSTACK"
                ) as Stack);
                let (condition, substack_1) = if !first_if_branch {
                    alternative_branches.push((
                        condition.unwrap_or_default(),
                        substack_1.unwrap_or_default(),
                    ));
                    (None, None)
                } else {
                    (condition, substack_1)
                };
                let (else_if_else, substack_2) = if let Some(input) = block.inputs.get("SUBSTACK2")
                {
                    tracked_args += 1;
                    let input_repr = get_primary_input_repr(input);
                    match input_repr {
                        project_json::Sb3InputRepr::Reference(block_id) => {
                            if let Some(block) = unwrap_or_emit_message!(
                                blocks
                                    .get(block_id)
                                    .ok_or_else(|| {
                                        GrazeDetranspilerError::InvalidBlockReference {
                                            block_id: block_id.clone(),
                                        }
                                    })
                                    .map(Some),
                                context,
                                None
                            ) {
                                if let project_json::Sb3Block::Normal(block) = block
                                    && block.opcode.as_str() == "control_if_else"
                                    && block.next.is_none()
                                {
                                    (
                                        unwrap_or_emit_message!(
                                            convert_if_else(
                                                block,
                                                block_id,
                                                blocks,
                                                context,
                                                target_index,
                                                alternative_branches,
                                                false
                                            )
                                            .map(Some),
                                            context,
                                            None
                                        ),
                                        None,
                                    )
                                } else if let project_json::Sb3Block::Normal(block) = block
                                    && block.opcode.as_str() == "control_if"
                                    && block.next.is_none()
                                {
                                    if let Some(if_branch) = unwrap_or_emit_message!(
                                        convert_if(block, block_id, blocks, context, target_index)
                                            .map(Some),
                                        context,
                                        None
                                    ) {
                                        alternative_branches.push(if_branch);
                                    }
                                    (Some((None, None)), None)
                                } else {
                                    (
                                        None,
                                        Some(convert_block_stack(
                                            block,
                                            block_id,
                                            blocks,
                                            context,
                                            target_index,
                                        )?),
                                    )
                                }
                            } else {
                                (None, None)
                            }
                        }
                        project_json::Sb3InputRepr::PrimitiveBlock(_) => {
                            emit_error!(
                                GrazeDetranspilerError::PrimitiveBlockAsSubstack {
                                    block_id: block_id.to_string(),
                                    input_name: "SUBSTACK2".to_string()
                                },
                                context
                            );
                            (None, None)
                        }
                        project_json::Sb3InputRepr::Missing => (None, None),
                    }
                } else {
                    (None, None)
                };
                if tracked_args != block.inputs.len() {
                    for key in block.inputs.keys() {
                        if matches!(key.as_str(), "CONDITION" | "SUBSTACK" | "SUBSTACK2") {
                            continue;
                        }
                        emit_message(
                            context,
                            || {
                                GrazeDetranspilerWarning::UnusedInput {
                                    input: key.clone(),
                                    block_id: block_id.to_string(),
                                }
                                .into()
                            },
                            GrazeMessageSetting::Warnings,
                        );
                    }
                }
                for key in block.fields.keys() {
                    emit_message(
                        context,
                        || {
                            GrazeDetranspilerWarning::UnusedField {
                                field: key.clone(),
                                block_id: block_id.to_string(),
                            }
                            .into()
                        },
                        GrazeMessageSetting::Warnings,
                    );
                }
                Ok((
                    if first_if_branch {
                        Some((
                            condition.unwrap_or_default(),
                            substack_1.unwrap_or_default(),
                        ))
                    } else {
                        None
                    },
                    if let Some((_, else_branch)) = else_if_else {
                        else_branch
                    } else {
                        Some(substack_2.unwrap_or_default())
                    },
                ))
            }
            let mut alternative_branches = Vec::new();
            let (first_branch, else_branch) = convert_if_else(
                block,
                block_id,
                blocks,
                context,
                target_index,
                &mut alternative_branches,
                true,
            )?;
            Ok((
                ast_types::Statement::IfElse {
                    first_branch: first_branch.unwrap(),
                    alternative_branches,
                    else_branch,
                },
                block.next.as_deref().map(Into::into),
            ))
        }
        SpecialStackBlockInfo::ClearList => {
            let Some(field_value) = block.fields.get(LIST_STR) else {
                return Err(GrazeDetranspilerError::MissingField {
                    field: LIST_STR.to_string(),
                    block_id: block_id.to_string(),
                });
            };
            let (name, id) = match field_value {
                project_json::Sb3FieldValue::Normal(name) => {
                    return Err(GrazeDetranspilerError::UnknownList {
                        id: "null".to_string(),
                        name: name.to_string(),
                    });
                }
                project_json::Sb3FieldValue::WithId { value, id } => (value, id),
            };
            let (list, is_list) =
                lookup_var_or_list(&name.as_cow_str(), id, target_index, context)?
                    .map(|value| {
                        (
                            Some(value.name.clone()),
                            matches!(value.kind, DetranspilerVarOrListKind::List { .. }),
                        )
                    })
                    .unwrap_or_else(|| {
                        emit_message(
                            context,
                            || {
                                GrazeDetranspilerWarning::UnknownVLBValue {
                                    field: LIST_STR.to_string(),
                                    block_id: block_id.to_string(),
                                }
                                .into()
                            },
                            GrazeMessageSetting::Warnings,
                        );
                        (None, false)
                    });
            for input in block.inputs.keys() {
                emit_message(
                    context,
                    || {
                        GrazeDetranspilerWarning::UnusedInput {
                            input: input.clone(),
                            block_id: block_id.to_string(),
                        }
                        .into()
                    },
                    GrazeMessageSetting::Warnings,
                );
            }
            if block.fields.len() != 1 {
                for field in block.fields.keys() {
                    if field.as_str() == LIST_STR {
                        continue;
                    }
                    emit_message(
                        context,
                        || {
                            GrazeDetranspilerWarning::UnusedField {
                                field: field.clone(),
                                block_id: block_id.to_string(),
                            }
                            .into()
                        },
                        GrazeMessageSetting::Warnings,
                    );
                }
            }
            let next_block_id = block.next.as_deref().map(IString::from);
            if is_list && let Some(list) = &list {
                const LENGTH_THRESHOLD: usize = 3;
                let mut current_string_values = Vec::new();
                let mut current_string = String::new();
                let mut current_string_chars = 0;
                let mut next_block_id = next_block_id.clone();
                let mut entries = Vec::new();
                while let Some(next_block_id_non_null) = &next_block_id
                    && let Some(project_json::Sb3Block::Normal(next_block)) =
                        blocks.get(next_block_id_non_null.as_str())
                    && let Some(SpecialStackBlockInfo::AddToList) =
                        check_special_stack_block(next_block)
                    && matches!(
                        next_block.fields.get(LIST_STR),
                        Some(project_json::Sb3FieldValue::WithId { value: next_block_value, id: next_block_id }) if next_block_value == name && next_block_id == id
                    )
                {
                    let (expression, new_block_id) = unwrap_or_emit_message!(
                        convert_special_stack_block(
                            SpecialStackBlockInfo::AddToList,
                            next_block,
                            next_block_id_non_null,
                            blocks,
                            context,
                            target_index,
                        )
                        .map(|(statement, next_block_id)| {
                            (
                                match statement {
                                    ast_types::Statement::Call {
                                        function: _,
                                        arguments,
                                    } => arguments.into_iter().next_back(),
                                    _ => None,
                                },
                                next_block_id,
                            )
                        }),
                        context,
                        (None, next_block.next.as_deref().map(Into::into))
                    );
                    if let Some(ast_types::Expression::Literal(ast_types::Literal::String(value))) =
                        &expression
                        && value
                            .chars()
                            .try_fold(0, |state, _| (state == 0).then_some(1))
                            == Some(1)
                    {
                        current_string_values.push(value.clone());
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
                            for c in &current_string_values {
                                entries.push(ast_types::ListEntry::Expression(
                                    ast_types::Expression::Literal(ast_types::Literal::String(
                                        c.clone(),
                                    )),
                                ));
                            }
                        }
                        current_string_values.clear();
                        current_string.clear();
                        current_string_chars = 0;
                        entries.push(ast_types::ListEntry::Expression(
                            expression.unwrap_or_default(),
                        ));
                    }
                    next_block_id = new_block_id;
                }
                if current_string_chars >= LENGTH_THRESHOLD {
                    entries.push(ast_types::ListEntry::Unwrap(ast_types::Literal::String(
                        current_string.as_str().into(),
                    )));
                } else if current_string_chars > 0 {
                    entries.reserve(current_string_chars);
                    for c in &current_string_values {
                        entries.push(ast_types::ListEntry::Expression(
                            ast_types::Expression::Literal(ast_types::Literal::String(c.clone())),
                        ));
                    }
                }
                if !entries.is_empty() {
                    return Ok((
                        ast_types::Statement::ListAssignment {
                            target: create_simple_identifier(list.clone()),
                            value: entries,
                        },
                        next_block_id,
                    ));
                }
            }
            Ok((
                if is_list && let Some(list) = list {
                    ast_types::Statement::Call {
                        function: ast_types::identifier![list, literal!("clear")],
                        arguments: Vec::new(),
                    }
                } else {
                    ast_types::Statement::Call {
                        function: ast_types::identifier![literal!("delete_all_of_list")],
                        arguments: vec![
                            list.map(|value| {
                                ast_types::Expression::Identifier(create_simple_identifier(value))
                            })
                            .unwrap_or_default(),
                        ],
                    }
                },
                next_block_id,
            ))
        }
        SpecialStackBlockInfo::AddToList => {
            let Some(field_value) = block.fields.get(LIST_STR) else {
                return Err(GrazeDetranspilerError::MissingField {
                    field: LIST_STR.to_string(),
                    block_id: block_id.to_string(),
                });
            };
            let (name, id) = match field_value {
                project_json::Sb3FieldValue::Normal(name) => {
                    return Err(GrazeDetranspilerError::UnknownList {
                        id: "null".to_string(),
                        name: name.to_string(),
                    });
                }
                project_json::Sb3FieldValue::WithId { value, id } => (value, id),
            };
            let (list, is_list) =
                lookup_var_or_list(&name.as_cow_str(), id, target_index, context)?
                    .map(|value| {
                        (
                            Some(value.name.clone()),
                            matches!(value.kind, DetranspilerVarOrListKind::List { .. }),
                        )
                    })
                    .unwrap_or_else(|| {
                        emit_message(
                            context,
                            || {
                                GrazeDetranspilerWarning::UnknownVLBValue {
                                    field: LIST_STR.to_string(),
                                    block_id: block_id.to_string(),
                                }
                                .into()
                            },
                            GrazeMessageSetting::Warnings,
                        );
                        (None, false)
                    });
            let Some(input) = block.inputs.get(ITEM_STR) else {
                return Err(GrazeDetranspilerError::MissingInput {
                    input: ITEM_STR.to_string(),
                    block_id: block_id.to_string(),
                });
            };
            let input_repr = get_primary_input_repr(input);
            let item = match input_repr {
                project_json::Sb3InputRepr::Reference(block_id) => 'a: {
                    let inner_block = unwrap_or_emit_message!(
                        blocks.get(block_id).ok_or_else(|| {
                            GrazeDetranspilerError::InvalidBlockReference {
                                block_id: block_id.clone(),
                            }
                        }),
                        context,
                        break 'a ast_types::Expression::default()
                    );
                    convert_reporter_block(inner_block, block_id, blocks, context, target_index)?
                }
                project_json::Sb3InputRepr::PrimitiveBlock(block) => unwrap_or_emit_message!(
                    convert_primitive_reporter_block(block, context, target_index),
                    context,
                    ast_types::Expression::default()
                ),
                project_json::Sb3InputRepr::Missing => {
                    emit_error!(
                        GrazeDetranspilerError::MissingInput {
                            input: ITEM_STR.to_string(),
                            block_id: block_id.to_string(),
                        },
                        context
                    );
                    ast_types::Expression::default()
                }
            };
            if block.inputs.len() != 1 {
                for input in block.inputs.keys() {
                    if input.as_str() == ITEM_STR {
                        continue;
                    }
                    emit_message(
                        context,
                        || {
                            GrazeDetranspilerWarning::UnusedInput {
                                input: input.clone(),
                                block_id: block_id.to_string(),
                            }
                            .into()
                        },
                        GrazeMessageSetting::Warnings,
                    );
                }
            }
            if block.fields.len() != 1 {
                for field in block.fields.keys() {
                    if field.as_str() == LIST_STR {
                        continue;
                    }
                    emit_message(
                        context,
                        || {
                            GrazeDetranspilerWarning::UnusedField {
                                field: field.clone(),
                                block_id: block_id.to_string(),
                            }
                            .into()
                        },
                        GrazeMessageSetting::Warnings,
                    );
                }
            }
            Ok((
                if is_list && let Some(list) = list {
                    ast_types::Statement::Call {
                        function: ast_types::identifier![list, literal!("push")],
                        arguments: vec![item],
                    }
                } else {
                    ast_types::Statement::Call {
                        function: create_simple_identifier(literal!("add_to_list")),
                        arguments: vec![
                            list.map(|value| {
                                ast_types::Expression::Identifier(create_simple_identifier(value))
                            })
                            .unwrap_or_default(),
                            item,
                        ],
                    }
                },
                block.next.as_deref().map(Into::into),
            ))
        }
        SpecialStackBlockInfo::Assignment { kind, input_name } => {
            let Some(input) = block.inputs.get(input_name.as_str()) else {
                return Err(GrazeDetranspilerError::MissingInput {
                    input: input_name.to_string(),
                    block_id: block_id.to_string(),
                });
            };
            let input_repr = get_primary_input_repr(input);
            let value = match input_repr {
                project_json::Sb3InputRepr::Reference(block_id) => 'a: {
                    let inner_block = unwrap_or_emit_message!(
                        blocks.get(block_id).ok_or_else(|| {
                            GrazeDetranspilerError::InvalidBlockReference {
                                block_id: block_id.clone(),
                            }
                        }),
                        context,
                        break 'a ast_types::Expression::default()
                    );
                    convert_reporter_block(inner_block, block_id, blocks, context, target_index)?
                }
                project_json::Sb3InputRepr::PrimitiveBlock(block) => unwrap_or_emit_message!(
                    convert_primitive_reporter_block(block, context, target_index),
                    context,
                    ast_types::Expression::default()
                ),
                project_json::Sb3InputRepr::Missing => {
                    emit_error!(
                        GrazeDetranspilerError::MissingInput {
                            input: input_name.to_string(),
                            block_id: block_id.to_string(),
                        },
                        context
                    );
                    ast_types::Expression::default()
                }
            };
            Ok((
                ast_types::Statement::Assignment {
                    target: create_simple_identifier(kind.get_identifier()),
                    value,
                },
                block.next.as_deref().map(Into::into),
            ))
        }
        SpecialStackBlockInfo::AssignVariable => {
            const VARIABLE_STR: &str = "VARIABLE";
            const VALUE_STR: &str = "VALUE";
            let Some(value_input) = block.inputs.get(VALUE_STR) else {
                return Err(GrazeDetranspilerError::MissingInput {
                    input: VALUE_STR.to_string(),
                    block_id: block_id.to_string(),
                });
            };
            let value_input_repr = get_primary_input_repr(value_input);
            let value = match value_input_repr {
                project_json::Sb3InputRepr::Reference(block_id) => 'a: {
                    let inner_block = unwrap_or_emit_message!(
                        blocks.get(block_id).ok_or_else(|| {
                            GrazeDetranspilerError::InvalidBlockReference {
                                block_id: block_id.clone(),
                            }
                        }),
                        context,
                        break 'a ast_types::Expression::default()
                    );
                    convert_reporter_block(inner_block, block_id, blocks, context, target_index)?
                }
                project_json::Sb3InputRepr::PrimitiveBlock(block) => unwrap_or_emit_message!(
                    convert_primitive_reporter_block(block, context, target_index),
                    context,
                    ast_types::Expression::default()
                ),
                project_json::Sb3InputRepr::Missing => {
                    emit_error!(
                        GrazeDetranspilerError::MissingInput {
                            input: VALUE_STR.to_string(),
                            block_id: block_id.to_string(),
                        },
                        context
                    );
                    ast_types::Expression::default()
                }
            };
            let Some(field_value) = block.fields.get(VARIABLE_STR) else {
                return Err(GrazeDetranspilerError::MissingField {
                    field: VARIABLE_STR.to_string(),
                    block_id: block_id.to_string(),
                });
            };
            let (name, id) = match field_value {
                project_json::Sb3FieldValue::Normal(name) => {
                    return Err(GrazeDetranspilerError::UnknownVariable {
                        id: "null".to_string(),
                        name: name.to_string(),
                    });
                }
                project_json::Sb3FieldValue::WithId { value, id } => (value, id),
            };
            let (var, is_var) = lookup_var_or_list(&name.as_cow_str(), id, target_index, context)?
                .map(|value| {
                    (
                        Some(value.name.clone()),
                        matches!(value.kind, DetranspilerVarOrListKind::Variable { .. }),
                    )
                })
                .unwrap_or_else(|| {
                    emit_message(
                        context,
                        || {
                            GrazeDetranspilerWarning::UnknownVLBValue {
                                field: VARIABLE_STR.to_string(),
                                block_id: block_id.to_string(),
                            }
                            .into()
                        },
                        GrazeMessageSetting::Warnings,
                    );
                    (None, false)
                });
            if block.inputs.len() != 1 {
                for input in block.inputs.keys() {
                    if input.as_str() == VALUE_STR {
                        continue;
                    }
                    emit_message(
                        context,
                        || {
                            GrazeDetranspilerWarning::UnusedInput {
                                input: input.clone(),
                                block_id: block_id.to_string(),
                            }
                            .into()
                        },
                        GrazeMessageSetting::Warnings,
                    );
                }
            }
            if block.fields.len() != 1 {
                for field in block.fields.keys() {
                    if field.as_str() == VARIABLE_STR {
                        continue;
                    }
                    emit_message(
                        context,
                        || {
                            GrazeDetranspilerWarning::UnusedField {
                                field: field.clone(),
                                block_id: block_id.to_string(),
                            }
                            .into()
                        },
                        GrazeMessageSetting::Warnings,
                    );
                }
            }
            Ok((
                if is_var && let Some(var) = var {
                    ast_types::Statement::Assignment {
                        target: create_simple_identifier(var),
                        value,
                    }
                } else {
                    ast_types::Statement::Call {
                        function: create_simple_identifier(literal!("set_variable_to")),
                        arguments: vec![
                            var.map(|value| {
                                ast_types::Expression::Identifier(create_simple_identifier(value))
                            })
                            .unwrap_or_default(),
                            value,
                        ],
                    }
                },
                block.next.as_deref().map(Into::into),
            ))
        }
        SpecialStackBlockInfo::SetItem => {
            let Some(field_value) = block.fields.get(LIST_STR) else {
                return Err(GrazeDetranspilerError::MissingField {
                    field: LIST_STR.to_string(),
                    block_id: block_id.to_string(),
                });
            };
            let (name, id) = match field_value {
                project_json::Sb3FieldValue::Normal(name) => {
                    return Err(GrazeDetranspilerError::UnknownList {
                        id: "null".to_string(),
                        name: name.to_string(),
                    });
                }
                project_json::Sb3FieldValue::WithId { value, id } => (value, id),
            };
            let (list, is_list) =
                lookup_var_or_list(&name.as_cow_str(), id, target_index, context)?
                    .map(|value| {
                        (
                            Some(value.name.clone()),
                            matches!(value.kind, DetranspilerVarOrListKind::List { .. }),
                        )
                    })
                    .unwrap_or_else(|| {
                        emit_message(
                            context,
                            || {
                                GrazeDetranspilerWarning::UnknownVLBValue {
                                    field: LIST_STR.to_string(),
                                    block_id: block_id.to_string(),
                                }
                                .into()
                            },
                            GrazeMessageSetting::Warnings,
                        );
                        (None, false)
                    });
            let Some(item_input) = block.inputs.get(ITEM_STR) else {
                return Err(GrazeDetranspilerError::MissingInput {
                    input: ITEM_STR.to_string(),
                    block_id: block_id.to_string(),
                });
            };
            let item_input_repr = get_primary_input_repr(item_input);
            let item = match item_input_repr {
                project_json::Sb3InputRepr::Reference(block_id) => 'a: {
                    let inner_block = unwrap_or_emit_message!(
                        blocks.get(block_id).ok_or_else(|| {
                            GrazeDetranspilerError::InvalidBlockReference {
                                block_id: block_id.clone(),
                            }
                        }),
                        context,
                        break 'a ast_types::Expression::default()
                    );
                    convert_reporter_block(inner_block, block_id, blocks, context, target_index)?
                }
                project_json::Sb3InputRepr::PrimitiveBlock(block) => unwrap_or_emit_message!(
                    convert_primitive_reporter_block(block, context, target_index),
                    context,
                    ast_types::Expression::default()
                ),
                project_json::Sb3InputRepr::Missing => {
                    emit_error!(
                        GrazeDetranspilerError::MissingInput {
                            input: ITEM_STR.to_string(),
                            block_id: block_id.to_string(),
                        },
                        context
                    );
                    ast_types::Expression::default()
                }
            };
            let Some(index_input) = block.inputs.get(INDEX_STR) else {
                return Err(GrazeDetranspilerError::MissingInput {
                    input: INDEX_STR.to_string(),
                    block_id: block_id.to_string(),
                });
            };
            let index_input_repr = get_primary_input_repr(index_input);
            let index = match index_input_repr {
                project_json::Sb3InputRepr::Reference(block_id) => 'a: {
                    let inner_block = unwrap_or_emit_message!(
                        blocks.get(block_id).ok_or_else(|| {
                            GrazeDetranspilerError::InvalidBlockReference {
                                block_id: block_id.clone(),
                            }
                        }),
                        context,
                        break 'a ast_types::Expression::default()
                    );
                    convert_reporter_block(inner_block, block_id, blocks, context, target_index)?
                }
                project_json::Sb3InputRepr::PrimitiveBlock(block) => unwrap_or_emit_message!(
                    convert_primitive_reporter_block(block, context, target_index),
                    context,
                    ast_types::Expression::default()
                ),
                project_json::Sb3InputRepr::Missing => {
                    emit_error!(
                        GrazeDetranspilerError::MissingInput {
                            input: INDEX_STR.to_string(),
                            block_id: block_id.to_string(),
                        },
                        context
                    );
                    ast_types::Expression::default()
                }
            };
            if block.inputs.len() != 2 {
                for input in block.inputs.keys() {
                    if input.as_str() == ITEM_STR || input.as_str() == INDEX_STR {
                        continue;
                    }
                    emit_message(
                        context,
                        || {
                            GrazeDetranspilerWarning::UnusedInput {
                                input: input.clone(),
                                block_id: block_id.to_string(),
                            }
                            .into()
                        },
                        GrazeMessageSetting::Warnings,
                    );
                }
            }
            if block.fields.len() != 1 {
                for field in block.fields.keys() {
                    if field.as_str() == LIST_STR {
                        continue;
                    }
                    emit_message(
                        context,
                        || {
                            GrazeDetranspilerWarning::UnusedField {
                                field: field.clone(),
                                block_id: block_id.to_string(),
                            }
                            .into()
                        },
                        GrazeMessageSetting::Warnings,
                    );
                }
            }
            Ok((
                if is_list && let Some(list) = list {
                    ast_types::Statement::SetItem {
                        list: create_simple_identifier(list),
                        item: index,
                        value: item,
                    }
                } else {
                    ast_types::Statement::Call {
                        function: create_simple_identifier(literal!("replace_item_of_list")),
                        arguments: vec![
                            list.map(|value| {
                                ast_types::Expression::Identifier(create_simple_identifier(value))
                            })
                            .unwrap_or_default(),
                            index,
                            item,
                        ],
                    }
                },
                block.next.as_deref().map(Into::into),
            ))
        }
    }
}
