use std::collections::{HashMap, HashSet};

use arcstr::{ArcStr as IString, literal};
use grazelang_types::project_json;

use super::{
    core::{
        DetranspilerContext, DetranspilerResult, DetranspilerVarOrListKind, INDEX_STR, LIST_STR,
        NextBlockId, convert_block_stack, convert_field_value_info,
        convert_primitive_reporter_block, convert_reporter_block, create_simple_identifier,
        create_vlb_identifier, emit_error, emit_message, emit_message_eager, find_property_name,
        get_primary_input_repr, lookup_broadcast, lookup_var_or_list, lookup_vlb,
        unwrap_or_emit_message,
    },
    get_info::{
        SpecialReporterInfo, SpecialStackBlockInfo, check_special_stack_block, get_field_value_info,
    },
};
use crate::{
    ast::types::{self as ast_types, identifier},
    messages::types::{GrazeDetranspilerError, GrazeDetranspilerWarning},
    settings::GrazeMessageSetting,
};

macro_rules! get_required_input {
    ($block:expr, $block_id:expr, $input_name:expr, $input_name_key:expr, $blocks:expr, $context:expr, $target_index:expr) => {
        'a: {
            let Some(input) = $block.inputs.get($input_name_key) else {
                emit_error!(
                    GrazeDetranspilerError::MissingInput {
                        input: $input_name.to_string(),
                        block_id: $block_id.to_string(),
                    },
                    $context
                );
                break 'a ast_types::Expression::default();
            };
            let input_repr = get_primary_input_repr(input);
            match input_repr {
                project_json::Sb3InputRepr::Reference(block_id) => {
                    let inner_block = unwrap_or_emit_message!(
                        $blocks.get(block_id).ok_or_else(|| {
                            GrazeDetranspilerError::InvalidBlockReference {
                                block_id: block_id.clone(),
                            }
                        }),
                        $context,
                        break 'a ast_types::Expression::default()
                    );
                    convert_reporter_block(inner_block, block_id, $blocks, $context, $target_index)?
                }
                project_json::Sb3InputRepr::PrimitiveBlock(block) => unwrap_or_emit_message!(
                    convert_primitive_reporter_block(block, $context, $target_index),
                    $context,
                    ast_types::Expression::default()
                ),
                project_json::Sb3InputRepr::Missing => {
                    emit_error!(
                        GrazeDetranspilerError::MissingInput {
                            input: $input_name.to_string(),
                            block_id: $block_id.to_string(),
                        },
                        $context
                    );
                    ast_types::Expression::default()
                }
            }
        }
    };
}

macro_rules! if_else_expression {
    (true, $a:expr, $b:expr) => {
        $a
    };
    (false, $a:expr, $b:expr) => {
        $b
    };
}

macro_rules! get_vlb_field {
    ($block:expr, $block_id:expr, $field_name:expr, $field_name_key:expr, $kind_pat:pat, $error_ty:ident, $context:expr, $target_index:expr) => {
        get_vlb_field!(
            $block,
            $block_id,
            $field_name,
            $field_name_key,
            $kind_pat,
            $error_ty,
            $context,
            $target_index,
            false
        )
    };
    ($block:expr, $block_id:expr, $field_name:expr, $field_name_key:expr, $kind_pat:pat, $error_ty:ident, $context:expr, $target_index:expr, $return_vlb_data:ident) => {
        'a: {
            let Some(field_value) = $block.fields.get($field_name_key) else {
                emit_error!(
                    GrazeDetranspilerError::MissingField {
                        field: $field_name.to_string(),
                        block_id: $block_id.to_string(),
                    },
                    $context
                );
                break 'a if_else_expression!(
                    $return_vlb_data,
                    (None, false, None, None),
                    (None, false)
                );
            };
            let (name, id) = match field_value {
                project_json::Sb3FieldValue::Normal(name) => {
                    emit_error!(
                        GrazeDetranspilerError::$error_ty {
                            id: "null".to_string(),
                            name: name.to_string(),
                        },
                        $context
                    );
                    break 'a if_else_expression!(
                        $return_vlb_data,
                        (None, false, None, None),
                        (None, false)
                    );
                }
                project_json::Sb3FieldValue::WithId { value, id } => (value, id),
            };
            let (value, conforms) =
                lookup_var_or_list(&name.as_cow_str(), id, $target_index, $context)?
                    .map(|value| (Some(value.name.clone()), matches!(value.kind, $kind_pat)))
                    .unwrap_or_else(|| {
                        emit_message(
                            $context,
                            || {
                                GrazeDetranspilerWarning::UnknownVLBValue {
                                    field: $field_name.to_string(),
                                    block_id: $block_id.to_string(),
                                }
                                .into()
                            },
                            GrazeMessageSetting::Warnings,
                        );
                        (None, false)
                    });
            if_else_expression!(
                $return_vlb_data,
                (value, conforms, Some(name), Some(id)),
                (value, conforms)
            )
        }
    };
}

macro_rules! continue_if {
    (false) => {};
    ($condition:expr) => {
        if $condition {
            continue;
        }
    };
}

macro_rules! check_unused_fields {
    (true, $arg:pat => $continue_if:expr, $block:expr, $block_id:expr, $context:expr, $target_index:expr) => {
        for key in $block.fields.keys() {
            {
                let $arg = key;
                continue_if!($continue_if);
            }
            let arg = key.clone();
            emit_message(
                $context,
                || {
                    GrazeDetranspilerWarning::UnusedField {
                        field: arg,
                        block_id: $block_id.to_string(),
                    }
                    .into()
                },
                GrazeMessageSetting::Warnings,
            );
        }
    };
    ($condition:expr, $arg:pat => $continue_if:expr, $block:expr, $block_id:expr, $context:expr, $target_index:expr) => {
        if $condition {
            check_unused_fields!(true, $arg => $continue_if, $block, $block_id, $context, $target_index);
        }
    }
}

macro_rules! check_unused_inputs {
    (true, $arg:pat => $continue_if:expr, $block:expr, $block_id:expr, $context:expr, $target_index:expr) => {
        for key in $block.inputs.keys() {
            {
                let $arg = key;
                continue_if!($continue_if);
            }
            let arg = key.clone();
            emit_message(
                $context,
                || {
                    GrazeDetranspilerWarning::UnusedInput {
                        input: arg,
                        block_id: $block_id.to_string(),
                    }
                    .into()
                },
                GrazeMessageSetting::Warnings,
            );
        }
    };
    ($condition:expr, $arg:pat => $continue_if:expr, $block:expr, $block_id:expr, $context:expr, $target_index:expr) => {
        if $condition {
            check_unused_inputs!(true, $arg => $continue_if, $block, $block_id, $context, $target_index);
        }
    }
}

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
            check_unused_fields!(true, _ => false, block, block_id, context, target_index);
            check_unused_inputs!(
                block.inputs.len() != operands_present,
                key => key.as_str() == left_operand.as_str()
                    || key.as_str() == right_operand.as_str(),
                block,
                block_id,
                context,
                target_index
            );
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
            check_unused_fields!(true, _ => false, block, block_id, context, target_index);
            check_unused_inputs!(
                block.inputs.len() != 1,
                key => key.as_str() == outer_operand.as_str(),
                block,
                block_id,
                context,
                target_index
            );
            check_unused_fields!(true, _ => false, inner_block, inner_block_id, context, target_index);
            check_unused_inputs!(
                inner_block.inputs.len() != inner_operands_present,
                key => key.as_str() == inner_left_operand.as_str()
                    || key.as_str() == inner_right_operand.as_str(),
                inner_block,
                inner_block_id,
                context,
                target_index
            );
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
            check_unused_fields!(
                block.fields.len() != unused_field_present,
                key => matches!(&unused_field, Some(unused_field) if key.as_str() == unused_field.as_str()),
                block,
                block_id,
                context,
                target_index
            );
            check_unused_inputs!(
                block.inputs.len() != operands_present,
                key => key.as_str() == operand.as_str()
                    || matches!(&unused_operand, Some(unused_operand) if key.as_str() == unused_operand.as_str()),
                block,
                block_id,
                context,
                target_index
            );
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
                check_unused_fields!(
                    true,
                    _ => false,
                    block,
                    block_id,
                    context,
                    target_index
                );
                check_unused_inputs!(
                    block.inputs.len() != operands_present,
                    key => key.as_str() == LEFT_OPERAND || key.as_str() == RIGHT_OPERAND,
                    block,
                    block_id,
                    context,
                    target_index
                );
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
            check_unused_fields!(
                true,
                _ => false,
                block,
                block_id,
                context,
                target_index
            );
            check_unused_inputs!(
                block.inputs.len() != operands_present,
                key => key.as_str() == LEFT_OPERAND || key.as_str() == RIGHT_OPERAND,
                block,
                block_id,
                context,
                target_index
            );
            ast_types::Expression::BinOp {
                operator: ast_types::BinOp::Join,
                left_operand: Box::new(left_operand_expression),
                right_operand: Box::new(right_operand_expression),
            }
        }
        SpecialReporterInfo::GetItem => {
            let (list, is_list) = get_vlb_field!(
                block,
                block_id,
                LIST_STR,
                LIST_STR,
                DetranspilerVarOrListKind::List { .. },
                UnknownList,
                context,
                target_index
            );
            let index = get_required_input!(
                block,
                block_id,
                INDEX_STR,
                INDEX_STR,
                blocks,
                context,
                target_index
            );
            check_unused_fields!(
                block.fields.len() != 1,
                field => field.as_str() == LIST_STR,
                block,
                block_id,
                context,
                target_index
            );
            check_unused_inputs!(
                block.inputs.len() != 1,
                input => input.as_str() == INDEX_STR,
                block,
                block_id,
                context,
                target_index
            );
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
        SpecialReporterInfo::PropertyOf => {
            const MENU_OPCODE: &str = "sensing_of_object_menu";
            const PROPERTY_STR: &str = "PROPERTY";
            const OBJECT_STR: &str = "OBJECT";
            let (object, target) = 'a: {
                let Some(input) = block.inputs.get(OBJECT_STR) else {
                    emit_error!(
                        GrazeDetranspilerError::MissingInput {
                            input: OBJECT_STR.to_string(),
                            block_id: block_id.to_string(),
                        },
                        context
                    );
                    break 'a (Some(ast_types::Expression::default()), None);
                };
                let input_repr = get_primary_input_repr(input);
                match input_repr {
                    project_json::Sb3InputRepr::Reference(block_id) => {
                        let inner_block = unwrap_or_emit_message!(
                            blocks.get(block_id).ok_or_else(|| {
                                GrazeDetranspilerError::InvalidBlockReference {
                                    block_id: block_id.clone(),
                                }
                            }),
                            context,
                            break 'a (Some(ast_types::Expression::default()), None)
                        );
                        if let project_json::Sb3Block::Normal(inner_block) = inner_block
                            && inner_block.opcode.as_str() == MENU_OPCODE
                        {
                            for key in inner_block.fields.keys() {
                                if key.as_str() == OBJECT_STR {
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
                            if let Some(field_value) = inner_block.fields.get(OBJECT_STR) {
                                if let project_json::Sb3FieldValue::Normal(field_value) =
                                    field_value
                                    && let Some(identifier) = context
                                        .target_indices
                                        .get(&*field_value.as_cow_str())
                                        .and_then(|value| {
                                            context
                                                .targets
                                                .get(*value)
                                                .map(|target| (target, *value))
                                        })
                                        .map(|value| (value.0.internal_name.clone(), value.1))
                                {
                                    (None, Some(identifier))
                                } else {
                                    (
                                        Some(unwrap_or_emit_message!(
                                            convert_field_value_info(
                                                get_field_value_info(field_value, MENU_OPCODE),
                                                field_value,
                                                target_index,
                                                context
                                            ),
                                            context,
                                            ast_types::Expression::default()
                                        )),
                                        None,
                                    )
                                }
                            } else {
                                emit_error!(
                                    GrazeDetranspilerError::MissingMenuField {
                                        field: OBJECT_STR.to_string(),
                                        block_id: block_id.to_string(),
                                    },
                                    context
                                );
                                break 'a (Some(ast_types::Expression::default()), None);
                            }
                        } else {
                            (
                                Some(convert_reporter_block(
                                    inner_block,
                                    block_id,
                                    blocks,
                                    context,
                                    target_index,
                                )?),
                                None,
                            )
                        }
                    }
                    project_json::Sb3InputRepr::PrimitiveBlock(block) => (
                        Some(unwrap_or_emit_message!(
                            convert_primitive_reporter_block(block, context, target_index),
                            context,
                            ast_types::Expression::default()
                        )),
                        None,
                    ),
                    project_json::Sb3InputRepr::Missing => {
                        emit_error!(
                            GrazeDetranspilerError::MissingInput {
                                input: OBJECT_STR.to_string(),
                                block_id: block_id.to_string(),
                            },
                            context
                        );
                        (Some(ast_types::Expression::default()), None)
                    }
                }
            };
            let Some(field_value) = block.fields.get(PROPERTY_STR) else {
                emit_error!(
                    GrazeDetranspilerError::MissingField {
                        field: PROPERTY_STR.to_string(),
                        block_id: block_id.to_string(),
                    },
                    context
                );
                return Ok(ast_types::Expression::default());
            };
            let (property, property_name, target) =
                if let project_json::Sb3FieldValue::Normal(value) = field_value
                    && let Some(property_name) = target.as_ref().and_then(|target| {
                        find_property_name(
                            context.targets.get(target.1).unwrap(),
                            &value.as_cow_str(),
                        )
                    })
                    && let Some(target) = target
                {
                    (None, Some(property_name), Some(target.0))
                } else {
                    (
                        Some(unwrap_or_emit_message!(
                            Ok(match field_value {
                                project_json::Sb3FieldValue::Normal(value) => {
                                    ast_types::Expression::Literal(value.into())
                                }
                                project_json::Sb3FieldValue::WithId { value, id } => 'a: {
                                    ast_types::Expression::Identifier({
                                        let Some(vlb) = unwrap_or_emit_message!(
                                            lookup_vlb(
                                                &value.as_cow_str(),
                                                id,
                                                target_index,
                                                context
                                            ),
                                            context,
                                            break 'a ast_types::Expression::default()
                                        ) else {
                                            emit_error!(
                                                GrazeDetranspilerError::UnknownVariable {
                                                    id: id.clone(),
                                                    name: value.to_string(),
                                                },
                                                context
                                            );
                                            break 'a ast_types::Expression::default();
                                        };
                                        create_vlb_identifier(vlb.into())
                                    })
                                }
                            }),
                            context,
                            ast_types::Expression::default()
                        )),
                        None,
                        target.map(|(value, _)| value),
                    )
                };
            if let Some(property_name) = property_name
                && let Some(target) = target
            {
                ast_types::Expression::Identifier(identifier![target, property_name])
            } else if let Some(object) = object
                && let Some(property) = property
            {
                ast_types::Expression::Call {
                    function: create_simple_identifier(literal!("property_of_object")),
                    arguments: vec![property, object],
                }
            } else if let Some(target) = target
                && let Some(property) = property
            {
                ast_types::Expression::Call {
                    function: create_simple_identifier(literal!("property_of_object")),
                    arguments: vec![
                        property,
                        ast_types::Expression::Identifier(create_simple_identifier(target)),
                    ],
                }
            } else {
                ast_types::Expression::default()
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
            check_unused_fields!(
                true,
                _ => false,
                block,
                block_id,
                context,
                target_index
            );
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
                check_unused_fields!(
                    true,
                    _ => false,
                    block,
                    block_id,
                    context,
                    target_index
                );
                check_unused_inputs!(
                    tracked_args != block.inputs.len(),
                    key => matches!(key.as_str(), "CONDITION" | "SUBSTACK"),
                    block,
                    block_id,
                    context,
                    target_index
                );
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
                check_unused_fields!(
                    true,
                    _ => false,
                    block,
                    block_id,
                    context,
                    target_index
                );
                check_unused_inputs!(
                    tracked_args != block.inputs.len(),
                    key => matches!(key.as_str(), "CONDITION" | "SUBSTACK" | "SUBSTACK2"),
                    block,
                    block_id,
                    context,
                    target_index
                );
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
            let (list, is_list, name, id) = get_vlb_field!(
                block,
                block_id,
                LIST_STR,
                LIST_STR,
                DetranspilerVarOrListKind::List { .. },
                UnknownList,
                context,
                target_index,
                true
            );
            check_unused_fields!(
                block.fields.len() != 1,
                field => field.as_str() == LIST_STR,
                block,
                block_id,
                context,
                target_index
            );
            check_unused_inputs!(
                true,
                _ => false,
                block,
                block_id,
                context,
                target_index
            );
            let next_block_id = block.next.as_deref().map(IString::from);
            if is_list
                && let Some(name) = name
                && let Some(id) = id
                && let Some(list) = &list
            {
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
            let (list, is_list) = get_vlb_field!(
                block,
                block_id,
                LIST_STR,
                LIST_STR,
                DetranspilerVarOrListKind::List { .. },
                UnknownList,
                context,
                target_index
            );
            let item = get_required_input!(
                block,
                block_id,
                ITEM_STR,
                ITEM_STR,
                blocks,
                context,
                target_index
            );
            check_unused_fields!(
                block.fields.len() != 1,
                field => field.as_str() == LIST_STR,
                block,
                block_id,
                context,
                target_index
            );
            check_unused_inputs!(
                block.inputs.len() != 1,
                input => input.as_str() == ITEM_STR,
                block,
                block_id,
                context,
                target_index
            );
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
            let value = get_required_input!(
                block,
                block_id,
                input_name,
                input_name.as_str(),
                blocks,
                context,
                target_index
            );
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
            let value = get_required_input!(
                block,
                block_id,
                VALUE_STR,
                VALUE_STR,
                blocks,
                context,
                target_index
            );
            let (var, is_var) = get_vlb_field!(
                block,
                block_id,
                VARIABLE_STR,
                VARIABLE_STR,
                DetranspilerVarOrListKind::Variable { .. },
                UnknownVariable,
                context,
                target_index
            );
            check_unused_fields!(
                block.fields.len() != 1,
                field => field.as_str() == VARIABLE_STR,
                block,
                block_id,
                context,
                target_index
            );
            check_unused_inputs!(
                block.inputs.len() != 1,
                input => input.as_str() == VALUE_STR,
                block,
                block_id,
                context,
                target_index
            );
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
            let (list, is_list) = get_vlb_field!(
                block,
                block_id,
                LIST_STR,
                LIST_STR,
                DetranspilerVarOrListKind::List { .. },
                UnknownList,
                context,
                target_index
            );
            let item = get_required_input!(
                block,
                block_id,
                ITEM_STR,
                ITEM_STR,
                blocks,
                context,
                target_index
            );
            let index = get_required_input!(
                block,
                block_id,
                INDEX_STR,
                INDEX_STR,
                blocks,
                context,
                target_index
            );
            check_unused_fields!(
                block.fields.len() != 1,
                field => field.as_str() == LIST_STR,
                block,
                block_id,
                context,
                target_index
            );
            check_unused_inputs!(
                block.inputs.len() != 2,
                input => input.as_str() == ITEM_STR || input.as_str() == INDEX_STR,
                block,
                block_id,
                context,
                target_index
            );
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
