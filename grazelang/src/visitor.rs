use std::ops::Deref;

use crate::{
    lexer::PosRange,
    parser::ast::{
        AssetDeclaration, BackdropKeyword, BinOp, BroadcastKeyword, CanonicalIdentifier, CodeBlock,
        Comma, CostumeKeyword, DataDeclaration, DataDeclarationScope, Expression,
        FormattedStringContent, GrazeProgram, Identifier, LeftBrace, LeftBracket, LeftParens,
        LetKeyword, LetterAccessLeftBracket, ListEntry, ListKeyword, ListsKeyword, Literal,
        NormalAssignmentOperator, RightBrace, RightBracket, RightParens, Semicolon,
        SingleDataDeclaration, SoundKeyword, SpriteCodeBlock, SpriteKeyword, SpriteStatement,
        StageCodeBlock, StageKeyword, StageStatement, Statement, SyntacticElse, SyntacticIf,
        TopLevelStatement, UnOp, VarKeyword, VarsKeyword,
    },
};
pub trait GrazeVisitor<C, E> {
    fn visit_graze_program(&self, value: &GrazeProgram, context: &mut C) -> Result<(), E> {
        default_visit_graze_program(self, value, context)
    }

    fn visit_top_level_statement(
        &self,
        value: &TopLevelStatement,
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_top_level_statement(self, value, context)
    }

    fn visit_top_level_statement_stage(
        &self,
        value: (
            &StageKeyword,
            &StageCodeBlock,
            &Option<Semicolon>,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_top_level_statement_stage(self, value, context)
    }

    fn visit_stage_code_block(&self, value: &StageCodeBlock, context: &mut C) -> Result<(), E> {
        default_visit_stage_code_block(self, value, context)
    }

    fn visit_top_level_statement_sprite(
        &self,
        value: (
            &SpriteKeyword,
            &Option<CanonicalIdentifier>,
            &Identifier,
            &SpriteCodeBlock,
            &Option<Semicolon>,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_top_level_statement_sprite(self, value, context)
    }

    fn visit_sprite_code_block(&self, value: &SpriteCodeBlock, context: &mut C) -> Result<(), E> {
        default_visit_sprite_code_block(self, value, context)
    }

    fn visit_top_level_statement_broadcast_declaration(
        &self,
        value: (
            &BroadcastKeyword,
            &Option<CanonicalIdentifier>,
            &Identifier,
            &Semicolon,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_top_level_statement_broadcast_declaration(self, value, context)
    }

    fn visit_empty_statement(&self, value: &Semicolon, context: &mut C) -> Result<(), E> {
        default_visit_empty_statement(self, value, context)
    }

    fn visit_stage_statement(&self, value: &StageStatement, context: &mut C) -> Result<(), E> {
        default_visit_stage_statement(self, value, context)
    }

    fn visit_sprite_statement(&self, value: &SpriteStatement, context: &mut C) -> Result<(), E> {
        default_visit_sprite_statement(self, value, context)
    }

    fn visit_stage_statement_data_declaration(
        &self,
        value: (&LetKeyword, &DataDeclaration, &Semicolon, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_stage_statement_data_declaration(self, value, context)
    }

    fn visit_stage_statement_backdrop_declaration(
        &self,
        value: (&BackdropKeyword, &AssetDeclaration, &Semicolon, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_stage_statement_backdrop_declaration(self, value, context)
    }

    fn visit_sound_declaration(
        &self,
        value: (&SoundKeyword, &AssetDeclaration, &Semicolon, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_sound_declaration(self, value, context)
    }

    fn visit_single_input_hat_statement(
        &self,
        value: (
            &Identifier,
            &Expression,
            &CodeBlock,
            &Option<Semicolon>,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_single_input_hat_statement(self, value, context)
    }

    fn visit_multi_input_hat_statement(
        &self,
        value: (
            &Identifier,
            &LeftParens,
            &Vec<(Expression, Option<Comma>)>,
            &RightParens,
            &CodeBlock,
            &Option<Semicolon>,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_multi_input_hat_statement(self, value, context)
    }

    fn visit_isolated_block(
        &self,
        value: (&CodeBlock, &Option<Semicolon>, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_isolated_block(self, value, context)
    }

    fn visit_isolated_expression(
        &self,
        value: (
            &LeftParens,
            &Expression,
            &RightParens,
            &Option<Semicolon>,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_isolated_expression(self, value, context)
    }

    fn visit_sprite_statement_data_declaration(
        &self,
        value: (&LetKeyword, &DataDeclaration, &Semicolon, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_sprite_statement_data_declaration(self, value, context)
    }

    fn visit_sprite_statement_costume_declaration(
        &self,
        value: (&CostumeKeyword, &AssetDeclaration, &Semicolon, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_sprite_statement_costume_declaration(self, value, context)
    }

    fn visit_code_block(&self, value: &CodeBlock, context: &mut C) -> Result<(), E> {
        default_visit_code_block(self, value, context)
    }

    fn visit_statement(&self, value: &Statement, context: &mut C) -> Result<(), E> {
        default_visit_statement(self, value, context)
    }

    fn visit_statement_data_declaration(
        &self,
        value: (&LetKeyword, &DataDeclaration, &Semicolon, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_statement_data_declaration(self, value, context)
    }

    fn visit_statement_assignment(
        &self,
        value: (
            &Identifier,
            &NormalAssignmentOperator,
            &Expression,
            &Semicolon,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_statement_assignment(self, value, context)
    }

    fn visit_statement_list_assignment(
        &self,
        value: (
            &Identifier,
            &NormalAssignmentOperator,
            &LeftBracket,
            &Vec<(ListEntry, Option<Comma>)>,
            &RightBracket,
            &Semicolon,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_statement_list_assignment(self, value, context)
    }

    fn visit_statement_set_item(
        &self,
        value: (
            &Identifier,
            &LeftBracket,
            &Expression,
            &RightBracket,
            &NormalAssignmentOperator,
            &Expression,
            &Semicolon,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_statement_set_item(self, value, context)
    }

    fn visit_statement_call(
        &self,
        value: (
            &Identifier,
            &LeftParens,
            &Vec<(Expression, Option<Comma>)>,
            &RightParens,
            &Semicolon,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_statement_call(self, value, context)
    }

    fn visit_statement_single_input_control(
        &self,
        value: (
            &Identifier,
            &Expression,
            &CodeBlock,
            &Option<Semicolon>,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_statement_single_input_control(self, value, context)
    }

    fn visit_statement_multi_input_control(
        &self,
        value: (
            &Identifier,
            &LeftParens,
            &Vec<(Expression, Option<Comma>)>,
            &RightParens,
            &CodeBlock,
            &Option<Semicolon>,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_statement_multi_input_control(self, value, context)
    }

    fn visit_statement_forever(
        &self,
        value: (&Identifier, &CodeBlock, &Option<Semicolon>, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_statement_forever(self, value, context)
    }

    fn visit_statement_if_else(
        &self,
        value: (
            &(SyntacticIf, Expression, CodeBlock),
            &Vec<(SyntacticElse, SyntacticIf, Expression, CodeBlock)>,
            &Option<(SyntacticElse, CodeBlock)>,
            &Option<Semicolon>,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_statement_if_else(self, value, context)
    }

    fn visit_expression(&self, value: &Expression, context: &mut C) -> Result<(), E> {
        default_visit_expression(self, value, context)
    }

    fn visit_list_content(
        &self,
        value: &Vec<(ListEntry, Option<Comma>)>,
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_list_content(self, value, context)
    }

    fn visit_data_declaration(&self, value: &DataDeclaration, context: &mut C) -> Result<(), E> {
        default_visit_data_declaration(self, value, context)
    }

    fn visit_mixed_data_declaration(
        &self,
        value: (
            &DataDeclarationScope,
            &LeftParens,
            &Vec<(SingleDataDeclaration, Option<Comma>)>,
            &RightParens,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_mixed_data_declaration(self, value, context)
    }

    fn visit_vars_data_declaration(
        &self,
        value: (
            &DataDeclarationScope,
            &VarsKeyword,
            &LeftBrace,
            &Vec<(SingleDataDeclaration, Option<Comma>)>,
            &RightBrace,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_vars_data_declaration(self, value, context)
    }

    fn visit_lists_data_declaration(
        &self,
        value: (
            &DataDeclarationScope,
            &ListsKeyword,
            &LeftBrace,
            &Vec<(SingleDataDeclaration, Option<Comma>)>,
            &RightBrace,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_lists_data_declaration(self, value, context)
    }

    fn visit_single_data_declaration(
        &self,
        value: &SingleDataDeclaration,
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_single_data_declaration(self, value, context)
    }

    fn visit_single_variable_declaration(
        &self,
        value: (
            &Option<VarKeyword>,
            &DataDeclarationScope,
            &Option<CanonicalIdentifier>,
            &Identifier,
            &NormalAssignmentOperator,
            &Expression,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_single_variable_declaration(self, value, context)
    }

    fn visit_single_empty_variable_declaration(
        &self,
        value: (
            &Option<VarKeyword>,
            &DataDeclarationScope,
            &Option<CanonicalIdentifier>,
            &Identifier,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_single_empty_variable_declaration(self, value, context)
    }

    fn visit_single_list_declaration(
        &self,
        value: (
            &Option<ListKeyword>,
            &DataDeclarationScope,
            &Option<CanonicalIdentifier>,
            &Identifier,
            &NormalAssignmentOperator,
            &LeftBracket,
            &Vec<(ListEntry, Option<Comma>)>,
            &RightBracket,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_single_list_declaration(self, value, context)
    }

    fn visit_single_empty_list_declaration(
        &self,
        value: (
            &Option<ListKeyword>,
            &DataDeclarationScope,
            &Option<CanonicalIdentifier>,
            &Identifier,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_single_empty_list_declaration(self, value, context)
    }

    fn visit_expression_literal(&self, value: &Literal, context: &mut C) -> Result<(), E> {
        default_visit_expression_literal(self, value, context)
    }

    fn visit_expression_formatted_string(
        &self,
        value: (&Vec<FormattedStringContent>, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_expression_formatted_string(self, value, context)
    }

    fn visit_expression_binary_operation(
        &self,
        value: (&Box<Expression>, &BinOp, &Box<Expression>, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_expression_binary_operation(self, value, context)
    }

    fn visit_expression_unary_operation(
        &self,
        value: (&UnOp, &Box<Expression>, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_expression_unary_operation(self, value, context)
    }

    fn visit_expression_identifier(&self, value: &Identifier, context: &mut C) -> Result<(), E> {
        default_visit_expression_identifier(self, value, context)
    }

    fn visit_expression_call(
        &self,
        value: (
            &Identifier,
            &LeftParens,
            &Vec<(Expression, Option<Comma>)>,
            &RightParens,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_expression_call(self, value, context)
    }

    fn visit_expression_get_item(
        &self,
        value: (
            &Identifier,
            &LeftBracket,
            &Box<Expression>,
            &RightBracket,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_expression_get_item(self, value, context)
    }

    fn visit_expression_get_letter(
        &self,
        value: (
            &Box<Expression>,
            &LetterAccessLeftBracket,
            &Box<Expression>,
            &RightBracket,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_expression_get_letter(self, value, context)
    }

    fn visit_expression_parentheses(
        &self,
        value: (&LeftParens, &Box<Expression>, &RightParens, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_expression_parentheses(self, value, context)
    }

    fn visit_formatted_string_content(
        &self,
        value: &FormattedStringContent,
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_formatted_string_content(self, value, context)
    }

    // fn visit_(
    //     &self,
    //     value: &Literal,
    //     context: &mut C,
    // ) -> Result<(), E> {
    //     Ok(())
    // }
}

pub fn default_visit_graze_program<V, C, E>(
    visitor: &V,
    value: &GrazeProgram,
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    for item in &value.0 {
        visitor.visit_top_level_statement(item, context)?;
    }
    Ok(())
}

pub fn default_visit_top_level_statement<V, C, E>(
    visitor: &V,
    value: &TopLevelStatement,
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    match value {
        TopLevelStatement::Stage(stage_keyword, stage_code_block, semicolon, pos_range) => {
            visitor.visit_top_level_statement_stage(
                (stage_keyword, stage_code_block, semicolon, pos_range),
                context,
            )?;
        }
        TopLevelStatement::Sprite(
            sprite_keyword,
            canonical_identifier,
            identifier,
            sprite_code_block,
            semicolon,
            pos_range,
        ) => visitor.visit_top_level_statement_sprite(
            (
                sprite_keyword,
                canonical_identifier,
                identifier,
                sprite_code_block,
                semicolon,
                pos_range,
            ),
            context,
        )?,
        TopLevelStatement::BroadcastDeclaration(
            broadcast_keyword,
            canonical_identifier,
            identifier,
            semicolon,
            pos_range,
        ) => {
            visitor.visit_top_level_statement_broadcast_declaration(
                (
                    broadcast_keyword,
                    canonical_identifier,
                    identifier,
                    semicolon,
                    pos_range,
                ),
                context,
            )?;
        }
        TopLevelStatement::EmptyStatement(semicolon) => {
            visitor.visit_empty_statement(semicolon, context)?;
        }
    }
    Ok(())
}

pub fn default_visit_top_level_statement_stage<V, C, E>(
    visitor: &V,
    value: (
        &StageKeyword,
        &StageCodeBlock,
        &Option<Semicolon>,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_stage_code_block(value.1, context)?;
    Ok(())
}

pub fn default_visit_stage_code_block<V, C, E>(
    visitor: &V,
    value: &StageCodeBlock,
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    for item in &value.statements {
        visitor.visit_stage_statement(item, context)?;
    }
    Ok(())
}

pub fn default_visit_top_level_statement_sprite<V, C, E>(
    visitor: &V,
    value: (
        &SpriteKeyword,
        &Option<CanonicalIdentifier>,
        &Identifier,
        &SpriteCodeBlock,
        &Option<Semicolon>,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_sprite_code_block(value.3, context)?;
    Ok(())
}

pub fn default_visit_sprite_code_block<V, C, E>(
    visitor: &V,
    value: &SpriteCodeBlock,
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    for item in &value.statements {
        visitor.visit_sprite_statement(item, context)?;
    }
    Ok(())
}

pub fn default_visit_top_level_statement_broadcast_declaration<V, C, E>(
    visitor: &V,
    value: (
        &BroadcastKeyword,
        &Option<CanonicalIdentifier>,
        &Identifier,
        &Semicolon,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    Ok(())
}

pub fn default_visit_empty_statement<V, C, E>(
    visitor: &V,
    value: &Semicolon,
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    Ok(())
}

pub fn default_visit_stage_statement<V, C, E>(
    visitor: &V,
    value: &StageStatement,
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    match value {
        StageStatement::DataDeclaration(let_keyword, data_declaration, semicolon, pos_range) => {
            visitor.visit_stage_statement_data_declaration(
                (let_keyword, data_declaration, semicolon, pos_range),
                context,
            )?;
        }
        StageStatement::BackdropDeclaration(
            backdrop_keyword,
            asset_declaration,
            semicolon,
            pos_range,
        ) => {
            visitor.visit_stage_statement_backdrop_declaration(
                (backdrop_keyword, asset_declaration, semicolon, pos_range),
                context,
            )?;
        }
        StageStatement::SoundDeclaration(
            sound_keyword,
            asset_declaration,
            semicolon,
            pos_range,
        ) => {
            visitor.visit_sound_declaration(
                (sound_keyword, asset_declaration, semicolon, pos_range),
                context,
            )?;
        }
        StageStatement::SingleInputHatStatement(
            identifier,
            expression,
            code_block,
            semicolon,
            pos_range,
        ) => {
            visitor.visit_single_input_hat_statement(
                (identifier, expression, code_block, semicolon, pos_range),
                context,
            )?;
        }
        StageStatement::MultiInputHatStatement(
            identifier,
            left_parens,
            items,
            right_parens,
            code_block,
            semicolon,
            pos_range,
        ) => {
            visitor.visit_multi_input_hat_statement(
                (
                    identifier,
                    left_parens,
                    items,
                    right_parens,
                    code_block,
                    semicolon,
                    pos_range,
                ),
                context,
            )?;
        }
        StageStatement::IsolatedBlock(code_block, semicolon, pos_range) => {
            visitor.visit_isolated_block((code_block, semicolon, pos_range), context)?;
        }
        StageStatement::IsolatedExpression(
            left_parens,
            expression,
            right_parens,
            semicolon,
            pos_range,
        ) => {
            visitor.visit_isolated_expression(
                (left_parens, expression, right_parens, semicolon, pos_range),
                context,
            )?;
        }
        StageStatement::EmptyStatement(semicolon) => {
            visitor.visit_empty_statement(semicolon, context)?;
        }
    }
    Ok(())
}

pub fn default_visit_sprite_statement<V, C, E>(
    visitor: &V,
    value: &SpriteStatement,
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    match value {
        SpriteStatement::DataDeclaration(let_keyword, data_declaration, semicolon, pos_range) => {
            visitor.visit_sprite_statement_data_declaration(
                (let_keyword, data_declaration, semicolon, pos_range),
                context,
            )?;
        }
        SpriteStatement::CostumeDeclaration(
            costume_keyword,
            asset_declaration,
            semicolon,
            pos_range,
        ) => {
            visitor.visit_sprite_statement_costume_declaration(
                (costume_keyword, asset_declaration, semicolon, pos_range),
                context,
            )?;
        }
        SpriteStatement::SoundDeclaration(
            sound_keyword,
            asset_declaration,
            semicolon,
            pos_range,
        ) => {
            visitor.visit_sound_declaration(
                (sound_keyword, asset_declaration, semicolon, pos_range),
                context,
            )?;
        }
        SpriteStatement::SingleInputHatStatement(
            identifier,
            expression,
            code_block,
            semicolon,
            pos_range,
        ) => {
            visitor.visit_single_input_hat_statement(
                (identifier, expression, code_block, semicolon, pos_range),
                context,
            )?;
        }
        SpriteStatement::MultiInputHatStatement(
            identifier,
            left_parens,
            items,
            right_parens,
            code_block,
            semicolon,
            pos_range,
        ) => {
            visitor.visit_multi_input_hat_statement(
                (
                    identifier,
                    left_parens,
                    items,
                    right_parens,
                    code_block,
                    semicolon,
                    pos_range,
                ),
                context,
            )?;
        }
        SpriteStatement::IsolatedBlock(code_block, semicolon, pos_range) => {
            visitor.visit_isolated_block((code_block, semicolon, pos_range), context)?;
        }
        SpriteStatement::IsolatedExpression(
            left_parens,
            expression,
            right_parens,
            semicolon,
            pos_range,
        ) => {
            visitor.visit_isolated_expression(
                (left_parens, expression, right_parens, semicolon, pos_range),
                context,
            )?;
        }
        SpriteStatement::EmptyStatement(semicolon) => {
            visitor.visit_empty_statement(semicolon, context)?;
        }
    }
    Ok(())
}

pub fn default_visit_stage_statement_data_declaration<V, C, E>(
    visitor: &V,
    value: (&LetKeyword, &DataDeclaration, &Semicolon, &PosRange),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_data_declaration(value.1, context)?;
    Ok(())
}

pub fn default_visit_stage_statement_backdrop_declaration<V, C, E>(
    visitor: &V,
    value: (&BackdropKeyword, &AssetDeclaration, &Semicolon, &PosRange),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    Ok(())
}

pub fn default_visit_sound_declaration<V, C, E>(
    visitor: &V,
    value: (&SoundKeyword, &AssetDeclaration, &Semicolon, &PosRange),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    Ok(())
}

pub fn default_visit_single_input_hat_statement<V, C, E>(
    visitor: &V,
    value: (
        &Identifier,
        &Expression,
        &CodeBlock,
        &Option<Semicolon>,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_expression(value.1, context)?;
    visitor.visit_code_block(value.2, context)?;
    Ok(())
}

pub fn default_visit_multi_input_hat_statement<V, C, E>(
    visitor: &V,
    value: (
        &Identifier,
        &LeftParens,
        &Vec<(Expression, Option<Comma>)>,
        &RightParens,
        &CodeBlock,
        &Option<Semicolon>,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_code_block(value.4, context)?;
    Ok(())
}

pub fn default_visit_isolated_block<V, C, E>(
    visitor: &V,
    value: (&CodeBlock, &Option<Semicolon>, &PosRange),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_code_block(value.0, context)?;
    Ok(())
}

pub fn default_visit_isolated_expression<V, C, E>(
    visitor: &V,
    value: (
        &LeftParens,
        &Expression,
        &RightParens,
        &Option<Semicolon>,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_expression(value.1, context)?;
    Ok(())
}

pub fn default_visit_sprite_statement_data_declaration<V, C, E>(
    visitor: &V,
    value: (&LetKeyword, &DataDeclaration, &Semicolon, &PosRange),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_data_declaration(value.1, context)?;
    Ok(())
}

pub fn default_visit_sprite_statement_costume_declaration<V, C, E>(
    visitor: &V,
    value: (&CostumeKeyword, &AssetDeclaration, &Semicolon, &PosRange),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    Ok(())
}

pub fn default_visit_code_block<V, C, E>(
    visitor: &V,
    value: &CodeBlock,
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    for item in &value.statements {
        visitor.visit_statement(item, context)?;
    }
    Ok(())
}

pub fn default_visit_statement<V, C, E>(
    visitor: &V,
    value: &Statement,
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    match value {
        Statement::DataDeclaration(let_keyword, data_declaration, semicolon, pos_range) => {
            visitor.visit_statement_data_declaration(
                (let_keyword, data_declaration, semicolon, pos_range),
                context,
            )?;
        }
        Statement::Assignment(
            identifier,
            normal_assignment_operator,
            expression,
            semicolon,
            pos_range,
        ) => {
            visitor.visit_statement_assignment(
                (
                    identifier,
                    normal_assignment_operator,
                    expression,
                    semicolon,
                    pos_range,
                ),
                context,
            )?;
        }
        Statement::ListAssignment(
            identifier,
            normal_assignment_operator,
            left_bracket,
            items,
            right_bracket,
            semicolon,
            pos_range,
        ) => {
            visitor.visit_statement_list_assignment(
                (
                    identifier,
                    normal_assignment_operator,
                    left_bracket,
                    items,
                    right_bracket,
                    semicolon,
                    pos_range,
                ),
                context,
            )?;
        }
        Statement::SetItem(
            identifier,
            left_bracket,
            expression,
            right_bracket,
            normal_assignment_operator,
            expression1,
            semicolon,
            pos_range,
        ) => {
            visitor.visit_statement_set_item(
                (
                    identifier,
                    left_bracket,
                    expression,
                    right_bracket,
                    normal_assignment_operator,
                    expression1,
                    semicolon,
                    pos_range,
                ),
                context,
            )?;
        }
        Statement::Call(identifier, left_parens, items, right_parens, semicolon, pos_range) => {
            visitor.visit_statement_call(
                (
                    identifier,
                    left_parens,
                    items,
                    right_parens,
                    semicolon,
                    pos_range,
                ),
                context,
            )?;
        }
        Statement::SingleInputControl(identifier, expression, code_block, semicolon, pos_range) => {
            visitor.visit_statement_single_input_control(
                (identifier, expression, code_block, semicolon, pos_range),
                context,
            )?;
        }
        Statement::MultiInputControl(
            identifier,
            left_parens,
            items,
            right_parens,
            code_block,
            semicolon,
            pos_range,
        ) => {
            visitor.visit_statement_multi_input_control(
                (
                    identifier,
                    left_parens,
                    items,
                    right_parens,
                    code_block,
                    semicolon,
                    pos_range,
                ),
                context,
            )?;
        }
        Statement::Forever(identifier, code_block, semicolon, pos_range) => {
            visitor
                .visit_statement_forever((identifier, code_block, semicolon, pos_range), context)?;
        }
        Statement::IfElse(start_branch, branches, else_branch, semicolon, pos_range) => {
            visitor.visit_statement_if_else(
                (start_branch, branches, else_branch, semicolon, pos_range),
                context,
            )?;
        }
        Statement::EmptyStatement(semicolon) => {
            visitor.visit_empty_statement(semicolon, context)?;
        }
    }
    Ok(())
}

pub fn default_visit_statement_data_declaration<V, C, E>(
    visitor: &V,
    value: (&LetKeyword, &DataDeclaration, &Semicolon, &PosRange),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_data_declaration(value.1, context)?;
    Ok(())
}

pub fn default_visit_statement_assignment<V, C, E>(
    visitor: &V,
    value: (
        &Identifier,
        &NormalAssignmentOperator,
        &Expression,
        &Semicolon,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_expression(value.2, context)?;
    Ok(())
}

pub fn default_visit_statement_list_assignment<V, C, E>(
    visitor: &V,
    value: (
        &Identifier,
        &NormalAssignmentOperator,
        &LeftBracket,
        &Vec<(ListEntry, Option<Comma>)>,
        &RightBracket,
        &Semicolon,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_list_content(value.3, context)?;
    Ok(())
}

pub fn default_visit_statement_set_item<V, C, E>(
    visitor: &V,
    value: (
        &Identifier,
        &LeftBracket,
        &Expression,
        &RightBracket,
        &NormalAssignmentOperator,
        &Expression,
        &Semicolon,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_expression(value.2, context)?;
    visitor.visit_expression(value.5, context)?;
    Ok(())
}

pub fn default_visit_statement_call<V, C, E>(
    visitor: &V,
    value: (
        &Identifier,
        &LeftParens,
        &Vec<(Expression, Option<Comma>)>,
        &RightParens,
        &Semicolon,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    for item in value.2 {
        visitor.visit_expression(&item.0, context)?;
    }
    Ok(())
}

pub fn default_visit_statement_single_input_control<V, C, E>(
    visitor: &V,
    value: (
        &Identifier,
        &Expression,
        &CodeBlock,
        &Option<Semicolon>,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_expression(value.1, context)?;
    visitor.visit_code_block(value.2, context)?;
    Ok(())
}

pub fn default_visit_statement_multi_input_control<V, C, E>(
    visitor: &V,
    value: (
        &Identifier,
        &LeftParens,
        &Vec<(Expression, Option<Comma>)>,
        &RightParens,
        &CodeBlock,
        &Option<Semicolon>,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    for item in value.2 {
        visitor.visit_expression(&item.0, context)?;
    }
    visitor.visit_code_block(value.4, context)?;
    Ok(())
}

pub fn default_visit_statement_forever<V, C, E>(
    visitor: &V,
    value: (&Identifier, &CodeBlock, &Option<Semicolon>, &PosRange),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_code_block(value.1, context)?;
    Ok(())
}

pub fn default_visit_statement_if_else<V, C, E>(
    visitor: &V,
    value: (
        &(SyntacticIf, Expression, CodeBlock),
        &Vec<(SyntacticElse, SyntacticIf, Expression, CodeBlock)>,
        &Option<(SyntacticElse, CodeBlock)>,
        &Option<Semicolon>,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_expression(&value.0.1, context)?;
    visitor.visit_code_block(&value.0.2, context)?;
    for item in value.1 {
        visitor.visit_expression(&item.2, context)?;
        visitor.visit_code_block(&item.3, context)?;
    }
    if let Some(value) = value.2 {
        visitor.visit_code_block(&value.1, context)?;
    }
    Ok(())
}

pub fn default_visit_expression<V, C, E>(
    visitor: &V,
    value: &Expression,
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    match value {
        Expression::Literal(literal) => {
            visitor.visit_expression_literal(literal, context)?;
        }
        Expression::FormattedString(formatted_string_contents, pos_range) => {
            visitor.visit_expression_formatted_string(
                (formatted_string_contents, pos_range),
                context,
            )?;
        }
        Expression::BinOp(expression, bin_op, expression1, pos_range) => {
            visitor.visit_expression_binary_operation(
                (expression, bin_op, expression1, pos_range),
                context,
            )?;
        }
        Expression::UnOp(un_op, expression, pos_range) => {
            visitor.visit_expression_unary_operation((un_op, expression, pos_range), context)?;
        }
        Expression::Identifier(identifier) => {
            visitor.visit_expression_identifier(identifier, context)?;
        }
        Expression::Call(identifier, left_parens, items, right_parens, pos_range) => {
            visitor.visit_expression_call(
                (identifier, left_parens, items, right_parens, pos_range),
                context,
            )?;
        }
        Expression::GetItem(identifier, left_bracket, expression, right_bracket, pos_range) => {
            visitor.visit_expression_get_item(
                (
                    identifier,
                    left_bracket,
                    expression,
                    right_bracket,
                    pos_range,
                ),
                context,
            )?;
        }
        Expression::GetLetter(
            string_expression,
            letter_access_left_bracket,
            expression,
            right_bracket,
            pos_range,
        ) => {
            visitor.visit_expression_get_letter(
                (
                    string_expression,
                    letter_access_left_bracket,
                    expression,
                    right_bracket,
                    pos_range,
                ),
                context,
            )?;
        }
        Expression::Parentheses(left_parens, expression, right_parens, pos_range) => {
            visitor.visit_expression_parentheses(
                (left_parens, expression, right_parens, pos_range),
                context,
            )?;
        }
    }
    Ok(())
}

pub fn default_visit_list_content<V, C, E>(
    visitor: &V,
    value: &Vec<(ListEntry, Option<Comma>)>,
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    for item in value {
        match &item.0 {
            ListEntry::Expression(expression) => visitor.visit_expression(expression, context)?,
            ListEntry::Unwrap(literal, _) => (),
        }
    }
    Ok(())
}

pub fn default_visit_data_declaration<V, C, E>(
    visitor: &V,
    value: &DataDeclaration,
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    match value {
        DataDeclaration::Mixed(
            data_declaration_scope,
            left_parens,
            items,
            right_parens,
            pos_range,
        ) => {
            visitor.visit_mixed_data_declaration(
                (
                    data_declaration_scope,
                    left_parens,
                    items,
                    right_parens,
                    pos_range,
                ),
                context,
            )?;
        }
        DataDeclaration::Vars(
            data_declaration_scope,
            vars_keyword,
            left_brace,
            items,
            right_brace,
            pos_range,
        ) => {
            visitor.visit_vars_data_declaration(
                (
                    data_declaration_scope,
                    vars_keyword,
                    left_brace,
                    items,
                    right_brace,
                    pos_range,
                ),
                context,
            )?;
        }
        DataDeclaration::Lists(
            data_declaration_scope,
            lists_keyword,
            left_brace,
            items,
            right_brace,
            pos_range,
        ) => {
            visitor.visit_lists_data_declaration(
                (
                    data_declaration_scope,
                    lists_keyword,
                    left_brace,
                    items,
                    right_brace,
                    pos_range,
                ),
                context,
            )?;
        }
        DataDeclaration::Single(single_data_declaration) => {
            visitor.visit_single_data_declaration(single_data_declaration, context)?;
        }
    }
    Ok(())
}

pub fn default_visit_mixed_data_declaration<V, C, E>(
    visitor: &V,
    value: (
        &DataDeclarationScope,
        &LeftParens,
        &Vec<(SingleDataDeclaration, Option<Comma>)>,
        &RightParens,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    for item in value.2 {
        visitor.visit_single_data_declaration(&item.0, context)?;
    }
    Ok(())
}

pub fn default_visit_vars_data_declaration<V, C, E>(
    visitor: &V,
    value: (
        &DataDeclarationScope,
        &VarsKeyword,
        &LeftBrace,
        &Vec<(SingleDataDeclaration, Option<Comma>)>,
        &RightBrace,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    for item in value.3 {
        visitor.visit_single_data_declaration(&item.0, context)?;
    }
    Ok(())
}

pub fn default_visit_lists_data_declaration<V, C, E>(
    visitor: &V,
    value: (
        &DataDeclarationScope,
        &ListsKeyword,
        &LeftBrace,
        &Vec<(SingleDataDeclaration, Option<Comma>)>,
        &RightBrace,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    for item in value.3 {
        visitor.visit_single_data_declaration(&item.0, context)?;
    }
    Ok(())
}

pub fn default_visit_single_data_declaration<V, C, E>(
    visitor: &V,
    value: &SingleDataDeclaration,
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    match value {
        SingleDataDeclaration::Variable(
            var_keyword,
            data_declaration_scope,
            canonical_identifier,
            identifier,
            normal_assignment_operator,
            expression,
            pos_range,
        ) => {
            visitor.visit_single_variable_declaration(
                (
                    var_keyword,
                    data_declaration_scope,
                    canonical_identifier,
                    identifier,
                    normal_assignment_operator,
                    expression,
                    pos_range,
                ),
                context,
            )?;
        }
        SingleDataDeclaration::EmptyVariable(
            var_keyword,
            data_declaration_scope,
            canonical_identifier,
            identifier,
            pos_range,
        ) => {
            visitor.visit_single_empty_variable_declaration(
                (
                    var_keyword,
                    data_declaration_scope,
                    canonical_identifier,
                    identifier,
                    pos_range,
                ),
                context,
            )?;
        }
        SingleDataDeclaration::List(
            list_keyword,
            data_declaration_scope,
            canonical_identifier,
            identifier,
            normal_assignment_operator,
            left_bracket,
            items,
            right_bracket,
            pos_range,
        ) => {
            visitor.visit_single_list_declaration(
                (
                    list_keyword,
                    data_declaration_scope,
                    canonical_identifier,
                    identifier,
                    normal_assignment_operator,
                    left_bracket,
                    items,
                    right_bracket,
                    pos_range,
                ),
                context,
            )?;
        }
        SingleDataDeclaration::EmptyList(
            list_keyword,
            data_declaration_scope,
            canonical_identifier,
            identifier,
            pos_range,
        ) => {
            visitor.visit_single_empty_list_declaration(
                (
                    list_keyword,
                    data_declaration_scope,
                    canonical_identifier,
                    identifier,
                    pos_range,
                ),
                context,
            )?;
        }
    }
    Ok(())
}

pub fn default_visit_single_variable_declaration<V, C, E>(
    visitor: &V,
    value: (
        &Option<VarKeyword>,
        &DataDeclarationScope,
        &Option<CanonicalIdentifier>,
        &Identifier,
        &NormalAssignmentOperator,
        &Expression,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_expression(value.5, context)?;
    Ok(())
}

pub fn default_visit_single_empty_variable_declaration<V, C, E>(
    visitor: &V,
    value: (
        &Option<VarKeyword>,
        &DataDeclarationScope,
        &Option<CanonicalIdentifier>,
        &Identifier,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    Ok(())
}

pub fn default_visit_single_list_declaration<V, C, E>(
    visitor: &V,
    value: (
        &Option<ListKeyword>,
        &DataDeclarationScope,
        &Option<CanonicalIdentifier>,
        &Identifier,
        &NormalAssignmentOperator,
        &LeftBracket,
        &Vec<(ListEntry, Option<Comma>)>,
        &RightBracket,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_list_content(value.6, context)?;
    Ok(())
}

pub fn default_visit_single_empty_list_declaration<V, C, E>(
    visitor: &V,
    value: (
        &Option<ListKeyword>,
        &DataDeclarationScope,
        &Option<CanonicalIdentifier>,
        &Identifier,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    Ok(())
}

pub fn default_visit_expression_literal<V, C, E>(
    visitor: &V,
    value: &Literal,
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    Ok(())
}

pub fn default_visit_expression_formatted_string<V, C, E>(
    visitor: &V,
    value: (&Vec<FormattedStringContent>, &PosRange),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    for item in value.0 {
        visitor.visit_formatted_string_content(item, context)?;
    }
    Ok(())
}

pub fn default_visit_expression_binary_operation<V, C, E>(
    visitor: &V,
    value: (&Box<Expression>, &BinOp, &Box<Expression>, &PosRange),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_expression(value.0.deref(), context)?;
    visitor.visit_expression(value.2.deref(), context)?;
    Ok(())
}

pub fn default_visit_expression_unary_operation<V, C, E>(
    visitor: &V,
    value: (&UnOp, &Box<Expression>, &PosRange),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_expression(value.1.deref(), context)?;
    Ok(())
}

pub fn default_visit_expression_identifier<V, C, E>(
    visitor: &V,
    value: &Identifier,
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    Ok(())
}

pub fn default_visit_expression_call<V, C, E>(
    visitor: &V,
    value: (
        &Identifier,
        &LeftParens,
        &Vec<(Expression, Option<Comma>)>,
        &RightParens,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    for item in value.2 {
        visitor.visit_expression(&item.0, context)?;
    }
    Ok(())
}

pub fn default_visit_expression_get_item<V, C, E>(
    visitor: &V,
    value: (
        &Identifier,
        &LeftBracket,
        &Box<Expression>,
        &RightBracket,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_expression(value.2.deref(), context)?;
    Ok(())
}

pub fn default_visit_expression_get_letter<V, C, E>(
    visitor: &V,
    value: (
        &Box<Expression>,
        &LetterAccessLeftBracket,
        &Box<Expression>,
        &RightBracket,
        &PosRange,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_expression(value.0.deref(), context)?;
    visitor.visit_expression(value.2.deref(), context)?;
    Ok(())
}

pub fn default_visit_expression_parentheses<V, C, E>(
    visitor: &V,
    value: (&LeftParens, &Box<Expression>, &RightParens, &PosRange),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_expression(value.1.deref(), context)?;
    Ok(())
}

pub fn default_visit_formatted_string_content<V, C, E>(
    visitor: &V,
    value: &FormattedStringContent,
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    match value {
        FormattedStringContent::Expression(expression) => {
            visitor.visit_expression(expression, context)?;
        }
        FormattedStringContent::String(_arc_str, _) => (),
    }
    Ok(())
}
