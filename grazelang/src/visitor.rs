use crate::{
    lexer::SourceSpan,
    parser::cst::{
        AssetDeclaration, BackdropKeyword, BinOp, BroadcastKeyword, CanonicalIdentifier, CodeBlock,
        Comma, CommaSeparated, CostumeKeyword, CustomBlockParamKind, DataDeclaration,
        DataDeclarationScope, Expression, FormattedStringContent, GrazeProgram, Identifier,
        LeftBrace, LeftBracket, LeftParens, LetKeyword, LetterAccessLeftBracket, ListEntry,
        ListKeyword, ListsKeyword, Literal, NormalAssignmentOperator, ProcKeyword, RightBrace,
        RightBracket, RightParens, Semicolon, SingleDataDeclaration, SoundKeyword, SpriteCodeBlock,
        SpriteKeyword, SpriteStatement, StageCodeBlock, StageKeyword, StageStatement, Statement,
        SyntacticElse, SyntacticIf, TopLevelStatement, UnOp, VarKeyword, VarsKeyword,
        WarpSpecifier,
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
            Option<&Semicolon>,
            &SourceSpan,
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
            Option<&CanonicalIdentifier>,
            &Identifier,
            &SpriteCodeBlock,
            Option<&Semicolon>,
            &SourceSpan,
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
            Option<&CanonicalIdentifier>,
            &Identifier,
            &Semicolon,
            &SourceSpan,
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
        value: (&LetKeyword, &DataDeclaration, &Semicolon, &SourceSpan),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_stage_statement_data_declaration(self, value, context)
    }

    fn visit_stage_statement_backdrop_declaration(
        &self,
        value: (&BackdropKeyword, &AssetDeclaration, &Semicolon, &SourceSpan),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_stage_statement_backdrop_declaration(self, value, context)
    }

    fn visit_sound_declaration(
        &self,
        value: (&SoundKeyword, &AssetDeclaration, &Semicolon, &SourceSpan),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_sound_declaration(self, value, context)
    }

    fn visit_no_input_hat_statement(
        &self,
        value: (&Identifier, &CodeBlock, Option<&Semicolon>, &SourceSpan),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_no_input_hat_statement(self, value, context)
    }

    fn visit_single_input_hat_statement(
        &self,
        value: (
            &Identifier,
            &Expression,
            &CodeBlock,
            Option<&Semicolon>,
            &SourceSpan,
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
            &[(Expression, Option<Comma>)],
            &RightParens,
            &CodeBlock,
            Option<&Semicolon>,
            &SourceSpan,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_multi_input_hat_statement(self, value, context)
    }

    fn visit_custom_block_definition(
        &self,
        value: (
            Option<&WarpSpecifier>,
            &ProcKeyword,
            Option<&CanonicalIdentifier>,
            &Identifier,
            &LeftParens,
            &[(
                Option<CustomBlockParamKind>,
                Option<CanonicalIdentifier>,
                Identifier,
                Option<Comma>,
            )],
            &RightParens,
            &CodeBlock,
            Option<&Semicolon>,
            &SourceSpan,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_custom_block_definition(self, value, context)
    }

    fn visit_isolated_block(
        &self,
        value: (&CodeBlock, Option<&Semicolon>, &SourceSpan),
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
            Option<&Semicolon>,
            &SourceSpan,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_isolated_expression(self, value, context)
    }

    fn visit_sprite_statement_data_declaration(
        &self,
        value: (&LetKeyword, &DataDeclaration, &Semicolon, &SourceSpan),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_sprite_statement_data_declaration(self, value, context)
    }

    fn visit_sprite_statement_costume_declaration(
        &self,
        value: (&CostumeKeyword, &AssetDeclaration, &Semicolon, &SourceSpan),
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
        value: (&LetKeyword, &DataDeclaration, &Semicolon, &SourceSpan),
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
            &SourceSpan,
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
            &CommaSeparated<ListEntry>,
            &RightBracket,
            &Semicolon,
            &SourceSpan,
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
            &SourceSpan,
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
            &[(Expression, Option<Comma>)],
            &RightParens,
            &Semicolon,
            &SourceSpan,
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
            Option<&Semicolon>,
            &SourceSpan,
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
            &[(Expression, Option<Comma>)],
            &RightParens,
            &CodeBlock,
            Option<&Semicolon>,
            &SourceSpan,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_statement_multi_input_control(self, value, context)
    }

    fn visit_statement_forever(
        &self,
        value: (&Identifier, &CodeBlock, Option<&Semicolon>, &SourceSpan),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_statement_forever(self, value, context)
    }

    fn visit_statement_if_else(
        &self,
        value: (
            &(SyntacticIf, Expression, CodeBlock),
            &[(SyntacticElse, SyntacticIf, Expression, CodeBlock)],
            Option<&(SyntacticElse, CodeBlock)>,
            Option<&Semicolon>,
            &SourceSpan,
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
        value: &CommaSeparated<ListEntry>,
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
            &[(SingleDataDeclaration, Option<Comma>)],
            &RightParens,
            &SourceSpan,
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
            &[(SingleDataDeclaration, Option<Comma>)],
            &RightBrace,
            &SourceSpan,
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
            &[(SingleDataDeclaration, Option<Comma>)],
            &RightBrace,
            &SourceSpan,
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
            Option<&VarKeyword>,
            &DataDeclarationScope,
            Option<&CanonicalIdentifier>,
            &Identifier,
            &NormalAssignmentOperator,
            &Expression,
            &SourceSpan,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_single_variable_declaration(self, value, context)
    }

    fn visit_single_empty_variable_declaration(
        &self,
        value: (
            Option<&VarKeyword>,
            &DataDeclarationScope,
            Option<&CanonicalIdentifier>,
            &Identifier,
            &SourceSpan,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_single_empty_variable_declaration(self, value, context)
    }

    fn visit_single_list_declaration(
        &self,
        value: (
            Option<&ListKeyword>,
            &DataDeclarationScope,
            Option<&CanonicalIdentifier>,
            &Identifier,
            &NormalAssignmentOperator,
            &LeftBracket,
            &CommaSeparated<ListEntry>,
            &RightBracket,
            &SourceSpan,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_single_list_declaration(self, value, context)
    }

    fn visit_single_empty_list_declaration(
        &self,
        value: (
            Option<&ListKeyword>,
            &DataDeclarationScope,
            Option<&CanonicalIdentifier>,
            &Identifier,
            &SourceSpan,
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
        value: (&[FormattedStringContent], &SourceSpan),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_expression_formatted_string(self, value, context)
    }

    fn visit_expression_binary_operation(
        &self,
        value: (&Expression, &BinOp, &Expression, &SourceSpan),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_expression_binary_operation(self, value, context)
    }

    fn visit_expression_unary_operation(
        &self,
        value: (&UnOp, &Expression, &SourceSpan),
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
            &[(Expression, Option<Comma>)],
            &RightParens,
            &SourceSpan,
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
            &Expression,
            &RightBracket,
            &SourceSpan,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_expression_get_item(self, value, context)
    }

    fn visit_expression_get_letter(
        &self,
        value: (
            &Expression,
            &LetterAccessLeftBracket,
            &Expression,
            &RightBracket,
            &SourceSpan,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        default_visit_expression_get_letter(self, value, context)
    }

    fn visit_expression_parentheses(
        &self,
        value: (&LeftParens, &Expression, &RightParens, &SourceSpan),
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
        TopLevelStatement::Stage(stage_keyword, stage_code_block, semicolon, source_span) => {
            visitor.visit_top_level_statement_stage(
                (
                    stage_keyword,
                    stage_code_block,
                    semicolon.as_ref(),
                    source_span,
                ),
                context,
            )?;
        }
        TopLevelStatement::Sprite(
            sprite_keyword,
            canonical_identifier,
            identifier,
            sprite_code_block,
            semicolon,
            source_span,
        ) => visitor.visit_top_level_statement_sprite(
            (
                sprite_keyword,
                canonical_identifier.as_ref(),
                identifier,
                sprite_code_block,
                semicolon.as_ref(),
                source_span,
            ),
            context,
        )?,
        TopLevelStatement::BroadcastDeclaration(
            broadcast_keyword,
            canonical_identifier,
            identifier,
            semicolon,
            source_span,
        ) => {
            visitor.visit_top_level_statement_broadcast_declaration(
                (
                    broadcast_keyword,
                    canonical_identifier.as_ref(),
                    identifier,
                    semicolon,
                    source_span,
                ),
                context,
            )?;
        }
        TopLevelStatement::EmptyStatement(semicolon) => {
            visitor.visit_empty_statement(semicolon, context)?;
        }
        TopLevelStatement::InvalidStatement(_) => {
            panic!("You must pass a valid AST to the visitor.")
        }
    }
    Ok(())
}

pub fn default_visit_top_level_statement_stage<V, C, E>(
    visitor: &V,
    value: (
        &StageKeyword,
        &StageCodeBlock,
        Option<&Semicolon>,
        &SourceSpan,
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
        Option<&CanonicalIdentifier>,
        &Identifier,
        &SpriteCodeBlock,
        Option<&Semicolon>,
        &SourceSpan,
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
    _visitor: &V,
    _value: (
        &BroadcastKeyword,
        Option<&CanonicalIdentifier>,
        &Identifier,
        &Semicolon,
        &SourceSpan,
    ),
    _context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    Ok(())
}

pub fn default_visit_empty_statement<V, C, E>(
    _visitor: &V,
    _value: &Semicolon,
    _context: &mut C,
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
        StageStatement::DataDeclaration(let_keyword, data_declaration, semicolon, source_span) => {
            visitor.visit_stage_statement_data_declaration(
                (let_keyword, data_declaration, semicolon, source_span),
                context,
            )?;
        }
        StageStatement::BackdropDeclaration(
            backdrop_keyword,
            asset_declaration,
            semicolon,
            source_span,
        ) => {
            visitor.visit_stage_statement_backdrop_declaration(
                (backdrop_keyword, asset_declaration, semicolon, source_span),
                context,
            )?;
        }
        StageStatement::SoundDeclaration(
            sound_keyword,
            asset_declaration,
            semicolon,
            source_span,
        ) => {
            visitor.visit_sound_declaration(
                (sound_keyword, asset_declaration, semicolon, source_span),
                context,
            )?;
        }
        StageStatement::NoInputHatStatement(identifier, code_block, semicolon, source_span) => {
            visitor.visit_no_input_hat_statement(
                (identifier, code_block, semicolon.as_ref(), source_span),
                context,
            )?;
        }
        StageStatement::SingleInputHatStatement(
            identifier,
            expression,
            code_block,
            semicolon,
            source_span,
        ) => {
            visitor.visit_single_input_hat_statement(
                (
                    identifier,
                    expression,
                    code_block,
                    semicolon.as_ref(),
                    source_span,
                ),
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
            source_span,
        ) => {
            visitor.visit_multi_input_hat_statement(
                (
                    identifier,
                    left_parens,
                    items,
                    right_parens,
                    code_block,
                    semicolon.as_ref(),
                    source_span,
                ),
                context,
            )?;
        }
        StageStatement::CustomBlockDefinition(
            proc_keyword,
            warp_specifier,
            canonical_identifier,
            identifier,
            left_parens,
            items,
            right_parens,
            code_block,
            semicolon,
            source_span,
        ) => {
            visitor.visit_custom_block_definition(
                (
                    proc_keyword.as_ref(),
                    warp_specifier,
                    canonical_identifier.as_ref(),
                    identifier,
                    left_parens,
                    items,
                    right_parens,
                    code_block,
                    semicolon.as_ref(),
                    source_span,
                ),
                context,
            )?;
        }
        StageStatement::IsolatedBlock(code_block, semicolon, source_span) => {
            visitor.visit_isolated_block((code_block, semicolon.as_ref(), source_span), context)?;
        }
        StageStatement::IsolatedExpression(
            left_parens,
            expression,
            right_parens,
            semicolon,
            source_span,
        ) => {
            visitor.visit_isolated_expression(
                (
                    left_parens,
                    expression,
                    right_parens,
                    semicolon.as_ref(),
                    source_span,
                ),
                context,
            )?;
        }
        StageStatement::EmptyStatement(semicolon) => {
            visitor.visit_empty_statement(semicolon, context)?;
        }
        StageStatement::InvalidStatement(_) => panic!("You must pass a valid AST to the visitor."),
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
        SpriteStatement::DataDeclaration(let_keyword, data_declaration, semicolon, source_span) => {
            visitor.visit_sprite_statement_data_declaration(
                (let_keyword, data_declaration, semicolon, source_span),
                context,
            )?;
        }
        SpriteStatement::CostumeDeclaration(
            costume_keyword,
            asset_declaration,
            semicolon,
            source_span,
        ) => {
            visitor.visit_sprite_statement_costume_declaration(
                (costume_keyword, asset_declaration, semicolon, source_span),
                context,
            )?;
        }
        SpriteStatement::SoundDeclaration(
            sound_keyword,
            asset_declaration,
            semicolon,
            source_span,
        ) => {
            visitor.visit_sound_declaration(
                (sound_keyword, asset_declaration, semicolon, source_span),
                context,
            )?;
        }
        SpriteStatement::NoInputHatStatement(identifier, code_block, semicolon, source_span) => {
            visitor.visit_no_input_hat_statement(
                (identifier, code_block, semicolon.as_ref(), source_span),
                context,
            )?;
        }
        SpriteStatement::SingleInputHatStatement(
            identifier,
            expression,
            code_block,
            semicolon,
            source_span,
        ) => {
            visitor.visit_single_input_hat_statement(
                (
                    identifier,
                    expression,
                    code_block,
                    semicolon.as_ref(),
                    source_span,
                ),
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
            source_span,
        ) => {
            visitor.visit_multi_input_hat_statement(
                (
                    identifier,
                    left_parens,
                    items,
                    right_parens,
                    code_block,
                    semicolon.as_ref(),
                    source_span,
                ),
                context,
            )?;
        }
        SpriteStatement::CustomBlockDefinition(
            warp_specifier,
            proc_keyword,
            canonical_identifier,
            identifier,
            left_parens,
            items,
            right_parens,
            code_block,
            semicolon,
            source_span,
        ) => {
            visitor.visit_custom_block_definition(
                (
                    warp_specifier.as_ref(),
                    proc_keyword,
                    canonical_identifier.as_ref(),
                    identifier,
                    left_parens,
                    items,
                    right_parens,
                    code_block,
                    semicolon.as_ref(),
                    source_span,
                ),
                context,
            )?;
        }
        SpriteStatement::IsolatedBlock(code_block, semicolon, source_span) => {
            visitor.visit_isolated_block((code_block, semicolon.as_ref(), source_span), context)?;
        }
        SpriteStatement::IsolatedExpression(
            left_parens,
            expression,
            right_parens,
            semicolon,
            source_span,
        ) => {
            visitor.visit_isolated_expression(
                (
                    left_parens,
                    expression,
                    right_parens,
                    semicolon.as_ref(),
                    source_span,
                ),
                context,
            )?;
        }
        SpriteStatement::EmptyStatement(semicolon) => {
            visitor.visit_empty_statement(semicolon, context)?;
        }
        SpriteStatement::InvalidStatement(_) => panic!("You must pass a valid AST to the visitor."),
    }
    Ok(())
}

pub fn default_visit_stage_statement_data_declaration<V, C, E>(
    visitor: &V,
    value: (&LetKeyword, &DataDeclaration, &Semicolon, &SourceSpan),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_data_declaration(value.1, context)?;
    Ok(())
}

pub fn default_visit_stage_statement_backdrop_declaration<V, C, E>(
    _visitor: &V,
    _value: (&BackdropKeyword, &AssetDeclaration, &Semicolon, &SourceSpan),
    _context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    Ok(())
}

pub fn default_visit_sound_declaration<V, C, E>(
    _visitor: &V,
    _value: (&SoundKeyword, &AssetDeclaration, &Semicolon, &SourceSpan),
    _context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    Ok(())
}

pub fn default_visit_no_input_hat_statement<V, C, E>(
    visitor: &V,
    value: (&Identifier, &CodeBlock, Option<&Semicolon>, &SourceSpan),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_code_block(value.1, context)?;
    Ok(())
}

pub fn default_visit_single_input_hat_statement<V, C, E>(
    visitor: &V,
    value: (
        &Identifier,
        &Expression,
        &CodeBlock,
        Option<&Semicolon>,
        &SourceSpan,
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
        &[(Expression, Option<Comma>)],
        &RightParens,
        &CodeBlock,
        Option<&Semicolon>,
        &SourceSpan,
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

pub fn default_visit_custom_block_definition<V, C, E>(
    visitor: &V,
    value: (
        Option<&WarpSpecifier>,
        &ProcKeyword,
        Option<&CanonicalIdentifier>,
        &Identifier,
        &LeftParens,
        &[(
            Option<CustomBlockParamKind>,
            Option<CanonicalIdentifier>,
            Identifier,
            Option<Comma>,
        )],
        &RightParens,
        &CodeBlock,
        Option<&Semicolon>,
        &SourceSpan,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_code_block(value.7, context)?;
    Ok(())
}

pub fn default_visit_isolated_block<V, C, E>(
    visitor: &V,
    value: (&CodeBlock, Option<&Semicolon>, &SourceSpan),
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
        Option<&Semicolon>,
        &SourceSpan,
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
    value: (&LetKeyword, &DataDeclaration, &Semicolon, &SourceSpan),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_data_declaration(value.1, context)?;
    Ok(())
}

pub fn default_visit_sprite_statement_costume_declaration<V, C, E>(
    _visitor: &V,
    _value: (&CostumeKeyword, &AssetDeclaration, &Semicolon, &SourceSpan),
    _context: &mut C,
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
        Statement::DataDeclaration(let_keyword, data_declaration, semicolon, source_span) => {
            visitor.visit_statement_data_declaration(
                (let_keyword, data_declaration, semicolon, source_span),
                context,
            )?;
        }
        Statement::Assignment(
            identifier,
            normal_assignment_operator,
            expression,
            semicolon,
            source_span,
        ) => {
            visitor.visit_statement_assignment(
                (
                    identifier,
                    normal_assignment_operator,
                    expression,
                    semicolon,
                    source_span,
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
            source_span,
        ) => {
            visitor.visit_statement_list_assignment(
                (
                    identifier,
                    normal_assignment_operator,
                    left_bracket,
                    items,
                    right_bracket,
                    semicolon,
                    source_span,
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
            source_span,
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
                    source_span,
                ),
                context,
            )?;
        }
        Statement::Call(identifier, left_parens, items, right_parens, semicolon, source_span) => {
            visitor.visit_statement_call(
                (
                    identifier,
                    left_parens,
                    items,
                    right_parens,
                    semicolon,
                    source_span,
                ),
                context,
            )?;
        }
        Statement::SingleInputControl(
            identifier,
            expression,
            code_block,
            semicolon,
            source_span,
        ) => {
            visitor.visit_statement_single_input_control(
                (
                    identifier,
                    expression,
                    code_block,
                    semicolon.as_ref(),
                    source_span,
                ),
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
            source_span,
        ) => {
            visitor.visit_statement_multi_input_control(
                (
                    identifier,
                    left_parens,
                    items,
                    right_parens,
                    code_block,
                    semicolon.as_ref(),
                    source_span,
                ),
                context,
            )?;
        }
        Statement::Forever(identifier, code_block, semicolon, source_span) => {
            visitor.visit_statement_forever(
                (identifier, code_block, semicolon.as_ref(), source_span),
                context,
            )?;
        }
        Statement::IfElse(start_branch, branches, else_branch, semicolon, source_span) => {
            visitor.visit_statement_if_else(
                (
                    start_branch,
                    branches,
                    else_branch.as_ref(),
                    semicolon.as_ref(),
                    source_span,
                ),
                context,
            )?;
        }
        Statement::EmptyStatement(semicolon) => {
            visitor.visit_empty_statement(semicolon, context)?;
        }
        Statement::InvalidStatement(_) => panic!("You must pass a valid AST to the visitor."),
    }
    Ok(())
}

pub fn default_visit_statement_data_declaration<V, C, E>(
    visitor: &V,
    value: (&LetKeyword, &DataDeclaration, &Semicolon, &SourceSpan),
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
        &SourceSpan,
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
        &CommaSeparated<ListEntry>,
        &RightBracket,
        &Semicolon,
        &SourceSpan,
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
        &SourceSpan,
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
        &[(Expression, Option<Comma>)],
        &RightParens,
        &Semicolon,
        &SourceSpan,
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
        Option<&Semicolon>,
        &SourceSpan,
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
        &[(Expression, Option<Comma>)],
        &RightParens,
        &CodeBlock,
        Option<&Semicolon>,
        &SourceSpan,
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
    value: (&Identifier, &CodeBlock, Option<&Semicolon>, &SourceSpan),
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
        &[(SyntacticElse, SyntacticIf, Expression, CodeBlock)],
        Option<&(SyntacticElse, CodeBlock)>,
        Option<&Semicolon>,
        &SourceSpan,
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
        Expression::FormattedString(formatted_string_contents, source_span) => {
            visitor.visit_expression_formatted_string(
                (formatted_string_contents, source_span),
                context,
            )?;
        }
        Expression::BinOp(expression, bin_op, expression1, source_span) => {
            visitor.visit_expression_binary_operation(
                (expression, bin_op, expression1, source_span),
                context,
            )?;
        }
        Expression::UnOp(un_op, expression, source_span) => {
            visitor.visit_expression_unary_operation((un_op, expression, source_span), context)?;
        }
        Expression::Identifier(identifier) => {
            visitor.visit_expression_identifier(identifier, context)?;
        }
        Expression::Call(identifier, left_parens, items, right_parens, source_span) => {
            visitor.visit_expression_call(
                (identifier, left_parens, items, right_parens, source_span),
                context,
            )?;
        }
        Expression::GetItem(identifier, left_bracket, expression, right_bracket, source_span) => {
            visitor.visit_expression_get_item(
                (
                    identifier,
                    left_bracket,
                    expression,
                    right_bracket,
                    source_span,
                ),
                context,
            )?;
        }
        Expression::GetLetter(
            string_expression,
            letter_access_left_bracket,
            expression,
            right_bracket,
            source_span,
        ) => {
            visitor.visit_expression_get_letter(
                (
                    string_expression,
                    letter_access_left_bracket,
                    expression,
                    right_bracket,
                    source_span,
                ),
                context,
            )?;
        }
        Expression::Parentheses(left_parens, expression, right_parens, source_span) => {
            visitor.visit_expression_parentheses(
                (left_parens, expression, right_parens, source_span),
                context,
            )?;
        }
    }
    Ok(())
}

pub fn default_visit_list_content<V, C, E>(
    visitor: &V,
    value: &CommaSeparated<ListEntry>,
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    for item in value {
        match item {
            ListEntry::Expression(expression) => visitor.visit_expression(expression, context)?,
            ListEntry::Unwrap(..) => (),
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
            source_span,
        ) => {
            visitor.visit_mixed_data_declaration(
                (
                    data_declaration_scope,
                    left_parens,
                    items,
                    right_parens,
                    source_span,
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
            source_span,
        ) => {
            visitor.visit_vars_data_declaration(
                (
                    data_declaration_scope,
                    vars_keyword,
                    left_brace,
                    items,
                    right_brace,
                    source_span,
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
            source_span,
        ) => {
            visitor.visit_lists_data_declaration(
                (
                    data_declaration_scope,
                    lists_keyword,
                    left_brace,
                    items,
                    right_brace,
                    source_span,
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
        &[(SingleDataDeclaration, Option<Comma>)],
        &RightParens,
        &SourceSpan,
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
        &[(SingleDataDeclaration, Option<Comma>)],
        &RightBrace,
        &SourceSpan,
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
        &[(SingleDataDeclaration, Option<Comma>)],
        &RightBrace,
        &SourceSpan,
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
            source_span,
        ) => {
            visitor.visit_single_variable_declaration(
                (
                    var_keyword.as_ref(),
                    data_declaration_scope,
                    canonical_identifier.as_ref(),
                    identifier,
                    normal_assignment_operator,
                    expression,
                    source_span,
                ),
                context,
            )?;
        }
        SingleDataDeclaration::EmptyVariable(
            var_keyword,
            data_declaration_scope,
            canonical_identifier,
            identifier,
            source_span,
        ) => {
            visitor.visit_single_empty_variable_declaration(
                (
                    var_keyword.as_ref(),
                    data_declaration_scope,
                    canonical_identifier.as_ref(),
                    identifier,
                    source_span,
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
            source_span,
        ) => {
            visitor.visit_single_list_declaration(
                (
                    list_keyword.as_ref(),
                    data_declaration_scope,
                    canonical_identifier.as_ref(),
                    identifier,
                    normal_assignment_operator,
                    left_bracket,
                    items,
                    right_bracket,
                    source_span,
                ),
                context,
            )?;
        }
        SingleDataDeclaration::EmptyList(
            list_keyword,
            data_declaration_scope,
            canonical_identifier,
            identifier,
            source_span,
        ) => {
            visitor.visit_single_empty_list_declaration(
                (
                    list_keyword.as_ref(),
                    data_declaration_scope,
                    canonical_identifier.as_ref(),
                    identifier,
                    source_span,
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
        Option<&VarKeyword>,
        &DataDeclarationScope,
        Option<&CanonicalIdentifier>,
        &Identifier,
        &NormalAssignmentOperator,
        &Expression,
        &SourceSpan,
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
    _visitor: &V,
    _value: (
        Option<&VarKeyword>,
        &DataDeclarationScope,
        Option<&CanonicalIdentifier>,
        &Identifier,
        &SourceSpan,
    ),
    _context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    Ok(())
}

pub fn default_visit_single_list_declaration<V, C, E>(
    visitor: &V,
    value: (
        Option<&ListKeyword>,
        &DataDeclarationScope,
        Option<&CanonicalIdentifier>,
        &Identifier,
        &NormalAssignmentOperator,
        &LeftBracket,
        &CommaSeparated<ListEntry>,
        &RightBracket,
        &SourceSpan,
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
    _visitor: &V,
    _value: (
        Option<&ListKeyword>,
        &DataDeclarationScope,
        Option<&CanonicalIdentifier>,
        &Identifier,
        &SourceSpan,
    ),
    _context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    Ok(())
}

pub fn default_visit_expression_literal<V, C, E>(
    _visitor: &V,
    _value: &Literal,
    _context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    Ok(())
}

pub fn default_visit_expression_formatted_string<V, C, E>(
    visitor: &V,
    value: (&[FormattedStringContent], &SourceSpan),
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
    value: (&Expression, &BinOp, &Expression, &SourceSpan),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_expression(value.0, context)?;
    visitor.visit_expression(value.2, context)?;
    Ok(())
}

pub fn default_visit_expression_unary_operation<V, C, E>(
    visitor: &V,
    value: (&UnOp, &Expression, &SourceSpan),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_expression(value.1, context)?;
    Ok(())
}

pub fn default_visit_expression_identifier<V, C, E>(
    _visitor: &V,
    _value: &Identifier,
    _context: &mut C,
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
        &[(Expression, Option<Comma>)],
        &RightParens,
        &SourceSpan,
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
        &Expression,
        &RightBracket,
        &SourceSpan,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_expression(value.2, context)?;
    Ok(())
}

pub fn default_visit_expression_get_letter<V, C, E>(
    visitor: &V,
    value: (
        &Expression,
        &LetterAccessLeftBracket,
        &Expression,
        &RightBracket,
        &SourceSpan,
    ),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_expression(value.0, context)?;
    visitor.visit_expression(value.2, context)?;
    Ok(())
}

pub fn default_visit_expression_parentheses<V, C, E>(
    visitor: &V,
    value: (&LeftParens, &Expression, &RightParens, &SourceSpan),
    context: &mut C,
) -> Result<(), E>
where
    V: GrazeVisitor<C, E> + ?Sized,
{
    visitor.visit_expression(value.1, context)?;
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
