use std::ops::Deref;

use crate::{
    lexer::PosRange,
    parser::ast::{
        AssetDeclaration, BackdropKeyword, BinOp, BroadcastKeyword, CanonicalIdentifier, CodeBlock,
        Comma, CostumeKeyword, DataDeclaration, DataDeclarationScope, Expression,
        FormattedStringContent, GrazeProgram, Identifier, LeftBrace, LeftBracket, LeftParens,
        LetKeyword, ListEntry, ListKeyword, ListsKeyword, Literal, NormalAssignmentOperator,
        RightBrace, RightBracket, RightParens, Semicolon, SingleDataDeclaration, SoundKeyword,
        SpriteCodeBlock, SpriteKeyword, SpriteStatement, StageCodeBlock, StageKeyword,
        StageStatement, Statement, TopLevelStatement, UnOp, VarKeyword, VarsKeyword,
    },
};

pub trait GrazeVisitor<C, E> {
    fn visit_program(&self, value: &GrazeProgram, context: &mut C) -> Result<(), E> {
        for item in &value.0 {
            self.visit_top_level_statement(item, context)?;
        }
        Ok(())
    }

    fn visit_top_level_statement(
        &self,
        value: &TopLevelStatement,
        context: &mut C,
    ) -> Result<(), E> {
        match value {
            TopLevelStatement::Stage(stage_keyword, stage_code_block, semicolon, pos_range) => {
                self.visit_stage(
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
            ) => self.visit_sprite_code_block(
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
                self.visit_broadcast_declaration(
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
                self.visit_empty_statement(semicolon, context)?;
            }
        }
        Ok(())
    }

    fn visit_stage(
        &self,
        value: (
            &StageKeyword,
            &StageCodeBlock,
            &Option<Semicolon>,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        for item in &value.1.statements {
            self.visit_stage_statement(item, context)?;
        }
        Ok(())
    }

    fn visit_sprite_code_block(
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
        for item in &value.3.statements {
            self.visit_sprite_statement(item, context)?;
        }
        Ok(())
    }

    fn visit_broadcast_declaration(
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
        Ok(())
    }

    fn visit_empty_statement(&self, value: &Semicolon, context: &mut C) -> Result<(), E> {
        Ok(())
    }

    fn visit_stage_statement(&self, value: &StageStatement, context: &mut C) -> Result<(), E> {
        match value {
            StageStatement::DataDeclaration(
                let_keyword,
                data_declaration,
                semicolon,
                pos_range,
            ) => {
                self.visit_stage_data_declaration(
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
                self.visit_backdrop_declaration(
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
                self.visit_sound_declaration(
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
                self.visit_single_input_hat_statement(
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
                self.visit_multi_input_hat_statement(
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
            StageStatement::EmptyStatement(semicolon) => {
                self.visit_empty_statement(semicolon, context)?;
            }
        }
        Ok(())
    }

    fn visit_sprite_statement(&self, value: &SpriteStatement, context: &mut C) -> Result<(), E> {
        match value {
            SpriteStatement::DataDeclaration(
                let_keyword,
                data_declaration,
                semicolon,
                pos_range,
            ) => {
                self.visit_sprite_data_declaration(
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
                self.visit_costume_declaration(
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
                self.visit_sound_declaration(
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
                self.visit_single_input_hat_statement(
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
                self.visit_multi_input_hat_statement(
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
            SpriteStatement::EmptyStatement(semicolon) => {
                self.visit_empty_statement(semicolon, context)?;
            }
        }
        Ok(())
    }

    fn visit_stage_data_declaration(
        &self,
        value: (&LetKeyword, &DataDeclaration, &Semicolon, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        self.visit_data_declaration(value.1, context)?;
        Ok(())
    }

    fn visit_backdrop_declaration(
        &self,
        value: (&BackdropKeyword, &AssetDeclaration, &Semicolon, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        Ok(())
    }

    fn visit_sound_declaration(
        &self,
        value: (&SoundKeyword, &AssetDeclaration, &Semicolon, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        Ok(())
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
        self.visit_expression(value.1, context)?;
        self.visit_code_block(value.2, context)?;
        Ok(())
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
        self.visit_code_block(value.4, context)?;
        Ok(())
    }

    fn visit_sprite_data_declaration(
        &self,
        value: (&LetKeyword, &DataDeclaration, &Semicolon, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        self.visit_data_declaration(value.1, context)?;
        Ok(())
    }

    fn visit_costume_declaration(
        &self,
        value: (&CostumeKeyword, &AssetDeclaration, &Semicolon, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        Ok(())
    }

    fn visit_code_block(&self, value: &CodeBlock, context: &mut C) -> Result<(), E> {
        for item in &value.statements {
            self.visit_statement(item, context)?;
        }
        Ok(())
    }

    fn visit_statement(&self, value: &Statement, context: &mut C) -> Result<(), E> {
        match value {
            Statement::DataDeclaration(let_keyword, data_declaration, semicolon, pos_range) => {
                self.visit_data_declaration_statement(
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
                self.visit_assignment(
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
                left_brace,
                items,
                right_brace,
                semicolon,
                pos_range,
            ) => {
                self.visit_list_assignment(
                    (
                        identifier,
                        normal_assignment_operator,
                        left_brace,
                        items,
                        right_brace,
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
                self.visit_set_item(
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
                self.visit_call_statement(
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
            Statement::SingleInputControl(
                identifier,
                expression,
                code_block,
                semicolon,
                pos_range,
            ) => {
                self.visit_single_input_control(
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
                self.visit_multi_input_control(
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
                self.visit_forever((identifier, code_block, semicolon, pos_range), context)?;
            }
            Statement::IfElse(start_branch, branches, else_branch, semicolon, pos_range) => {
                self.visit_if_else(
                    (start_branch, branches, else_branch, semicolon, pos_range),
                    context,
                )?;
            }
            Statement::EmptyStatement(semicolon) => {
                self.visit_empty_statement(semicolon, context)?;
            }
        }
        Ok(())
    }

    fn visit_data_declaration_statement(
        &self,
        value: (&LetKeyword, &DataDeclaration, &Semicolon, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        self.visit_data_declaration(value.1, context)?;
        Ok(())
    }

    fn visit_assignment(
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
        self.visit_expression(value.2, context)?;
        Ok(())
    }

    fn visit_list_assignment(
        &self,
        value: (
            &Identifier,
            &NormalAssignmentOperator,
            &LeftBrace,
            &Vec<(ListEntry, Option<Comma>)>,
            &RightBrace,
            &Semicolon,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        self.visit_list_content(value.3, context)?;
        Ok(())
    }

    fn visit_set_item(
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
        self.visit_expression(value.2, context)?;
        self.visit_expression(value.5, context)?;
        Ok(())
    }

    fn visit_call_statement(
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
        Ok(())
    }

    fn visit_single_input_control(
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
        self.visit_expression(value.1, context)?;
        self.visit_code_block(value.2, context)?;
        Ok(())
    }

    fn visit_multi_input_control(
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
        for item in value.2 {
            self.visit_expression(&item.0, context)?;
        }
        self.visit_code_block(value.4, context)?;
        Ok(())
    }

    fn visit_forever(
        &self,
        value: (&Identifier, &CodeBlock, &Option<Semicolon>, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        self.visit_code_block(value.1, context)?;
        Ok(())
    }

    fn visit_if_else(
        &self,
        value: (
            &(Identifier, Expression, CodeBlock),
            &Vec<(Identifier, Identifier, Expression, CodeBlock)>,
            &Option<(Identifier, CodeBlock)>,
            &Option<Semicolon>,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        self.visit_code_block(&value.0.2, context)?;
        for item in value.1 {
            self.visit_code_block(&item.3, context)?;
        }
        if let Some(value) = value.2 {
            self.visit_code_block(&value.1, context)?;
        }
        Ok(())
    }

    fn visit_expression(&self, value: &Expression, context: &mut C) -> Result<(), E> {
        match value {
            Expression::Literal(literal) => {
                self.visit_literal_expression(literal, context)?;
            }
            Expression::FormattedString(formatted_string_contents, pos_range) => {
                self.visit_formatted_string((formatted_string_contents, pos_range), context)?;
            }
            Expression::BinOp(expression, bin_op, expression1, pos_range) => {
                self.visit_binary_operation((expression, bin_op, expression1, pos_range), context)?;
            }
            Expression::UnOp(un_op, expression, pos_range) => {
                self.visit_unary_operation((un_op, expression, pos_range), context)?;
            }
            Expression::Identifier(identifier) => {
                self.visit_identifier_expression(identifier, context)?;
            }
            Expression::Call(identifier, left_parens, items, right_parens, pos_range) => {
                self.visit_call_expression(
                    (identifier, left_parens, items, right_parens, pos_range),
                    context,
                )?;
            }
            Expression::GetItem(identifier, left_bracket, expression, right_bracket, pos_range) => {
                self.visit_get_item(
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
            Expression::Parentheses(left_parens, expression, right_parens, pos_range) => {
                self.visit_parentheses_expression(
                    (left_parens, expression, right_parens, pos_range),
                    context,
                )?;
            }
        }
        Ok(())
    }

    fn visit_list_content(
        &self,
        value: &Vec<(ListEntry, Option<Comma>)>,
        context: &mut C,
    ) -> Result<(), E> {
        for item in value {
            match &item.0 {
                ListEntry::Expression(expression) => self.visit_expression(&expression, context)?,
                ListEntry::Unwrap(literal, _) => (),
            }
        }
        Ok(())
    }

    fn visit_data_declaration(&self, value: &DataDeclaration, context: &mut C) -> Result<(), E> {
        match value {
            DataDeclaration::Mixed(
                data_declaration_scope,
                left_parens,
                items,
                right_parens,
                pos_range,
            ) => {
                self.visit_mixed_data_declaration(
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
                self.visit_vars_data_declaration(
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
                self.visit_lists_data_declaration(
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
                self.visit_single_data_declaration(single_data_declaration, context)?;
            }
        }
        Ok(())
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
        for item in value.2 {
            self.visit_single_data_declaration(&item.0, context)?;
        }
        Ok(())
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
        for item in value.3 {
            self.visit_single_data_declaration(&item.0, context)?;
        }
        Ok(())
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
        for item in value.3 {
            self.visit_single_data_declaration(&item.0, context)?;
        }
        Ok(())
    }

    fn visit_single_data_declaration(
        &self,
        value: &SingleDataDeclaration,
        context: &mut C,
    ) -> Result<(), E> {
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
                self.visit_single_variable_declaration(
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
                self.visit_single_empty_variable_declaration(
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
                left_brace,
                items,
                right_brace,
                pos_range,
            ) => {
                self.visit_single_list_declaration(
                    (
                        list_keyword,
                        data_declaration_scope,
                        canonical_identifier,
                        identifier,
                        normal_assignment_operator,
                        left_brace,
                        items,
                        right_brace,
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
                self.visit_single_empty_list_declaration(
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
        self.visit_expression(value.5, context)?;
        Ok(())
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
        Ok(())
    }

    fn visit_single_list_declaration(
        &self,
        value: (
            &Option<ListKeyword>,
            &DataDeclarationScope,
            &Option<CanonicalIdentifier>,
            &Identifier,
            &NormalAssignmentOperator,
            &LeftBrace,
            &Vec<(ListEntry, Option<Comma>)>,
            &RightBrace,
            &PosRange,
        ),
        context: &mut C,
    ) -> Result<(), E> {
        self.visit_list_content(value.6, context)?;
        Ok(())
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
        Ok(())
    }

    fn visit_literal_expression(&self, value: &Literal, context: &mut C) -> Result<(), E> {
        Ok(())
    }

    fn visit_formatted_string(
        &self,
        value: (&Vec<FormattedStringContent>, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        for item in value.0 {
            self.visit_formatted_string_content(item, context)?;
        }
        Ok(())
    }

    fn visit_binary_operation(
        &self,
        value: (&Box<Expression>, &BinOp, &Box<Expression>, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        self.visit_expression(value.0.deref(), context)?;
        self.visit_expression(value.2.deref(), context)?;
        Ok(())
    }

    fn visit_unary_operation(
        &self,
        value: (&UnOp, &Box<Expression>, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        self.visit_expression(value.1.deref(), context)?;
        Ok(())
    }

    fn visit_identifier_expression(&self, value: &Identifier, context: &mut C) -> Result<(), E> {
        Ok(())
    }

    fn visit_call_expression(
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
        for item in value.2 {
            self.visit_expression(&item.0, context)?;
        }
        Ok(())
    }

    fn visit_get_item(
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
        self.visit_expression(value.2.deref(), context)?;
        Ok(())
    }

    fn visit_parentheses_expression(
        &self,
        value: (&LeftParens, &Box<Expression>, &RightParens, &PosRange),
        context: &mut C,
    ) -> Result<(), E> {
        self.visit_expression(value.1.deref(), context)?;
        Ok(())
    }

    fn visit_formatted_string_content(
        &self,
        value: &FormattedStringContent,
        context: &mut C,
    ) -> Result<(), E> {
        match value {
            FormattedStringContent::Expression(expression) => {
                self.visit_expression(expression, context)?;
            }
            FormattedStringContent::String(arc_str, _) => (),
        }
        Ok(())
    }

    // fn visit_(
    //     &self,
    //     value: &Literal,
    //     context: &mut C,
    // ) -> Result<(), E> {
    //     Ok(())
    // }
}
