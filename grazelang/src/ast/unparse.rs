use std::fmt::{Error as FormatError, Result as FormatResult, Write};

use serde::{Deserialize, Serialize};

use crate::{
    ast::types::{
        AssetDeclaration, BinOp, CanonicalIdentifier, CodeBlock, CustomBlockParamKind,
        DataDeclaration, DataDeclarationScope, DictionaryEntry, DictionaryValue, Expression,
        Identifier, ListEntry, Literal, MonitorValue, SingleAssetDeclaration,
        SingleAssetDeclarationValue, SingleDataDeclaration, SingleIdentifier, SpriteCodeBlock,
        SpriteStatement, StageCodeBlock, StageStatement, Statement, TopLevelStatement, UnOp,
        UseStatementContent, WarpSpecifier,
    },
    parser::cst::Associativity,
    utils::string_escape,
};

pub trait UnparseAST {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write;

    fn unparse_to_string(&self) -> Result<String, FormatError> {
        let mut string = String::new();
        self.unparse_into(&mut string)?;
        Ok(string)
    }
}

struct UnparseASTAdapter<'a, T>(&'a T)
where
    T: UnparseAST;

impl<'a, T> std::fmt::Display for UnparseASTAdapter<'a, T>
where
    T: UnparseAST,
{
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> FormatResult {
        self.0.unparse_into(f)
    }
}

fn unparse_expression_list<W>(value: &[Expression], f: &mut W) -> FormatResult
where
    W: Write,
{
    f.write_char('(')?;
    for i in value {
        i.unparse_into(f)?;
        f.write_str(", ")?;
    }
    f.write_char(')')
}

fn unparse_flexible_expression_list<W>(value: &[Expression], f: &mut W) -> FormatResult
where
    W: Write,
{
    match value.len() {
        1 => value.first().unwrap().unparse_into(f),
        2.. => {
            unparse_expression_list(value, f)
        }
        0 => Ok(()),
    }
}

impl UnparseAST for Literal {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        match self {
            Literal::String(value) => {
                write!(f, "\"{}\"", string_escape::normal_string_escaper(value))
            }
            Literal::DecimalInt(value)
            | Literal::DecimalFloat(value)
            | Literal::HexadecimalInt(value)
            | Literal::OctalInt(value)
            | Literal::BinaryInt(value) => write!(f, "{value} "),
            Literal::Bool(value) => write!(f, "{value} "),
            Literal::EmptyExpression => write!(f, "()"),
        }
    }
}

impl UnparseAST for UnOp {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        match self {
            UnOp::Minus => write!(f, "-"),
            UnOp::Not => write!(f, "!"),
            UnOp::Exp => write!(f, "10^"),
            UnOp::Pow => write!(f, "e^"),
        }
    }
}

impl UnparseAST for BinOp {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        write!(
            f,
            "{}",
            match self {
                BinOp::Plus => "+",
                BinOp::Minus => "-",
                BinOp::Times => "*",
                BinOp::Div => "/",
                BinOp::Mod => "%",
                BinOp::Join => "++",
                BinOp::Contains => "contains",
                BinOp::And => "&&",
                BinOp::Or => "||",
                BinOp::Equals => "==",
                BinOp::NotEquals => "!=",
                BinOp::LessThan => "<",
                BinOp::GreaterThan => ">",
                BinOp::LessThanOrEqual => "<=",
                BinOp::GreaterThanOrEqual => ">=",
            }
        )
    }
}

impl UnparseAST for SingleIdentifier {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        write!(f, "{}", &self.value)
    }
}

impl UnparseAST for Identifier {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        use UnparseASTAdapter as u;
        let mut iter = self.path.iter();
        if let Some(value) = iter.next() {
            value.unparse_into(f)?;
        }
        for i in iter {
            write!(f, "::{}", u(i))?;
        }
        Ok(())
    }
}

impl UnparseAST for CanonicalIdentifier {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        write!(f, "`{}`", string_escape::canonical_name_escaper(&self.name))
    }
}

impl Expression {
    pub fn requires_parentheses_for_unops(&self) -> bool {
        matches!(self, Expression::BinOp { .. })
    }
}

impl BinOp {
    pub fn get_precedence(self) -> (u8, Associativity) {
        use Associativity::Left as L;
        match self {
            BinOp::Plus => (4, L),
            BinOp::Minus => (4, L),
            BinOp::Times => (5, L),
            BinOp::Div => (5, L),
            BinOp::Mod => (5, L),
            BinOp::Join => (3, L),
            BinOp::Contains => (3, L),
            BinOp::And => (1, L),
            BinOp::Or => (0, L),
            BinOp::Equals => (2, L),
            BinOp::NotEquals => (2, L),
            BinOp::LessThan => (2, L),
            BinOp::GreaterThan => (2, L),
            BinOp::LessThanOrEqual => (2, L),
            BinOp::GreaterThanOrEqual => (2, L),
        }
    }
}

impl UnparseAST for Expression {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        use UnparseASTAdapter as u;
        match self {
            Expression::Literal(value) => value.unparse_into(f),
            Expression::FormattedString(value) => {
                f.write_char('"')?;
                for i in value {
                    match i {
                        crate::ast::types::FormattedStringContent::Expression(expression) => {
                            write!(f, "${{{}}}", u(expression.as_ref()))?;
                        }
                        crate::ast::types::FormattedStringContent::String(value) => {
                            string_escape::format_string_escaper(value).escape_into(f)?;
                        }
                    }
                }
                f.write_char('"')
            }
            Expression::BinOp {
                operator,
                left_operand,
                right_operand,
            } => {
                fn unparse_expression_in_binop<W>(
                    expression: &Expression,
                    left: bool,
                    precedence: u8,
                    associativity: Associativity,
                    f: &mut W,
                ) -> FormatResult
                where
                    W: Write,
                {
                    if let Expression::BinOp {
                        operator,
                        left_operand: _,
                        right_operand: _,
                    } = expression
                        && let (inner_precedence, _) = operator.get_precedence()
                        && (inner_precedence < precedence
                            || (inner_precedence == precedence
                                && if left {
                                    associativity != Associativity::Left
                                } else {
                                    associativity != Associativity::Right
                                }))
                    {
                        return write!(f, "({})", u(expression));
                    }
                    expression.unparse_into(f)
                }
                let (precedence, associativity) = operator.get_precedence();
                unparse_expression_in_binop(left_operand, true, precedence, associativity, f)?;
                write!(f, " {} ", u(operator))?;
                unparse_expression_in_binop(right_operand, false, precedence, associativity, f)
            }
            Expression::UnOp { operator, operand } => {
                if operand.requires_parentheses_for_unops() {
                    write!(f, "{}({})", u(operator), u(operand.as_ref()))
                } else {
                    operator.unparse_into(f)?;
                    operand.unparse_into(f)
                }
            }
            Expression::Identifier(identifier) => identifier.unparse_into(f),
            Expression::Call {
                function,
                arguments,
            } => {
                function.unparse_into(f)?;
                unparse_expression_list(arguments, f)
            }
            Expression::GetItem { list, item } => {
                write!(f, "{}[{}]", u(list), u(item.as_ref()))
            }
            Expression::GetLetter { expression, letter } => {
                write!(f, "{}@[{}]", u(expression.as_ref()), u(letter.as_ref()))
            }
        }
    }
}

impl UnparseAST for Statement {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        use UnparseASTAdapter as u;
        match self {
            Statement::DataDeclaration(data_declaration) => {
                data_declaration.unparse_into(f)?;
                f.write_char(';')
            }
            Statement::Assignment { target, value } => {
                write!(f, "{} = {};", u(target), u(value))
            }
            Statement::ListAssignment { target, value } => {
                target.unparse_into(f)?;
                f.write_str(" = [")?;
                for i in value {
                    i.unparse_into(f)?;
                    f.write_str(", ")?;
                }
                f.write_str("];")
            }
            Statement::SetItem { list, item, value } => {
                write!(f, "{}[{}] = {};", u(list), u(item), u(value))
            }
            Statement::Call {
                function,
                arguments,
            } => {
                function.unparse_into(f)?;
                unparse_expression_list(arguments, f)?;
                f.write_char(';')
            }
            Statement::Control {
                control_function,
                arguments,
                code_block,
            } => {
                control_function.unparse_into(f)?;
                unparse_flexible_expression_list(arguments, f)?;
                f.write_char(' ')?;
                code_block.unparse_into(f)
            }
            Statement::IfElse {
                first_branch,
                alternative_branches,
                else_branch,
            } => {
                write!(f, "if {} {}", u(&first_branch.0), u(&first_branch.1))?;
                for i in alternative_branches {
                    write!(f, "else if {} {}", u(&i.0), u(&i.1))?;
                }
                if let Some(else_branch) = else_branch {
                    f.write_str("else ")?;
                    else_branch.unparse_into(f)?;
                }
                Ok(())
            }
            Statement::UseStatement(use_statement_content) => {
                write!(f, "use {};", u(use_statement_content))
            }
            Statement::UseExtensionStatement(use_statement_content) => {
                write!(f, "use extension {};", u(use_statement_content))
            }
        }
    }
}

impl UnparseAST for CodeBlock {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        f.write_char('{')?;
        for i in &self.statements {
            i.unparse_into(f)?;
        }
        f.write_char('}')
    }
}

impl UnparseAST for UseStatementContent {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        match self {
            UseStatementContent::SingleUse { identifier, rename } => {
                identifier.unparse_into(f)?;
                if let Some(rename) = rename {
                    f.write_str(" as ")?;
                    rename.unparse_into(f)?;
                }
                Ok(())
            }
            UseStatementContent::MultiUse { root, content } => {
                root.unparse_into(f)?;
                f.write_str("::{")?;
                for i in content {
                    i.unparse_into(f)?;
                    f.write_str(", ")?;
                }
                f.write_char('}')
            }
        }
    }
}

impl UnparseAST for DataDeclaration {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        use UnparseASTAdapter as u;
        match self {
            DataDeclaration::Mixed {
                scope,
                declarations,
            } => {
                if scope == &DataDeclarationScope::Unset {
                    f.write_str("let (")?;
                } else {
                    write!(f, "let {} (", u(scope))?;
                }
                for i in declarations {
                    (i, SingleDataDeclarationDefaultKind::Variable).unparse_into(f)?;
                    f.write_str(", ")?;
                }
                f.write_char(')')
            }
            DataDeclaration::Vars {
                scope,
                declarations,
            } => {
                if scope == &DataDeclarationScope::Unset {
                    f.write_str("let vars {")?;
                } else {
                    write!(f, "let {} vars {{", u(scope))?;
                }
                for i in declarations {
                    (i, SingleDataDeclarationDefaultKind::Variable).unparse_into(f)?;
                    f.write_str(", ")?;
                }
                f.write_char('}')
            }
            DataDeclaration::Lists {
                scope,
                declarations,
            } => {
                if scope == &DataDeclarationScope::Unset {
                    f.write_str("let lists {")?;
                } else {
                    write!(f, "let {} lists {{", u(scope))?;
                }
                for i in declarations {
                    (i, SingleDataDeclarationDefaultKind::List).unparse_into(f)?;
                    f.write_str(", ")?;
                }
                f.write_char('}')
            }
            DataDeclaration::Single(single_data_declaration) => (
                single_data_declaration.as_ref(),
                SingleDataDeclarationDefaultKind::Variable,
            )
                .unparse_into(f),
        }
    }
}

impl UnparseAST for DataDeclarationScope {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        match self {
            DataDeclarationScope::Global => f.write_str("global"),
            DataDeclarationScope::Local => f.write_str("local"),
            DataDeclarationScope::Cloud => f.write_str("cloud"),
            DataDeclarationScope::Unset => Ok(()),
        }
    }
}

impl UnparseAST for SingleDataDeclaration {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        (self, SingleDataDeclarationDefaultKind::Variable).unparse_into(f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum SingleDataDeclarationDefaultKind {
    List,
    Variable,
}

impl UnparseAST for (&SingleDataDeclaration, SingleDataDeclarationDefaultKind) {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        match self.0 {
            SingleDataDeclaration::Variable {
                scope,
                canonical_identifier,
                identifier,
                value,
            } => {
                if self.1 != SingleDataDeclarationDefaultKind::Variable {
                    f.write_str("var ")?;
                }
                if scope != &DataDeclarationScope::Unset {
                    scope.unparse_into(f)?;
                    f.write_char(' ')?;
                }
                if let Some(canonical_identifier) = canonical_identifier {
                    canonical_identifier.unparse_into(f)?;
                    f.write_char(' ')?;
                }
                identifier.unparse_into(f)?;
                if let Some(value) = value {
                    f.write_str(" = ")?;
                    value.unparse_into(f)?;
                }
                Ok(())
            }
            SingleDataDeclaration::List {
                scope,
                canonical_identifier,
                identifier,
                value,
            } => {
                if self.1 != SingleDataDeclarationDefaultKind::List {
                    f.write_str("list ")?;
                }
                if scope != &DataDeclarationScope::Unset {
                    scope.unparse_into(f)?;
                    f.write_char(' ')?;
                }
                if let Some(canonical_identifier) = canonical_identifier {
                    canonical_identifier.unparse_into(f)?;
                    f.write_char(' ')?;
                }
                identifier.unparse_into(f)?;
                if !value.is_empty() {
                    f.write_str(" = [")?;
                    for i in value {
                        i.unparse_into(f)?;
                        f.write_str(", ")?;
                    }
                    f.write_char(']')?;
                }
                Ok(())
            }
            SingleDataDeclaration::FileList {
                scope,
                canonical_identifier,
                identifier,
                source,
            } => {
                if self.1 != SingleDataDeclarationDefaultKind::List {
                    f.write_str("list ")?;
                }
                if scope != &DataDeclarationScope::Unset {
                    scope.unparse_into(f)?;
                    f.write_char(' ')?;
                }
                if let Some(canonical_identifier) = canonical_identifier {
                    canonical_identifier.unparse_into(f)?;
                    f.write_char(' ')?;
                }
                identifier.unparse_into(f)?;
                f.write_str(" = file ")?;
                source.unparse_into(f)
            }
        }
    }
}

impl UnparseAST for ListEntry {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        match self {
            ListEntry::Expression(expression) => expression.unparse_into(f),
            ListEntry::Unwrap(literal) => {
                f.write_str("..")?;
                literal.unparse_into(f)
            }
        }
    }
}

impl UnparseAST for StageStatement {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        match self {
            StageStatement::DataDeclaration(data_declaration) => {
                data_declaration.unparse_into(f)?;
                f.write_char(';')
            }
            StageStatement::BackdropDeclaration(asset_declaration) => {
                f.write_str("backdrop ")?;
                asset_declaration.unparse_into(f)?;
                f.write_char(';')
            }
            StageStatement::SoundDeclaration(asset_declaration) => {
                f.write_str("sound ")?;
                asset_declaration.unparse_into(f)?;
                f.write_char(';')
            }
            StageStatement::HatStatement {
                hat_function,
                arguments,
                code_block,
            } => {
                hat_function.unparse_into(f)?;
                unparse_flexible_expression_list(arguments, f)?;
                f.write_char(' ')?;
                code_block.unparse_into(f)
            }
            StageStatement::CustomBlockDefinition {
                is_warp,
                canonical_identifier,
                identifier,
                parameters,
                code_block,
            } => {
                is_warp.unparse_into(f)?;
                f.write_str(" proc ")?;
                if let Some(canonical_identifier) = canonical_identifier {
                    canonical_identifier.unparse_into(f)?;
                    f.write_char(' ')?;
                }
                identifier.unparse_into(f)?;
                f.write_char('(')?;
                for i in parameters {
                    if let Some(param_kind) = &i.0 {
                        param_kind.unparse_into(f)?;
                        f.write_char(' ')?;
                    }
                    if let Some(canonical_identifier) = &i.1 {
                        canonical_identifier.unparse_into(f)?;
                        f.write_char(' ')?;
                    }
                    i.2.unparse_into(f)?;
                    f.write_str(", ")?;
                }
                f.write_str(") ")?;
                code_block.unparse_into(f)
            }
            StageStatement::IsolatedBlock(code_block) => code_block.unparse_into(f),
            StageStatement::IsolatedExpression(expression) => {
                f.write_char('(')?;
                expression.unparse_into(f)?;
                f.write_char(')')
            }
            StageStatement::ConfigStatement(items) => {
                f.write_str("config {")?;
                for i in items {
                    i.unparse_into(f)?;
                    f.write_str(", ")?;
                }
                f.write_char('}')
            }
            StageStatement::MonitorDeclaration {
                value,
                configuration,
            } => {
                f.write_str("monitor ")?;
                value.unparse_into(f)?;
                f.write_str(" {")?;
                for i in configuration {
                    i.unparse_into(f)?;
                    f.write_str(", ")?;
                }
                f.write_char('}')
            }
            StageStatement::UseStatement(use_statement_content) => {
                f.write_str("use ")?;
                use_statement_content.unparse_into(f)?;
                f.write_char(';')
            }
            StageStatement::UseExtensionStatement(use_statement_content) => {
                f.write_str("use extension ")?;
                use_statement_content.unparse_into(f)?;
                f.write_char(';')
            }
        }
    }
}

impl UnparseAST for SpriteStatement {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        match self {
            SpriteStatement::DataDeclaration(data_declaration) => {
                data_declaration.unparse_into(f)?;
                f.write_char(';')
            }
            SpriteStatement::CostumeDeclaration(asset_declaration) => {
                f.write_str("costume ")?;
                asset_declaration.unparse_into(f)?;
                f.write_char(';')
            }
            SpriteStatement::SoundDeclaration(asset_declaration) => {
                f.write_str("sound ")?;
                asset_declaration.unparse_into(f)?;
                f.write_char(';')
            }
            SpriteStatement::HatStatement {
                hat_function,
                arguments,
                code_block,
            } => {
                hat_function.unparse_into(f)?;
                unparse_flexible_expression_list(arguments, f)?;
                f.write_char(' ')?;
                code_block.unparse_into(f)
            }
            SpriteStatement::CustomBlockDefinition {
                is_warp,
                canonical_identifier,
                identifier,
                parameters,
                code_block,
            } => {
                is_warp.unparse_into(f)?;
                f.write_str(" proc ")?;
                if let Some(canonical_identifier) = canonical_identifier {
                    canonical_identifier.unparse_into(f)?;
                    f.write_char(' ')?;
                }
                identifier.unparse_into(f)?;
                f.write_char('(')?;
                for i in parameters {
                    if let Some(param_kind) = &i.0 {
                        param_kind.unparse_into(f)?;
                        f.write_char(' ')?;
                    }
                    if let Some(canonical_identifier) = &i.1 {
                        canonical_identifier.unparse_into(f)?;
                        f.write_char(' ')?;
                    }
                    i.2.unparse_into(f)?;
                    f.write_str(", ")?;
                }
                f.write_str(") ")?;
                code_block.unparse_into(f)
            }
            SpriteStatement::IsolatedBlock(code_block) => code_block.unparse_into(f),
            SpriteStatement::IsolatedExpression(expression) => {
                f.write_char('(')?;
                expression.unparse_into(f)?;
                f.write_char(')')
            }
            SpriteStatement::ConfigStatement(items) => {
                f.write_str("config {")?;
                for i in items {
                    i.unparse_into(f)?;
                    f.write_str(", ")?;
                }
                f.write_char('}')
            }
            SpriteStatement::MonitorDeclaration {
                value,
                configuration,
            } => {
                f.write_str("monitor ")?;
                value.unparse_into(f)?;
                f.write_str(" {")?;
                for i in configuration {
                    i.unparse_into(f)?;
                    f.write_str(", ")?;
                }
                f.write_char('}')
            }
            SpriteStatement::UseStatement(use_statement_content) => {
                f.write_str("use ")?;
                use_statement_content.unparse_into(f)?;
                f.write_char(';')
            }
            SpriteStatement::UseExtensionStatement(use_statement_content) => {
                f.write_str("use extension ")?;
                use_statement_content.unparse_into(f)?;
                f.write_char(';')
            }
        }
    }
}

impl UnparseAST for AssetDeclaration {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        match self {
            AssetDeclaration::Multiple(single_asset_declarations) => {
                f.write_char('(')?;
                for i in single_asset_declarations {
                    i.unparse_into(f)?;
                    f.write_str(", ")?;
                }
                f.write_char(')')
            }
            AssetDeclaration::Single(single_asset_declaration) => {
                single_asset_declaration.unparse_into(f)
            }
        }
    }
}

impl UnparseAST for SingleAssetDeclaration {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        if let Some(canonical_identifier) = &self.canonical_identifier {
            canonical_identifier.unparse_into(f)?;
            f.write_char(' ')?;
        }
        self.identifier.unparse_into(f)?;
        self.value.unparse_into(f)?;
        Ok(())
    }
}

impl UnparseAST for SingleAssetDeclarationValue {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        match self {
            SingleAssetDeclarationValue::Simple(value) => {
                write!(f, "(\"{}\")", string_escape::normal_string_escaper(value))
            }
            SingleAssetDeclarationValue::Dictionary(items) => {
                f.write_char('{')?;
                for i in items {
                    i.unparse_into(f)?;
                    f.write_str(", ")?;
                }
                f.write_char('}')
            }
        }
    }
}

impl UnparseAST for DictionaryEntry {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        self.identifier.unparse_into(f)?;
        f.write_str(": ")?;
        self.value.unparse_into(f)
    }
}

impl UnparseAST for DictionaryValue {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        match self {
            DictionaryValue::Primitive(value) => value.unparse_into(f),
            DictionaryValue::Dictionary(items) => {
                f.write_char('{')?;
                for i in items {
                    i.unparse_into(f)?;
                    f.write_str(", ")?;
                }
                f.write_char('}')
            }
            DictionaryValue::List(items) => {
                f.write_char('[')?;
                for i in items {
                    i.unparse_into(f)?;
                    f.write_str(", ")?;
                }
                f.write_char(']')
            }
        }
    }
}

impl UnparseAST for WarpSpecifier {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        if self.is_warp {
            f.write_str("warp")
        } else {
            f.write_str("nowarp")
        }
    }
}

impl UnparseAST for CustomBlockParamKind {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        f.write_str(match self {
            CustomBlockParamKind::Number => "num",
            CustomBlockParamKind::String => "str",
            CustomBlockParamKind::Boolean => "bool",
        })
    }
}

impl UnparseAST for MonitorValue {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        match self {
            MonitorValue::Identifier(identifier) => identifier.unparse_into(f),
            MonitorValue::Call {
                function,
                arguments,
            } => {
                function.unparse_into(f)?;
                unparse_expression_list(arguments, f)
            }
        }
    }
}

impl UnparseAST for StageCodeBlock {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        f.write_char('{')?;
        for i in &self.statements {
            i.unparse_into(f)?;
        }
        f.write_char('}')
    }
}

impl UnparseAST for SpriteCodeBlock {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        f.write_char('{')?;
        for i in &self.statements {
            i.unparse_into(f)?;
        }
        f.write_char('}')
    }
}

impl UnparseAST for TopLevelStatement {
    fn unparse_into<W>(&self, f: &mut W) -> FormatResult
    where
        W: Write,
    {
        match self {
            TopLevelStatement::Stage { code_block } => {
                f.write_str("stage ")?;
                code_block.unparse_into(f)
            }
            TopLevelStatement::Sprite {
                canonical_identifier,
                identifier,
                code_block,
            } => {
                f.write_str("sprite ")?;
                if let Some(canonical_identifier) = canonical_identifier {
                    canonical_identifier.unparse_into(f)?;
                    f.write_char(' ')?;
                }
                identifier.unparse_into(f)?;
                f.write_char(' ')?;
                code_block.unparse_into(f)
            }
            TopLevelStatement::BroadcastDeclaration {
                canonical_identifier,
                identifier,
            } => {
                f.write_str("broadcast ")?;
                if let Some(canonical_identifier) = canonical_identifier {
                    canonical_identifier.unparse_into(f)?;
                    f.write_char(' ')?;
                }
                identifier.unparse_into(f)?;
                f.write_char(';')
            }
            TopLevelStatement::UseStatement(use_statement_content) => {
                f.write_str("use ")?;
                use_statement_content.unparse_into(f)?;
                f.write_char(';')
            }
            TopLevelStatement::UseExtensionStatement(use_statement_content) => {
                f.write_str("use extension ")?;
                use_statement_content.unparse_into(f)?;
                f.write_char(';')
            }
        }
    }
}
