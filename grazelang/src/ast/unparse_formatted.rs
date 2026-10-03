use std::borrow::Cow;

use pretty::{Arena, DocAllocator, DocBuilder};
use serde::{Deserialize, Serialize};

use super::unparse::SingleDataDeclarationDefaultKind;
use crate::{
    ast::types::{
        AssetDeclaration, BinOp, CanonicalIdentifier, CodeBlock, CustomBlockParamKind,
        DataDeclaration, DataDeclarationScope, DictionaryEntry, DictionaryValue, Expression,
        GrazeProgram, Identifier, ListEntry, Literal, MonitorValue, SingleAssetDeclaration,
        SingleAssetDeclarationValue, SingleDataDeclaration, SingleIdentifier, SpriteCodeBlock,
        SpriteStatement, StageCodeBlock, StageStatement, Statement, TopLevelStatement, UnOp,
        UseStatementContent, WarpSpecifier,
    },
    parser::cst::Associativity,
    utils::string_escape::{self, normal_string_escaper},
};

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct UnparseASTFormattedSettings {
    pub indentation: isize,
    pub width: usize,
    pub hard_lines_in_code_blocks: bool,
}

use UnparseASTFormattedSettings as Settings;

pub trait UnparseASTFormatted: Sized {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone;

    fn with_pretty_doc<F, O>(self, f: F, settings: &Settings) -> O
    where
        F: for<'a> FnOnce(DocBuilder<'a, Arena<'a>, ()>) -> O,
    {
        let allocator = Arena::new();
        let value = self.into_pretty_doc(&allocator, settings);
        f(value)
    }

    fn unparse_formatted_into<W>(self, f: &mut W, settings: &Settings) -> std::fmt::Result
    where
        W: std::fmt::Write,
    {
        self.with_pretty_doc(|value| value.render_fmt(settings.width, f), settings)
    }

    fn unparse_formatted_into_io<W>(self, f: &mut W, settings: &Settings) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        self.with_pretty_doc(|value| value.render(settings.width, f), settings)
    }

    fn unparse_formatted_to_string(self, settings: &Settings) -> String {
        let mut out = String::new();
        self.with_pretty_doc(|value| value.render_fmt(settings.width, &mut out), settings)
            .unwrap();
        out
    }
}

macro_rules! simple_impl_unparse_ast_formatted {
    ($t:ty, $s:pat => $e:expr) => {
        impl UnparseASTFormatted for &$t {
            fn into_pretty_doc<'a, D>(
                self,
                allocator: &'a D,
                _settings: &Settings,
            ) -> DocBuilder<'a, D, ()>
            where
                Self: 'a,
                D: DocAllocator<'a, ()>,
                D::Doc: Clone,
            {
                let $s = self;
                allocator.text($e)
            }
        }
    };
}

macro_rules! wrap_csv {
    ($items:expr, $startend:expr, $start:expr, $end:expr, $line_type:ident, $allocator:expr, $settings:expr) => {
        if $items.is_empty() {
            $allocator.text($startend)
        } else {
            $allocator
                .text($start)
                .append(
                    $allocator
                        .$line_type()
                        .append(
                            $allocator.intersperse(
                                $items
                                    .iter()
                                    .map(|value| value.into_pretty_doc($allocator, $settings)),
                                $allocator.text(",").append($allocator.line()),
                            ),
                        )
                        .nest($settings.indentation)
                        .append($allocator.text(",").flat_alt($allocator.nil()))
                        .append($allocator.$line_type())
                        .group(),
                )
                .append($end)
        }
    };
    ($items:expr, BRACKETS, $allocator:expr, $settings:expr) => {
        wrap_csv!($items, "[]", "[", "]", line_, $allocator, $settings)
    };
    ($items:expr, PARENS, $allocator:expr, $settings:expr) => {
        wrap_csv!($items, "()", "(", ")", line_, $allocator, $settings)
    };
    ($items:expr, BRACES, $allocator:expr, $settings:expr) => {
        wrap_csv!($items, "{}", "{", "}", line, $allocator, $settings)
    };
}

macro_rules! wrap_code_block {
    ($items:expr, $allocator:expr, $settings:expr) => {
        if $items.is_empty() {
            $allocator.text("{}")
        } else {
            let line = if $settings.hard_lines_in_code_blocks {
                $allocator.hardline()
            } else {
                $allocator.line()
            };
            $allocator
                .text("{")
                .append(
                    line.clone()
                        .append(
                            $allocator.intersperse(
                                $items
                                    .iter()
                                    .map(|value| value.into_pretty_doc($allocator, $settings)),
                                line.clone(),
                            ),
                        )
                        .nest($settings.indentation)
                        .append(line)
                        .group(),
                )
                .append("}")
        }
    };
}

macro_rules! impl_unparse_ast_for_code_block {
    ($t:ty) => {
        impl UnparseASTFormatted for &$t {
            fn into_pretty_doc<'a, D>(
                self,
                allocator: &'a D,
                settings: &Settings,
            ) -> DocBuilder<'a, D, ()>
            where
                Self: 'a,
                D: DocAllocator<'a, ()>,
                D::Doc: Clone,
            {
                wrap_code_block!(self.statements, allocator, settings)
            }
        }
    };
}

fn unparse_flexible_expression_list<'a, D>(
    value: &'a [Expression],
    allocator: &'a D,
    settings: &Settings,
) -> Option<DocBuilder<'a, D, ()>>
where
    D: DocAllocator<'a, ()>,
    D::Doc: Clone,
{
    Some(match value.len() {
        1 => value.first().unwrap().into_pretty_doc(allocator, settings),
        2.. => wrap_csv!(value, PARENS, allocator, settings),

        0 => return None,
    })
}

impl UnparseASTFormatted for &Literal {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, _settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        allocator.text(match self {
            Literal::String(value) => Cow::Owned(format!(
                "\"{}\"",
                string_escape::normal_string_escaper(value)
            )),
            Literal::DecimalInt(value)
            | Literal::DecimalFloat(value)
            | Literal::HexadecimalInt(value)
            | Literal::OctalInt(value)
            | Literal::BinaryInt(value) => Cow::Borrowed(value.as_str()),
            Literal::Bool(value) => {
                if *value {
                    Cow::Borrowed("true")
                } else {
                    Cow::Borrowed("false")
                }
            }
            Literal::EmptyExpression => Cow::Borrowed("()"),
        })
    }
}

simple_impl_unparse_ast_formatted!(UnOp, this => this.as_str());
simple_impl_unparse_ast_formatted!(BinOp, this => this.as_str());
simple_impl_unparse_ast_formatted!(SingleIdentifier, this => &this.value);

impl UnparseASTFormatted for &Identifier {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        let mut iter = self.path.iter();
        let mut value = iter
            .next()
            .map(|value| value.into_pretty_doc(allocator, settings))
            .unwrap_or_else(|| allocator.nil());
        for i in iter {
            value = value
                .append("::")
                .append(i.into_pretty_doc(allocator, settings));
        }
        value
    }
}

impl UnparseASTFormatted for &CanonicalIdentifier {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, _settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        allocator.text(format!(
            "`{}`",
            string_escape::canonical_name_escaper(&self.name)
        ))
    }
}

impl UnparseASTFormatted for &Expression {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            Expression::Literal(value) => value.into_pretty_doc(allocator, settings),
            Expression::FormattedString(content) => {
                let mut value = allocator.text("\"");
                for i in content {
                    match i {
                        crate::ast::types::FormattedStringContent::Expression(expression) => {
                            value = value
                                .append("${")
                                .append(
                                    allocator
                                        .line_()
                                        .append(expression.into_pretty_doc(allocator, settings))
                                        .nest(settings.indentation)
                                        .append(allocator.line_())
                                        .group(),
                                )
                                .append("}");
                        }
                        crate::ast::types::FormattedStringContent::String(content) => {
                            value = value.append(
                                string_escape::format_string_escaper(content).escape_to_string(),
                            );
                        }
                    }
                }
                value.append("\"")
            }
            Expression::BinOp {
                operator,
                left_operand,
                right_operand,
            } => {
                fn unparse_binop_chain<'a, D>(
                    expression: &'a Expression,
                    left: bool,
                    precedence: u8,
                    associativity: Associativity,
                    allocator: &'a D,
                    settings: &Settings,
                ) -> DocBuilder<'a, D, ()>
                where
                    D: DocAllocator<'a, ()>,
                    D::Doc: Clone,
                {
                    let Expression::BinOp {
                        operator,
                        left_operand,
                        right_operand,
                    } = expression
                    else {
                        return expression.into_pretty_doc(allocator, settings);
                    };
                    let (inner_precedence, inner_associativity) = operator.get_precedence();
                    if inner_precedence < precedence
                        || (inner_precedence == precedence
                            && if left {
                                associativity != Associativity::Left
                            } else {
                                associativity != Associativity::Right
                            })
                    {
                        allocator
                            .text("(")
                            .append(
                                allocator
                                    .line_()
                                    .append(expression.into_pretty_doc(allocator, settings))
                                    .nest(settings.indentation)
                                    .append(allocator.line_())
                                    .group(),
                            )
                            .append(allocator.text(")"))
                    } else {
                        let value = unparse_binop_chain(
                            left_operand,
                            true,
                            inner_precedence,
                            inner_associativity,
                            allocator,
                            settings,
                        )
                        .append(allocator.line())
                        .append(operator.into_pretty_doc(allocator, settings))
                        .append(" ")
                        .append(unparse_binop_chain(
                            right_operand,
                            false,
                            inner_precedence,
                            inner_associativity,
                            allocator,
                            settings,
                        ));
                        if inner_precedence != precedence {
                            value.group()
                        } else {
                            value
                        }
                    }
                }
                let (precedence, associativity) = operator.get_precedence();
                unparse_binop_chain(
                    left_operand,
                    true,
                    precedence,
                    associativity,
                    allocator,
                    settings,
                )
                .append(allocator.line())
                .append(operator.into_pretty_doc(allocator, settings))
                .append(" ")
                .append(unparse_binop_chain(
                    right_operand,
                    false,
                    precedence,
                    associativity,
                    allocator,
                    settings,
                ))
                .nest(settings.indentation)
                .group()
            }
            Expression::UnOp { operator, operand } => {
                if operand.requires_parentheses_for_unops() {
                    operator
                        .into_pretty_doc(allocator, settings)
                        .append("(")
                        .append(
                            allocator
                                .line_()
                                .append(operand.into_pretty_doc(allocator, settings))
                                .nest(settings.indentation)
                                .append(allocator.line_())
                                .group(),
                        )
                        .append(")")
                } else {
                    operator
                        .into_pretty_doc(allocator, settings)
                        .append(operand.into_pretty_doc(allocator, settings))
                }
            }
            Expression::Identifier(identifier) => identifier.into_pretty_doc(allocator, settings),
            Expression::Call {
                function,
                arguments,
            } => function
                .into_pretty_doc(allocator, settings)
                .append(wrap_csv!(arguments, PARENS, allocator, settings)),
            Expression::GetItem { list, item } => list
                .into_pretty_doc(allocator, settings)
                .append("[")
                .append(
                    allocator
                        .line_()
                        .append(item.into_pretty_doc(allocator, settings))
                        .nest(settings.indentation)
                        .append(allocator.line_())
                        .group(),
                )
                .append("]"),
            Expression::GetLetter { expression, letter } => expression
                .into_pretty_doc(allocator, settings)
                .append("@[")
                .append(
                    allocator
                        .line_()
                        .append(letter.into_pretty_doc(allocator, settings))
                        .nest(settings.indentation)
                        .append(allocator.line_())
                        .group(),
                )
                .append("]"),
        }
    }
}

impl UnparseASTFormatted for &Statement {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            Statement::DataDeclaration(data_declaration) => data_declaration
                .into_pretty_doc(allocator, settings)
                .append(";"),
            Statement::Assignment { target, value } => target
                .into_pretty_doc(allocator, settings)
                .append(" =")
                .append(allocator.line())
                .append(value.into_pretty_doc(allocator, settings))
                .append(";")
                .nest(settings.indentation)
                .group(),
            Statement::ListAssignment { target, value } => target
                .into_pretty_doc(allocator, settings)
                .append(" =")
                .append(allocator.line())
                .append({
                    if value.is_empty() {
                        allocator.text("[];")
                    } else {
                        allocator
                            .text("[")
                            .append(allocator.line_())
                            .append(
                                allocator.intersperse(
                                    value
                                        .iter()
                                        .map(|value| value.into_pretty_doc(allocator, settings)),
                                    allocator.text(",").append(allocator.line()),
                                ),
                            )
                            .nest(settings.indentation)
                            .append(allocator.text(",").flat_alt(allocator.nil()))
                            .append(allocator.line_())
                            .group()
                            .append("];")
                    }
                })
                .nest(settings.indentation)
                .group(),
            Statement::SetItem { list, item, value } => list
                .into_pretty_doc(allocator, settings)
                .append("[")
                .append(
                    allocator
                        .line_()
                        .append(item.into_pretty_doc(allocator, settings))
                        .nest(settings.indentation)
                        .append(allocator.line_())
                        .group(),
                )
                .append("] =")
                .append(allocator.line())
                .append(value.into_pretty_doc(allocator, settings))
                .append(";")
                .nest(settings.indentation)
                .group(),
            Statement::Call {
                function,
                arguments,
            } => function
                .into_pretty_doc(allocator, settings)
                .append(wrap_csv!(arguments, PARENS, allocator, settings))
                .append(";"),
            Statement::Control {
                control_function,
                arguments,
                code_block,
            } => control_function
                .into_pretty_doc(allocator, settings)
                .append({
                    unparse_flexible_expression_list(arguments, allocator, settings)
                        .map(|value| allocator.text(" ").append(value).append(" "))
                        .unwrap_or_else(|| allocator.text(" "))
                })
                .append(code_block.into_pretty_doc(allocator, settings)),
            Statement::IfElse {
                first_branch,
                alternative_branches,
                else_branch,
            } => {
                let mut value = allocator
                    .text("if ")
                    .append(first_branch.0.into_pretty_doc(allocator, settings))
                    .append(" ")
                    .append(first_branch.1.into_pretty_doc(allocator, settings));
                for i in alternative_branches {
                    value = value
                        .append(" else if ")
                        .append(i.0.into_pretty_doc(allocator, settings))
                        .append(" ")
                        .append(i.1.into_pretty_doc(allocator, settings));
                }
                if let Some(else_branch) = else_branch {
                    value = value
                        .append(" else ")
                        .append(else_branch.into_pretty_doc(allocator, settings));
                }
                value
            }
            Statement::UseStatement(use_statement_content) => allocator
                .text("use ")
                .append(use_statement_content.into_pretty_doc(allocator, settings))
                .append(";"),
            Statement::UseExtensionStatement(use_statement_content) => allocator
                .text("use extension ")
                .append(use_statement_content.into_pretty_doc(allocator, settings))
                .append(";"),
        }
    }
}

impl UnparseASTFormatted for &UseStatementContent {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            UseStatementContent::SingleUse { identifier, rename } => {
                let value = identifier.into_pretty_doc(allocator, settings);
                if let Some(rename) = rename {
                    return value
                        .append(" as ")
                        .append(rename.into_pretty_doc(allocator, settings));
                }
                value
            }
            UseStatementContent::MultiUse { root, content } => root
                .into_pretty_doc(allocator, settings)
                .append("::{")
                .append({
                    if content.is_empty() {
                        allocator.text("}")
                    } else {
                        allocator
                            .line_()
                            .append(
                                allocator.intersperse(
                                    content
                                        .iter()
                                        .map(|value| value.into_pretty_doc(allocator, settings)),
                                    allocator.text(",").append(allocator.line()),
                                ),
                            )
                            .nest(settings.indentation)
                            .append(allocator.text(",").flat_alt(allocator.nil()))
                            .append(allocator.line_())
                            .group()
                            .append("}")
                    }
                }),
        }
    }
}

impl UnparseASTFormatted for &DataDeclaration {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        let mut value = allocator.text("let ");
        match self {
            DataDeclaration::Mixed {
                scope,
                declarations,
            } => {
                if scope == &DataDeclarationScope::Unset {
                    value = value.append("(");
                } else {
                    value = value
                        .append(scope.into_pretty_doc(allocator, settings))
                        .append(" (");
                }
                value = value.append(if declarations.is_empty() {
                    allocator.text(")")
                } else {
                    allocator
                        .line_()
                        .append(allocator.intersperse(
                            declarations.iter().map(|value| {
                                (value, SingleDataDeclarationDefaultKind::Variable)
                                    .into_pretty_doc(allocator, settings)
                            }),
                            allocator.text(",").append(allocator.line()),
                        ))
                        .nest(settings.indentation)
                        .append(allocator.text(",").flat_alt(allocator.nil()))
                        .append(allocator.line_())
                        .group()
                        .append(")")
                });
            }
            DataDeclaration::Vars {
                scope,
                declarations,
            } => {
                if scope == &DataDeclarationScope::Unset {
                    value = value.append("vars {");
                } else {
                    value = value
                        .append(scope.into_pretty_doc(allocator, settings))
                        .append(" vars {");
                }
                value = value.append(if declarations.is_empty() {
                    allocator.text("}")
                } else {
                    allocator
                        .line_()
                        .append(allocator.intersperse(
                            declarations.iter().map(|value| {
                                (value, SingleDataDeclarationDefaultKind::Variable)
                                    .into_pretty_doc(allocator, settings)
                            }),
                            allocator.text(",").append(allocator.line()),
                        ))
                        .nest(settings.indentation)
                        .append(allocator.text(",").flat_alt(allocator.nil()))
                        .append(allocator.line_())
                        .group()
                        .append("}")
                });
            }
            DataDeclaration::Lists {
                scope,
                declarations,
            } => {
                if scope == &DataDeclarationScope::Unset {
                    value = value.append("lists {");
                } else {
                    value = value
                        .append(scope.into_pretty_doc(allocator, settings))
                        .append(" list {");
                }
                value = value.append(if declarations.is_empty() {
                    allocator.text("}")
                } else {
                    allocator
                        .line_()
                        .append(allocator.intersperse(
                            declarations.iter().map(|value| {
                                (value, SingleDataDeclarationDefaultKind::List)
                                    .into_pretty_doc(allocator, settings)
                            }),
                            allocator.text(",").append(allocator.line()),
                        ))
                        .nest(settings.indentation)
                        .append(allocator.text(",").flat_alt(allocator.nil()))
                        .append(allocator.line_())
                        .group()
                        .append("}")
                });
            }
            DataDeclaration::Single(single_data_declaration) => {
                value = value.append(
                    (
                        single_data_declaration.as_ref(),
                        SingleDataDeclarationDefaultKind::Variable,
                    )
                        .into_pretty_doc(allocator, settings),
                );
            }
        }
        value
    }
}

impl UnparseASTFormatted for &DataDeclarationScope {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, _settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        allocator.text(match self {
            DataDeclarationScope::Global => "global",
            DataDeclarationScope::Local => "local",
            DataDeclarationScope::Cloud => "cloud",
            DataDeclarationScope::Unset => return allocator.nil(),
        })
    }
}

impl UnparseASTFormatted for &SingleDataDeclaration {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        (self, SingleDataDeclarationDefaultKind::Variable).into_pretty_doc(allocator, settings)
    }
}

impl UnparseASTFormatted for (&SingleDataDeclaration, SingleDataDeclarationDefaultKind) {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self.0 {
            SingleDataDeclaration::Variable {
                scope,
                canonical_identifier,
                identifier,
                value: decl_value,
            } => {
                let mut value = allocator.nil();
                if self.1 != SingleDataDeclarationDefaultKind::Variable {
                    value = value.append("var ");
                }
                if scope != &DataDeclarationScope::Unset {
                    value = value
                        .append(scope.into_pretty_doc(allocator, settings))
                        .append(" ");
                }
                if let Some(canonical_identifier) = canonical_identifier {
                    value = value
                        .append(canonical_identifier.into_pretty_doc(allocator, settings))
                        .append(" ");
                }
                value = value.append(identifier.into_pretty_doc(allocator, settings));
                if let Some(decl_value) = decl_value {
                    value = value
                        .append(" =")
                        .append(allocator.line())
                        .append(decl_value.into_pretty_doc(allocator, settings))
                        .nest(settings.indentation)
                        .group();
                }
                value
            }
            SingleDataDeclaration::List {
                scope,
                canonical_identifier,
                identifier,
                value: decl_value,
            } => {
                let mut value = allocator.nil();
                if self.1 != SingleDataDeclarationDefaultKind::List {
                    value = value.append("list ");
                }
                if scope != &DataDeclarationScope::Unset {
                    value = value
                        .append(scope.into_pretty_doc(allocator, settings))
                        .append(" ");
                }
                if let Some(canonical_identifier) = canonical_identifier {
                    value = value
                        .append(canonical_identifier.into_pretty_doc(allocator, settings))
                        .append(" ");
                }
                value = value.append(identifier.into_pretty_doc(allocator, settings));
                if !decl_value.is_empty() {
                    value = value
                        .append(" =")
                        .append(allocator.line())
                        .append(wrap_csv!(decl_value, BRACKETS, allocator, settings))
                        .nest(settings.indentation)
                        .group();
                }
                value
            }
            SingleDataDeclaration::FileList {
                scope,
                canonical_identifier,
                identifier,
                source,
            } => {
                let mut value = allocator.nil();
                if self.1 != SingleDataDeclarationDefaultKind::List {
                    value = value.append("list ");
                }
                if scope != &DataDeclarationScope::Unset {
                    value = value
                        .append(scope.into_pretty_doc(allocator, settings))
                        .append(" ");
                }
                if let Some(canonical_identifier) = canonical_identifier {
                    value = value
                        .append(canonical_identifier.into_pretty_doc(allocator, settings))
                        .append(" ");
                }
                value
                    .append(identifier.into_pretty_doc(allocator, settings))
                    .append(" =")
                    .append(allocator.line())
                    .append("file ")
                    .append(source.into_pretty_doc(allocator, settings))
                    .nest(settings.indentation)
                    .group()
            }
        }
    }
}

impl UnparseASTFormatted for &ListEntry {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            ListEntry::Expression(expression) => expression.into_pretty_doc(allocator, settings),
            ListEntry::Unwrap(literal) => allocator
                .text("..")
                .append(literal.into_pretty_doc(allocator, settings)),
        }
    }
}

impl_unparse_ast_for_code_block!(CodeBlock);
impl_unparse_ast_for_code_block!(SpriteCodeBlock);
impl_unparse_ast_for_code_block!(StageCodeBlock);

impl UnparseASTFormatted for &SpriteStatement {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            SpriteStatement::DataDeclaration(data_declaration) => data_declaration
                .into_pretty_doc(allocator, settings)
                .append(";"),
            SpriteStatement::CostumeDeclaration(asset_declaration) => allocator
                .text("costume ")
                .append(asset_declaration.into_pretty_doc(allocator, settings))
                .append(";"),
            SpriteStatement::SoundDeclaration(asset_declaration) => allocator
                .text("sound ")
                .append(asset_declaration.into_pretty_doc(allocator, settings))
                .append(";"),
            SpriteStatement::HatStatement {
                hat_function,
                arguments,
                code_block,
            } => hat_function
                .into_pretty_doc(allocator, settings)
                .append(
                    unparse_flexible_expression_list(arguments, allocator, settings)
                        .map(|value| allocator.text(" ").append(value).append(" "))
                        .unwrap_or_else(|| allocator.text(" ")),
                )
                .append(code_block.into_pretty_doc(allocator, settings)),
            SpriteStatement::CustomBlockDefinition {
                is_warp,
                canonical_identifier,
                identifier,
                parameters,
                code_block,
            } => {
                let mut value = is_warp
                    .into_pretty_doc(allocator, settings)
                    .append(" proc ");
                if let Some(canonical_identifier) = canonical_identifier {
                    value = value
                        .append(canonical_identifier.into_pretty_doc(allocator, settings))
                        .append(" ");
                }
                value
                    .append(identifier.into_pretty_doc(allocator, settings))
                    .append("(")
                    .append(if parameters.is_empty() {
                        allocator.nil()
                    } else {
                        allocator
                            .line_()
                            .append(allocator.intersperse(
                                parameters.iter().map(|i| {
                                    let mut value = allocator.nil();
                                    if let Some(param_kind) = &i.0 {
                                        value = value
                                            .append(param_kind.into_pretty_doc(allocator, settings))
                                            .append(" ");
                                    }
                                    if let Some(canonical_identifier) = &i.1 {
                                        value = value
                                            .append(
                                                canonical_identifier
                                                    .into_pretty_doc(allocator, settings),
                                            )
                                            .append(" ");
                                    }
                                    value.append(i.2.into_pretty_doc(allocator, settings))
                                }),
                                allocator.text(",").append(allocator.line()),
                            ))
                            .nest(settings.indentation)
                            .append(allocator.text(",").flat_alt(allocator.nil()))
                            .append(allocator.line_())
                            .group()
                    })
                    .append(") ")
                    .append(code_block.into_pretty_doc(allocator, settings))
            }
            SpriteStatement::IsolatedBlock(code_block) => {
                code_block.into_pretty_doc(allocator, settings)
            }
            SpriteStatement::IsolatedExpression(expression) => allocator
                .text("(")
                .append(
                    allocator
                        .line_()
                        .append(expression.into_pretty_doc(allocator, settings))
                        .nest(settings.indentation)
                        .append(allocator.line_())
                        .group(),
                )
                .append(")"),
            SpriteStatement::ConfigStatement(items) => allocator
                .text("config {")
                .append(if items.is_empty() {
                    allocator.nil()
                } else {
                    allocator
                        .line()
                        .append(
                            allocator.intersperse(
                                items
                                    .iter()
                                    .map(|value| value.into_pretty_doc(allocator, settings)),
                                allocator.text(",").append(allocator.line()),
                            ),
                        )
                        .nest(settings.indentation)
                        .append(allocator.text(",").flat_alt(allocator.nil()))
                        .append(allocator.line())
                        .group()
                })
                .append("}"),
            SpriteStatement::MonitorDeclaration {
                value,
                configuration,
            } => allocator
                .text("monitor ")
                .append(value.into_pretty_doc(allocator, settings))
                .append(" {")
                .append(if configuration.is_empty() {
                    allocator.nil()
                } else {
                    allocator
                        .line()
                        .append(
                            allocator.intersperse(
                                configuration
                                    .iter()
                                    .map(|value| value.into_pretty_doc(allocator, settings)),
                                allocator.text(",").append(allocator.line()),
                            ),
                        )
                        .nest(settings.indentation)
                        .append(allocator.text(",").flat_alt(allocator.nil()))
                        .append(allocator.line())
                        .group()
                })
                .append("}"),
            SpriteStatement::UseStatement(use_statement_content) => allocator
                .text("use ")
                .append(use_statement_content.into_pretty_doc(allocator, settings))
                .append(";"),
            SpriteStatement::UseExtensionStatement(use_statement_content) => allocator
                .text("use extension ")
                .append(use_statement_content.into_pretty_doc(allocator, settings))
                .append(";"),
        }
    }
}

impl UnparseASTFormatted for &StageStatement {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            StageStatement::DataDeclaration(data_declaration) => data_declaration
                .into_pretty_doc(allocator, settings)
                .append(";"),
            StageStatement::BackdropDeclaration(asset_declaration) => allocator
                .text("backdrop ")
                .append(asset_declaration.into_pretty_doc(allocator, settings))
                .append(";"),
            StageStatement::SoundDeclaration(asset_declaration) => allocator
                .text("sound ")
                .append(asset_declaration.into_pretty_doc(allocator, settings))
                .append(";"),
            StageStatement::HatStatement {
                hat_function,
                arguments,
                code_block,
            } => hat_function
                .into_pretty_doc(allocator, settings)
                .append(
                    unparse_flexible_expression_list(arguments, allocator, settings)
                        .map(|value| allocator.text(" ").append(value).append(" "))
                        .unwrap_or_else(|| allocator.text(" ")),
                )
                .append(code_block.into_pretty_doc(allocator, settings)),
            StageStatement::CustomBlockDefinition {
                is_warp,
                canonical_identifier,
                identifier,
                parameters,
                code_block,
            } => {
                let mut value = is_warp
                    .into_pretty_doc(allocator, settings)
                    .append(" proc ");
                if let Some(canonical_identifier) = canonical_identifier {
                    value = value
                        .append(canonical_identifier.into_pretty_doc(allocator, settings))
                        .append(" ");
                }
                value
                    .append(identifier.into_pretty_doc(allocator, settings))
                    .append("(")
                    .append(if parameters.is_empty() {
                        allocator.nil()
                    } else {
                        allocator
                            .line_()
                            .append(allocator.intersperse(
                                parameters.iter().map(|i| {
                                    let mut value = allocator.nil();
                                    if let Some(param_kind) = &i.0 {
                                        value = value
                                            .append(param_kind.into_pretty_doc(allocator, settings))
                                            .append(" ");
                                    }
                                    if let Some(canonical_identifier) = &i.1 {
                                        value = value
                                            .append(
                                                canonical_identifier
                                                    .into_pretty_doc(allocator, settings),
                                            )
                                            .append(" ");
                                    }
                                    value.append(i.2.into_pretty_doc(allocator, settings))
                                }),
                                allocator.text(",").append(allocator.line()),
                            ))
                            .nest(settings.indentation)
                            .append(allocator.text(",").flat_alt(allocator.nil()))
                            .append(allocator.line_())
                            .group()
                    })
                    .append(") ")
                    .append(code_block.into_pretty_doc(allocator, settings))
            }
            StageStatement::IsolatedBlock(code_block) => {
                code_block.into_pretty_doc(allocator, settings)
            }
            StageStatement::IsolatedExpression(expression) => allocator
                .text("(")
                .append(
                    allocator
                        .line_()
                        .append(expression.into_pretty_doc(allocator, settings))
                        .nest(settings.indentation)
                        .append(allocator.line_())
                        .group(),
                )
                .append(")"),
            StageStatement::ConfigStatement(items) => allocator
                .text("config {")
                .append(if items.is_empty() {
                    allocator.nil()
                } else {
                    allocator
                        .line()
                        .append(
                            allocator.intersperse(
                                items
                                    .iter()
                                    .map(|value| value.into_pretty_doc(allocator, settings)),
                                allocator.text(",").append(allocator.line()),
                            ),
                        )
                        .nest(settings.indentation)
                        .append(allocator.text(",").flat_alt(allocator.nil()))
                        .append(allocator.line())
                        .group()
                })
                .append("}"),
            StageStatement::MonitorDeclaration {
                value,
                configuration,
            } => allocator
                .text("monitor ")
                .append(value.into_pretty_doc(allocator, settings))
                .append(" {")
                .append(if configuration.is_empty() {
                    allocator.nil()
                } else {
                    allocator
                        .line()
                        .append(
                            allocator.intersperse(
                                configuration
                                    .iter()
                                    .map(|value| value.into_pretty_doc(allocator, settings)),
                                allocator.text(",").append(allocator.line()),
                            ),
                        )
                        .nest(settings.indentation)
                        .append(allocator.text(",").flat_alt(allocator.nil()))
                        .append(allocator.line())
                        .group()
                })
                .append("}"),
            StageStatement::UseStatement(use_statement_content) => allocator
                .text("use ")
                .append(use_statement_content.into_pretty_doc(allocator, settings))
                .append(";"),
            StageStatement::UseExtensionStatement(use_statement_content) => allocator
                .text("use extension ")
                .append(use_statement_content.into_pretty_doc(allocator, settings))
                .append(";"),
        }
    }
}

impl UnparseASTFormatted for &AssetDeclaration {
    fn into_pretty_doc<'a, D>(
        self,
        allocator: &'a D,
        settings: &UnparseASTFormattedSettings,
    ) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            AssetDeclaration::Multiple(single_asset_declarations) => {
                wrap_csv!(single_asset_declarations, PARENS, allocator, settings)
            }
            AssetDeclaration::Single(single_asset_declaration) => {
                single_asset_declaration.into_pretty_doc(allocator, settings)
            }
        }
    }
}

impl SingleAssetDeclarationValue {
    fn requires_whitespace_prefix(&self) -> bool {
        matches!(self, SingleAssetDeclarationValue::Dictionary(..))
    }
}

impl UnparseASTFormatted for &SingleAssetDeclaration {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        self.canonical_identifier
            .as_ref()
            .map(|value| value.into_pretty_doc(allocator, settings).append(" "))
            .unwrap_or_else(|| allocator.nil())
            .append(self.identifier.into_pretty_doc(allocator, settings))
            .append(if self.value.requires_whitespace_prefix() {
                allocator.text(" ")
            } else {
                allocator.nil()
            })
            .append(self.value.into_pretty_doc(allocator, settings))
    }
}

impl UnparseASTFormatted for &SingleAssetDeclarationValue {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            SingleAssetDeclarationValue::Simple(value) => {
                allocator.text(format!("(\"{}\")", normal_string_escaper(value)))
            }
            SingleAssetDeclarationValue::Dictionary(items) => {
                wrap_csv!(items, BRACES, allocator, settings)
            }
        }
    }
}

impl UnparseASTFormatted for &DictionaryEntry {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        self.identifier
            .into_pretty_doc(allocator, settings)
            .append(": ")
            .append(self.value.into_pretty_doc(allocator, settings))
    }
}

impl UnparseASTFormatted for &DictionaryValue {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            DictionaryValue::Primitive(value) => value.into_pretty_doc(allocator, settings),
            DictionaryValue::Dictionary(items) => {
                wrap_csv!(items, BRACES, allocator, settings)
            }
            DictionaryValue::List(items) => {
                wrap_csv!(items, BRACKETS, allocator, settings)
            }
        }
    }
}

impl UnparseASTFormatted for WarpSpecifier {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, _settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        allocator.text(self.as_str())
    }
}

impl UnparseASTFormatted for CustomBlockParamKind {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, _settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        allocator.text(self.as_str())
    }
}

impl UnparseASTFormatted for &MonitorValue {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            MonitorValue::Identifier(identifier) => identifier.into_pretty_doc(allocator, settings),
            MonitorValue::Call {
                function,
                arguments,
            } => function
                .into_pretty_doc(allocator, settings)
                .append(wrap_csv!(arguments, PARENS, allocator, settings)),
        }
    }
}

impl UnparseASTFormatted for &TopLevelStatement {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        match self {
            TopLevelStatement::Stage { code_block } => allocator
                .text("stage ")
                .append(code_block.into_pretty_doc(allocator, settings)),
            TopLevelStatement::Sprite {
                canonical_identifier,
                identifier,
                code_block,
            } => allocator
                .text("sprite ")
                .append(
                    canonical_identifier
                        .as_ref()
                        .map(|value| value.into_pretty_doc(allocator, settings).append(" "))
                        .unwrap_or_else(|| allocator.nil()),
                )
                .append(identifier.into_pretty_doc(allocator, settings))
                .append(" ")
                .append(code_block.into_pretty_doc(allocator, settings)),
            TopLevelStatement::BroadcastDeclaration {
                canonical_identifier,
                identifier,
            } => allocator
                .text("broadcast ")
                .append(
                    canonical_identifier
                        .as_ref()
                        .map(|value| value.into_pretty_doc(allocator, settings).append(" "))
                        .unwrap_or_else(|| allocator.nil()),
                )
                .append(identifier.into_pretty_doc(allocator, settings))
                .append(";"),
            TopLevelStatement::UseStatement(use_statement_content) => allocator
                .text("use ")
                .append(use_statement_content.into_pretty_doc(allocator, settings))
                .append(";"),
            TopLevelStatement::UseExtensionStatement(use_statement_content) => allocator
                .text("use extension ")
                .append(use_statement_content.into_pretty_doc(allocator, settings))
                .append(";"),
        }
    }
}

impl UnparseASTFormatted for &GrazeProgram {
    fn into_pretty_doc<'a, D>(self, allocator: &'a D, settings: &Settings) -> DocBuilder<'a, D, ()>
    where
        Self: 'a,
        D: DocAllocator<'a, ()>,
        D::Doc: Clone,
    {
        allocator
            .intersperse(
                self.0
                    .iter()
                    .map(|value| value.into_pretty_doc(allocator, settings)),
                allocator.hardline(),
            )
            .append(allocator.hardline())
    }
}

pub fn test(a: &[&str]) {
    let arena = Arena::<()>::new();
    arena
        .text("{")
        .append(
            arena
                .line_()
                .append(arena.intersperse(
                    a.iter().map(|value| arena.text(*value)),
                    arena.text(",").append(arena.line()),
                ))
                .nest(4),
        )
        .append(arena.line_())
        .append("}")
        .group()
        .1
        .render(10, &mut std::io::stdout())
        .unwrap()
}
