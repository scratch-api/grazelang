use std::convert::Infallible;

use super::types as ast_types;
use crate::parser::cst as cst_types;

pub trait CSTToAST: Sized {
    type AST: Sized;
    type Error: Sized;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error>;
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error>;
}

macro_rules! cst_to_ast_with_cheap_clone_cst_to_ast {
    ($cst_ty:ty, $ast_ty:ty, $this:ident => $clone:expr) => {
        impl CSTToAST for $cst_ty {
            type AST = $ast_ty;
            type Error = Infallible;
            fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
                let $this = self;
                Ok($clone)
            }
            fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
                self.clone_cst_to_ast()
            }
        }
    };
}

cst_to_ast_with_cheap_clone_cst_to_ast!(cst_types::BinOp, ast_types::BinOp, this => {
    match this {
        cst_types::BinOp::Plus(_) => ast_types::BinOp::Plus,
        cst_types::BinOp::Minus(_) => ast_types::BinOp::Minus,
        cst_types::BinOp::Times(_) => ast_types::BinOp::Times,
        cst_types::BinOp::Div(_) => ast_types::BinOp::Div,
        cst_types::BinOp::Mod(_) => ast_types::BinOp::Mod,
        cst_types::BinOp::Join(_) => ast_types::BinOp::Join,
        cst_types::BinOp::Contains(_) => ast_types::BinOp::Contains,
        cst_types::BinOp::And(_) => ast_types::BinOp::And,
        cst_types::BinOp::Or(_) => ast_types::BinOp::Or,
        cst_types::BinOp::Equals(_) => ast_types::BinOp::Equals,
        cst_types::BinOp::NotEquals(_) => ast_types::BinOp::NotEquals,
        cst_types::BinOp::LessThan(_) => ast_types::BinOp::LessThan,
        cst_types::BinOp::GreaterThan(_) => ast_types::BinOp::GreaterThan,
        cst_types::BinOp::LessThanOrEqual(_) => ast_types::BinOp::LessThanOrEqual,
        cst_types::BinOp::GreaterThanOrEqual(_) => ast_types::BinOp::GreaterThanOrEqual,
    }
});
cst_to_ast_with_cheap_clone_cst_to_ast!(cst_types::UnOp, ast_types::UnOp, this => {
    match this {
        cst_types::UnOp::Minus(_) => ast_types::UnOp::Minus,
        cst_types::UnOp::Not(_) => ast_types::UnOp::Not,
        cst_types::UnOp::Exp(_) => ast_types::UnOp::Exp,
        cst_types::UnOp::Pow(_) => ast_types::UnOp::Pow,
    }
});

impl CSTToAST for cst_types::Literal {
    type AST = ast_types::Literal;
    type Error = Infallible;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::Literal::String(value, _) => ast_types::Literal::String(value.clone()),
            cst_types::Literal::DecimalInt(value, _) => {
                ast_types::Literal::DecimalInt(value.clone())
            }
            cst_types::Literal::DecimalFloat(value, _) => {
                ast_types::Literal::DecimalFloat(value.clone())
            }
            cst_types::Literal::HexadecimalInt(value, _) => {
                ast_types::Literal::HexadecimalInt(value.clone())
            }
            cst_types::Literal::OctalInt(value, _) => ast_types::Literal::OctalInt(value.clone()),
            cst_types::Literal::BinaryInt(value, _) => ast_types::Literal::BinaryInt(value.clone()),
            cst_types::Literal::Bool(value, _) => ast_types::Literal::Bool(*value),
            cst_types::Literal::EmptyExpression(_, _, _) => ast_types::Literal::EmptyExpression,
        })
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::Literal::String(value, _) => ast_types::Literal::String(value),
            cst_types::Literal::DecimalInt(value, _) => ast_types::Literal::DecimalInt(value),
            cst_types::Literal::DecimalFloat(value, _) => ast_types::Literal::DecimalFloat(value),
            cst_types::Literal::HexadecimalInt(value, _) => {
                ast_types::Literal::HexadecimalInt(value)
            }
            cst_types::Literal::OctalInt(value, _) => ast_types::Literal::OctalInt(value),
            cst_types::Literal::BinaryInt(value, _) => ast_types::Literal::BinaryInt(value),
            cst_types::Literal::Bool(value, _) => ast_types::Literal::Bool(value),
            cst_types::Literal::EmptyExpression(_, _, _) => ast_types::Literal::EmptyExpression,
        })
    }
}

impl CSTToAST for cst_types::SingleIdentifier {
    type AST = ast_types::SingleIdentifier;
    type Error = Infallible;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(ast_types::SingleIdentifier {
            value: self.value.clone(),
        })
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(ast_types::SingleIdentifier { value: self.value })
    }
}

impl CSTToAST for cst_types::Identifier {
    type AST = ast_types::Identifier;
    type Error = Infallible;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        let mut path = Vec::with_capacity(1 + self.path.len() + self.fields.len());
        path.push(self.root.clone_cst_to_ast()?);
        for i in &self.path {
            path.push(i.1.clone_cst_to_ast()?);
        }
        for i in &self.fields {
            path.push(i.1.clone_cst_to_ast()?);
        }
        Ok(ast_types::Identifier { path })
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        let mut path = Vec::with_capacity(1 + self.path.len() + self.fields.len());
        path.push(self.root.cst_to_ast()?);
        for i in self.path {
            path.push(i.1.cst_to_ast()?);
        }
        for i in self.fields {
            path.push(i.1.cst_to_ast()?);
        }
        Ok(ast_types::Identifier { path })
    }
}

impl CSTToAST for cst_types::CanonicalIdentifier {
    type AST = ast_types::CanonicalIdentifier;
    type Error = Infallible;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(ast_types::CanonicalIdentifier {
            name: self.name.clone(),
        })
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(ast_types::CanonicalIdentifier { name: self.name })
    }
}

impl CSTToAST for cst_types::FormattedStringContent {
    type AST = ast_types::FormattedStringContent;
    type Error = Infallible;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::FormattedStringContent::Expression(expression) => {
                ast_types::FormattedStringContent::Expression(Box::new(
                    expression.clone_cst_to_ast()?,
                ))
            }
            cst_types::FormattedStringContent::String(value, _) => {
                ast_types::FormattedStringContent::String(value.clone())
            }
        })
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::FormattedStringContent::Expression(expression) => {
                ast_types::FormattedStringContent::Expression(Box::new(expression.cst_to_ast()?))
            }
            cst_types::FormattedStringContent::String(value, _) => {
                ast_types::FormattedStringContent::String(value)
            }
        })
    }
}

impl CSTToAST for cst_types::Expression {
    type AST = ast_types::Expression;
    type Error = Infallible;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::Expression::Literal(value) => {
                ast_types::Expression::Literal(value.clone_cst_to_ast()?)
            }
            cst_types::Expression::FormattedString(value, _) => {
                ast_types::Expression::FormattedString(
                    value
                        .iter()
                        .map(|value| value.clone_cst_to_ast())
                        .collect::<Result<_, _>>()?,
                )
            }
            cst_types::Expression::BinOp(left_operand, bin_op, right_operand, _) => {
                ast_types::Expression::BinOp {
                    operator: bin_op.clone_cst_to_ast()?,
                    left_operand: Box::new(left_operand.clone_cst_to_ast()?),
                    right_operand: Box::new(right_operand.clone_cst_to_ast()?),
                }
            }
            cst_types::Expression::UnOp(un_op, operand, _) => ast_types::Expression::UnOp {
                operator: un_op.clone_cst_to_ast()?,
                operand: Box::new(operand.clone_cst_to_ast()?),
            },
            cst_types::Expression::Identifier(identifier) => {
                ast_types::Expression::Identifier(identifier.clone_cst_to_ast()?)
            }
            cst_types::Expression::Call(function, _, arguments, _, _) => {
                ast_types::Expression::Call {
                    function: function.clone_cst_to_ast()?,
                    arguments: arguments
                        .iter()
                        .map(|value| value.clone_cst_to_ast())
                        .collect::<Result<_, _>>()?,
                }
            }
            cst_types::Expression::GetItem(list, _, item, _, _) => ast_types::Expression::GetItem {
                list: list.clone_cst_to_ast()?,
                item: Box::new(item.clone_cst_to_ast()?),
            },
            cst_types::Expression::GetLetter(expression, _, letter, _, _) => {
                ast_types::Expression::GetLetter {
                    expression: Box::new(expression.clone_cst_to_ast()?),
                    letter: Box::new(letter.clone_cst_to_ast()?),
                }
            }
            cst_types::Expression::Parentheses(_, expression, _, _) => {
                expression.clone_cst_to_ast()?
            }
        })
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::Expression::Literal(value) => {
                ast_types::Expression::Literal(value.cst_to_ast()?)
            }
            cst_types::Expression::FormattedString(value, _) => {
                ast_types::Expression::FormattedString(
                    value
                        .into_iter()
                        .map(|value| value.cst_to_ast())
                        .collect::<Result<_, _>>()?,
                )
            }
            cst_types::Expression::BinOp(left_operand, bin_op, right_operand, _) => {
                ast_types::Expression::BinOp {
                    operator: bin_op.cst_to_ast()?,
                    left_operand: Box::new(left_operand.cst_to_ast()?),
                    right_operand: Box::new(right_operand.cst_to_ast()?),
                }
            }
            cst_types::Expression::UnOp(un_op, operand, _) => ast_types::Expression::UnOp {
                operator: un_op.cst_to_ast()?,
                operand: Box::new(operand.cst_to_ast()?),
            },
            cst_types::Expression::Identifier(identifier) => {
                ast_types::Expression::Identifier(identifier.cst_to_ast()?)
            }
            cst_types::Expression::Call(function, _, arguments, _, _) => {
                ast_types::Expression::Call {
                    function: function.cst_to_ast()?,
                    arguments: arguments
                        .into_iter()
                        .map(|value| value.cst_to_ast())
                        .collect::<Result<_, _>>()?,
                }
            }
            cst_types::Expression::GetItem(list, _, item, _, _) => ast_types::Expression::GetItem {
                list: list.cst_to_ast()?,
                item: Box::new(item.cst_to_ast()?),
            },
            cst_types::Expression::GetLetter(expression, _, letter, _, _) => {
                ast_types::Expression::GetLetter {
                    expression: Box::new(expression.cst_to_ast()?),
                    letter: Box::new(letter.cst_to_ast()?),
                }
            }
            cst_types::Expression::Parentheses(_, expression, _, _) => expression.cst_to_ast()?,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct InvalidCST;

impl From<Infallible> for InvalidCST {
    fn from(value: Infallible) -> Self {
        match value {}
    }
}

impl CSTToAST for cst_types::DictionaryValue {
    type AST = ast_types::DictionaryValue;
    type Error = InvalidCST;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::DictionaryValue::Primitive(value) => {
                ast_types::DictionaryValue::Primitive(value.clone_cst_to_ast()?)
            }
            cst_types::DictionaryValue::Dictionary(_, value, _, _) => {
                ast_types::DictionaryValue::Dictionary(
                    value
                        .iter()
                        .map(|value| value.clone_cst_to_ast())
                        .collect::<Result<_, _>>()?,
                )
            }
            cst_types::DictionaryValue::List(_, value, _, _) => ast_types::DictionaryValue::List(
                value
                    .iter()
                    .map(|value| value.clone_cst_to_ast())
                    .collect::<Result<_, _>>()?,
            ),
            cst_types::DictionaryValue::Invalid(_) => return Err(InvalidCST),
        })
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::DictionaryValue::Primitive(value) => {
                ast_types::DictionaryValue::Primitive(value.cst_to_ast()?)
            }
            cst_types::DictionaryValue::Dictionary(_, value, _, _) => {
                ast_types::DictionaryValue::Dictionary(
                    value
                        .into_iter()
                        .map(|value| value.cst_to_ast())
                        .collect::<Result<_, _>>()?,
                )
            }
            cst_types::DictionaryValue::List(_, value, _, _) => ast_types::DictionaryValue::List(
                value
                    .into_iter()
                    .map(|value| value.cst_to_ast())
                    .collect::<Result<_, _>>()?,
            ),
            cst_types::DictionaryValue::Invalid(_) => return Err(InvalidCST),
        })
    }
}

impl CSTToAST for cst_types::DictionaryEntry {
    type AST = ast_types::DictionaryEntry;
    type Error = InvalidCST;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::DictionaryEntry::Valid(key, _, value, _) => ast_types::DictionaryEntry {
                identifier: key.clone_cst_to_ast()?,
                value: value.clone_cst_to_ast()?,
            },
            cst_types::DictionaryEntry::Invalid(_) => return Err(InvalidCST),
        })
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::DictionaryEntry::Valid(key, _, value, _) => ast_types::DictionaryEntry {
                identifier: key.cst_to_ast()?,
                value: value.cst_to_ast()?,
            },
            cst_types::DictionaryEntry::Invalid(_) => return Err(InvalidCST),
        })
    }
}

cst_to_ast_with_cheap_clone_cst_to_ast!(cst_types::DataDeclarationScope, ast_types::DataDeclarationScope, this => {
    match this {
        cst_types::DataDeclarationScope::Unset => ast_types::DataDeclarationScope::Unset,
        cst_types::DataDeclarationScope::Global(_) => ast_types::DataDeclarationScope::Global,
        cst_types::DataDeclarationScope::Local(_) => ast_types::DataDeclarationScope::Local,
        cst_types::DataDeclarationScope::Cloud(_) => ast_types::DataDeclarationScope::Cloud,
    }
});

impl CSTToAST for cst_types::ListEntry {
    type AST = ast_types::ListEntry;
    type Error = Infallible;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::ListEntry::Expression(value) => {
                ast_types::ListEntry::Expression(value.clone_cst_to_ast()?)
            }
            cst_types::ListEntry::Unwrap(value, _) => {
                ast_types::ListEntry::Unwrap(value.clone_cst_to_ast()?)
            }
        })
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::ListEntry::Expression(value) => {
                ast_types::ListEntry::Expression(value.cst_to_ast()?)
            }
            cst_types::ListEntry::Unwrap(value, _) => {
                ast_types::ListEntry::Unwrap(value.cst_to_ast()?)
            }
        })
    }
}

impl CSTToAST for cst_types::MonitorValue {
    type AST = ast_types::MonitorValue;
    type Error = Infallible;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::MonitorValue::Identifier(value) => {
                ast_types::MonitorValue::Identifier(value.clone_cst_to_ast()?)
            }
            cst_types::MonitorValue::Call(function, _, value, _, _) => {
                ast_types::MonitorValue::Call {
                    function: function.clone_cst_to_ast()?,
                    arguments: value
                        .iter()
                        .map(|value| value.clone_cst_to_ast())
                        .collect::<Result<_, _>>()?,
                }
            }
        })
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::MonitorValue::Identifier(value) => {
                ast_types::MonitorValue::Identifier(value.cst_to_ast()?)
            }
            cst_types::MonitorValue::Call(function, _, value, _, _) => {
                ast_types::MonitorValue::Call {
                    function: function.cst_to_ast()?,
                    arguments: value
                        .into_iter()
                        .map(|value| value.cst_to_ast())
                        .collect::<Result<_, _>>()?,
                }
            }
        })
    }
}

impl CSTToAST for cst_types::SingleDataDeclaration {
    type AST = ast_types::SingleDataDeclaration;
    type Error = Infallible;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::SingleDataDeclaration::Variable(
                _,
                scope,
                canonical_identifier,
                identifier,
                _,
                value,
                _,
            ) => ast_types::SingleDataDeclaration::Variable {
                scope: scope.clone_cst_to_ast()?,
                canonical_identifier: canonical_identifier
                    .as_ref()
                    .map(|value| value.clone_cst_to_ast())
                    .transpose()?,
                identifier: identifier.clone_cst_to_ast()?,
                value: Some(value.clone_cst_to_ast()?),
            },
            cst_types::SingleDataDeclaration::EmptyVariable(
                _,
                scope,
                canonical_identifier,
                identifier,
                _,
            ) => ast_types::SingleDataDeclaration::Variable {
                scope: scope.clone_cst_to_ast()?,
                canonical_identifier: canonical_identifier
                    .as_ref()
                    .map(|value| value.clone_cst_to_ast())
                    .transpose()?,
                identifier: identifier.clone_cst_to_ast()?,
                value: None,
            },
            cst_types::SingleDataDeclaration::List(
                _,
                scope,
                canonical_identifier,
                identifier,
                _,
                _,
                value,
                _,
                _,
            ) => ast_types::SingleDataDeclaration::List {
                scope: scope.clone_cst_to_ast()?,
                canonical_identifier: canonical_identifier
                    .as_ref()
                    .map(|value| value.clone_cst_to_ast())
                    .transpose()?,
                identifier: identifier.clone_cst_to_ast()?,
                value: value
                    .iter()
                    .map(|value| value.clone_cst_to_ast())
                    .collect::<Result<_, _>>()?,
            },
            cst_types::SingleDataDeclaration::FileList(
                _,
                scope,
                canonical_identifier,
                identifier,
                _,
                _,
                value,
                _,
            ) => ast_types::SingleDataDeclaration::FileList {
                scope: scope.clone_cst_to_ast()?,
                canonical_identifier: canonical_identifier
                    .as_ref()
                    .map(|value| value.clone_cst_to_ast())
                    .transpose()?,
                identifier: identifier.clone_cst_to_ast()?,
                source: value.clone_cst_to_ast()?,
            },
            cst_types::SingleDataDeclaration::EmptyList(
                _,
                scope,
                canonical_identifier,
                identifier,
                _,
            ) => ast_types::SingleDataDeclaration::List {
                scope: scope.clone_cst_to_ast()?,
                canonical_identifier: canonical_identifier
                    .as_ref()
                    .map(|value| value.clone_cst_to_ast())
                    .transpose()?,
                identifier: identifier.clone_cst_to_ast()?,
                value: Vec::new(),
            },
        })
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::SingleDataDeclaration::Variable(
                _,
                scope,
                canonical_identifier,
                identifier,
                _,
                value,
                _,
            ) => ast_types::SingleDataDeclaration::Variable {
                scope: scope.cst_to_ast()?,
                canonical_identifier: canonical_identifier
                    .map(|value| value.cst_to_ast())
                    .transpose()?,
                identifier: identifier.cst_to_ast()?,
                value: Some(value.cst_to_ast()?),
            },
            cst_types::SingleDataDeclaration::EmptyVariable(
                _,
                scope,
                canonical_identifier,
                identifier,
                _,
            ) => ast_types::SingleDataDeclaration::Variable {
                scope: scope.cst_to_ast()?,
                canonical_identifier: canonical_identifier
                    .map(|value| value.cst_to_ast())
                    .transpose()?,
                identifier: identifier.cst_to_ast()?,
                value: None,
            },
            cst_types::SingleDataDeclaration::List(
                _,
                scope,
                canonical_identifier,
                identifier,
                _,
                _,
                value,
                _,
                _,
            ) => ast_types::SingleDataDeclaration::List {
                scope: scope.cst_to_ast()?,
                canonical_identifier: canonical_identifier
                    .map(|value| value.cst_to_ast())
                    .transpose()?,
                identifier: identifier.cst_to_ast()?,
                value: value
                    .into_iter()
                    .map(|value| value.cst_to_ast())
                    .collect::<Result<_, _>>()?,
            },
            cst_types::SingleDataDeclaration::FileList(
                _,
                scope,
                canonical_identifier,
                identifier,
                _,
                _,
                value,
                _,
            ) => ast_types::SingleDataDeclaration::FileList {
                scope: scope.cst_to_ast()?,
                canonical_identifier: canonical_identifier
                    .map(|value| value.cst_to_ast())
                    .transpose()?,
                identifier: identifier.cst_to_ast()?,
                source: value.cst_to_ast()?,
            },
            cst_types::SingleDataDeclaration::EmptyList(
                _,
                scope,
                canonical_identifier,
                identifier,
                _,
            ) => ast_types::SingleDataDeclaration::List {
                scope: scope.cst_to_ast()?,
                canonical_identifier: canonical_identifier
                    .map(|value| value.cst_to_ast())
                    .transpose()?,
                identifier: identifier.cst_to_ast()?,
                value: Vec::new(),
            },
        })
    }
}

impl CSTToAST for cst_types::DataDeclaration {
    type AST = ast_types::DataDeclaration;
    type Error = Infallible;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::DataDeclaration::Mixed(scope, _, value, _, _) => {
                ast_types::DataDeclaration::Mixed {
                    scope: scope.clone_cst_to_ast()?,
                    declarations: value
                        .iter()
                        .map(|value| value.clone_cst_to_ast())
                        .collect::<Result<_, _>>()?,
                }
            }
            cst_types::DataDeclaration::Vars(scope, _, _, value, _, _) => {
                ast_types::DataDeclaration::Vars {
                    scope: scope.clone_cst_to_ast()?,
                    declarations: value
                        .iter()
                        .map(|value| value.clone_cst_to_ast())
                        .collect::<Result<_, _>>()?,
                }
            }
            cst_types::DataDeclaration::Lists(scope, _, _, value, _, _) => {
                ast_types::DataDeclaration::Lists {
                    scope: scope.clone_cst_to_ast()?,
                    declarations: value
                        .iter()
                        .map(|value| value.clone_cst_to_ast())
                        .collect::<Result<_, _>>()?,
                }
            }
            cst_types::DataDeclaration::Single(value) => {
                ast_types::DataDeclaration::Single(Box::new(value.clone_cst_to_ast()?))
            }
        })
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::DataDeclaration::Mixed(scope, _, value, _, _) => {
                ast_types::DataDeclaration::Mixed {
                    scope: scope.cst_to_ast()?,
                    declarations: value
                        .into_iter()
                        .map(|value| value.cst_to_ast())
                        .collect::<Result<_, _>>()?,
                }
            }
            cst_types::DataDeclaration::Vars(scope, _, _, value, _, _) => {
                ast_types::DataDeclaration::Vars {
                    scope: scope.cst_to_ast()?,
                    declarations: value
                        .into_iter()
                        .map(|value| value.cst_to_ast())
                        .collect::<Result<_, _>>()?,
                }
            }
            cst_types::DataDeclaration::Lists(scope, _, _, value, _, _) => {
                ast_types::DataDeclaration::Lists {
                    scope: scope.cst_to_ast()?,
                    declarations: value
                        .into_iter()
                        .map(|value| value.cst_to_ast())
                        .collect::<Result<_, _>>()?,
                }
            }
            cst_types::DataDeclaration::Single(value) => {
                ast_types::DataDeclaration::Single(Box::new(value.cst_to_ast()?))
            }
        })
    }
}

impl CSTToAST for cst_types::UseStatementContent {
    type AST = ast_types::UseStatementContent;
    type Error = InvalidCST;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::UseStatementContent::SingleUse {
                identifier,
                rename,
                source_span: _,
            } => ast_types::UseStatementContent::SingleUse {
                identifier: identifier.clone_cst_to_ast()?,
                rename: rename
                    .as_ref()
                    .map(|value| value.1.clone_cst_to_ast())
                    .transpose()?,
            },
            cst_types::UseStatementContent::MultiUse {
                root,
                double_colon_or_dot: _,
                left_brace: _,
                content,
                right_brace: _,
                source_span: _,
            } => ast_types::UseStatementContent::MultiUse {
                root: root.clone_cst_to_ast()?,
                content: content
                    .iter()
                    .map(|value| value.clone_cst_to_ast())
                    .collect::<Result<_, _>>()?,
            },
            cst_types::UseStatementContent::Invalid(_) => return Err(InvalidCST),
        })
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::UseStatementContent::SingleUse {
                identifier,
                rename,
                source_span: _,
            } => ast_types::UseStatementContent::SingleUse {
                identifier: identifier.cst_to_ast()?,
                rename: rename.map(|value| value.1.cst_to_ast()).transpose()?,
            },
            cst_types::UseStatementContent::MultiUse {
                root,
                double_colon_or_dot: _,
                left_brace: _,
                content,
                right_brace: _,
                source_span: _,
            } => ast_types::UseStatementContent::MultiUse {
                root: root.cst_to_ast()?,
                content: content
                    .into_iter()
                    .map(|value| value.cst_to_ast())
                    .collect::<Result<_, _>>()?,
            },
            cst_types::UseStatementContent::Invalid(_) => return Err(InvalidCST),
        })
    }
}

impl CSTToAST for cst_types::Statement {
    type AST = Option<ast_types::Statement>;
    type Error = InvalidCST;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(Some(match self {
            cst_types::Statement::DataDeclaration(_, value, _, _) => {
                ast_types::Statement::DataDeclaration(value.clone_cst_to_ast()?)
            }
            cst_types::Statement::Assignment(identifier, _, value, _, _) => {
                ast_types::Statement::Assignment {
                    target: identifier.clone_cst_to_ast()?,
                    value: value.clone_cst_to_ast()?,
                }
            }
            cst_types::Statement::ListAssignment(identifier, _, _, value, _, _, _) => {
                ast_types::Statement::ListAssignment {
                    target: identifier.clone_cst_to_ast()?,
                    value: value
                        .iter()
                        .map(|value| value.clone_cst_to_ast())
                        .collect::<Result<_, _>>()?,
                }
            }
            cst_types::Statement::SetItem(identifier, _, key, _, _, value, _, _) => {
                ast_types::Statement::SetItem {
                    list: identifier.clone_cst_to_ast()?,
                    item: key.clone_cst_to_ast()?,
                    value: value.clone_cst_to_ast()?,
                }
            }
            cst_types::Statement::Call(identifier, _, value, _, _, _) => {
                ast_types::Statement::Call {
                    function: identifier.clone_cst_to_ast()?,
                    arguments: value
                        .iter()
                        .map(|value| value.clone_cst_to_ast())
                        .collect::<Result<_, _>>()?,
                }
            }
            cst_types::Statement::SingleInputControl(identifier, expression, code_block, _, _) => {
                ast_types::Statement::Control {
                    control_function: identifier.clone_cst_to_ast()?,
                    arguments: vec![expression.clone_cst_to_ast()?],
                    code_block: code_block.clone_cst_to_ast()?,
                }
            }
            cst_types::Statement::MultiInputControl(identifier, _, value, _, code_block, _, _) => {
                ast_types::Statement::Control {
                    control_function: identifier.clone_cst_to_ast()?,
                    arguments: value
                        .iter()
                        .map(|value| value.clone_cst_to_ast())
                        .collect::<Result<_, _>>()?,
                    code_block: code_block.clone_cst_to_ast()?,
                }
            }
            cst_types::Statement::Forever(identifier, code_block, _, _) => {
                ast_types::Statement::Control {
                    control_function: identifier.clone_cst_to_ast()?,
                    arguments: Vec::new(),
                    code_block: code_block.clone_cst_to_ast()?,
                }
            }
            cst_types::Statement::IfElse(first_branch, alternative_branches, else_branch, _, _) => {
                ast_types::Statement::IfElse {
                    first_branch: (
                        first_branch.1.clone_cst_to_ast()?,
                        first_branch.2.clone_cst_to_ast()?,
                    ),
                    alternative_branches: alternative_branches
                        .iter()
                        .map(|value| Ok((value.2.clone_cst_to_ast()?, value.3.clone_cst_to_ast()?)))
                        .collect::<Result<_, InvalidCST>>()?,
                    else_branch: else_branch
                        .as_ref()
                        .map(|value| value.1.clone_cst_to_ast())
                        .transpose()?,
                }
            }
            cst_types::Statement::UseStatement(
                _,
                extension_keyword,
                use_statement_content,
                _,
                _,
            ) => {
                if extension_keyword.is_some() {
                    ast_types::Statement::UseExtensionStatement(
                        use_statement_content.clone_cst_to_ast()?,
                    )
                } else {
                    ast_types::Statement::UseStatement(use_statement_content.clone_cst_to_ast()?)
                }
            }
            cst_types::Statement::EmptyStatement(_) => return Ok(None),
            cst_types::Statement::Invalid(_) => return Err(InvalidCST),
        }))
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(Some(match self {
            cst_types::Statement::DataDeclaration(_, value, _, _) => {
                ast_types::Statement::DataDeclaration(value.cst_to_ast()?)
            }
            cst_types::Statement::Assignment(identifier, _, value, _, _) => {
                ast_types::Statement::Assignment {
                    target: identifier.cst_to_ast()?,
                    value: value.cst_to_ast()?,
                }
            }
            cst_types::Statement::ListAssignment(identifier, _, _, value, _, _, _) => {
                ast_types::Statement::ListAssignment {
                    target: identifier.cst_to_ast()?,
                    value: value
                        .into_iter()
                        .map(|value| value.cst_to_ast())
                        .collect::<Result<_, _>>()?,
                }
            }
            cst_types::Statement::SetItem(identifier, _, key, _, _, value, _, _) => {
                ast_types::Statement::SetItem {
                    list: identifier.cst_to_ast()?,
                    item: key.cst_to_ast()?,
                    value: value.cst_to_ast()?,
                }
            }
            cst_types::Statement::Call(identifier, _, value, _, _, _) => {
                ast_types::Statement::Call {
                    function: identifier.cst_to_ast()?,
                    arguments: value
                        .into_iter()
                        .map(|value| value.cst_to_ast())
                        .collect::<Result<_, _>>()?,
                }
            }
            cst_types::Statement::SingleInputControl(identifier, expression, code_block, _, _) => {
                ast_types::Statement::Control {
                    control_function: identifier.cst_to_ast()?,
                    arguments: vec![expression.cst_to_ast()?],
                    code_block: code_block.cst_to_ast()?,
                }
            }
            cst_types::Statement::MultiInputControl(identifier, _, value, _, code_block, _, _) => {
                ast_types::Statement::Control {
                    control_function: identifier.cst_to_ast()?,
                    arguments: value
                        .into_iter()
                        .map(|value| value.cst_to_ast())
                        .collect::<Result<_, _>>()?,
                    code_block: code_block.cst_to_ast()?,
                }
            }
            cst_types::Statement::Forever(identifier, code_block, _, _) => {
                ast_types::Statement::Control {
                    control_function: identifier.cst_to_ast()?,
                    arguments: Vec::new(),
                    code_block: code_block.cst_to_ast()?,
                }
            }
            cst_types::Statement::IfElse(first_branch, alternative_branches, else_branch, _, _) => {
                ast_types::Statement::IfElse {
                    first_branch: (first_branch.1.cst_to_ast()?, first_branch.2.cst_to_ast()?),
                    alternative_branches: alternative_branches
                        .into_iter()
                        .map(|value| Ok((value.2.cst_to_ast()?, value.3.cst_to_ast()?)))
                        .collect::<Result<_, InvalidCST>>()?,
                    else_branch: else_branch.map(|value| value.1.cst_to_ast()).transpose()?,
                }
            }
            cst_types::Statement::UseStatement(
                _,
                extension_keyword,
                use_statement_content,
                _,
                _,
            ) => {
                if extension_keyword.is_some() {
                    ast_types::Statement::UseExtensionStatement(use_statement_content.cst_to_ast()?)
                } else {
                    ast_types::Statement::UseStatement(use_statement_content.cst_to_ast()?)
                }
            }
            cst_types::Statement::EmptyStatement(_) => return Ok(None),
            cst_types::Statement::Invalid(_) => return Err(InvalidCST),
        }))
    }
}

impl CSTToAST for cst_types::CodeBlock {
    type AST = ast_types::CodeBlock;
    type Error = InvalidCST;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(ast_types::CodeBlock {
            statements: self
                .statements
                .iter()
                .filter_map(|value| value.clone_cst_to_ast().transpose())
                .collect::<Result<_, _>>()?,
        })
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(ast_types::CodeBlock {
            statements: self
                .statements
                .into_iter()
                .filter_map(|value| value.cst_to_ast().transpose())
                .collect::<Result<_, _>>()?,
        })
    }
}

impl CSTToAST for cst_types::SingleAssetDeclarationValue {
    type AST = ast_types::SingleAssetDeclarationValue;
    type Error = InvalidCST;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::SingleAssetDeclarationValue::Simple(_, (value, _), _, _) => {
                ast_types::SingleAssetDeclarationValue::Simple(value.clone())
            }
            cst_types::SingleAssetDeclarationValue::Dictionary(_, value, _, _) => {
                ast_types::SingleAssetDeclarationValue::Dictionary(
                    value
                        .iter()
                        .map(|value| value.clone_cst_to_ast())
                        .collect::<Result<_, _>>()?,
                )
            }
        })
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::SingleAssetDeclarationValue::Simple(_, (value, _), _, _) => {
                ast_types::SingleAssetDeclarationValue::Simple(value)
            }
            cst_types::SingleAssetDeclarationValue::Dictionary(_, value, _, _) => {
                ast_types::SingleAssetDeclarationValue::Dictionary(
                    value
                        .into_iter()
                        .map(|value| value.cst_to_ast())
                        .collect::<Result<_, _>>()?,
                )
            }
        })
    }
}

impl CSTToAST for cst_types::SingleAssetDeclaration {
    type AST = ast_types::SingleAssetDeclaration;
    type Error = InvalidCST;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(ast_types::SingleAssetDeclaration {
            canonical_identifier: self
                .0
                .as_ref()
                .map(|value| value.clone_cst_to_ast())
                .transpose()?,
            identifier: self.1.clone_cst_to_ast()?,
            value: self.2.clone_cst_to_ast()?,
        })
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(ast_types::SingleAssetDeclaration {
            canonical_identifier: self.0.map(|value| value.cst_to_ast()).transpose()?,
            identifier: self.1.cst_to_ast()?,
            value: self.2.cst_to_ast()?,
        })
    }
}

impl CSTToAST for cst_types::AssetDeclaration {
    type AST = ast_types::AssetDeclaration;
    type Error = InvalidCST;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::AssetDeclaration::Multiple(_, value, _, _) => {
                ast_types::AssetDeclaration::Multiple(
                    value
                        .iter()
                        .map(|value| value.clone_cst_to_ast())
                        .collect::<Result<_, _>>()?,
                )
            }
            cst_types::AssetDeclaration::Single(value) => {
                ast_types::AssetDeclaration::Single(value.clone_cst_to_ast()?)
            }
        })
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(match self {
            cst_types::AssetDeclaration::Multiple(_, value, _, _) => {
                ast_types::AssetDeclaration::Multiple(
                    value
                        .into_iter()
                        .map(|value| value.cst_to_ast())
                        .collect::<Result<_, _>>()?,
                )
            }
            cst_types::AssetDeclaration::Single(value) => {
                ast_types::AssetDeclaration::Single(value.cst_to_ast()?)
            }
        })
    }
}

cst_to_ast_with_cheap_clone_cst_to_ast!(cst_types::WarpSpecifier, ast_types::WarpSpecifier, this => {
    ast_types::WarpSpecifier { is_warp: this.is_warp }
});
cst_to_ast_with_cheap_clone_cst_to_ast!(cst_types::CustomBlockParamKind, ast_types::CustomBlockParamKind, this => {
    match this.kind {
        cst_types::CustomBlockParamKindValue::Number => ast_types::CustomBlockParamKind::Number,
        cst_types::CustomBlockParamKindValue::String => ast_types::CustomBlockParamKind::String,
        cst_types::CustomBlockParamKindValue::Boolean => ast_types::CustomBlockParamKind::Boolean,
    }
});

impl CSTToAST for cst_types::StageStatement {
    type AST = Option<ast_types::StageStatement>;
    type Error = InvalidCST;
    fn clone_cst_to_ast(&self) -> Result<Self::AST, Self::Error> {
        Ok(Some(match self {
            cst_types::StageStatement::DataDeclaration(_, data_declaration, _, _) => {
                ast_types::StageStatement::DataDeclaration(data_declaration.clone_cst_to_ast()?)
            }
            cst_types::StageStatement::BackdropDeclaration(_, value, _, _) => {
                ast_types::StageStatement::BackdropDeclaration(value.clone_cst_to_ast()?)
            }
            cst_types::StageStatement::SoundDeclaration(_, value, _, _) => {
                ast_types::StageStatement::SoundDeclaration(value.clone_cst_to_ast()?)
            }
            cst_types::StageStatement::NoInputHatStatement(identifier, code_block, _, _) => {
                ast_types::StageStatement::HatStatement {
                    hat_function: identifier.clone_cst_to_ast()?,
                    arguments: Vec::new(),
                    code_block: code_block.clone_cst_to_ast()?,
                }
            }
            cst_types::StageStatement::SingleInputHatStatement(
                identifier,
                expression,
                code_block,
                _,
                _,
            ) => ast_types::StageStatement::HatStatement {
                hat_function: identifier.clone_cst_to_ast()?,
                arguments: vec![expression.clone_cst_to_ast()?],
                code_block: code_block.clone_cst_to_ast()?,
            },
            cst_types::StageStatement::MultiInputHatStatement(
                identifier,
                _,
                value,
                _,
                code_block,
                _,
                _,
            ) => ast_types::StageStatement::HatStatement {
                hat_function: identifier.clone_cst_to_ast()?,
                arguments: value
                    .iter()
                    .map(|value| value.clone_cst_to_ast())
                    .collect::<Result<_, _>>()?,
                code_block: code_block.clone_cst_to_ast()?,
            },
            cst_types::StageStatement::CustomBlockDefinition(
                warp_specifier,
                _,
                canonical_identifier,
                identifier,
                _,
                params,
                _,
                code_block,
                _,
                _,
            ) => ast_types::StageStatement::CustomBlockDefinition {
                is_warp: warp_specifier
                    .as_ref()
                    .map(|value| value.clone_cst_to_ast())
                    .transpose()?
                    .unwrap_or(ast_types::WarpSpecifier { is_warp: false }),
                canonical_identifier: canonical_identifier
                    .as_ref()
                    .map(|value| value.clone_cst_to_ast())
                    .transpose()?,
                identifier: identifier.clone_cst_to_ast()?,
                parameters: params
                    .iter()
                    .map(|value| {
                        Ok((
                            value
                                .0
                                .as_ref()
                                .map(|value| value.clone_cst_to_ast())
                                .transpose()?,
                            value
                                .1
                                .as_ref()
                                .map(|value| value.clone_cst_to_ast())
                                .transpose()?,
                            value.2.clone_cst_to_ast()?,
                        ))
                    })
                    .collect::<Result<_, InvalidCST>>()?,
                code_block: code_block.clone_cst_to_ast()?,
            },
            cst_types::StageStatement::IsolatedBlock(code_block, _, _) => {
                ast_types::StageStatement::IsolatedBlock(code_block.clone_cst_to_ast()?)
            }
            cst_types::StageStatement::IsolatedExpression(_, expression, _, _, _) => {
                ast_types::StageStatement::IsolatedExpression(expression.clone_cst_to_ast()?)
            }
            cst_types::StageStatement::ConfigStatement(_, _, value, _, _, _) => {
                ast_types::StageStatement::ConfigStatement(
                    value
                        .iter()
                        .map(|value| value.clone_cst_to_ast())
                        .collect::<Result<_, _>>()?,
                )
            }
            cst_types::StageStatement::MonitorDeclaration(_, value, _, config, _, _, _) => {
                ast_types::StageStatement::MonitorDeclaration {
                    value: value.clone_cst_to_ast()?,
                    configuration: config
                        .iter()
                        .map(|value| value.clone_cst_to_ast())
                        .collect::<Result<_, _>>()?,
                }
            }
            cst_types::StageStatement::UseStatement(
                _,
                extension_keyword,
                use_statement_content,
                _,
                _,
            ) => {
                if extension_keyword.is_some() {
                    ast_types::StageStatement::UseExtensionStatement(
                        use_statement_content.clone_cst_to_ast()?,
                    )
                } else {
                    ast_types::StageStatement::UseStatement(use_statement_content.clone_cst_to_ast()?)
                }
            }
            cst_types::StageStatement::EmptyStatement(_) => return Ok(None),
            cst_types::StageStatement::Invalid(_) => return Err(InvalidCST),
        }))
    }
    fn cst_to_ast(self) -> Result<Self::AST, Self::Error> {
        Ok(Some(match self {
            cst_types::StageStatement::DataDeclaration(_, data_declaration, _, _) => {
                ast_types::StageStatement::DataDeclaration(data_declaration.cst_to_ast()?)
            }
            cst_types::StageStatement::BackdropDeclaration(_, value, _, _) => {
                ast_types::StageStatement::BackdropDeclaration(value.cst_to_ast()?)
            }
            cst_types::StageStatement::SoundDeclaration(_, value, _, _) => {
                ast_types::StageStatement::SoundDeclaration(value.cst_to_ast()?)
            }
            cst_types::StageStatement::NoInputHatStatement(identifier, code_block, _, _) => {
                ast_types::StageStatement::HatStatement {
                    hat_function: identifier.cst_to_ast()?,
                    arguments: Vec::new(),
                    code_block: code_block.cst_to_ast()?,
                }
            }
            cst_types::StageStatement::SingleInputHatStatement(
                identifier,
                expression,
                code_block,
                _,
                _,
            ) => ast_types::StageStatement::HatStatement {
                hat_function: identifier.cst_to_ast()?,
                arguments: vec![expression.cst_to_ast()?],
                code_block: code_block.cst_to_ast()?,
            },
            cst_types::StageStatement::MultiInputHatStatement(
                identifier,
                _,
                value,
                _,
                code_block,
                _,
                _,
            ) => ast_types::StageStatement::HatStatement {
                hat_function: identifier.cst_to_ast()?,
                arguments: value
                    .into_iter()
                    .map(|value| value.cst_to_ast())
                    .collect::<Result<_, _>>()?,
                code_block: code_block.cst_to_ast()?,
            },
            cst_types::StageStatement::CustomBlockDefinition(
                warp_specifier,
                _,
                canonical_identifier,
                identifier,
                _,
                params,
                _,
                code_block,
                _,
                _,
            ) => ast_types::StageStatement::CustomBlockDefinition {
                is_warp: warp_specifier
                    .map(|value| value.cst_to_ast())
                    .transpose()?
                    .unwrap_or(ast_types::WarpSpecifier { is_warp: false }),
                canonical_identifier: canonical_identifier
                    .map(|value| value.cst_to_ast())
                    .transpose()?,
                identifier: identifier.cst_to_ast()?,
                parameters: params
                    .into_iter()
                    .map(|value| {
                        Ok((
                            value
                                .0
                                .map(|value| value.cst_to_ast())
                                .transpose()?,
                            value
                                .1
                                .map(|value| value.cst_to_ast())
                                .transpose()?,
                            value.2.cst_to_ast()?,
                        ))
                    })
                    .collect::<Result<_, InvalidCST>>()?,
                code_block: code_block.cst_to_ast()?,
            },
            cst_types::StageStatement::IsolatedBlock(code_block, _, _) => {
                ast_types::StageStatement::IsolatedBlock(code_block.cst_to_ast()?)
            }
            cst_types::StageStatement::IsolatedExpression(_, expression, _, _, _) => {
                ast_types::StageStatement::IsolatedExpression(expression.cst_to_ast()?)
            }
            cst_types::StageStatement::ConfigStatement(_, _, value, _, _, _) => {
                ast_types::StageStatement::ConfigStatement(
                    value
                        .into_iter()
                        .map(|value| value.cst_to_ast())
                        .collect::<Result<_, _>>()?,
                )
            }
            cst_types::StageStatement::MonitorDeclaration(_, value, _, config, _, _, _) => {
                ast_types::StageStatement::MonitorDeclaration {
                    value: value.clone_cst_to_ast()?,
                    configuration: config
                        .into_iter()
                        .map(|value| value.cst_to_ast())
                        .collect::<Result<_, _>>()?,
                }
            }
            cst_types::StageStatement::UseStatement(
                _,
                extension_keyword,
                use_statement_content,
                _,
                _,
            ) => {
                if extension_keyword.is_some() {
                    ast_types::StageStatement::UseExtensionStatement(
                        use_statement_content.cst_to_ast()?,
                    )
                } else {
                    ast_types::StageStatement::UseStatement(use_statement_content.cst_to_ast()?)
                }
            }
            cst_types::StageStatement::EmptyStatement(_) => return Ok(None),
            cst_types::StageStatement::Invalid(_) => return Err(InvalidCST),
        }))
    }
}
