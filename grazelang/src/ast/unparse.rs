use std::fmt::{Error as FormatError, Result as FormatResult, Write};

use crate::{
    ast::types::{Associativity, BinOp, Expression, Identifier, Literal, SingleIdentifier, UnOp},
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
                write!(f, "\"")?;
                for i in value {
                    match i {
                        crate::ast::types::FormattedStringContent::Expression(expression) => {
                            write!(f, "${{{}}}", u(expression.as_ref()))?;
                        }
                        crate::ast::types::FormattedStringContent::String(value) => {
                            write!(f, "{}", string_escape::format_string_escaper(value))?;
                        }
                    }
                }
                write!(f, "\"")
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
                operator.unparse_into(f)?;
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
                write!(f, "(")?;
                for i in arguments {
                    write!(f, "{},", u(i))?;
                }
                write!(f, ")")
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
