use super::{
    ast::{self, BinOp, Expression, Identifier, ParseError, Statement},
    parse_context::ParseContext,
};
use crate::{
    lexer::{
        self, PosRange, Token, get_pos_range as internal_get_pos_range,
        get_position as internal_get_position,
    },
    parser::ast::{CodeBlock, Semicolon},
};
use arcstr::ArcStr as IString;
use logos::Lexer;
use std::{collections::VecDeque, iter::Peekable, vec};

macro_rules! expect_token {
    ($token_stream:expr, $pattern:expr => (), $msg1:expr, $msg2:expr) => {{
        if next_token!($token_stream) != $pattern {
            emit_unexpected_token!($token_stream, $msg1, $msg2)
        }
    }};
    ($token_stream:expr, $pattern:pat => $value:expr, $msg1:expr, $msg2:expr) => {{
        match next_token!($token_stream) {
            $pattern => $value,
            _ => emit_unexpected_token!($token_stream, $msg1, $msg2),
        }
    }};
}

// /// Not currently required
// macro_rules! consume_if {
//     ($token_stream:expr, $pattern:pat => $body:expr) => {{
//         match peek_token!($token_stream) {
//             $pattern => {
//                 skip_token!($token_stream);
//                 Some($body)
//             }
//             _ => None,
//         }
//     }};
// }

/// The difference between this and `consume_if` is that we cannot use values from the match in `consume_if` due to the borrow checker.
macro_rules! consume_and_use_if {
    ($token_stream:expr, $pattern:pat => $body:expr) => {{
        match peek_token!($token_stream) {
            $pattern => match next_token!($token_stream) {
                $pattern => Some($body),
                _ => panic!(),
            },
            _ => None,
        }
    }};
}

macro_rules! consume_then_never_if {
    ($token_stream:expr, $pattern:pat => $body:expr) => {{
        if matches!(peek_token!($token_stream), $pattern) {
            skip_token!($token_stream);
            $body;
        }
    }};
}

macro_rules! match_token_or_return_none {
    ($token_stream:expr, { $($tok:pat => $variant:path),* $(,)? }) => {{
        match peek_token!(optional $token_stream) {
            $(
                $tok => {
                    skip_token!($token_stream);
                    from_stream_pos!($token_stream => $variant)
                }
            )*
            _ => return Ok(None),
        }
    }};
}

macro_rules! peek_token {
    ($token_stream:expr) => {
        match $token_stream.0.peek() {
            Some(Ok(value)) => value,
            Some(Err(_)) => {
                return { Err(ParseError::LexerStuck(get_token_position!($token_stream))) }
            }
            None => return Err(ParseError::UnexpectedEndOfInput),
        }
    };
    (optional $token_stream:expr) => {
        match $token_stream.0.peek() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return Err(ParseError::LexerStuck(get_token_position!($token_stream))),
            None => return Ok(None),
        }
    };
    ($token_stream:expr => Option) => {
        match $token_stream.0.peek() {
            Some(Ok(value)) => Some(value),
            Some(Err(_)) => return Err(ParseError::LexerStuck(get_token_position!($token_stream))),
            None => None,
        }
    };
}

macro_rules! next_token {
    ($token_stream:expr) => {{
        $token_stream.1.next();
        match $token_stream.0.next() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return Err(ParseError::LexerStuck(get_token_position!($token_stream))),
            None => return Err(ParseError::UnexpectedEndOfInput),
        }
    }};
    (optional $token_stream:expr) => {{
        $token_stream.1.next();
        match $token_stream.0.next() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return Err(ParseError::LexerStuck(get_token_position!($token_stream))),
            None => return Ok(None),
        }
    }};
    ($token_stream:expr => Option) => {{
        $token_stream.1.next();
        match $token_stream.0.next() {
            Some(Ok(value)) => Some(value),
            Some(Err(_)) => return Err(ParseError::LexerStuck(get_token_position!($token_stream))),
            None => None,
        }
    }};
}

macro_rules! skip_token {
    ($token_stream:expr) => {{
        $token_stream.1.next();
        if let Some(Err(_)) = $token_stream.0.next() {
            return Err(ParseError::LexerStuck(get_token_position!($token_stream)));
        }
    }};
}

macro_rules! get_token_position {
    ($token_stream:expr) => {
        internal_get_pos_range($token_stream.1, $token_stream.1.span())
    };
}

macro_rules! get_token_start {
    ($token_stream:expr) => {
        internal_get_position($token_stream.1, $token_stream.1.span().start)
    };
}

macro_rules! get_token_end {
    ($token_stream:expr) => {
        internal_get_position($token_stream.1, $token_stream.1.span().end)
    };
}

macro_rules! emit_unexpected_token {
    ($token_stream:expr, $msg_1:expr, $msg_2:expr) => {
        return Err(ParseError::UnexpectedToken(
            $msg_1,
            $msg_2,
            get_token_position!($token_stream),
        ))
    };
}

macro_rules! from_stream_pos {
    ($token_stream:expr => $node:path) => {
        $node(get_token_position!($token_stream))
    };
}

#[macro_export]
macro_rules! make_parse_in {
    ($lexer:expr) => {
        &mut (&mut (&mut $lexer.clone()).peekable(), $lexer)
    };
}

type ParseIn<'a, 'b, 'c, 'd, 'e, 'f> = &'a mut (
    &'b mut Peekable<&'c mut Lexer<'d, Token>>,
    &'e mut Lexer<'f, Token>,
);
type ParseOut<T> = Result<T, ParseError>;

pub fn enter(lex: &mut Lexer<Token>) {
    let mut context = ParseContext::new();
    parse_lexed_entrypoint(make_parse_in!(lex), &mut context);
}

pub fn parse_lexed_entrypoint(token_stream: ParseIn, context: &mut ParseContext) {
    todo!()
}

pub fn parse_single_identifier(
    token_stream: ParseIn,
    context: &mut ParseContext,
) -> ParseOut<IString> {
    expect_token!(token_stream, Token::Identifier(value) => Ok(value), "Expected an identifier.", "an identifier")
}

pub fn parse_full_identifier(
    token_stream: ParseIn,
    context: &mut ParseContext,
) -> ParseOut<Identifier> {
    let mut names: Vec<(IString, PosRange)> = vec![(
        parse_single_identifier(token_stream, context)?,
        get_token_position!(token_stream),
    )];
    let start_pos = get_token_start!(token_stream);
    let mut scope: Option<Vec<(IString, PosRange)>> = None;
    loop {
        match match peek_token!(token_stream => Option) {
            Some(value) => value,
            None => break
        } {
            Token::ScopeResolution => {
                if let Some(_) = scope {
                    skip_token!(token_stream);
                    emit_unexpected_token!(token_stream, "Expected a dot.", "a dot");
                }
            }
            Token::Dot => {
                if let None = scope {
                    scope = Some(names);
                    names = Vec::new();
                }
            }
            _ => break,
        }
        skip_token!(token_stream);
        names.push(expect_token!(
            token_stream,
            Token::Identifier(value) => (value, get_token_position!(token_stream)),
            "Expected an identifier.",
            "an identifier"
        ));
    }
    if names.len() > 1 && scope == None {
        scope = Some(names);
        names = Vec::new();
    }
    Ok(Identifier {
        scope: scope.unwrap_or_default(),
        names,
        pos_range: (start_pos, get_token_end!(token_stream)),
    })
}

pub fn parse_full_identifier_starting_with(
    token_stream: ParseIn,
    context: &mut ParseContext,
    value: IString,
) -> ParseOut<Identifier> {
    let mut names: Vec<(IString, PosRange)> = vec![(
        value,
        get_token_position!(token_stream),
    )];
    let start_pos = get_token_start!(token_stream);
    let mut scope: Option<Vec<(IString, PosRange)>> = None;
    loop {
        match match peek_token!(token_stream => Option) {
            Some(value) => value,
            None => break
        } {
            Token::ScopeResolution => {
                if let Some(_) = scope {
                    skip_token!(token_stream);
                    emit_unexpected_token!(token_stream, "Expected a dot.", "a dot");
                }
            }
            Token::Dot => {
                if let None = scope {
                    scope = Some(names);
                    names = Vec::new();
                }
            }
            _ => break,
        }
        skip_token!(token_stream);
        names.push(expect_token!(
            token_stream,
            Token::Identifier(value) => (value, get_token_position!(token_stream)),
            "Expected an identifier.",
            "an identifier"
        ));
    }
    if names.len() > 1 && scope == None {
        scope = Some(names);
        names = Vec::new();
    }
    Ok(Identifier {
        scope: scope.unwrap_or_default(),
        names,
        pos_range: (start_pos, get_token_end!(token_stream)),
    })
}

// Statements do not include semicolons.
pub mod statement {
    use core::panic;

    use serde::{Deserialize, Serialize};

    use crate::{
        lexer,
        parser::ast::{
            AssignmentOperator, CanonicalIdentifier, DataDeclarationScope, LeftBrace, LeftBracket,
            LeftParens, LetKeyword, ListEntry, ListKeyword, ListsKeyword, MultiDataDeclaration,
            RightBrace, RightBracket, RightParens, SingleDataDeclaration,
            SingleDataDeclarationType, VarKeyword, VarsKeyword,
        },
    };

    use super::*;

    pub fn parse_literal(
        token_stream: ParseIn,
        context: &mut ParseContext,
    ) -> ParseOut<ast::Literal> {
        let token = next_token!(token_stream);
        use Expression::Literal as ELiteral;
        use ast::Literal as LLiteral;
        match token {
            Token::SimpleString(value) => {
                Ok(LLiteral::String(value, get_token_position!(token_stream)))
            }
            Token::DecimalInt(value) => Ok(LLiteral::DecimalInt(
                value,
                get_token_position!(token_stream),
            )),
            Token::DecimalFloat(value) => Ok(LLiteral::DecimalFloat(
                value,
                get_token_position!(token_stream),
            )),
            Token::HexadecimalInt(value) => Ok(LLiteral::HexadecimalInt(
                value,
                get_token_position!(token_stream),
            )),
            Token::OctalInt(value) => {
                Ok(LLiteral::OctalInt(value, get_token_position!(token_stream)))
            }
            Token::BinaryInt(value) => Ok(LLiteral::BinaryInt(
                value,
                get_token_position!(token_stream),
            )),
            Token::LeftParens => {
                let left_parens_position = get_token_position!(token_stream);
                consume_then_never_if!(token_stream, Token::RightParens =>
                    return Ok(LLiteral::EmptyExpression(
                        LeftParens(left_parens_position),
                        from_stream_pos!(token_stream => RightParens),
                        (left_parens_position.0, get_token_end!(token_stream)),
                    ))
                );
                let value = parse_literal(token_stream, context)?;
                expect_token!(
                    token_stream,
                    Token::RightParens => (),
                    "Expected ')'.",
                    "')'"
                );
                Ok(value)
            }
            _ => {
                emit_unexpected_token!(token_stream, "Expected a literal.", "a literal");
            }
        }
    }

    pub fn parse_list_content(
        token_stream: ParseIn,
        context: &mut ParseContext,
    ) -> ParseOut<(LeftBrace, Vec<ast::ListEntry>, RightBrace)> {
        let mut entries = Vec::<ast::ListEntry>::new();
        let left_brace = expect_token!(
            token_stream,
            Token::LeftBrace => from_stream_pos!(token_stream => LeftBrace),
            "Expected '{'.",
            "'{'"
        );
        let right_brace = loop {
            entries.push(match peek_token!(token_stream) {
                Token::RightBrace(lexer::LexedRightBrace::Normal) => {
                    skip_token!(token_stream);
                    break from_stream_pos!(token_stream => RightBrace);
                }
                Token::Unwrap => {
                    skip_token!(token_stream);
                    let unwrap_start = get_token_start!(token_stream);
                    expect_token!(
                        token_stream,
                        Token::SimpleString(value) => ListEntry::Unwrap(
                            ast::Literal::String(value, get_token_position!(token_stream)),
                            (unwrap_start, get_token_end!(token_stream)),
                        ),
                        "Expected a simple string literal.",
                        "a simple string literal"
                    )
                }
                _ => ListEntry::Expression(parse_expression(token_stream, context)?),
            });
            match next_token!(token_stream) {
                Token::RightBrace(lexer::LexedRightBrace::Normal) => {
                    break from_stream_pos!(token_stream => RightBrace);
                }
                Token::Comma => (),
                _ => {
                    emit_unexpected_token!(
                        token_stream,
                        "Expected a comma or '}'.",
                        "a comma or '}'"
                    );
                }
            }
        };
        Ok((left_brace, entries, right_brace))
    }

    #[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
    pub enum DefaultDataDeclarationType {
        Var,
        List,
    }

    #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
    enum DeclarationValue {
        None,
        List(
            AssignmentOperator,
            LeftBrace,
            Vec<ast::ListEntry>,
            RightBrace,
        ),
        Var(AssignmentOperator, Expression),
    }

    pub fn parse_inner_single_declarations(
        token_stream: ParseIn,
        context: &mut ParseContext,
        default_type: DefaultDataDeclarationType,
    ) -> ParseOut<Vec<SingleDataDeclaration>> {
        let mut declarations = Vec::<SingleDataDeclaration>::new();
        loop {
            let mut start_pos = None::<(usize, usize)>;
            let scope = match peek_token!(token_stream) {
                Token::GlobalKeyword => {
                    skip_token!(token_stream);
                    start_pos = Some(get_token_start!(token_stream));
                    from_stream_pos!(token_stream => DataDeclarationScope::Global)
                }
                Token::LocalKeyword => {
                    skip_token!(token_stream);
                    start_pos = Some(get_token_start!(token_stream));
                    from_stream_pos!(token_stream => DataDeclarationScope::Local)
                }
                Token::CloudKeyword => {
                    skip_token!(token_stream);
                    start_pos = Some(get_token_start!(token_stream));
                    from_stream_pos!(token_stream => DataDeclarationScope::Cloud)
                }
                Token::RightBrace(lexer::LexedRightBrace::Normal) => break,
                Token::RightParens => break,
                _ => DataDeclarationScope::Unset,
            };
            let dec_type = match peek_token!(token_stream) {
                Token::VarKeyword => {
                    skip_token!(token_stream);
                    if start_pos == None {
                        start_pos = Some(get_token_start!(token_stream));
                    }
                    from_stream_pos!(token_stream => SingleDataDeclarationType::Var)
                }
                Token::ListKeyword => {
                    skip_token!(token_stream);
                    if start_pos == None {
                        start_pos = Some(get_token_start!(token_stream));
                    }
                    from_stream_pos!(token_stream => SingleDataDeclarationType::List)
                }
                _ => SingleDataDeclarationType::Unset,
            };
            let canonical_identifier = consume_and_use_if!(token_stream, Token::CanonicalName(value) => {
                let value = value.clone();
                if start_pos == None {
                    start_pos = Some(get_token_start!(token_stream));
                }
                CanonicalIdentifier {
                    name: value,
                    pos_range: get_token_position!(token_stream),
                }
            });
            let identifier = if let Token::Identifier(value) = next_token!(token_stream) {
                if start_pos == None {
                    start_pos = Some(get_token_start!(token_stream));
                }
                Identifier {
                    scope: Vec::new(),
                    names: vec![(value, get_token_position!(token_stream))],
                    pos_range: get_token_position!(token_stream),
                }
            } else {
                match (scope, dec_type, canonical_identifier) {
                    (DataDeclarationScope::Unset, SingleDataDeclarationType::Unset, None) => {
                        emit_unexpected_token!(
                            token_stream,
                            "Expected scope modifer, declaration type, canonical identifier or identifier.",
                            "scope modifer, declaration type, canonical identifier or identifier"
                        );
                    }
                    (_, SingleDataDeclarationType::Unset, None) => {
                        emit_unexpected_token!(
                            token_stream,
                            "Expected declaration type, canonical identifier or identifier.",
                            "declaration type, canonical identifier or identifier"
                        );
                    }
                    (_, _, None) => {
                        emit_unexpected_token!(
                            token_stream,
                            "Expected canonical identifier or identifier.",
                            "canonical identifier or identifier"
                        );
                    }
                    (_, _, _) => {
                        emit_unexpected_token!(token_stream, "Expected identifier.", "identifier");
                    }
                }
            };
            let value = match peek_token!(token_stream) {
                // Token::LeftBrace => {
                //     if default_type == DefaultDataDeclarationType::Var {
                //         match dec_type {
                //             SingleDataDeclarationType::Unset => {
                //                 skip_token!(token_stream);
                //                 emit_unexpected_token!(token_stream, "Expected '='.", "'='")
                //             }
                //             SingleDataDeclarationType::Var(_) => {
                //                 skip_token!(token_stream);
                //                 emit_unexpected_token!(token_stream, "Expected '='.", "'='")
                //             }
                //             _ => (),
                //         }
                //     }
                //     let (left_brace, list_content, right_brace) =
                //         parse_list_content(token_stream, context)?;

                //     DeclarationValue::List(left_brace, list_content, right_brace)
                // }
                Token::Assign => {
                    skip_token!(token_stream);
                    let assignment_operator = from_stream_pos!(token_stream => AssignmentOperator);
                    if let Token::LeftBrace = peek_token!(token_stream) {
                        match (&default_type, &dec_type) {
                            (
                                DefaultDataDeclarationType::Var,
                                SingleDataDeclarationType::List(_),
                            ) => (),
                            (DefaultDataDeclarationType::Var, _)
                            | (
                                DefaultDataDeclarationType::List,
                                SingleDataDeclarationType::Var(_),
                            ) => {
                                emit_unexpected_token!(
                                    token_stream,
                                    "Expected an expression.",
                                    "an expression"
                                );
                            }
                            _ => (),
                        }
                        let (left_brace, list_content, right_brace) =
                            parse_list_content(token_stream, context)?;
                        DeclarationValue::List(
                            assignment_operator,
                            left_brace,
                            list_content,
                            right_brace,
                        )
                    } else {
                        if default_type == DefaultDataDeclarationType::List
                            && !matches!(dec_type, SingleDataDeclarationType::Var(_))
                        {
                            emit_unexpected_token!(token_stream, "Expected '{'.", "'{'");
                        }
                        DeclarationValue::Var(
                            assignment_operator,
                            parse_expression(token_stream, context)?,
                        )
                    }
                }
                Token::Comma => DeclarationValue::None,
                Token::RightBrace(lexer::LexedRightBrace::Normal) => DeclarationValue::None,
                Token::RightParens => DeclarationValue::None,
                _ => emit_unexpected_token!(
                    token_stream,
                    "Expected ',', '=', '{', '}' or ')'",
                    "',', '=', '{', '}' or ')'"
                ),
            };
            let declaration = match value {
                DeclarationValue::None => match dec_type {
                    SingleDataDeclarationType::Unset => match default_type {
                        DefaultDataDeclarationType::Var => SingleDataDeclaration::EmptyVariable(
                            None,
                            scope,
                            canonical_identifier,
                            identifier,
                            (start_pos.unwrap(), get_token_end!(token_stream)),
                        ),
                        DefaultDataDeclarationType::List => SingleDataDeclaration::EmptyList(
                            None,
                            scope,
                            canonical_identifier,
                            identifier,
                            (start_pos.unwrap(), get_token_end!(token_stream)),
                        ),
                    },
                    SingleDataDeclarationType::Var(p) => SingleDataDeclaration::EmptyVariable(
                        Some(VarKeyword(p)),
                        scope,
                        canonical_identifier,
                        identifier,
                        (start_pos.unwrap(), get_token_end!(token_stream)),
                    ),
                    SingleDataDeclarationType::List(p) => SingleDataDeclaration::EmptyList(
                        Some(ListKeyword(p)),
                        scope,
                        canonical_identifier,
                        identifier,
                        (start_pos.unwrap(), get_token_end!(token_stream)),
                    ),
                },
                DeclarationValue::Var(assignment_operator, literal) => {
                    SingleDataDeclaration::Variable(
                        match dec_type {
                            SingleDataDeclarationType::Unset => None,
                            SingleDataDeclarationType::Var(p) => Some(VarKeyword(p)),
                            SingleDataDeclarationType::List(_) => None,
                        },
                        scope,
                        canonical_identifier,
                        identifier,
                        assignment_operator,
                        literal,
                        (start_pos.unwrap(), get_token_end!(token_stream)),
                    )
                }
                DeclarationValue::List(assignment_operator, left_brace, items, right_brace) => {
                    SingleDataDeclaration::List(
                        match dec_type {
                            SingleDataDeclarationType::Unset => None,
                            SingleDataDeclarationType::Var(_) => None,
                            SingleDataDeclarationType::List(p) => Some(ListKeyword(p)),
                        },
                        scope,
                        canonical_identifier,
                        identifier,
                        assignment_operator,
                        left_brace,
                        items,
                        right_brace,
                        (start_pos.unwrap(), get_token_end!(token_stream)),
                    )
                }
            };
            declarations.push(declaration);
            match peek_token!(token_stream) {
                Token::Comma => skip_token!(token_stream),
                Token::RightBrace(lexer::LexedRightBrace::Normal) => break,
                Token::RightParens => break,
                _ => {
                    skip_token!(token_stream);
                    emit_unexpected_token!(
                        token_stream,
                        "Expected ',', ')' or '}'.",
                        "',', ')' or '}'"
                    )
                }
            }
        }
        Ok(declarations)
    }

    pub fn parse_data_declaration(
        token_stream: ParseIn,
        context: &mut ParseContext,
    ) -> ParseOut<Statement> {
        expect_token!(
            token_stream,
            Token::LetKeyword => (),
            "Data declaration needs to start with \"let\".",
            "\"let\""
        );
        let let_keyword_position = get_token_position!(token_stream);
        let mut start_pos = None::<PosRange>;
        let scope = match peek_token!(token_stream) {
            Token::GlobalKeyword => {
                skip_token!(token_stream);
                start_pos = from_stream_pos!(token_stream => Some);
                from_stream_pos!(token_stream => DataDeclarationScope::Global)
            }
            Token::LocalKeyword => {
                skip_token!(token_stream);
                start_pos = from_stream_pos!(token_stream => Some);
                from_stream_pos!(token_stream => DataDeclarationScope::Local)
            }
            Token::CloudKeyword => {
                skip_token!(token_stream);
                start_pos = from_stream_pos!(token_stream => Some);
                from_stream_pos!(token_stream => DataDeclarationScope::Cloud)
            }
            _ => DataDeclarationScope::Unset,
        };
        let dec_type = match peek_token!(token_stream) {
            Token::VarKeyword => {
                skip_token!(token_stream);
                if start_pos == None {
                    start_pos = from_stream_pos!(token_stream => Some);
                }
                from_stream_pos!(token_stream => SingleDataDeclarationType::Var)
            }
            Token::ListKeyword => {
                skip_token!(token_stream);
                if start_pos == None {
                    start_pos = from_stream_pos!(token_stream => Some);
                }
                from_stream_pos!(token_stream => SingleDataDeclarationType::List)
            }
            Token::VarsKeyword => {
                skip_token!(token_stream);
                let vars_keyword = from_stream_pos!(token_stream => VarsKeyword);
                if start_pos == None {
                    start_pos = from_stream_pos!(token_stream => Some);
                }
                let left_brace = expect_token!(
                    token_stream,
                    Token::LeftBrace => from_stream_pos!(token_stream => LeftBrace),
                    "Expected '{'.",
                    "'{'"
                );
                let declarations = parse_inner_single_declarations(
                    token_stream,
                    context,
                    DefaultDataDeclarationType::Var,
                )?;
                let right_brace = expect_token!(
                    token_stream,
                    Token::RightBrace(lexer::LexedRightBrace::Normal) =>
                        from_stream_pos!(token_stream => RightBrace),
                    "Expected '}'.",
                    "'}'"
                );
                return Ok(Statement::DataDeclaration(
                    LetKeyword(let_keyword_position),
                    MultiDataDeclaration::Vars(
                        scope,
                        vars_keyword,
                        left_brace,
                        declarations,
                        right_brace,
                        (start_pos.unwrap().0, get_token_end!(token_stream)),
                    ),
                    (let_keyword_position.0, get_token_end!(token_stream)),
                ));
            }
            Token::ListsKeyword => {
                skip_token!(token_stream);
                let lists_keyword = from_stream_pos!(token_stream => ListsKeyword);
                if start_pos == None {
                    start_pos = from_stream_pos!(token_stream => Some);
                }
                let left_brace = expect_token!(
                    token_stream,
                    Token::LeftBrace => from_stream_pos!(token_stream => LeftBrace),
                    "Expected '{'.",
                    "'{'"
                );
                let declarations = parse_inner_single_declarations(
                    token_stream,
                    context,
                    DefaultDataDeclarationType::List,
                )?;
                let right_brace = expect_token!(
                    token_stream,
                    Token::RightBrace(lexer::LexedRightBrace::Normal) =>
                        from_stream_pos!(token_stream => RightBrace),
                    "Expected '}'.",
                    "'}'"
                );
                return Ok(Statement::DataDeclaration(
                    LetKeyword(let_keyword_position),
                    MultiDataDeclaration::Lists(
                        scope,
                        lists_keyword,
                        left_brace,
                        declarations,
                        right_brace,
                        (start_pos.unwrap().0, get_token_end!(token_stream)),
                    ),
                    (let_keyword_position.0, get_token_end!(token_stream)),
                ));
            }
            Token::LeftParens => {
                skip_token!(token_stream);
                let left_parens = from_stream_pos!(token_stream => LeftParens);
                if start_pos == None {
                    start_pos = from_stream_pos!(token_stream => Some);
                }
                let declarations = parse_inner_single_declarations(
                    token_stream,
                    context,
                    DefaultDataDeclarationType::Var,
                )?;
                let right_parens = expect_token!(
                    token_stream,
                    Token::RightParens => from_stream_pos!(token_stream => RightParens),
                    "Expected ')'.",
                    "')'"
                );
                return Ok(Statement::DataDeclaration(
                    LetKeyword(let_keyword_position),
                    MultiDataDeclaration::Mixed(
                        scope,
                        left_parens,
                        declarations,
                        right_parens,
                        (start_pos.unwrap().0, get_token_end!(token_stream)),
                    ),
                    (let_keyword_position.0, get_token_end!(token_stream)),
                ));
            }
            _ => SingleDataDeclarationType::Unset,
        };
        let canonical_identifier = consume_and_use_if!(token_stream, Token::CanonicalName(value) => {
            let value = value.clone();
            if start_pos == None {
                start_pos = from_stream_pos!(token_stream => Some);
            }
            CanonicalIdentifier {
                name: value,
                pos_range: get_token_position!(token_stream),
            }
        });
        let identifier = if let Token::Identifier(value) = next_token!(token_stream) {
            if start_pos == None {
                start_pos = from_stream_pos!(token_stream => Some);
            }
            Identifier {
                scope: Vec::new(),
                names: vec![(value, get_token_position!(token_stream))],
                pos_range: get_token_position!(token_stream),
            }
        } else {
            match (scope, dec_type, canonical_identifier) {
                (DataDeclarationScope::Unset, SingleDataDeclarationType::Unset, None) => {
                    emit_unexpected_token!(
                        token_stream,
                        "Expected scope modifer, declaration type, canonical identifier or identifier.",
                        "scope modifer, declaration type, canonical identifier or identifier"
                    );
                }
                (_, SingleDataDeclarationType::Unset, None) => {
                    emit_unexpected_token!(
                        token_stream,
                        "Expected declaration type, canonical identifier or identifier.",
                        "declaration type, canonical identifier or identifier"
                    );
                }
                (_, _, None) => {
                    emit_unexpected_token!(
                        token_stream,
                        "Expected canonical identifier or identifier.",
                        "canonical identifier or identifier"
                    );
                }
                (_, _, Some(_)) => {
                    emit_unexpected_token!(token_stream, "Expected identifier.", "identifier");
                }
            }
        };
        let value = match peek_token!(token_stream) {
            // Token::LeftBrace => {
            //     match dec_type {
            //         SingleDataDeclarationType::Unset => {
            //             skip_token!(token_stream);
            //             emit_unexpected_token!(token_stream, "Expected '='.", "'='")
            //         }
            //         SingleDataDeclarationType::Var(_) => {
            //             skip_token!(token_stream);
            //             emit_unexpected_token!(token_stream, "Expected '='.", "'='")
            //         }
            //         SingleDataDeclarationType::List(_) => (),
            //     }
            //     let (left_brace, list_content, right_brace) =
            //         parse_list_content(token_stream, context)?;

            //     DeclarationValue::List(left_brace, list_content, right_brace)
            // }
            Token::Assign => {
                skip_token!(token_stream);
                let assignment_operator = from_stream_pos!(token_stream => AssignmentOperator);

                if let Token::LeftBrace = peek_token!(token_stream) {
                    if !matches!(dec_type, SingleDataDeclarationType::List(_)) {
                        emit_unexpected_token!(
                            token_stream,
                            "Expected an expression.",
                            "an expression"
                        );
                    }
                    let (left_brace, list_content, right_brace) =
                        parse_list_content(token_stream, context)?;
                    DeclarationValue::List(
                        assignment_operator,
                        left_brace,
                        list_content,
                        right_brace,
                    )
                } else {
                    if matches!(dec_type, SingleDataDeclarationType::List(_)) {
                        emit_unexpected_token!(token_stream, "Expected '{'.", "'{'");
                    }
                    DeclarationValue::Var(
                        assignment_operator,
                        parse_expression(token_stream, context)?,
                    )
                }
            }
            Token::Semicolon => DeclarationValue::None,
            _ => {
                skip_token!(token_stream);
                emit_unexpected_token!(token_stream, "Expected '=', '{' or ';'", "'=', '{' or ';'");
            }
        };
        let declaration = match value {
            DeclarationValue::None => match dec_type {
                SingleDataDeclarationType::Unset => SingleDataDeclaration::EmptyVariable(
                    None,
                    scope,
                    canonical_identifier,
                    identifier,
                    (start_pos.unwrap().0, get_token_end!(token_stream)),
                ),
                SingleDataDeclarationType::Var(p) => SingleDataDeclaration::EmptyVariable(
                    Some(VarKeyword(p)),
                    scope,
                    canonical_identifier,
                    identifier,
                    (start_pos.unwrap().0, get_token_end!(token_stream)),
                ),
                SingleDataDeclarationType::List(p) => SingleDataDeclaration::EmptyList(
                    Some(ListKeyword(p)),
                    scope,
                    canonical_identifier,
                    identifier,
                    (start_pos.unwrap().0, get_token_end!(token_stream)),
                ),
            },
            DeclarationValue::Var(assignment_operator, literal) => SingleDataDeclaration::Variable(
                match dec_type {
                    SingleDataDeclarationType::Unset => None,
                    SingleDataDeclarationType::Var(p) => Some(VarKeyword(p)),
                    SingleDataDeclarationType::List(_) => panic!(),
                },
                scope,
                canonical_identifier,
                identifier,
                assignment_operator,
                literal,
                (start_pos.unwrap().0, get_token_end!(token_stream)),
            ),
            DeclarationValue::List(assignment_operator, left_brace, items, right_brace) => {
                SingleDataDeclaration::List(
                    match dec_type {
                        SingleDataDeclarationType::Unset => None,
                        SingleDataDeclarationType::Var(_) => panic!(),
                        SingleDataDeclarationType::List(p) => Some(ListKeyword(p)),
                    },
                    scope,
                    canonical_identifier,
                    identifier,
                    assignment_operator,
                    left_brace,
                    items,
                    right_brace,
                    (start_pos.unwrap().0, get_token_end!(token_stream)),
                )
            }
        };
        Ok(Statement::DataDeclaration(
            LetKeyword(let_keyword_position),
            MultiDataDeclaration::Single(declaration),
            (let_keyword_position.0, get_token_end!(token_stream)),
        ))
    }

    pub fn parse_assignment(
        token_stream: ParseIn,
        context: &mut ParseContext,
        identifier: Identifier,
    ) -> ParseOut<Statement> {
        let start_pos = get_token_start!(token_stream);
        expect_token!(token_stream, Token::Assign => (), "Expected '='.", "'='");
        let assignment_operator = from_stream_pos!(token_stream => AssignmentOperator);
        let expression = parse_expression(token_stream, context)?;
        Ok(Statement::Assignment(
            identifier,
            assignment_operator,
            expression,
            (start_pos, get_token_end!(token_stream)),
        ))
    }

    pub fn parse_call_or_control(
        token_stream: ParseIn,
        context: &mut ParseContext,
        identifier: Identifier,
    ) -> ParseOut<Statement> {
        let start_pos = get_token_start!(token_stream);
        expect_token!(token_stream, Token::LeftParens => (), "Expected '('", "'('");
        let (left_parens, expressions, right_parens) =
            parse_expression_list(token_stream, context)?;
        if peek_token!(token_stream) == &Token::LeftBrace {
            let expression = match expressions.len() {
                0 => {
                    let pos_range = (left_parens.0.0, right_parens.0.1);
                    Expression::Literal(ast::Literal::EmptyExpression(
                        left_parens,
                        right_parens,
                        pos_range,
                    ))
                }
                1 => {
                    let pos_range = (left_parens.0.0, right_parens.0.1);
                    Expression::Parentheses(
                        left_parens,
                        Box::new(expressions.into_iter().next().unwrap()),
                        right_parens,
                        pos_range,
                    )
                }
                _ => emit_unexpected_token!(
                    token_stream,
                    "Control blocks cannot have multiple expression inputs.",
                    "';'"
                ),
            };
            let code_block = parse_code_block(token_stream, context)?;
            if identifier.is_if() {
                let initial_code_block = (identifier, expression, code_block);
                let mut code_blocks = Vec::<(Identifier, Identifier, Expression, CodeBlock)>::new();
                let final_code_block = loop {
                    match match peek_token!(token_stream => Option) {
                        Some(value) => value,
                        None => break None,
                    } {
                        Token::Semicolon => break None,
                        Token::Identifier(_) => {
                            let else_identifier = parse_full_identifier(token_stream, context)?;
                            if !else_identifier.is_else() {
                                emit_unexpected_token!(
                                    token_stream,
                                    "Expected \"else\" identifier.",
                                    "\"else\" identifier"
                                );
                            }
                            if matches!(peek_token!(token_stream), Token::Identifier(_)) {
                                let if_identifier = parse_full_identifier(token_stream, context)?;
                                if !if_identifier.is_if() {
                                    emit_unexpected_token!(
                                        token_stream,
                                        "Expected \"if\" identifier.",
                                        "\"if\" identifier"
                                    );
                                }
                                code_blocks.push((
                                    else_identifier,
                                    if_identifier,
                                    parse_expression(token_stream, context)?,
                                    parse_code_block(token_stream, context)?,
                                ));
                            } else {
                                break Some((
                                    else_identifier,
                                    parse_code_block(token_stream, context)?,
                                ));
                            }
                        }
                        _ => break None,
                    }
                };
                return Ok(Statement::IfElse(
                    initial_code_block,
                    code_blocks,
                    final_code_block,
                    (start_pos, get_token_end!(token_stream)),
                ));
            }
            return Ok(Statement::Control(
                identifier,
                expression,
                code_block,
                (start_pos, get_token_end!(token_stream)),
            ));
        }
        Ok(Statement::Call(
            identifier,
            left_parens,
            expressions,
            right_parens,
            (start_pos, get_token_end!(token_stream)),
        ))
    }

    pub fn parse_forever(
        token_stream: ParseIn,
        context: &mut ParseContext,
        identifier: Identifier,
    ) -> ParseOut<Statement> {
        let start_pos = get_token_start!(token_stream);
        if peek_token!(token_stream) != &Token::LeftBrace {
            emit_unexpected_token!(token_stream, "Expected '{'", "'{'");
        }
        let code_block = parse_code_block(token_stream, context)?;
        Ok(Statement::Forever(
            identifier,
            code_block,
            (start_pos, get_token_end!(token_stream)),
        ))
    }

    #[inline]
    pub(crate) fn parse_statement_after_identifier(
        token_stream: ParseIn,
        context: &mut ParseContext,
        identifier: Identifier,
    ) -> ParseOut<Statement> {
        match peek_token!(token_stream) {
            Token::Assign => parse_assignment(token_stream, context, identifier),
            Token::LeftParens => parse_call_or_control(token_stream, context, identifier),
            Token::LeftBrace => {
                if !identifier.is_forever() {
                    skip_token!(token_stream);
                    emit_unexpected_token!(token_stream, "Expected '=' or '('.", "'=' or '('");
                }
                parse_forever(token_stream, context, identifier)
            }
            Token::LeftBracket => {
                let start_pos = get_token_start!(token_stream);
                skip_token!(token_stream);
                let left_bracket = from_stream_pos!(token_stream => LeftBracket);
                let item = parse_expression(token_stream, context)?;
                let right_bracket = expect_token!(
                    token_stream,
                    Token::RightBracket => from_stream_pos!(token_stream => RightBracket),
                    "Expected ']'.",
                    "']'"
                );
                let assignment_operator = expect_token!(
                    token_stream,
                    Token::Assign => from_stream_pos!(token_stream => AssignmentOperator),
                    "Expected '='.",
                    "'='"
                );
                let value = parse_expression(token_stream, context)?;
                Ok(Statement::SetItem(
                    identifier,
                    left_bracket,
                    item,
                    right_bracket,
                    assignment_operator,
                    value,
                    (start_pos, get_token_end!(token_stream)),
                ))
            }
            _ => {
                skip_token!(token_stream);
                emit_unexpected_token!(token_stream, "Expected '=' or '('.", "'=' or '('");
            }
        }
    }
}

/// Statements do not include semicolons.
pub fn parse_statement(token_stream: ParseIn, context: &mut ParseContext) -> ParseOut<Statement> {
    match peek_token!(token_stream) {
        Token::LetKeyword => {
            let dec = statement::parse_data_declaration(token_stream, context)?;
            todo!("implement registering");
            Ok(dec)
        }
        Token::Identifier(_) => {
            let identifier = parse_full_identifier(token_stream, context)?;
            statement::parse_statement_after_identifier(token_stream, context, identifier)
        }
        Token::Semicolon => Ok(Statement::EmptyStatement((
            get_token_end!(token_stream), // It doesn't include the semicolon and so doesn't include anything.
            get_token_end!(token_stream), // Therefore it must be zero width.
        ))),
        _ => todo!(),
    }
}

pub fn parse_code_block(token_stream: ParseIn, context: &mut ParseContext) -> ParseOut<CodeBlock> {
    expect_token!(token_stream, Token::LeftBrace => (), "Expected '{'.", "'{'");
    let start_pos = get_token_start!(token_stream);
    let left_brace = from_stream_pos!(token_stream => ast::LeftBrace);
    let mut statements = Vec::<(Statement, Semicolon)>::new();
    consume_then_never_if!(token_stream, Token::RightBrace(lexer::LexedRightBrace::Normal) =>
        return Ok(CodeBlock {
            left_brace,
            statements,
            right_brace: from_stream_pos!(token_stream => ast::RightBrace),
            pos_range: (start_pos, get_token_end!(token_stream)),
        })
    );
    loop {
        statements.push((
            parse_statement(token_stream, context)?,
            expect_token!(
                token_stream,
                Token::Semicolon => from_stream_pos!(token_stream => Semicolon),
                "Expected ';'.",
                "';'"
            ),
        ));
        consume_then_never_if!(token_stream, Token::RightBrace(lexer::LexedRightBrace::Normal) => {
            break;
        });
    }
    Ok(CodeBlock {
        left_brace,
        statements,
        right_brace: from_stream_pos!(token_stream => ast::RightBrace),
        pos_range: (start_pos, get_token_end!(token_stream)),
    })
}

pub mod expression {
    use std::collections::VecDeque;

    use crate::parser::ast::{
        Associativity, FormattedStringContent, GetPos, LeftBracket, LeftParens, RightBracket,
        RightParens,
    };

    use super::*;
    pub fn parse_expression_without_binops(
        token_stream: ParseIn,
        context: &mut ParseContext,
    ) -> ParseOut<Expression> {
        let token = next_token!(token_stream);
        let token_position = get_token_position!(token_stream);
        use super::super::ast::Literal as LLiteral;
        use super::super::ast::UnOp;
        use Expression::Literal as ELiteral;
        match token {
            Token::SimpleString(string) => {
                Ok(ELiteral(LLiteral::String(string.clone(), token_position)))
            }
            Token::DecimalInt(string) => Ok(ELiteral(LLiteral::DecimalInt(
                string.clone(),
                token_position,
            ))),
            Token::DecimalFloat(string) => Ok(ELiteral(LLiteral::DecimalFloat(
                string.clone(),
                token_position,
            ))),
            Token::HexadecimalInt(string) => Ok(ELiteral(LLiteral::HexadecimalInt(
                string.clone(),
                token_position,
            ))),
            Token::OctalInt(string) => {
                Ok(ELiteral(LLiteral::OctalInt(string.clone(), token_position)))
            }
            Token::BinaryInt(string) => Ok(ELiteral(LLiteral::BinaryInt(
                string.clone(),
                token_position,
            ))),
            Token::Minus => Ok(Expression::UnOp(
                UnOp::Minus(token_position),
                Box::new(parse_expression_without_binops(token_stream, context)?),
                (token_position.0, get_token_end!(token_stream)),
            )),
            Token::Not => Ok(Expression::UnOp(
                UnOp::Not(token_position),
                Box::new(parse_expression_without_binops(token_stream, context)?),
                (token_position.0, get_token_end!(token_stream)),
            )),
            Token::Exp => Ok(Expression::UnOp(
                UnOp::Exp(token_position),
                Box::new(parse_expression_without_binops(token_stream, context)?),
                (token_position.0, get_token_end!(token_stream)),
            )),
            Token::Pow => Ok(Expression::UnOp(
                UnOp::Pow(token_position),
                Box::new(parse_expression_without_binops(token_stream, context)?),
                (token_position.0, get_token_end!(token_stream)),
            )),
            Token::Identifier(value) => {
                let identifier = parse_full_identifier_starting_with(token_stream, context, value)?;
                match peek_token!(token_stream => Option) {
                    Some(Token::LeftParens) => {
                        let (left_parens, expressions, right_parens) =
                            parse_expression_list(token_stream, context)?;
                        Ok(Expression::Call(
                            identifier,
                            left_parens,
                            expressions,
                            right_parens,
                            (token_position.0, get_token_end!(token_stream)),
                        ))
                    }
                    Some(Token::LeftBracket) => {
                        skip_token!(token_stream);
                        Ok(Expression::GetItem(
                            identifier,
                            from_stream_pos!(token_stream => LeftBracket),
                            Box::new(parse_expression(token_stream, context)?),
                            {
                                expect_token!(token_stream, Token::RightBracket => (), "Expected ']'.", "']'");
                                from_stream_pos!(token_stream => RightBracket)
                            },
                            (token_position.0, get_token_end!(token_stream)),
                        ))
                    }
                    _ => Ok(Expression::Identifier(identifier)),
                }
            }
            Token::LeftParens => {
                let left_parens = LeftParens(token_position);
                consume_then_never_if!(token_stream, Token::RightParens => {
                    return Ok(ELiteral(LLiteral::EmptyExpression(
                        left_parens,
                        from_stream_pos!(token_stream => RightParens),
                        (token_position.0, get_token_end!(token_stream)),
                    )));
                });
                let expr = parse_expression(token_stream, context)?;
                expect_token!(token_stream, Token::RightParens => (), "Expected ')'.", "')'");
                let end_pos = get_token_end!(token_stream);
                Ok(Expression::Parentheses(
                    left_parens,
                    Box::new(expr),
                    from_stream_pos!(token_stream => RightParens),
                    (token_position.0, end_pos),
                ))
            }
            Token::LeftFormattedString(string) => {
                let mut expressions = Vec::<FormattedStringContent>::new();
                if !string.is_empty() {
                    expressions.push(FormattedStringContent::String(string, token_position));
                }
                loop {
                    expressions.push(FormattedStringContent::Expression(parse_expression(
                        token_stream,
                        context,
                    )?));
                    match next_token!(token_stream) {
                        Token::RightBrace(lexer::LexedRightBrace::MiddleFormattedString(
                            string,
                        )) => {
                            if !string.is_empty() {
                                expressions.push(FormattedStringContent::String(
                                    string,
                                    get_token_position!(token_stream),
                                ));
                            }
                        }
                        Token::RightBrace(lexer::LexedRightBrace::RightFormattedString(string)) => {
                            if !string.is_empty() {
                                expressions.push(FormattedStringContent::String(
                                    string,
                                    get_token_position!(token_stream),
                                ));
                            }
                            break;
                        }
                        _ => {
                            emit_unexpected_token!(token_stream, "Expected '}'.", "'}'");
                        }
                    }
                }
                Ok(Expression::FormattedString(
                    expressions,
                    (token_position.0, get_token_end!(token_stream)),
                ))
            }
            _ => emit_unexpected_token!(token_stream, "Expected an expression.", "an expression"),
        }
    }

    pub fn parse_binary_operation(
        token_stream: ParseIn,
        context: &mut ParseContext,
    ) -> ParseOut<Option<BinOp>> {
        use super::super::ast::BinOp;
        Ok(Some(match_token_or_return_none!(token_stream, {
            Token::Plus => BinOp::Plus,
            Token::Minus => BinOp::Minus,
            Token::Times => BinOp::Times,
            Token::Div => BinOp::Div,
            Token::Mod => BinOp::Mod,
            Token::Join => BinOp::Join,
            Token::And => BinOp::And,
            Token::Or => BinOp::Or,
            Token::Equals => BinOp::Equals,
            Token::NotEquals => BinOp::NotEquals,
            Token::LessThan => BinOp::LessThan,
            Token::GreaterThan => BinOp::GreaterThan,
            Token::LessThanOrEqual => BinOp::LessThanOrEqual,
            Token::GreaterThanOrEqual => BinOp::GreaterThanOrEqual,
        })))
    }

    /**
     `expressions` should have length `n` and `binops` should have length `n - 1`.
    Panics if this is not the case.
     */
    pub fn order_operations(
        mut expressions: VecDeque<Expression>,
        mut binops: VecDeque<BinOp>,
    ) -> Expression {
        let mut output_stack = Vec::<Expression>::new();
        let mut operator_stack = Vec::<BinOp>::new();
        for _ in 0..binops.len() {
            output_stack.push(expressions.pop_front().unwrap());
            let current_op = binops.pop_front().unwrap();
            let (current_prec, current_associativity) = current_op.get_precedence();
            loop {
                let (prev_prec, _) = match operator_stack.last() {
                    Some(value) => value,
                    None => break,
                }
                .get_precedence();
                if prev_prec < current_prec
                    || (prev_prec == current_prec && current_associativity != Associativity::Left)
                {
                    break;
                }
                let b = Box::new(output_stack.pop().unwrap());
                let b_pos = b.as_ref().get_position().1;
                let a = Box::new(output_stack.pop().unwrap());
                let a_pos = a.as_ref().get_position().0;
                let op = operator_stack.pop().unwrap();
                output_stack.push(Expression::BinOp(a, op, b, (a_pos, b_pos)));
            }
            operator_stack.push(current_op);
        }
        output_stack.push(expressions.pop_front().unwrap());
        while !operator_stack.is_empty() {
            let b = Box::new(output_stack.pop().unwrap());
            let b_pos = b.as_ref().get_position().1;
            let a = Box::new(output_stack.pop().unwrap());
            let a_pos = a.as_ref().get_position().0;
            let op = operator_stack.pop().unwrap();
            output_stack.push(Expression::BinOp(a, op, b, (a_pos, b_pos)));
        }
        output_stack.pop().unwrap()
    }
}

pub fn parse_expression(token_stream: ParseIn, context: &mut ParseContext) -> ParseOut<Expression> {
    use expression::*;
    let mut expressions = VecDeque::from([parse_expression_without_binops(token_stream, context)?]);
    let mut binops = VecDeque::<BinOp>::new();

    loop {
        binops.push_back(match parse_binary_operation(token_stream, context)? {
            Some(value) => value,
            None => break,
        });
        expressions.push_back(parse_expression_without_binops(token_stream, context)?);
    }

    Ok(order_operations(expressions, binops))
}

pub fn parse_expression_list(
    token_stream: ParseIn,
    context: &mut ParseContext,
) -> ParseOut<(ast::LeftParens, Vec<Expression>, ast::RightParens)> {
    expect_token!(token_stream, Token::LeftParens => (), "Expected '('.", "'('");
    let left_parens = from_stream_pos!(token_stream => ast::LeftParens);
    let mut expressions = Vec::<Expression>::new();
    loop {
        consume_then_never_if!(token_stream, Token::RightParens => {
            break;
        });
        expressions.push(parse_expression(token_stream, context)?);
        match next_token!(token_stream) {
            Token::Comma => (),
            Token::RightParens => break,
            _ => {
                emit_unexpected_token!(token_stream, "Expected a comma or ')'.", "a comma or ')'");
            }
        }
    }
    Ok((
        left_parens,
        expressions,
        from_stream_pos!(token_stream => ast::RightParens),
    ))
}
