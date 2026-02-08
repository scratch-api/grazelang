use super::ast::{BinOp, Expression, ParseError};
use crate::{
    lexer::{
        PosRange, Token, get_pos_range as internal_get_pos_range,
        get_position as internal_get_position,
    },
    names::Namespace,
    parser::ast::{self, Identifier, Statement},
};
use arcstr::ArcStr as IString;
use logos::Lexer;
use std::{collections::VecDeque, iter::Peekable};

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
    let mut namespace = Namespace::default();
    parse_lexed_entrypoint(make_parse_in!(lex), &mut namespace);
}

pub fn parse_lexed_entrypoint(token_stream: ParseIn, namespace: &mut Namespace) {
    todo!()
}

pub fn parse_single_identifier(
    token_stream: ParseIn,
    namespace: &mut Namespace,
) -> ParseOut<IString> {
    if let Token::Identifier(value) = next_token!(token_stream) {
        return Ok(value);
    }
    emit_unexpected_token!(token_stream, "Expected an identifier.", "an identifier");
}

pub fn parse_full_identifier(
    token_stream: ParseIn,
    namespace: &mut Namespace,
) -> ParseOut<Identifier> {
    let mut names: Vec<(IString, PosRange)> = vec![if let Token::Identifier(value) =
        next_token!(token_stream)
    {
        (value, get_token_position!(token_stream))
    } else {
        emit_unexpected_token!(token_stream, "Expected an identifier.", "an identifier");
    }];
    let start_pos = get_token_start!(token_stream);
    let mut scope: Option<Vec<(IString, PosRange)>> = None;
    loop {
        match peek_token!(token_stream) {
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
        if let Token::Identifier(value) = next_token!(token_stream) {
            names.push((value, get_token_position!(token_stream)));
        } else {
            emit_unexpected_token!(token_stream, "Expected an identifier.", "an identifier");
        }
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
    namespace: &mut Namespace,
    value: IString,
) -> ParseOut<Identifier> {
    let mut names: Vec<(IString, PosRange)> = vec![(value, get_token_position!(token_stream))];
    let start_pos = get_token_start!(token_stream);
    let mut scope: Option<Vec<(IString, PosRange)>> = None;
    loop {
        match peek_token!(token_stream) {
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
        if let Token::Identifier(value) = next_token!(token_stream) {
            names.push((value, get_token_position!(token_stream)));
        } else {
            emit_unexpected_token!(token_stream, "Expected an identifier.", "an identifier");
        }
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

/// `parse_*` `fn`s contained don't swallow semicolons.
pub mod statement {
    use core::panic;

    use serde::{Deserialize, Serialize};

    use crate::{
        lexer,
        parser::ast::{
            AssignmentOperator, CanonicalIdentifier, DataDeclarationScope, LeftBrace, LeftParens,
            LetKeyword, ListEntry, ListKeyword, ListsKeyword, MultiDataDeclaration, RightBrace,
            RightParens, SingleDataDeclaration, SingleDataDeclarationType, VarKeyword, VarsKeyword,
        },
    };

    use super::*;

    pub fn register_single_data_declaration(
        namespace: &mut Namespace,
        single_data_declaration: &SingleDataDeclaration,
    ) {
        todo!()
    }

    pub fn parse_literal(
        token_stream: ParseIn,
        namespace: &mut Namespace,
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
                if peek_token!(token_stream) == &Token::RightParens {
                    let left_parens_position = get_token_position!(token_stream);
                    return Ok(LLiteral::EmptyExpression(
                        LeftParens(left_parens_position),
                        {
                            skip_token!(token_stream);
                            from_stream_pos!(token_stream => RightParens)
                        },
                        (left_parens_position.0, get_token_end!(token_stream)),
                    ));
                }
                let value = parse_literal(token_stream, namespace)?;
                if next_token!(token_stream) != Token::RightParens {
                    emit_unexpected_token!(token_stream, "Expected ')'.", "')'");
                }
                Ok(value)
            }
            _ => {
                emit_unexpected_token!(token_stream, "Expected a literal.", "a literal");
            }
        }
    }

    pub fn parse_list_content(
        token_stream: ParseIn,
        namespace: &mut Namespace,
    ) -> ParseOut<(LeftBrace, Vec<ast::ListEntry>, RightBrace)> {
        let mut entries = Vec::<ast::ListEntry>::new();
        let left_brace = if let Token::LeftBrace = next_token!(token_stream) {
            from_stream_pos!(token_stream => LeftBrace)
        } else {
            emit_unexpected_token!(token_stream, "Expected '{'.", "'{'");
        };
        let right_brace = loop {
            entries.push(match peek_token!(token_stream) {
                Token::RightBrace(lexer::LexedRightBrace::Normal) => {
                    skip_token!(token_stream);
                    break from_stream_pos!(token_stream => RightBrace);
                }
                Token::Unwrap => {
                    skip_token!(token_stream);
                    let unwrap_start = get_token_start!(token_stream);
                    if let Token::SimpleString(value) = next_token!(token_stream) {
                        ListEntry::Unwrap(
                            ast::Literal::String(value, get_token_position!(token_stream)),
                            (unwrap_start, get_token_end!(token_stream)),
                        )
                    } else {
                        emit_unexpected_token!(
                            token_stream,
                            "Expected a simple string literal.",
                            "a simple string literal"
                        );
                    }
                }
                token => ListEntry::Expression(parse_expression(token_stream, namespace)?),
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
        List(LeftBrace, Vec<ast::ListEntry>, RightBrace),
        Var(AssignmentOperator, Expression),
    }

    pub fn parse_inner_single_declarations(
        token_stream: ParseIn,
        namespace: &mut Namespace,
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
            let canonical_identifier =
                if let Token::CanonicalName(value) = peek_token!(token_stream) {
                    let value = value.clone();
                    skip_token!(token_stream);
                    if start_pos == None {
                        start_pos = Some(get_token_start!(token_stream));
                    }
                    Some(CanonicalIdentifier {
                        name: value,
                        pos_range: get_token_position!(token_stream),
                    })
                } else {
                    None
                };
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
                Token::LeftBrace => {
                    if default_type == DefaultDataDeclarationType::Var {
                        match dec_type {
                            SingleDataDeclarationType::Unset => {
                                skip_token!(token_stream);
                                emit_unexpected_token!(token_stream, "Expected '='.", "'='")
                            }
                            SingleDataDeclarationType::Var(_) => {
                                skip_token!(token_stream);
                                emit_unexpected_token!(token_stream, "Expected '='.", "'='")
                            }
                            _ => (),
                        }
                    }
                    let (left_brace, list_content, right_brace) =
                        parse_list_content(token_stream, namespace)?;

                    DeclarationValue::List(left_brace, list_content, right_brace)
                }
                Token::Assign => {
                    skip_token!(token_stream);
                    if default_type == DefaultDataDeclarationType::List {
                        match dec_type {
                            SingleDataDeclarationType::Unset => {
                                emit_unexpected_token!(token_stream, "Expected '{'.", "'{'")
                            }
                            SingleDataDeclarationType::List(_) => {
                                emit_unexpected_token!(token_stream, "Expected '{'.", "'{'")
                            }
                            _ => (),
                        }
                    }
                    let assignment_operator =
                        from_stream_pos!(token_stream => AssignmentOperator::Set);

                    DeclarationValue::Var(
                        assignment_operator,
                        parse_expression(token_stream, namespace)?,
                    )
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
                DeclarationValue::List(left_brace, items, right_brace) => {
                    SingleDataDeclaration::List(
                        match dec_type {
                            SingleDataDeclarationType::Unset => None,
                            SingleDataDeclarationType::Var(_) => None,
                            SingleDataDeclarationType::List(p) => Some(ListKeyword(p)),
                        },
                        scope,
                        canonical_identifier,
                        identifier,
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
        namespace: &mut Namespace,
    ) -> ParseOut<Statement> {
        if next_token!(token_stream) != Token::LetKeyword {
            emit_unexpected_token!(
                token_stream,
                "Data declaration needs to start with \"let\".",
                "\"let\""
            );
        }
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
                let left_brace = if let Token::LeftBrace = next_token!(token_stream) {
                    from_stream_pos!(token_stream => LeftBrace)
                } else {
                    emit_unexpected_token!(token_stream, "Expected '{'.", "'{'");
                };
                let declarations = parse_inner_single_declarations(
                    token_stream,
                    namespace,
                    DefaultDataDeclarationType::Var,
                )?;
                let right_brace = if let Token::RightBrace(lexer::LexedRightBrace::Normal) =
                    next_token!(token_stream)
                {
                    from_stream_pos!(token_stream => RightBrace)
                } else {
                    emit_unexpected_token!(token_stream, "Expected '{'.", "'{'");
                };
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
                let left_brace = if let Token::LeftBrace = next_token!(token_stream) {
                    from_stream_pos!(token_stream => LeftBrace)
                } else {
                    emit_unexpected_token!(token_stream, "Expected '{'.", "'{'");
                };
                let declarations = parse_inner_single_declarations(
                    token_stream,
                    namespace,
                    DefaultDataDeclarationType::List,
                )?;
                let right_brace = if let Token::RightBrace(lexer::LexedRightBrace::Normal) =
                    next_token!(token_stream)
                {
                    from_stream_pos!(token_stream => RightBrace)
                } else {
                    emit_unexpected_token!(token_stream, "Expected '}'.", "'}'");
                };
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
                    namespace,
                    DefaultDataDeclarationType::Var,
                )?;
                let right_parens = if let Token::RightParens = next_token!(token_stream) {
                    from_stream_pos!(token_stream => RightParens)
                } else {
                    emit_unexpected_token!(token_stream, "Expected ')'.", "')'");
                };
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
        let canonical_identifier = if let Token::CanonicalName(value) = peek_token!(token_stream) {
            let value = value.clone();
            skip_token!(token_stream);
            if start_pos == None {
                start_pos = from_stream_pos!(token_stream => Some);
            }
            Some(CanonicalIdentifier {
                name: value,
                pos_range: get_token_position!(token_stream),
            })
        } else {
            None
        };
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
            Token::LeftBrace => {
                match dec_type {
                    SingleDataDeclarationType::Unset => {
                        skip_token!(token_stream);
                        emit_unexpected_token!(token_stream, "Expected '='.", "'='")
                    }
                    SingleDataDeclarationType::Var(_) => {
                        skip_token!(token_stream);
                        emit_unexpected_token!(token_stream, "Expected '='.", "'='")
                    }
                    SingleDataDeclarationType::List(_) => (),
                }
                let (left_brace, list_content, right_brace) =
                    parse_list_content(token_stream, namespace)?;

                DeclarationValue::List(left_brace, list_content, right_brace)
            }
            Token::Assign => {
                skip_token!(token_stream);
                match dec_type {
                    SingleDataDeclarationType::Unset => (),
                    SingleDataDeclarationType::Var(_) => (),
                    SingleDataDeclarationType::List(_) => {
                        emit_unexpected_token!(token_stream, "Expected '{'.", "'{'")
                    }
                }
                let assignment_operator = from_stream_pos!(token_stream => AssignmentOperator::Set);

                DeclarationValue::Var(
                    assignment_operator,
                    parse_expression(token_stream, namespace)?,
                )
            }
            Token::Semicolon => DeclarationValue::None,
            _ => {
                skip_token!(token_stream);
                emit_unexpected_token!(token_stream, "Expected '=', '{' or ';'", "'=', '{' or ';'")
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
            DeclarationValue::List(left_brace, items, right_brace) => SingleDataDeclaration::List(
                match dec_type {
                    SingleDataDeclarationType::Unset => None,
                    SingleDataDeclarationType::Var(_) => panic!(),
                    SingleDataDeclarationType::List(p) => Some(ListKeyword(p)),
                },
                scope,
                canonical_identifier,
                identifier,
                left_brace,
                items,
                right_brace,
                (start_pos.unwrap().0, get_token_end!(token_stream)),
            ),
        };
        Ok(Statement::DataDeclaration(
            LetKeyword(let_keyword_position),
            MultiDataDeclaration::Single(declaration),
            (let_keyword_position.0, get_token_end!(token_stream)),
        ))

        // let start_token = next_token!(token_stream);
        // let start_position = get_token_position!(token_stream);
        // let mut current_token = start_token.clone();
        // let mut scope = DataDeclarationScope::ImplicitLocal;
        // let mut dec_type = DDT::ImplicitVar;
        // let mut canonical_identifier = Option::<CanonicalIdentifier>::None;
        // loop {
        //     match current_token {
        //         Token::LeftParens => {
        //             let mut declarations =
        //                 Vec::<(SingleDataDeclarationType, SingleDataDeclaration)>::new();
        //             if peek_token!(token_stream) == &Token::RightParens {
        //                 skip_token!(token_stream);
        //                 return Ok(Statement::DataDeclaration(
        //                     LetKeyword(let_keyword_position),
        //                     MultiDataDeclaration::Mixed(
        //                         LeftParens(start_position),
        //                         declarations,
        //                         from_stream_pos!(token_stream => RightParens),
        //                         (start_position.0, get_token_end!(token_stream)),
        //                     ),
        //                     (let_keyword_position.0, get_token_end!(token_stream)),
        //                 ));
        //             }
        //             loop {
        //                 declarations.push(todo!());
        //                 match next_token!(token_stream) {
        //                     Token::Comma => {
        //                         if let Token::RightParens = peek_token!(token_stream) {
        //                             break;
        //                         }
        //                     }
        //                     Token::RightParens => break,
        //                     _ => {
        //                         return Err(ParseError::UnexpectedToken(
        //                             "Expected ',' or ')'.",
        //                             "',' or ')'",
        //                             get_token_position!(token_stream),
        //                         ));
        //                     }
        //                 }
        //             }
        //             return Ok(Statement::DataDeclaration(
        //                 LetKeyword(let_keyword_position),
        //                 MultiDataDeclaration::Mixed(
        //                     LeftParens(start_position),
        //                     declarations,
        //                     from_stream_pos!(token_stream => RightParens),
        //                     (start_position.0, get_token_end!(token_stream)),
        //                 ),
        //                 (let_keyword_position.0, get_token_end!(token_stream)),
        //             ));
        //         }
        //         Token::Identifier(identifier) => {
        //             match dec_type {
        //                 DDT::Vars(_) => {
        //                     return Err(ParseError::UnexpectedToken(
        //                         "A vars declaration cannot be completed via assignment. Use \"let vars {<declarations>};\" instead.",
        //                         "'{'",
        //                         get_token_position!(token_stream),
        //                     ));
        //                 }
        //                 DDT::Lists(_) => {
        //                     return Err(ParseError::UnexpectedToken(
        //                         "A lists declaration cannot be completed via assignment. Use \"let lists {<declarations>};\" instead.",
        //                         "'{'",
        //                         get_token_position!(token_stream),
        //                     ));
        //                 }
        //                 _ => (),
        //             }
        //             match peek_token!(token_stream) {
        //                 Token::Assign => {
        //                     if let DDT::List(_) = &dec_type {
        //                         return Err(ParseError::UnexpectedToken(
        //                             "Cannot declare a list via assignment. Use \"let list <list_name>{<elements>};\" instead.",
        //                             "'{'",
        //                             get_token_position!(token_stream),
        //                         ));
        //                     }
        //                     skip_token!(token_stream);
        //                     let assignment_operator_pos = get_token_position!(token_stream);
        //                     let value = parse_literal(token_stream, namespace)?;
        //                     return Ok(Statement::DataDeclaration(
        //                         LetKeyword(let_keyword_position),
        //                         MultiDataDeclaration::Single(
        //                             dec_type.get_single_data_declaration_type().unwrap(),
        //                             SingleDataDeclaration::Variable(
        //                                 scope,
        //                                 canonical_identifier,
        //                                 Identifier {
        //                                     scope: Vec::new(),
        //                                     names: vec![(identifier.clone(), start_position)],
        //                                     pos_range: start_position,
        //                                 },
        //                                 ast::AssignmentOperator::Set(assignment_operator_pos),
        //                                 value,
        //                                 (start_position.0, get_token_end!(token_stream)),
        //                             ),
        //                         ),
        //                         (let_keyword_position.0, get_token_end!(token_stream)),
        //                     ));
        //                 }
        //                 Token::Semicolon => {
        //                     return Ok(Statement::DataDeclaration(
        //                         LetKeyword(let_keyword_position),
        //                         MultiDataDeclaration::Single(
        //                             dec_type.get_single_data_declaration_type().unwrap(),
        //                             SingleDataDeclaration::EmptyVariable(
        //                                 ast::DataDeclarationScope::ImplicitLocal,
        //                                 canonical_identifier,
        //                                 Identifier {
        //                                     scope: Vec::new(),
        //                                     names: vec![(identifier.clone(), start_position)],
        //                                     pos_range: start_position,
        //                                 },
        //                                 (start_position.0, get_token_end!(token_stream)),
        //                             ),
        //                         ),
        //                         (let_keyword_position.0, get_token_end!(token_stream)),
        //                     ));
        //                 }
        //                 _ => {
        //                     return Err(ParseError::UnexpectedToken(
        //                         "Expected an assignment operator or ';'.",
        //                         "an assignment operator or ';'.",
        //                         get_token_position!(token_stream),
        //                     ));
        //                 }
        //             }
        //         }
        //         Token::GlobalKeyword => {
        //             if dec_type != DDT::ImplicitVar {
        //                 return Err(ParseError::UnexpectedToken(
        //                     "Scope modifier should appear before declaration type.",
        //                     "a canonical name or an identifier",
        //                     get_token_position!(token_stream),
        //                 ));
        //             }
        //             if scope != DataDeclarationScope::ImplicitLocal {
        //                 return Err(ParseError::UnexpectedToken(
        //                     "Cannot set the scope of a variable or list multiple times.",
        //                     "a canonical name, an identifier, \"var\", \"list\", \"vars\" or \"lists\"",
        //                     get_token_position!(token_stream),
        //                 ));
        //             }
        //             scope = from_stream_pos!(token_stream => DataDeclarationScope::Global);
        //         }
        //         Token::LocalKeyword => {
        //             if dec_type != DDT::ImplicitVar {
        //                 return Err(ParseError::UnexpectedToken(
        //                     "Scope modifier should appear before declaration type.",
        //                     "a canonical name or an identifier",
        //                     get_token_position!(token_stream),
        //                 ));
        //             }
        //             if scope != DataDeclarationScope::ImplicitLocal {
        //                 return Err(ParseError::UnexpectedToken(
        //                     "Cannot set the scope of a variable or list multiple times.",
        //                     "a canonical name, an identifier, \"var\", \"list\", \"vars\" or \"lists\"",
        //                     get_token_position!(token_stream),
        //                 ));
        //             }
        //             scope = from_stream_pos!(token_stream => DataDeclarationScope::Local);
        //         }
        //         Token::CloudKeyword => {
        //             if dec_type != DDT::ImplicitVar {
        //                 return Err(ParseError::UnexpectedToken(
        //                     "Scope modifier should appear before declaration type.",
        //                     "a canonical name or an identifier",
        //                     get_token_position!(token_stream),
        //                 ));
        //             }
        //             if scope != DataDeclarationScope::ImplicitLocal {
        //                 return Err(ParseError::UnexpectedToken(
        //                     "Cannot set the scope of a variable or list multiple times.",
        //                     "a canonical name, an identifier, \"var\", \"list\", \"vars\" or \"lists\"",
        //                     get_token_position!(token_stream),
        //                 ));
        //             }
        //             scope = from_stream_pos!(token_stream => DataDeclarationScope::Cloud);
        //         }
        //         Token::VarKeyword => {
        //             if dec_type != DDT::ImplicitVar {
        //                 return Err(ParseError::UnexpectedToken(
        //                     "Cannot set the type of a declaration multiple times.",
        //                     "a canonical name, an identifier, a modifier or '('",
        //                     get_token_position!(token_stream),
        //                 ));
        //             }
        //             dec_type = from_stream_pos!(token_stream => DDT::Var);
        //         }
        //         Token::ListKeyword => {
        //             if let DataDeclarationScope::Cloud(_) = scope {
        //                 return Err(ParseError::UnexpectedToken(
        //                     "Cannot set the type of a declaration to list for global declarations.",
        //                     "a canonical name, an identifier, \"var\" or \"vars\"",
        //                     get_token_position!(token_stream),
        //                 ));
        //             }
        //             if dec_type != DDT::ImplicitVar {
        //                 return Err(ParseError::UnexpectedToken(
        //                     "Cannot set the type of a declaration multiple times.",
        //                     "a canonical name, an identifier, a modifier or '('",
        //                     get_token_position!(token_stream),
        //                 ));
        //             }
        //             dec_type = from_stream_pos!(token_stream => DDT::List);
        //         }
        //         Token::VarsKeyword => {
        //             if dec_type != DDT::ImplicitVar {
        //                 return Err(ParseError::UnexpectedToken(
        //                     "Cannot set the type of a declaration multiple times.",
        //                     "a canonical name, an identifier, a modifier or '('",
        //                     get_token_position!(token_stream),
        //                 ));
        //             }
        //             dec_type = from_stream_pos!(token_stream => DDT::Vars);
        //         }
        //         Token::ListsKeyword => {
        //             if let DataDeclarationScope::Cloud(_) = scope {
        //                 return Err(ParseError::UnexpectedToken(
        //                     "Cannot set the type of a declaration to lists for global declarations.",
        //                     "a canonical name, an identifier, \"var\" or \"vars\"",
        //                     get_token_position!(token_stream),
        //                 ));
        //             }
        //             if dec_type != DDT::ImplicitVar {
        //                 return Err(ParseError::UnexpectedToken(
        //                     "Cannot set the type of a declaration multiple times.",
        //                     "a canonical name, an identifier, a modifier or '('",
        //                     get_token_position!(token_stream),
        //                 ));
        //             }
        //             dec_type = from_stream_pos!(token_stream => DDT::Lists);
        //         }
        //         Token::LeftBrace => {
        //             let left_brace = from_stream_pos!(token_stream => LeftBrace);
        //             match dec_type {
        //                 DDT::Vars(_) => (),
        //                 DDT::Lists(_) => (),
        //                 _ => {
        //                     return Err(ParseError::UnexpectedToken(
        //                         "May only use batch data declaration with vars and lists.",
        //                         if dec_type == DDT::ImplicitVar {
        //                             "an identifier or a modifier"
        //                         } else {
        //                             "an identifier"
        //                         },
        //                         get_token_position!(token_stream),
        //                     ));
        //                 }
        //             }
        //             let declarations =
        //                 parse_inner_single_declarations(token_stream, namespace, &scope, &dec_type)?;
        //             let right_brace_pos = match next_token!(token_stream) {
        //                 Token::RightBrace => get_token_position!(token_stream),
        //                 _ => {
        //                     return Err(ParseError::UnexpectedToken(
        //                         "Expected '}'.",
        //                         "'}'",
        //                         get_token_position!(token_stream),
        //                     ));
        //                 }
        //             };
        //             return match dec_type {
        //                 DDT::Vars(p) => Ok(Statement::DataDeclaration(
        //                     LetKeyword(let_keyword_position),
        //                     MultiDataDeclaration::Vars(
        //                         VarsKeyword(p),
        //                         left_brace,
        //                         declarations,
        //                         RightBrace(right_brace_pos),
        //                         (p.0, right_brace_pos.1),
        //                     ),
        //                     (let_keyword_position.0, right_brace_pos.1),
        //                 )),
        //                 DDT::Lists(p) => Ok(Statement::DataDeclaration(
        //                     LetKeyword(let_keyword_position),
        //                     MultiDataDeclaration::Lists(
        //                         ListsKeyword(p),
        //                         left_brace,
        //                         declarations,
        //                         RightBrace(right_brace_pos),
        //                         (p.0, right_brace_pos.1),
        //                     ),
        //                     (let_keyword_position.0, right_brace_pos.1),
        //                 )),
        //                 _ => panic!(), // Should never happen
        //             };
        //         }
        //         Token::CanonicalName(value) => {
        //             if canonical_identifier != None {
        //                 return Err(ParseError::UnexpectedToken(
        //                     "Cannot set the canonical name of a declaration multiple times.",
        //                     "an identifier, a modifier, '(', \"var\", \"list\", \"vars\" or \"lists\"",
        //                     get_token_position!(token_stream),
        //                 ));
        //             }
        //             canonical_identifier = Some(CanonicalIdentifier {
        //                 name: value,
        //                 pos_range: get_token_position!(token_stream),
        //             })
        //         }
        //         _ => {
        //             return Err(ParseError::UnexpectedToken(
        //                 "Expected a canonical name, an identifier, a modifier, '(', \"var\", \"list\", \"vars\" or \"lists\".",
        //                 "a canonical name, an identifier, a modifier, '(', \"var\", \"list\", \"vars\" or \"lists\"",
        //                 get_token_position!(token_stream),
        //             ));
        //         }
        //     }
        //     current_token = next_token!(token_stream);
        // }
    }
}

pub fn parse_statement(token_stream: ParseIn, namespace: &mut Namespace) -> ParseOut<Statement> {
    todo!()
}

pub mod expression {
    use std::collections::VecDeque;

    use crate::{
        lexer,
        parser::ast::{
            Associativity, FormattedStringContent, GetPos, LeftBracket, LeftParens, RightBracket,
            RightParens,
        },
    };

    use super::*;
    pub fn parse_expression_without_binops(
        token_stream: ParseIn,
        namespace: &mut Namespace,
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
                Box::new(parse_expression_without_binops(token_stream, namespace)?),
                (token_position.0, get_token_end!(token_stream)),
            )),
            Token::Not => Ok(Expression::UnOp(
                UnOp::Not(token_position),
                Box::new(parse_expression_without_binops(token_stream, namespace)?),
                (token_position.0, get_token_end!(token_stream)),
            )),
            Token::Exp => Ok(Expression::UnOp(
                UnOp::Exp(token_position),
                Box::new(parse_expression_without_binops(token_stream, namespace)?),
                (token_position.0, get_token_end!(token_stream)),
            )),
            Token::Pow => Ok(Expression::UnOp(
                UnOp::Pow(token_position),
                Box::new(parse_expression_without_binops(token_stream, namespace)?),
                (token_position.0, get_token_end!(token_stream)),
            )),
            Token::Identifier(value) => {
                let identifier =
                    parse_full_identifier_starting_with(token_stream, namespace, value)?;
                match peek_token!(token_stream) {
                    Token::LeftParens => {
                        let (left_parens, expressions, right_parens) =
                            parse_expression_list(token_stream, namespace)?;
                        Ok(Expression::Call(
                            identifier,
                            left_parens,
                            expressions,
                            right_parens,
                            (token_position.0, get_token_end!(token_stream)),
                        ))
                    }
                    Token::LeftBracket => {
                        skip_token!(token_stream);
                        Ok(Expression::GetItem(
                            identifier,
                            from_stream_pos!(token_stream => LeftBracket),
                            Box::new(parse_expression(token_stream, namespace)?),
                            {
                                if next_token!(token_stream) != Token::RightBracket {
                                    emit_unexpected_token!(token_stream, "Expected ']'.", "']'");
                                }
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
                if peek_token!(token_stream) == &Token::RightParens {
                    skip_token!(token_stream);
                    return Ok(ELiteral(LLiteral::EmptyExpression(
                        left_parens,
                        from_stream_pos!(token_stream => RightParens),
                        (token_position.0, get_token_end!(token_stream)),
                    )));
                }
                let expr = parse_expression(token_stream, namespace)?;
                let end_pos = if let Token::RightParens = next_token!(token_stream) {
                    get_token_end!(token_stream)
                } else {
                    emit_unexpected_token!(token_stream, "Excected ')'.", "')'");
                };
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
                        namespace,
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
        namespace: &mut Namespace,
    ) -> ParseOut<Option<BinOp>> {
        let token = peek_token!(optional token_stream);
        use super::super::ast::BinOp;
        let result = match token {
            Token::Plus => from_stream_pos!(token_stream => BinOp::Plus),
            Token::Minus => from_stream_pos!(token_stream => BinOp::Minus),
            Token::Times => from_stream_pos!(token_stream => BinOp::Times),
            Token::Div => from_stream_pos!(token_stream => BinOp::Div),
            Token::Mod => from_stream_pos!(token_stream => BinOp::Mod),
            Token::Join => from_stream_pos!(token_stream => BinOp::Join),
            Token::And => from_stream_pos!(token_stream => BinOp::And),
            Token::Or => from_stream_pos!(token_stream => BinOp::Or),
            Token::Equals => from_stream_pos!(token_stream => BinOp::Equals),
            Token::NotEquals => from_stream_pos!(token_stream => BinOp::NotEquals),
            Token::LessThan => from_stream_pos!(token_stream => BinOp::LessThan),
            Token::GreaterThan => from_stream_pos!(token_stream => BinOp::GreaterThan),
            Token::LessThanOrEqual => from_stream_pos!(token_stream => BinOp::LessThanOrEqual),
            Token::GreaterThanOrEqual => {
                from_stream_pos!(token_stream => BinOp::GreaterThanOrEqual)
            }
            _ => return Ok(None),
        };
        skip_token!(token_stream);
        Ok(Some(result))
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

pub fn parse_expression(token_stream: ParseIn, namespace: &mut Namespace) -> ParseOut<Expression> {
    use expression::*;
    let mut expressions =
        VecDeque::from([parse_expression_without_binops(token_stream, namespace)?]);
    let mut binops = VecDeque::<BinOp>::new();

    loop {
        binops.push_back(match parse_binary_operation(token_stream, namespace)? {
            Some(value) => value,
            None => break,
        });
        expressions.push_back(parse_expression_without_binops(token_stream, namespace)?);
    }

    Ok(order_operations(expressions, binops))
}

pub fn parse_expression_list(
    token_stream: ParseIn,
    namespace: &mut Namespace,
) -> ParseOut<(ast::LeftParens, Vec<Expression>, ast::RightParens)> {
    if next_token!(token_stream) != Token::LeftParens {
        emit_unexpected_token!(token_stream, "Expected '('.", "'('");
    }
    let left_parens = from_stream_pos!(token_stream => ast::LeftParens);
    let mut expressions = Vec::<Expression>::new();
    if peek_token!(token_stream) == &Token::RightParens {
        skip_token!(token_stream);
        return Ok((
            left_parens,
            expressions,
            from_stream_pos!(token_stream => ast::RightParens),
        ));
    }
    loop {
        expressions.push(parse_expression(token_stream, namespace)?);
        match next_token!(token_stream) {
            Token::Comma => {
                if let Token::RightParens = peek_token!(token_stream) {
                    break;
                }
            }
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
