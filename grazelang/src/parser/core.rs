use super::{
    context::ParseContext,
    cst::{
        self, BinOp, CodeBlock, Expression, Identifier, ParseError, SpriteStatement,
        StageStatement, Statement,
    },
};
use crate::{
    lexer::{
        self, SourceFileId, SourceSpan, TextSpan, Token,
        get_source_span as internal_get_source_span,
    },
    messages::GrazeMessage,
    parser::{
        context::{self, BroadcastDescriptor},
        cst::{GetPos, GrazeProgram, SpriteCodeBlock, StageCodeBlock, TopLevelStatement},
    },
    settings::GrazeMessageSetting,
};
use arcstr::{ArcStr as IString, literal};
use logos::{Lexer, Logos};
use std::{collections::VecDeque, vec};

#[cfg(feature = "include_context_in_parse_errors")]
macro_rules! static_current_context {
    () => {
        concat!(
            "file: ",
            file!(),
            ", line: ",
            line!(),
            ", column: ",
            column!()
        )
    };
}

macro_rules! expect_token {
    ($token_stream:expr, $pattern:pat => $value:expr, $message:expr, $expected:expr) => {{
        match next_token!($token_stream) {
            $pattern => $value,
            token => emit_unexpected_token!($token_stream, $message, $expected, token),
        }
    }};
}

macro_rules! expect_token_or_message {
    ($token_stream:expr, $context:expr, $pattern:pat => $value:expr, $message:expr, $expected:expr, $default_value:expr) => {{
        match next_token!($token_stream) {
            $pattern => $value,
            token => {
                emit_unexpected_token_message(
                    $context,
                    $token_stream,
                    literal!($message),
                    literal!($expected),
                    #[cfg(feature = "include_context_in_parse_errors")]
                    literal!(static_current_context!()),
                    token,
                )?;
                return $default_value;
            }
        }
    }};
}

/// The difference between this and `consume_if` is that we cannot use values from the match in `consume_if` due to the borrow checker.
macro_rules! consume_and_use_if {
    ($token_stream:expr, $pattern:pat => $body:expr) => {{
        #[expect(unused_variables)]
        if matches!(peek_token!($token_stream => Option), Some($pattern)) {
            match next_token!($token_stream) {
                $pattern => Some($body),
                _ => unreachable!(),
            }
        } else {
            None
        }
    }};
}

macro_rules! consume_if {
    ($token_stream:expr, $pattern:path => $body:expr) => {{
        match peek_token!($token_stream => Option) {
            Some($pattern) => {
                skip_token!($token_stream);
                Some($body)
            }
            _ => None,
        }
    }};
}

macro_rules! consume_then_never_if {
    ($token_stream:expr, $pattern:pat => $body:expr) => {{
        if matches!(peek_token!($token_stream => Option), Some($pattern)) {
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
        match $token_stream.peek() {
            Some(Ok(value)) => value,
            Some(Err(_)) => {
                return {
                    Err(ParseError::LexerStuck {
                        #[cfg(feature = "include_context_in_parse_errors")]
                        context: literal!(static_current_context!()),
                        source_span: get_token_source_span($token_stream),
                    })
                }
            }
            None => {
                return Err(ParseError::UnexpectedEndOfInput {
                    #[cfg(feature = "include_context_in_parse_errors")]
                    context: literal!(static_current_context!()),
                    source_span: get_token_source_span($token_stream),
                })
            }
        }
    };
    (optional $token_stream:expr) => {
        match $token_stream.peek() {
            Some(Ok(value)) => value,
            Some(Err(_)) => {
                return Err(ParseError::LexerStuck {
                    #[cfg(feature = "include_context_in_parse_errors")]
                    context: literal!(static_current_context!()),
                    source_span: get_token_source_span($token_stream),
                })
            }
            None => return Ok(None),
        }
    };
    ($token_stream:expr => Option) => {
        match $token_stream.peek() {
            Some(Ok(value)) => Some(value),
            Some(Err(_)) => {
                return Err(ParseError::LexerStuck {
                    #[cfg(feature = "include_context_in_parse_errors")]
                    context: literal!(static_current_context!()),
                    source_span: get_token_source_span($token_stream),
                })
            }
            None => None,
        }
    };
}

macro_rules! peek_back {
    ($token_stream:expr) => {
        match $token_stream.peek_back() {
            Some(Some(Ok(value))) => value,
            Some(Some(Err(_))) => {
                return {
                    Err(ParseError::LexerStuck {
                        #[cfg(feature = "include_context_in_parse_errors")]
                        context: literal!(static_current_context!()),
                        source_span: get_token_source_span($token_stream),
                    })
                }
            }
            Some(None) => {
                return Err(ParseError::UnexpectedEndOfInput {
                    #[cfg(feature = "include_context_in_parse_errors")]
                    context: literal!(static_current_context!()),
                    source_span: get_token_source_span($token_stream),
                })
            }
            None => {
                return Err(ParseError::PeekedBackAtBeginning {
                    #[cfg(feature = "include_context_in_parse_errors")]
                    context: literal!(static_current_context!()),
                    source_span: (Default::default(), $token_stream.source_file_id),
                })
            }
        }
    };
    (optional $token_stream:expr) => {
        match $token_stream.peek_back() {
            Some(Some(Ok(value))) => value,
            Some(Some(Err(_))) => {
                return Err(ParseError::LexerStuck {
                    #[cfg(feature = "include_context_in_parse_errors")]
                    context: literal!(static_current_context!()),
                    source_span: get_token_source_span($token_stream),
                })
            }
            None | Some(None) => return Ok(None),
        }
    };
    ($token_stream:expr => Option) => {
        match $token_stream.peek_back() {
            Some(Some(Ok(value))) => Some(value),
            Some(Some(Err(_))) => {
                return Err(ParseError::LexerStuck {
                    #[cfg(feature = "include_context_in_parse_errors")]
                    context: literal!(static_current_context!()),
                    source_span: get_token_source_span($token_stream),
                })
            }
            None | Some(None) => None,
        }
    };
}

macro_rules! next_token {
    ($token_stream:expr) => {{
        match $token_stream.next() {
            Some(Ok(value)) => value,
            Some(Err(_)) => {
                return Err(ParseError::LexerStuck {
                    #[cfg(feature = "include_context_in_parse_errors")]
                    context: literal!(static_current_context!()),
                    source_span: get_token_source_span($token_stream),
                });
            }
            None => {
                return Err(ParseError::UnexpectedEndOfInput {
                    #[cfg(feature = "include_context_in_parse_errors")]
                    context: literal!(static_current_context!()),
                    source_span: get_token_source_span($token_stream),
                });
            }
        }
    }};
    (optional $token_stream:expr) => {{
        match $token_stream.next() {
            Some(Ok(value)) => value,
            Some(Err(_)) => {
                return Err(ParseError::LexerStuck {
                    #[cfg(feature = "include_context_in_parse_errors")]
                    context: literal!(static_current_context!()),
                    source_span: get_token_source_span($token_stream),
                });
            }
            None => return Ok(None),
        }
    }};
    ($token_stream:expr => Option) => {{
        match $token_stream.next() {
            Some(Ok(value)) => Some(value),
            Some(Err(_)) => {
                return Err(ParseError::LexerStuck {
                    #[cfg(feature = "include_context_in_parse_errors")]
                    context: literal!(static_current_context!()),
                    source_span: get_token_source_span($token_stream),
                });
            }
            None => None,
        }
    }};
}

macro_rules! skip_token {
    ($token_stream:expr) => {{
        if let Some(Err(_)) = $token_stream.next() {
            return Err(ParseError::LexerStuck {
                #[cfg(feature = "include_context_in_parse_errors")]
                context: literal!(static_current_context!()),
                source_span: get_token_source_span($token_stream),
            });
        }
    }};
}

pub fn get_token_source_span(token_stream: ParseIn) -> SourceSpan {
    (token_stream.span_memoize(), token_stream.source_file_id)
}

pub fn get_token_start(token_stream: ParseIn) -> (usize, usize) {
    token_stream.span_memoize().0
}

pub fn get_token_end(token_stream: ParseIn) -> (usize, usize) {
    token_stream.span_memoize().1
}

macro_rules! emit_unexpected_token {
    ($token_stream:expr, $message:expr, $expected:expr, $found:expr) => {
        return Err(create_unexpected_token_error(
            $token_stream,
            literal!($message),
            literal!($expected),
            #[cfg(feature = "include_context_in_parse_errors")]
            literal!(static_current_context!()),
            $found,
        ))
    };
}

pub fn create_unexpected_token_error(
    token_stream: ParseIn,
    message: IString,
    expected: IString,
    #[cfg(feature = "include_context_in_parse_errors")] context: IString,
    found: Token,
) -> ParseError {
    ParseError::UnexpectedToken {
        message,
        expected,
        #[cfg(feature = "include_context_in_parse_errors")]
        context,
        found,
        source_span: get_token_source_span(token_stream),
    }
}

macro_rules! try_or_emit_message {
    ($value:expr, $context:expr, $default_value:expr) => {
        match $value {
            Ok(value) => value,
            Err(err) => {
                $context.successful = false;
                match $context.settings.message_setting {
                    GrazeMessageSetting::ExitOnError => {
                        $context.messages.push(err.clone().into());
                        return Err(err);
                    }
                    GrazeMessageSetting::ExitOnErrorUnlogged => return Err(err),
                    _ => (),
                }
                emit_message($context, err.into(), GrazeMessageSetting::Errors);
                return $default_value;
            }
        }
    };
}

macro_rules! find_statement_end_and_return_invalid {
    ($token_stream:expr, $start_pos:expr, $stmt_type:ty) => {{
        statement::find_statement_end($token_stream)?;
        Ok(<$stmt_type>::InvalidStatement(
            $token_stream.span_from_previous_to_current($start_pos),
        ))
    }};
}

macro_rules! find_top_level_statement_end_and_return_invalid {
    ($token_stream:expr, $start_pos:expr, $stmt_type:ty) => {{
        statement::find_top_level_statement_end($token_stream)?;
        Ok(<$stmt_type>::InvalidStatement(
            $token_stream.span_from_previous_to_current($start_pos),
        ))
    }};
}

macro_rules! parse_comma_separated {
    ($token_stream:expr, $context:expr, ($token_stream_ident:ident, $context_ident:ident) => $parse:expr, $end:pat, $end_pat_repr:expr) => {{
        let token_stream = &mut *$token_stream;
        let context = &mut *$context;
        let mut start_pos = None;
        let mut values = Vec::new();
        let tail_value = loop {
            if matches!(peek_token!(token_stream), $end) {
                start_pos.get_or_insert_with(|| get_token_start(token_stream));
                break None;
            }
            let value = {
                let $token_stream_ident = &mut *token_stream;
                let $context_ident = &mut *context;
                $parse
            };
            start_pos.get_or_insert_with(|| value.get_source_span().0.0);
            let comma = match peek_token!(token_stream) {
                Token::Comma => {
                    skip_token!(token_stream);
                    cst::Comma(get_token_source_span(token_stream))
                }
                $end => break Some(value),
                _ => {
                    let token = next_token!(token_stream);
                    emit_unexpected_token!(
                        token_stream,
                        concat!("Expected a comma or ", $end_pat_repr, "."),
                        concat!("a comma or ", $end_pat_repr),
                        token
                    );
                }
            };
            values.push((value, comma));
        };
        cst::CommaSeparated {
            values,
            tail_value,
            source_span: token_stream.span_from_previous_to_current(start_pos.unwrap()),
        }
    }};
}

pub fn emit_message(
    context: &mut ParseContext,
    message: GrazeMessage,
    message_type: GrazeMessageSetting,
) {
    if context.settings.message_setting >= message_type {
        context.messages.push(message);
    };
}

pub fn find_next_token<F>(token_stream: ParseIn, mut predicate: F) -> Result<(), ParseError>
where
    F: FnMut(&Token) -> bool,
{
    if predicate(peek_back!(token_stream)) {
        return Ok(());
    }
    loop {
        if predicate(&next_token!(token_stream)) {
            return Ok(());
        }
    }
}

pub fn emit_unexpected_token_message(
    context: &mut ParseContext,
    token_stream: ParseIn,
    message: IString,
    expected: IString,
    #[cfg(feature = "include_context_in_parse_errors")] code_context: IString,
    found: Token,
) -> ParseOut<()> {
    context.successful = false;
    let source_span = get_token_source_span(token_stream);
    if matches!(
        context.settings.message_setting,
        GrazeMessageSetting::ExitOnError | GrazeMessageSetting::ExitOnErrorUnlogged
    ) {
        if context.settings.message_setting == GrazeMessageSetting::ExitOnError {
            context.messages.push(
                ParseError::UnexpectedToken {
                    message: message.clone(),
                    expected: expected.clone(),
                    #[cfg(feature = "include_context_in_parse_errors")]
                    context: code_context.clone(),
                    found: found.clone(),
                    source_span,
                }
                .into(),
            );
        }
        return Err(ParseError::UnexpectedToken {
            message,
            expected,
            #[cfg(feature = "include_context_in_parse_errors")]
            context: code_context,
            found,
            source_span,
        });
    }
    emit_message(
        context,
        ParseError::UnexpectedToken {
            message,
            expected,
            #[cfg(feature = "include_context_in_parse_errors")]
            context: code_context,
            found,
            source_span,
        }
        .into(),
        GrazeMessageSetting::ExitOnError,
    );
    Ok(())
}

macro_rules! from_stream_pos {
    ($token_stream:expr => $node:path) => {
        $node(get_token_source_span($token_stream))
    };
}

macro_rules! with_mut_next_target {
    ($context:expr, $var:ident => $body:expr) => {
        if let Some($var) = &mut $context.next_target {
            $body
        }
    };
}

macro_rules! with_mut_target_scope_or_global_scope {
    ($context:expr, $stage:expr, $var:ident => $body:expr) => {
        if let Some($var) = if $stage {
            Some(&mut $context.global_symbols)
        } else {
            $context
                .next_target
                .as_mut()
                .map(|value| value.borrow_symbols_mut())
        } {
            $body
        }
    };
}

type ParseIn<'a, 'b> = &'a mut PeekableLexer<'b>;
type ParseOut<T> = Result<T, ParseError>;

#[derive(Debug, Clone)]
/// [`PeekableLexer`] has two possible states:
/// 1. peeked
/// 2. unpeeked
///
/// When peeked, `current_token` is `Some(_)`, `current_span` is `Some(_)`
///
/// When unpeeked, `current_token` is `None(_)`
pub struct PeekableLexer<'a> {
    /// Do not access directly
    pub lexer: Lexer<'a, Token>,
    /// Do not access directly
    pub current_token: Option<Option<Result<Token, <Token as Logos<'a>>::Error>>>,
    /// Do not access directly
    pub current_span: Option<TextSpan>,
    /// Do not access directly
    pub previous_token: Option<Option<Result<Token, <Token as Logos<'a>>::Error>>>,
    /// Do not access directly
    pub previous_span: Option<TextSpan>,
    pub source_file_id: SourceFileId,
}

impl<'a> Iterator for PeekableLexer<'a> {
    type Item = Result<Token, <Token as Logos<'a>>::Error>;
    /// Puts the PeekableLexer into unpeeked state
    fn next(&mut self) -> Option<Result<Token, <Token as Logos<'a>>::Error>> {
        // whether self is peeked
        if let Some(token) = self.current_token.take() {
            self.previous_span = self.current_span.take();
            self.previous_token = Some(token.clone());
            return token;
        }
        // self is unpeeked
        self.previous_span = Some(self.span());
        self.current_span = None;
        let token = self.lexer.next();
        self.previous_token = Some(token.clone());
        token
    }
}

impl<'a> PeekableLexer<'a> {
    pub fn new(lexer: Lexer<'a, Token>, source_file_id: SourceFileId) -> Self {
        Self {
            lexer,
            current_span: None,
            current_token: None,
            previous_token: None,
            previous_span: None,
            source_file_id,
        }
    }

    /// Puts the PeekableLexer into peeked state
    pub fn peek(&mut self) -> Option<&Result<Token, <Token as Logos<'a>>::Error>> {
        self.current_token
            .get_or_insert_with(|| {
                if self.current_span.is_none() {
                    self.current_span =
                        Some(internal_get_source_span(&self.lexer, self.lexer.span()));
                }
                self.lexer.next()
            })
            .as_ref()
        // if let Some(peeked) = &self.peeked_token {
        //     return Some(peeked);
        // }
        // self.peeked_token = self.lexer.next();
        // self.peeked_token.as_ref()
    }

    pub fn span(&self) -> TextSpan {
        self.current_span
            .unwrap_or_else(|| internal_get_source_span(&self.lexer, self.lexer.span()))
    }

    pub fn peek_back(&self) -> Option<Option<&Result<Token, <Token as Logos<'a>>::Error>>> {
        self.previous_token.as_ref().map(Option::as_ref)
    }

    pub fn span_back(&self) -> Option<TextSpan> {
        self.previous_span
    }

    pub fn step_back_if_unpeeked(&mut self) {
        if self.current_token.is_none() {
            self.current_token = self.previous_token.take();
            self.current_span = self.previous_span.take();
        }
    }

    pub fn span_memoize(&mut self) -> TextSpan {
        if let Some(current_span) = self.current_span {
            return current_span;
        }
        let current_span = internal_get_source_span(&self.lexer, self.lexer.span());
        self.current_span = Some(current_span);
        current_span
    }
}

impl<'a> PeekableLexer<'a> {
    #[inline]
    pub fn span_from_previous_to_current(&mut self, previous: (usize, usize)) -> SourceSpan {
        ((previous, get_token_end(self)), self.source_file_id)
    }
}

impl<'a> PeekableLexer<'a> {
    fn substitude_unexpected_token_message<F, T>(
        &mut self,
        action: F,
        message: IString,
        expected: IString,
    ) -> ParseOut<T>
    where
        F: FnOnce(&mut Self) -> ParseOut<T>,
    {
        self.peek();
        let peeked_span = internal_get_source_span(&self.lexer, self.lexer.span());
        action(self).map_err(|mut err| {
            if let ParseError::UnexpectedToken {
                expected: current_expected,
                message: current_message,
                #[cfg(feature = "include_context_in_parse_errors")]
                    context: _,
                found: _,
                source_span,
            } = &mut err
                && source_span.0 == peeked_span
            {
                *current_expected = expected;
                *current_message = message;
            }
            err
        })
    }
}

pub fn parse_graze_program(
    token_stream: ParseIn,
    context: &mut ParseContext,
) -> ParseOut<GrazeProgram> {
    let mut statements = Vec::<TopLevelStatement>::new();
    loop {
        if peek_token!(token_stream => Option).is_none() {
            break;
        }
        statements.push(parse_top_level_statement(token_stream, context)?);
    }
    Ok(GrazeProgram(statements))
}

pub fn parse_single_identifier(
    token_stream: ParseIn,
    _context: &mut ParseContext,
) -> ParseOut<IString> {
    Ok(match next_token!(token_stream) {
        Token::Identifier(value) => value,
        Token::StageKeyword => literal!("stage"),
        Token::VarsKeyword => literal!("vars"),
        Token::ListsKeyword => literal!("lists"),
        token => emit_unexpected_token!(
            token_stream,
            "Expected an identifier.",
            "an identifier",
            token
        ),
    })
}

pub fn parse_single_identifier_as_identifier(
    token_stream: ParseIn,
    context: &mut ParseContext,
) -> ParseOut<Identifier> {
    Ok(Identifier {
        path: Vec::new(),
        fields: vec![(
            parse_single_identifier(token_stream, context)?,
            get_token_source_span(token_stream),
        )],
        source_span: get_token_source_span(token_stream),
    })
}

pub fn parse_full_identifier(
    token_stream: ParseIn,
    context: &mut ParseContext,
) -> ParseOut<Identifier> {
    let mut names: Vec<(IString, SourceSpan)> = vec![(
        parse_single_identifier(token_stream, context)?,
        get_token_source_span(token_stream),
    )];
    let start_pos = get_token_start(token_stream);
    let mut path: Option<Vec<(IString, SourceSpan)>> = None;
    loop {
        match match peek_token!(token_stream => Option) {
            Some(value) => value,
            None => break,
        } {
            Token::ScopeResolution => {
                if path.is_some() {
                    let token = next_token!(token_stream);
                    emit_unexpected_token!(token_stream, "Expected a dot.", "a dot", token);
                }
            }
            Token::Dot => {
                if path.is_none() {
                    path = Some(names);
                    names = Vec::new();
                }
            }
            _ => break,
        }
        skip_token!(token_stream);
        names.push({
            match next_token!(token_stream) {
                Token::Identifier(value) => (value, get_token_source_span(token_stream)),
                Token::StageKeyword => (literal!("stage"), get_token_source_span(token_stream)),
                Token::VarsKeyword => (literal!("vars"), get_token_source_span(token_stream)),
                Token::ListsKeyword => (literal!("lists"), get_token_source_span(token_stream)),
                token => emit_unexpected_token!(
                    token_stream,
                    "Expected an identifier.",
                    "an identifier",
                    token
                ),
            }
        });
    }
    if path.is_none() {
        path = Some(std::mem::take(&mut names));
    }
    Ok(Identifier {
        path: path.unwrap_or_default(),
        fields: names,
        source_span: token_stream.span_from_previous_to_current(start_pos),
    })
}

pub fn parse_full_identifier_starting_with(
    token_stream: ParseIn,
    _context: &mut ParseContext,
    value: IString,
) -> ParseOut<Identifier> {
    let mut names: Vec<(IString, SourceSpan)> = vec![(value, get_token_source_span(token_stream))];
    let start_pos = get_token_start(token_stream);
    let mut scope: Option<Vec<(IString, SourceSpan)>> = None;
    loop {
        match match peek_token!(token_stream => Option) {
            Some(value) => value,
            None => break,
        } {
            Token::ScopeResolution => {
                if scope.is_some() {
                    let token = next_token!(token_stream);
                    emit_unexpected_token!(token_stream, "Expected a dot.", "a dot", token);
                }
            }
            Token::Dot => {
                if scope.is_none() {
                    scope = Some(names);
                    names = Vec::new();
                }
            }
            _ => break,
        }
        skip_token!(token_stream);
        names.push({
            match next_token!(token_stream) {
                Token::Identifier(value) => (value, get_token_source_span(token_stream)),
                Token::StageKeyword => (literal!("stage"), get_token_source_span(token_stream)),
                Token::VarsKeyword => (literal!("vars"), get_token_source_span(token_stream)),
                Token::ListsKeyword => (literal!("lists"), get_token_source_span(token_stream)),
                token => emit_unexpected_token!(
                    token_stream,
                    "Expected an identifier.",
                    "an identifier",
                    token
                ),
            }
        });
    }
    if names.len() > 1 && scope.is_none() {
        scope = Some(names);
        names = Vec::new();
    }
    Ok(Identifier {
        path: scope.unwrap_or_default(),
        fields: names,
        source_span: token_stream.span_from_previous_to_current(start_pos),
    })
}

// Statements *do* include semicolons.
pub mod statement {
    use std::collections::HashMap;

    use serde::{Deserialize, Serialize};

    use crate::{
        lexer::{self, LexedRightBrace},
        parser::cst::{
            AssetDeclaration, CanonicalIdentifier, Comma, CustomBlockParamKind,
            CustomBlockParamKindValue, DataDeclaration, DataDeclarationScope, EMPTY_ISTRING_REF,
            LeftBrace, LeftBracket, LeftParens, LetKeyword, ListEntry, ListKeyword, ListsKeyword,
            NormalAssignmentOperator, ProcKeyword, RightBrace, RightBracket, RightParens,
            Semicolon, SingleAssetDeclaration, SingleDataDeclaration, SingleDataDeclarationType,
            SyntacticElse, SyntacticIf, VarKeyword, VarsKeyword, WarpSpecifier,
        },
    };

    use super::*;

    pub fn parse_literal(
        token_stream: ParseIn,
        _context: &mut ParseContext,
    ) -> ParseOut<cst::Literal> {
        let token = next_token!(token_stream);
        use cst::Literal as LLiteral;
        match token {
            Token::SimpleString(value) => {
                Ok(LLiteral::String(value, get_token_source_span(token_stream)))
            }
            Token::DecimalInt(value) => Ok(LLiteral::DecimalInt(
                value,
                get_token_source_span(token_stream),
            )),
            Token::DecimalFloat(value) => Ok(LLiteral::DecimalFloat(
                value,
                get_token_source_span(token_stream),
            )),
            Token::HexadecimalInt(value) => Ok(LLiteral::HexadecimalInt(
                value,
                get_token_source_span(token_stream),
            )),
            Token::OctalInt(value) => Ok(LLiteral::OctalInt(
                value,
                get_token_source_span(token_stream),
            )),
            Token::BinaryInt(value) => Ok(LLiteral::BinaryInt(
                value,
                get_token_source_span(token_stream),
            )),
            Token::LeftParens => {
                let left_parens_position = get_token_source_span(token_stream);
                expect_token!(
                    token_stream,
                    Token::RightParens => {
                        Ok(LLiteral::EmptyExpression(
                            LeftParens(left_parens_position),
                            from_stream_pos!(token_stream => RightParens),
                            token_stream.span_from_previous_to_current(left_parens_position.0.0),
                        ))
                    },
                    "Expected ')'.",
                    "')'"
                )
            }
            Token::Plus => {
                let prefix_source_span = get_token_source_span(token_stream);
                let token = next_token!(token_stream);
                if get_token_start(token_stream) != prefix_source_span.0.1 {
                    emit_unexpected_token!(
                        token_stream,
                        "Expected a decimal integer or decimal float immediately following the plus.",
                        "a decimal integer or decimal float immediately following the plus",
                        token
                    );
                }
                match token {
                    Token::DecimalInt(value) => Ok(LLiteral::DecimalInt(
                        arcstr::format!("+{}", value.as_str()),
                        token_stream.span_from_previous_to_current(prefix_source_span.0.0),
                    )),
                    Token::DecimalFloat(value) => Ok(LLiteral::DecimalFloat(
                        arcstr::format!("+{}", value.as_str()),
                        token_stream.span_from_previous_to_current(prefix_source_span.0.0),
                    )),
                    token => {
                        emit_unexpected_token!(
                            token_stream,
                            "Expected a decimal integer or decimal float.",
                            "a decimal integer or decimal float",
                            token
                        );
                    }
                }
            }
            Token::Minus => {
                let prefix_source_span = get_token_source_span(token_stream);
                let token = next_token!(token_stream);
                if get_token_start(token_stream) != prefix_source_span.0.1 {
                    emit_unexpected_token!(
                        token_stream,
                        "Expected a decimal integer or decimal float immediately following the minus.",
                        "a decimal integer or decimal float immediately following the minus",
                        token
                    );
                }
                match token {
                    Token::DecimalInt(value) => Ok(LLiteral::DecimalInt(
                        arcstr::format!("-{}", value.as_str()),
                        token_stream.span_from_previous_to_current(prefix_source_span.0.0),
                    )),
                    Token::DecimalFloat(value) => Ok(LLiteral::DecimalFloat(
                        arcstr::format!("-{}", value.as_str()),
                        token_stream.span_from_previous_to_current(prefix_source_span.0.0),
                    )),
                    token => {
                        emit_unexpected_token!(
                            token_stream,
                            "Expected a decimal integer or decimal float.",
                            "a decimal integer or decimal float",
                            token
                        );
                    }
                }
            }
            token => {
                emit_unexpected_token!(token_stream, "Expected a literal.", "a literal", token);
            }
        }
    }

    pub fn parse_list_content(
        token_stream: ParseIn,
        context: &mut ParseContext,
    ) -> ParseOut<(
        LeftBracket,
        cst::CommaSeparated<cst::ListEntry>,
        RightBracket,
    )> {
        let left_bracket = expect_token!(
            token_stream,
            Token::LeftBracket => from_stream_pos!(token_stream => LeftBracket),
            "Expected '['.",
            "'['"
        );
        let values = parse_comma_separated!(
            token_stream,
            context,
            (token_stream, context) => match peek_token!(token_stream) {
                Token::Unwrap => {
                    skip_token!(token_stream);
                    let unwrap_start = get_token_start(token_stream);
                    expect_token!(
                        token_stream,
                        Token::SimpleString(value) => ListEntry::Unwrap(
                            cst::Literal::String(value, get_token_source_span(token_stream)),
                            token_stream.span_from_previous_to_current(unwrap_start),
                        ),
                        "Expected a simple string literal.",
                        "a simple string literal"
                    )
                }
                _ => ListEntry::Expression(token_stream.substitude_unexpected_token_message(
                    |token_stream| parse_expression(token_stream, context),
                    literal!("Expected ']', \"..\" or an expression."),
                    literal!("']', \"..\" or an expression"),
                )?),
            },
            Token::RightBracket, "']'"
        );
        let right_bracket = expect_token!(
            token_stream,
            Token::RightBracket => from_stream_pos!(token_stream => RightBracket),
            "Expected ']'.",
            "']'"
        );
        Ok((left_bracket, values, right_bracket))
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
            NormalAssignmentOperator,
            LeftBracket,
            cst::CommaSeparated<cst::ListEntry>,
            RightBracket,
        ),
        Var(NormalAssignmentOperator, Expression),
    }

    pub fn parse_inner_single_declarations(
        token_stream: ParseIn,
        context: &mut ParseContext,
        default_type: DefaultDataDeclarationType,
        default_scope: &DataDeclarationScope,
        values_are_initial_values: bool,
    ) -> ParseOut<Vec<(SingleDataDeclaration, Option<Comma>)>> {
        let mut declarations = Vec::<(SingleDataDeclaration, Option<Comma>)>::new();
        loop {
            if matches!(
                peek_token!(token_stream),
                Token::RightBrace(lexer::LexedRightBrace::Normal) | Token::RightParens
            ) {
                break;
            }
            let mut start_pos = None::<(usize, usize)>;
            let scope = match peek_token!(token_stream) {
                Token::GlobalKeyword => {
                    skip_token!(token_stream);
                    start_pos = Some(get_token_start(token_stream));
                    from_stream_pos!(token_stream => DataDeclarationScope::Global)
                }
                Token::LocalKeyword => {
                    skip_token!(token_stream);
                    start_pos = Some(get_token_start(token_stream));
                    from_stream_pos!(token_stream => DataDeclarationScope::Local)
                }
                Token::CloudKeyword => {
                    skip_token!(token_stream);
                    start_pos = Some(get_token_start(token_stream));
                    from_stream_pos!(token_stream => DataDeclarationScope::Cloud)
                }
                Token::RightBrace(lexer::LexedRightBrace::Normal) => break,
                Token::RightParens => break,
                _ => DataDeclarationScope::Unset,
            };
            let dec_type = match peek_token!(token_stream) {
                Token::VarKeyword => {
                    skip_token!(token_stream);
                    if start_pos.is_none() {
                        start_pos = Some(get_token_start(token_stream));
                    }
                    from_stream_pos!(token_stream => SingleDataDeclarationType::Var)
                }
                Token::ListKeyword => {
                    skip_token!(token_stream);
                    if start_pos.is_none() {
                        start_pos = Some(get_token_start(token_stream));
                    }
                    from_stream_pos!(token_stream => SingleDataDeclarationType::List)
                }
                _ => SingleDataDeclarationType::Unset,
            };
            let canonical_identifier = consume_and_use_if!(token_stream, Token::CanonicalIdentifier(value) => {
                let value = value.clone();
                if start_pos.is_none() {
                    start_pos = Some(get_token_start(token_stream));
                }
                CanonicalIdentifier {
                    name: value,
                    source_span: get_token_source_span(token_stream),
                }
            });
            let identifier = match next_token!(token_stream) {
                Token::Identifier(value) => {
                    if start_pos.is_none() {
                        start_pos = Some(get_token_start(token_stream));
                    }
                    Identifier {
                        path: Vec::new(),
                        fields: vec![(value, get_token_source_span(token_stream))],
                        source_span: get_token_source_span(token_stream),
                    }
                }
                token => match (scope, dec_type, canonical_identifier) {
                    (DataDeclarationScope::Unset, SingleDataDeclarationType::Unset, None) => {
                        emit_unexpected_token!(
                            token_stream,
                            "Expected scope modifer, declaration type, canonical identifier or identifier.",
                            "scope modifer, declaration type, canonical identifier or identifier",
                            token
                        );
                    }
                    (_, SingleDataDeclarationType::Unset, None) => {
                        emit_unexpected_token!(
                            token_stream,
                            "Expected declaration type, canonical identifier or identifier.",
                            "declaration type, canonical identifier or identifier",
                            token
                        );
                    }
                    (_, _, None) => {
                        emit_unexpected_token!(
                            token_stream,
                            "Expected canonical identifier or identifier.",
                            "canonical identifier or identifier",
                            token
                        );
                    }
                    (_, _, _) => {
                        emit_unexpected_token!(
                            token_stream,
                            "Expected identifier.",
                            "identifier",
                            token
                        );
                    }
                },
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
                    let assignment_operator =
                        from_stream_pos!(token_stream => NormalAssignmentOperator);
                    if let Token::LeftBracket = peek_token!(token_stream) {
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
                                let token = next_token!(token_stream);
                                emit_unexpected_token!(
                                    token_stream,
                                    "Expected an expression.",
                                    "an expression",
                                    token
                                );
                            }
                            _ => (),
                        }
                        let (left_bracket, list_content, right_bracket) =
                            parse_list_content(token_stream, context)?;
                        DeclarationValue::List(
                            assignment_operator,
                            left_bracket,
                            list_content,
                            right_bracket,
                        )
                    } else {
                        match (&default_type, &dec_type) {
                            (
                                DefaultDataDeclarationType::List,
                                SingleDataDeclarationType::Var(_),
                            ) => (),
                            (DefaultDataDeclarationType::List, _)
                            | (
                                DefaultDataDeclarationType::Var,
                                SingleDataDeclarationType::List(_),
                            ) => {
                                let token = next_token!(token_stream);
                                emit_unexpected_token!(token_stream, "Expected '['.", "'['", token);
                            }
                            _ => (),
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
                _ => {
                    let token = next_token!(token_stream);
                    emit_unexpected_token!(
                        token_stream,
                        "Expected ',', '=', '}' or ')'",
                        "',', '=', '}' or ')'",
                        token
                    )
                }
            };

            if matches!(
                (default_scope, &scope, &context.next_target),
                (
                    DataDeclarationScope::Local(_),
                    DataDeclarationScope::Unset,
                    Some(context::Target::Stage { .. })
                ) | (
                    _,
                    DataDeclarationScope::Local(_),
                    Some(context::Target::Stage { .. })
                )
            ) {
                return Err(ParseError::LocalSymbolInStage {
                    #[cfg(feature = "include_context_in_parse_errors")]
                    context: literal!(static_current_context!()),
                    source_span: token_stream.span_from_previous_to_current(start_pos.unwrap()),
                });
            }
            with_mut_target_scope_or_global_scope!(
                context,
                matches!(
                    (default_scope, &scope),
                    (
                        DataDeclarationScope::Global(_) | DataDeclarationScope::Cloud(_),
                        DataDeclarationScope::Unset
                    ) | (
                        _,
                        DataDeclarationScope::Global(_) | DataDeclarationScope::Cloud(_)
                    )
                ) || matches!(
                    (default_scope, &scope, &context.next_target),
                    (
                        DataDeclarationScope::Unset,
                        DataDeclarationScope::Unset,
                        Some(context::Target::Stage { .. })
                    )
                ),
                symbols => {
                    let name = &identifier.to_single().unwrap().0;
                    if name.as_str() == "super" {
                        return Err(ParseError::SymbolNamedSuper {
                            #[cfg(feature = "include_context_in_parse_errors")]
                            context: literal!(static_current_context!()),
                            source_span: identifier.to_single().unwrap().1,
                        });
                    }
                    let previous_symbol = symbols.insert(name.clone(), if(matches!(dec_type, SingleDataDeclarationType::List(_)) || (dec_type == SingleDataDeclarationType::Unset && default_type == DefaultDataDeclarationType::List)) {
                        context::TargetSymbolDescriptor::List(context::ListDescriptor {
                            name: name.clone(), canonical_name: canonical_identifier.as_ref().map(|value|value.name.clone()), value_is_initial_value: values_are_initial_values, value: match &value {
                                DeclarationValue::List(_, _, value, _) => {
                                    let mut expressions = Vec::with_capacity(value.len());
                                    for entry in value {
                                        match entry {
                                            ListEntry::Expression(expression) => expressions.push(expression.calculate_value().map_err(|source| {
                                                ParseError::InvalidConstantExpression {
                                                    expression: Box::new(expression.clone()),
                                                    source
                                                }
                                            })?),
                                            ListEntry::Unwrap(literal, _) => literal.get_string_value().as_str().chars().for_each(|c| expressions.push(grazelang_library::project_json::Sb3Primitive::String(c.to_string()))),
                                        }
                                    }
                                    expressions
                                },
                                DeclarationValue::None => Vec::new(),
                                DeclarationValue::Var(..) => unreachable!()
                            }
                        })
                    } else {
                        context::TargetSymbolDescriptor::Var(context::VarDescriptor {
                            name: name.clone(),
                            canonical_name: canonical_identifier.as_ref().map(|value|value.name.clone()),
                            value_is_initial_value: values_are_initial_values,
                            value: match &value {
                                DeclarationValue::None => grazelang_library::project_json::Sb3Primitive::String("".to_string()),
                                DeclarationValue::Var(_, value) => {
                                    value.calculate_value().map_err(|source| {
                                        ParseError::InvalidConstantExpression {
                                            expression: Box::new(value.clone()), source
                                        }
                                    })?
                                },
                                DeclarationValue::List(..) => unreachable!()
                            },
                            is_cloud: matches!(
                                (default_scope, &scope),
                                (DataDeclarationScope::Cloud(_), DataDeclarationScope::Unset) | (_, DataDeclarationScope::Cloud(_))
                            )
                        })
                    });
                    if let Some(previous_symbol) = previous_symbol {
                        symbols.insert(name.clone(), previous_symbol);
                        let single_identifier = identifier.to_single().unwrap();
                        return Err(ParseError::ShadowedSymbol {
                            #[cfg(feature = "include_context_in_parse_errors")]
                            context: literal!(static_current_context!()),
                            symbol: single_identifier.0.clone(),
                            source_span: single_identifier.1,
                        });
                    }
                }
            );
            let declaration = match value {
                DeclarationValue::None => match dec_type {
                    SingleDataDeclarationType::Unset => match default_type {
                        DefaultDataDeclarationType::Var => SingleDataDeclaration::EmptyVariable(
                            None,
                            scope,
                            canonical_identifier,
                            identifier,
                            token_stream.span_from_previous_to_current(start_pos.unwrap()),
                        ),
                        DefaultDataDeclarationType::List => SingleDataDeclaration::EmptyList(
                            None,
                            scope,
                            canonical_identifier,
                            identifier,
                            token_stream.span_from_previous_to_current(start_pos.unwrap()),
                        ),
                    },
                    SingleDataDeclarationType::Var(p) => SingleDataDeclaration::EmptyVariable(
                        Some(VarKeyword(p)),
                        scope,
                        canonical_identifier,
                        identifier,
                        token_stream.span_from_previous_to_current(start_pos.unwrap()),
                    ),
                    SingleDataDeclarationType::List(p) => SingleDataDeclaration::EmptyList(
                        Some(ListKeyword(p)),
                        scope,
                        canonical_identifier,
                        identifier,
                        token_stream.span_from_previous_to_current(start_pos.unwrap()),
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
                        token_stream.span_from_previous_to_current(start_pos.unwrap()),
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
                        token_stream.span_from_previous_to_current(start_pos.unwrap()),
                    )
                }
            };
            declarations.push((
                declaration,
                match peek_token!(token_stream) {
                    Token::Comma => {
                        skip_token!(token_stream);
                        Some(from_stream_pos!(token_stream => Comma))
                    }
                    Token::RightBrace(lexer::LexedRightBrace::Normal) => None,
                    Token::RightParens => None,
                    _ => {
                        let token = next_token!(token_stream);
                        emit_unexpected_token!(
                            token_stream,
                            "Expected ',', ')' or '}'.",
                            "',', ')' or '}'",
                            token
                        )
                    }
                },
            ));
        }
        Ok(declarations)
    }

    pub fn parse_data_declaration(
        token_stream: ParseIn,
        context: &mut ParseContext,
        values_are_initial_values: bool,
    ) -> ParseOut<(LetKeyword, DataDeclaration, SourceSpan)> {
        expect_token!(
            token_stream,
            Token::LetKeyword => (),
            "Data declaration needs to start with \"let\".",
            "\"let\""
        );
        let let_keyword_position = get_token_source_span(token_stream);
        let mut start_pos = None::<SourceSpan>;
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
                if start_pos.is_none() {
                    start_pos = from_stream_pos!(token_stream => Some);
                }
                from_stream_pos!(token_stream => SingleDataDeclarationType::Var)
            }
            Token::ListKeyword => {
                skip_token!(token_stream);
                if start_pos.is_none() {
                    start_pos = from_stream_pos!(token_stream => Some);
                }
                from_stream_pos!(token_stream => SingleDataDeclarationType::List)
            }
            Token::VarsKeyword => {
                skip_token!(token_stream);
                let vars_keyword = from_stream_pos!(token_stream => VarsKeyword);
                if start_pos.is_none() {
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
                    &scope,
                    values_are_initial_values,
                )?;
                let right_brace = expect_token!(
                    token_stream,
                    Token::RightBrace(lexer::LexedRightBrace::Normal) =>
                        from_stream_pos!(token_stream => RightBrace),
                    "Expected '}'.",
                    "'}'"
                );
                return Ok((
                    LetKeyword(let_keyword_position),
                    DataDeclaration::Vars(
                        scope,
                        vars_keyword,
                        left_brace,
                        declarations,
                        right_brace,
                        token_stream.span_from_previous_to_current(start_pos.unwrap().0.0),
                    ),
                    token_stream.span_from_previous_to_current(let_keyword_position.0.0),
                ));
            }
            Token::ListsKeyword => {
                skip_token!(token_stream);
                let lists_keyword = from_stream_pos!(token_stream => ListsKeyword);
                if start_pos.is_none() {
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
                    &scope,
                    values_are_initial_values,
                )?;
                let right_brace = expect_token!(
                    token_stream,
                    Token::RightBrace(lexer::LexedRightBrace::Normal) =>
                        from_stream_pos!(token_stream => RightBrace),
                    "Expected '}'.",
                    "'}'"
                );
                return Ok((
                    LetKeyword(let_keyword_position),
                    DataDeclaration::Lists(
                        scope,
                        lists_keyword,
                        left_brace,
                        declarations,
                        right_brace,
                        token_stream.span_from_previous_to_current(start_pos.unwrap().0.0),
                    ),
                    token_stream.span_from_previous_to_current(let_keyword_position.0.0),
                ));
            }
            Token::LeftParens => {
                skip_token!(token_stream);
                let left_parens = from_stream_pos!(token_stream => LeftParens);
                if start_pos.is_none() {
                    start_pos = from_stream_pos!(token_stream => Some);
                }
                let declarations = parse_inner_single_declarations(
                    token_stream,
                    context,
                    DefaultDataDeclarationType::Var,
                    &scope,
                    values_are_initial_values,
                )?;
                let right_parens = expect_token!(
                    token_stream,
                    Token::RightParens => from_stream_pos!(token_stream => RightParens),
                    "Expected ')'.",
                    "')'"
                );
                return Ok((
                    LetKeyword(let_keyword_position),
                    DataDeclaration::Mixed(
                        scope,
                        left_parens,
                        declarations,
                        right_parens,
                        token_stream.span_from_previous_to_current(start_pos.unwrap().0.0),
                    ),
                    token_stream.span_from_previous_to_current(let_keyword_position.0.0),
                ));
            }
            _ => SingleDataDeclarationType::Unset,
        };
        let canonical_identifier = consume_and_use_if!(token_stream, Token::CanonicalIdentifier(value) => {
            let value = value.clone();
            if start_pos.is_none() {
                start_pos = from_stream_pos!(token_stream => Some);
            }
            CanonicalIdentifier {
                name: value,
                source_span: get_token_source_span(token_stream),
            }
        });
        let identifier = match next_token!(token_stream) {
            Token::Identifier(value) => {
                if start_pos.is_none() {
                    start_pos = from_stream_pos!(token_stream => Some);
                }
                Identifier {
                    path: Vec::new(),
                    fields: vec![(value, get_token_source_span(token_stream))],
                    source_span: get_token_source_span(token_stream),
                }
            }
            token => match (scope, dec_type, canonical_identifier) {
                (DataDeclarationScope::Unset, SingleDataDeclarationType::Unset, None) => {
                    emit_unexpected_token!(
                        token_stream,
                        "Expected scope modifer, declaration type, canonical identifier or identifier.",
                        "scope modifer, declaration type, canonical identifier or identifier",
                        token
                    );
                }
                (_, SingleDataDeclarationType::Unset, None) => {
                    emit_unexpected_token!(
                        token_stream,
                        "Expected declaration type, canonical identifier or identifier.",
                        "declaration type, canonical identifier or identifier",
                        token
                    );
                }
                (_, _, None) => {
                    emit_unexpected_token!(
                        token_stream,
                        "Expected canonical identifier or identifier.",
                        "canonical identifier or identifier",
                        token
                    );
                }
                (_, _, Some(_)) => {
                    emit_unexpected_token!(
                        token_stream,
                        "Expected identifier.",
                        "identifier",
                        token
                    );
                }
            },
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
                let assignment_operator =
                    from_stream_pos!(token_stream => NormalAssignmentOperator);

                if let Token::LeftBracket = peek_token!(token_stream) {
                    if !matches!(dec_type, SingleDataDeclarationType::List(_)) {
                        let token = next_token!(token_stream);
                        emit_unexpected_token!(
                            token_stream,
                            "Expected an expression.",
                            "an expression",
                            token
                        );
                    }
                    let (left_bracket, list_content, right_bracket) =
                        parse_list_content(token_stream, context)?;
                    DeclarationValue::List(
                        assignment_operator,
                        left_bracket,
                        list_content,
                        right_bracket,
                    )
                } else {
                    if matches!(dec_type, SingleDataDeclarationType::List(_)) {
                        let token = next_token!(token_stream);
                        emit_unexpected_token!(token_stream, "Expected '['.", "'['", token);
                    }
                    DeclarationValue::Var(
                        assignment_operator,
                        parse_expression(token_stream, context)?,
                    )
                }
            }
            Token::Semicolon => DeclarationValue::None,
            _ => {
                let token = next_token!(token_stream);
                emit_unexpected_token!(token_stream, "Expected '=' or ';'", "'=' or ';'", token);
            }
        };
        if matches!(
            (&scope, &context.next_target),
            (
                DataDeclarationScope::Local(_),
                Some(context::Target::Stage { .. })
            )
        ) {
            return Err(ParseError::LocalSymbolInStage {
                #[cfg(feature = "include_context_in_parse_errors")]
                context: literal!(static_current_context!()),
                source_span: token_stream.span_from_previous_to_current(let_keyword_position.0.0),
            });
        }
        with_mut_target_scope_or_global_scope!(
            context,
            matches!(scope, DataDeclarationScope::Global(_) | DataDeclarationScope::Cloud(_)) ||
            matches!((&scope, &context.next_target), (DataDeclarationScope::Unset, Some(context::Target::Stage { .. }))),
            symbols => {
                let name = &identifier.to_single().unwrap().0;
                if name.as_str() == "super" {
                    return Err(ParseError::SymbolNamedSuper {
                        #[cfg(feature = "include_context_in_parse_errors")]
                        context: literal!(static_current_context!()),
                        source_span: identifier.to_single().unwrap().1,
                    });
                }
                let previous_symbol = symbols.insert(
                    name.clone(),
                    if matches!(dec_type, SingleDataDeclarationType::List(_)) {
                        context::TargetSymbolDescriptor::List(context::ListDescriptor {
                            name: name.clone(),
                            canonical_name: canonical_identifier.as_ref().map(|value| value.name.clone()),
                            value_is_initial_value: values_are_initial_values,
                            value: match &value {
                                DeclarationValue::List(_, _, value, _) => {
                                    let mut expressions = Vec::with_capacity(value.len());
                                    for entry in value {
                                        match entry {
                                            ListEntry::Expression(expression) => expressions.push(expression.calculate_value().map_err(|source| {
                                                ParseError::InvalidConstantExpression {
                                                    expression: Box::new(expression.clone()),source
                                                }
                                            })?),
                                            ListEntry::Unwrap(literal, _) => literal
                                                .get_string_value()
                                                .as_str()
                                                .chars()
                                                .for_each(|c| expressions.push(
                                                    grazelang_library::project_json::Sb3Primitive::String(c.to_string())
                                                )),
                                        }
                                    }
                                    expressions
                                },
                                DeclarationValue::None => Vec::new(),
                                DeclarationValue::Var(..) => unreachable!()
                            }
                        })
                    } else {
                        context::TargetSymbolDescriptor::Var(context::VarDescriptor {
                            name: name.clone(),
                            canonical_name: canonical_identifier.as_ref().map(|value| value.name.clone()),
                            value_is_initial_value: values_are_initial_values,
                            value: match &value {
                                DeclarationValue::None => grazelang_library::project_json::Sb3Primitive::String("".to_string()),
                                DeclarationValue::Var(_, value) => {
                                    value.calculate_value().map_err(|source| {
                                        ParseError::InvalidConstantExpression {
                                            expression: Box::new(value.clone()),source
                                        }
                                    })?
                                },
                                DeclarationValue::List(..) => unreachable!()
                            },
                            is_cloud: matches!(scope, DataDeclarationScope::Cloud(_))
                        })
                    }
                );
                if let Some(previous_symbol) = previous_symbol {
                    symbols.insert(name.clone(), previous_symbol);
                    let single_identifier = identifier.to_single().unwrap();
                    return Err(ParseError::ShadowedSymbol {
                        #[cfg(feature = "include_context_in_parse_errors")]
                        context: literal!(static_current_context!()),
                        symbol: single_identifier.0.clone(),
                        source_span: single_identifier.1,
                    });
                }
            }
        );
        let declaration = match value {
            DeclarationValue::None => match dec_type {
                SingleDataDeclarationType::Unset => SingleDataDeclaration::EmptyVariable(
                    None,
                    scope,
                    canonical_identifier,
                    identifier,
                    token_stream.span_from_previous_to_current(start_pos.unwrap().0.0),
                ),
                SingleDataDeclarationType::Var(p) => SingleDataDeclaration::EmptyVariable(
                    Some(VarKeyword(p)),
                    scope,
                    canonical_identifier,
                    identifier,
                    token_stream.span_from_previous_to_current(start_pos.unwrap().0.0),
                ),
                SingleDataDeclarationType::List(p) => SingleDataDeclaration::EmptyList(
                    Some(ListKeyword(p)),
                    scope,
                    canonical_identifier,
                    identifier,
                    token_stream.span_from_previous_to_current(start_pos.unwrap().0.0),
                ),
            },
            DeclarationValue::Var(assignment_operator, literal) => SingleDataDeclaration::Variable(
                match dec_type {
                    SingleDataDeclarationType::Unset => None,
                    SingleDataDeclarationType::Var(p) => Some(VarKeyword(p)),
                    SingleDataDeclarationType::List(_) => unreachable!(),
                },
                scope,
                canonical_identifier,
                identifier,
                assignment_operator,
                literal,
                token_stream.span_from_previous_to_current(start_pos.unwrap().0.0),
            ),
            DeclarationValue::List(assignment_operator, left_brace, items, right_brace) => {
                SingleDataDeclaration::List(
                    match dec_type {
                        SingleDataDeclarationType::Unset => None,
                        SingleDataDeclarationType::Var(_) => unreachable!(),
                        SingleDataDeclarationType::List(p) => Some(ListKeyword(p)),
                    },
                    scope,
                    canonical_identifier,
                    identifier,
                    assignment_operator,
                    left_brace,
                    items,
                    right_brace,
                    token_stream.span_from_previous_to_current(start_pos.unwrap().0.0),
                )
            }
        };
        Ok((
            LetKeyword(let_keyword_position),
            DataDeclaration::Single(declaration.into()),
            token_stream.span_from_previous_to_current(let_keyword_position.0.0),
        ))
    }

    pub fn parse_assignment(
        token_stream: ParseIn,
        context: &mut ParseContext,
        identifier: Identifier,
    ) -> ParseOut<Statement> {
        let start_pos = get_token_start(token_stream);
        expect_token!(token_stream, Token::Assign => (), "Expected '='.", "'='");
        let assignment_operator = from_stream_pos!(token_stream => NormalAssignmentOperator);
        if matches!(peek_token!(token_stream), Token::LeftBracket) {
            let (left_brace, expressions, right_brace) = parse_list_content(token_stream, context)?;
            return Ok(Statement::ListAssignment(
                identifier,
                assignment_operator,
                left_brace,
                expressions,
                right_brace,
                expect_token!(
                    token_stream,
                    Token::Semicolon => from_stream_pos!(token_stream => Semicolon),
                    "Expected ';'.",
                    "';'"
                ),
                token_stream.span_from_previous_to_current(start_pos),
            ));
        }
        Ok(Statement::Assignment(
            identifier,
            assignment_operator,
            token_stream.substitude_unexpected_token_message(
                |token_stream| parse_expression(token_stream, context),
                literal!("Expected '[' or an expression."),
                literal!("'[' or an expression"),
            )?,
            expect_token!(
                token_stream,
                Token::Semicolon => from_stream_pos!(token_stream => Semicolon),
                "Expected ';'.",
                "';'"
            ),
            token_stream.span_from_previous_to_current(start_pos),
        ))
    }

    #[inline]
    pub(crate) fn parse_rest_of_single_input_control(
        token_stream: ParseIn,
        context: &mut ParseContext,
        identifier: Identifier,
        expression: Expression,
        start_pos: (usize, usize),
    ) -> ParseOut<Statement> {
        let code_block = parse_code_block(token_stream, context)?;
        if let Some(syntactic_if) = identifier.to_syntactic_if() {
            let initial_code_block = (syntactic_if, expression, code_block);
            let mut code_blocks = Vec::<(SyntacticElse, SyntacticIf, Expression, CodeBlock)>::new();
            let final_code_block = loop {
                match match peek_token!(token_stream => Option) {
                    Some(value) => value,
                    None => break None,
                } {
                    Token::Semicolon => break None,
                    Token::Identifier(_)
                    | Token::StageKeyword
                    | Token::VarsKeyword
                    | Token::ListsKeyword => {
                        if let Token::Identifier(ident) = peek_token!(token_stream) {
                            if ident.as_str() != "else" {
                                break None;
                            }
                        } else {
                            break None;
                        }
                        let else_identifier = parse_full_identifier(token_stream, context)?;
                        let syntactic_else =
                            if let Some(value) = else_identifier.to_syntactic_else() {
                                value
                            } else {
                                emit_unexpected_token!(
                                    token_stream,
                                    "Expected \"else\".",
                                    "\"else\"",
                                    Token::Identifier(
                                        else_identifier
                                            .fields
                                            .last()
                                            .or_else(|| else_identifier.path.last())
                                            .unwrap()
                                            .0
                                            .clone()
                                    )
                                );
                            };
                        if matches!(peek_token!(token_stream), Token::Identifier(_)) {
                            let if_identifier = parse_full_identifier(token_stream, context)?;
                            let Some(syntactic_if) = if_identifier.to_syntactic_if() else {
                                emit_unexpected_token!(
                                    token_stream,
                                    "Expected \"if\".",
                                    "\"if\"",
                                    Token::Identifier(
                                        if_identifier
                                            .fields
                                            .last()
                                            .or_else(|| if_identifier.path.last())
                                            .unwrap()
                                            .0
                                            .clone()
                                    )
                                );
                            };
                            code_blocks.push((
                                syntactic_else,
                                syntactic_if,
                                parse_expression(token_stream, context)?,
                                parse_code_block(token_stream, context)?,
                            ));
                        } else {
                            break Some((syntactic_else, parse_code_block(token_stream, context)?));
                        }
                    }
                    _ => break None,
                }
            };
            return Ok(Statement::IfElse(
                initial_code_block,
                code_blocks,
                final_code_block,
                consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => Semicolon)),
                token_stream.span_from_previous_to_current(start_pos),
            ));
        }
        Ok(Statement::SingleInputControl(
            identifier,
            expression,
            code_block,
            consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => Semicolon)),
            token_stream.span_from_previous_to_current(start_pos),
        ))
    }

    pub fn parse_call_or_control(
        token_stream: ParseIn,
        context: &mut ParseContext,
        identifier: Identifier,
    ) -> ParseOut<Statement> {
        let start_pos = get_token_start(token_stream);
        let (left_parens, expressions, right_parens) = match peek_token!(token_stream) {
            Token::LeftParens => parse_expression_list(token_stream, context)?,
            _ => {
                let expression = token_stream.substitude_unexpected_token_message(
                    |token_stream| parse_expression(token_stream, context),
                    literal!("Expected '(' or an expression."),
                    literal!("'(' or an expression"),
                )?;
                return parse_rest_of_single_input_control(
                    token_stream,
                    context,
                    identifier,
                    expression,
                    start_pos,
                );
            }
        };
        if peek_token!(token_stream) == &Token::LeftBrace {
            let expression = match expressions.len() {
                0 => {
                    let source_span = (left_parens.0.0, right_parens.0.1);
                    Expression::Literal(cst::Literal::EmptyExpression(
                        left_parens,
                        right_parens,
                        source_span,
                    ))
                }
                1 => {
                    if expressions[0].1.is_some() {
                        return Ok(Statement::MultiInputControl(
                            identifier,
                            left_parens,
                            expressions,
                            right_parens,
                            parse_code_block(token_stream, context)?,
                            consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => Semicolon)),
                            token_stream.span_from_previous_to_current(start_pos),
                        ));
                    }
                    let source_span = (left_parens.0.0, right_parens.0.1);
                    Expression::Parentheses(
                        left_parens,
                        Box::new({
                            let (expression, _) = expressions.into_iter().next().unwrap();
                            expression
                        }),
                        right_parens,
                        source_span,
                    )
                }
                _ => {
                    return Ok(Statement::MultiInputControl(
                        identifier,
                        left_parens,
                        expressions,
                        right_parens,
                        parse_code_block(token_stream, context)?,
                        consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => Semicolon)),
                        token_stream.span_from_previous_to_current(start_pos),
                    ));
                }
            };
            return parse_rest_of_single_input_control(
                token_stream,
                context,
                identifier,
                expression,
                start_pos,
            );
        }
        Ok(Statement::Call(
            identifier,
            left_parens,
            expressions,
            right_parens,
            expect_token!(
                token_stream,
                Token::Semicolon => from_stream_pos!(token_stream => Semicolon),
                "Expected ';'.",
                "';'"
            ),
            token_stream.span_from_previous_to_current(start_pos),
        ))
    }

    pub fn parse_forever(
        token_stream: ParseIn,
        context: &mut ParseContext,
        identifier: Identifier,
    ) -> ParseOut<Statement> {
        let start_pos = get_token_start(token_stream);
        if peek_token!(token_stream) != &Token::LeftBrace {
            let token = next_token!(token_stream);
            emit_unexpected_token!(token_stream, "Expected '{'", "'{'", token);
        }
        let code_block = parse_code_block(token_stream, context)?;
        Ok(Statement::Forever(
            identifier,
            code_block,
            consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => Semicolon)),
            token_stream.span_from_previous_to_current(start_pos),
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
            Token::LeftBrace => parse_forever(token_stream, context, identifier),
            Token::LeftBracket => {
                let start_pos = get_token_start(token_stream);
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
                    Token::Assign => from_stream_pos!(token_stream => NormalAssignmentOperator),
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
                    expect_token!(
                        token_stream,
                        Token::Semicolon => from_stream_pos!(token_stream => Semicolon),
                        "Expected ';'.",
                        "';'"
                    ),
                    token_stream.span_from_previous_to_current(start_pos),
                ))
            }
            _ => token_stream.substitude_unexpected_token_message(
                |token_stream| parse_call_or_control(token_stream, context, identifier),
                literal!("Expected '=', '(', '{', '[' or an expression."),
                literal!("'=', '(', '{', '[' or an expression"),
            ),
        }
    }

    #[inline]
    pub fn parse_statement_data_declaration_fully(
        token_stream: ParseIn,
        context: &mut ParseContext,
    ) -> ParseOut<Statement> {
        let (let_keyword, data_dec, source_span) =
            statement::parse_data_declaration(token_stream, context, false)?;
        Ok(Statement::DataDeclaration(
            let_keyword,
            data_dec,
            expect_token!(
                token_stream,
                Token::Semicolon => from_stream_pos!(token_stream => Semicolon),
                "Expected ';'.",
                "';'"
            ),
            source_span,
        ))
    }

    #[inline]
    pub fn parse_sprite_data_declaration_fully(
        token_stream: ParseIn,
        context: &mut ParseContext,
    ) -> ParseOut<SpriteStatement> {
        let (let_keyword, data_dec, source_span) =
            statement::parse_data_declaration(token_stream, context, true)?;
        Ok(SpriteStatement::DataDeclaration(
            let_keyword,
            data_dec,
            expect_token!(
                token_stream,
                Token::Semicolon => from_stream_pos!(token_stream => Semicolon),
                "Expected ';'.",
                "';'"
            ),
            source_span,
        ))
    }

    #[inline]
    pub fn parse_stage_data_declaration_fully(
        token_stream: ParseIn,
        context: &mut ParseContext,
    ) -> ParseOut<StageStatement> {
        let (let_keyword, data_dec, source_span) =
            statement::parse_data_declaration(token_stream, context, true)?;
        Ok(StageStatement::DataDeclaration(
            let_keyword,
            data_dec,
            expect_token!(
                token_stream,
                Token::Semicolon => from_stream_pos!(token_stream => Semicolon),
                "Expected ';'.",
                "';'"
            ),
            source_span,
        ))
    }

    pub fn parse_flat_directionary_asset_value(
        token_stream: ParseIn,
        context: &mut ParseContext,
    ) -> ParseOut<cst::SingleAssetDeclarationValue> {
        let left_brace = expect_token!(
            token_stream,
            Token::LeftBrace => from_stream_pos!(token_stream => LeftBrace),
            "Expected '{'.",
            "'{'"
        );
        let mut items = Vec::<(
            Identifier,
            NormalAssignmentOperator,
            cst::Literal,
            Option<Comma>,
        )>::new();
        let right_brace = loop {
            let identifier = parse_single_identifier_as_identifier(token_stream, context)?;
            let assignment_operator = expect_token!(
                token_stream,
                Token::Assign => from_stream_pos!(token_stream => NormalAssignmentOperator),
                "Expected '='.",
                "'='"
            );
            let literal = parse_literal(token_stream, context)?;
            let comma = match peek_token!(token_stream) {
                Token::RightBrace(LexedRightBrace::Normal) => None,
                Token::Comma => {
                    skip_token!(token_stream);
                    Some(from_stream_pos!(token_stream => Comma))
                }
                _ => {
                    let token = next_token!(token_stream);
                    emit_unexpected_token!(
                        token_stream,
                        "Expected '}' or ','.",
                        "'}' or ','",
                        token
                    );
                }
            };
            items.push((identifier, assignment_operator, literal, comma));
            consume_then_never_if!(token_stream, Token::RightBrace(LexedRightBrace::Normal) => {
                break from_stream_pos!(token_stream => RightBrace);
            });
        };
        Ok(cst::SingleAssetDeclarationValue::FlatDictionary(
            left_brace,
            items,
            right_brace,
            left_brace.range_to(&right_brace),
        ))
    }

    pub fn parse_simple_asset_value(
        token_stream: ParseIn,
        _context: &mut ParseContext,
    ) -> ParseOut<cst::SingleAssetDeclarationValue> {
        let left_parens = expect_token!(
            token_stream,
            Token::LeftParens => from_stream_pos!(token_stream => LeftParens),
            "Expected '('.",
            "'('"
        );
        let path = expect_token!(
            token_stream,
            Token::SimpleString(string) => (string, get_token_source_span(token_stream)),
            "Expected '('.",
            "'('"
        );
        let right_parens = expect_token!(
            token_stream,
            Token::RightParens => from_stream_pos!(token_stream => RightParens),
            "Expected ')'.",
            "')'"
        );
        Ok(cst::SingleAssetDeclarationValue::Simple(
            left_parens,
            path,
            right_parens,
            left_parens.range_to(&right_parens),
        ))
    }

    #[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
    pub enum AssetDeclarationType {
        Costume,
        Backdrop,
        Sound,
    }

    pub fn parse_asset_declaration(
        token_stream: ParseIn,
        context: &mut ParseContext,
        asset_type: AssetDeclarationType,
    ) -> ParseOut<AssetDeclaration> {
        fn extract_f64_entry(
            data: &mut HashMap<IString, cst::Literal>,
            errors: &mut Vec<ParseError>,
            key: IString,
        ) -> Option<f64> {
            data.remove(key.as_str()).and_then(|value| {
                value
                    .get_string_value()
                    .parse::<f64>()
                    .map_err(|_| {
                        errors.push(ParseError::IncorrectFlatDictionaryEntryType {
                            key,
                            source_span: *value.get_source_span(),
                            value: Box::new(value),
                            #[cfg(feature = "include_context_in_parse_errors")]
                            context: literal!(static_current_context!()),
                        })
                    })
                    .ok()
            })
        }
        const ROTATION_CENTER_X_KEY: &IString = &literal!("rotation_center_x");
        const ROTATION_CENTER_Y_KEY: &IString = &literal!("rotation_center_y");
        match peek_token!(token_stream) {
            Token::LeftParens => {
                skip_token!(token_stream);
                let left_parens = from_stream_pos!(token_stream => LeftParens);
                let start_pos = get_token_start(token_stream);
                let mut declarations = Vec::<(SingleAssetDeclaration, Option<Comma>)>::new();
                let right_parens = loop {
                    consume_then_never_if!(
                        token_stream,
                        Token::RightParens => break from_stream_pos!(token_stream => RightParens)
                    );
                    let (canonical_identifier, start_pos) = consume_and_use_if!(
                        token_stream,
                        Token::CanonicalIdentifier(name) => (
                            Some(CanonicalIdentifier {
                                name, source_span: get_token_source_span(token_stream)
                            }),
                            Some(get_token_start(token_stream))
                        )
                    )
                    .unwrap_or((None, None));
                    let identifier = parse_single_identifier_as_identifier(token_stream, context)?;
                    let start_pos = start_pos.unwrap_or(get_token_start(token_stream));
                    let value = match peek_token!(token_stream) {
                        Token::LeftParens => parse_simple_asset_value(token_stream, context)?,
                        Token::LeftBrace => {
                            parse_flat_directionary_asset_value(token_stream, context)?
                        }
                        _ => {
                            let token = next_token!(token_stream);
                            emit_unexpected_token!(
                                token_stream,
                                "Expected '(' or '{'.",
                                "'(' or '{'",
                                token
                            );
                        }
                    };
                    let mut errors = Vec::new();
                    with_mut_next_target!(context, target => {
                        use context::{TargetSymbolDescriptor, CostumeDescriptor, BackdropDescriptor, SoundDescriptor};
                        let symbols = target.borrow_symbols_mut();
                        let name = &identifier.to_single().unwrap().0;
                        if name.as_str() == "super" {
                            errors.push(ParseError::SymbolNamedSuper {
                                #[cfg(feature = "include_context_in_parse_errors")]
                                context: literal!(static_current_context!()),
                                source_span: identifier.to_single().unwrap().1,
                            });
                        }
                        let canonical_name = canonical_identifier.as_ref().map(|value| value.name.clone());
                        let (path, mut data) = match &value {
                            cst::SingleAssetDeclarationValue::Simple(_, path, _, _) => (path.0.clone(), HashMap::new()),
                            cst::SingleAssetDeclarationValue::FlatDictionary(_, items, _, _) => {
                                let mut data = HashMap::with_capacity(items.len());
                                for (ident, _, value, _) in items {
                                    if data.insert(ident.to_single().unwrap().0.clone(), value.clone()).is_some() {
                                        errors.push(ParseError::RepeatedFlatDictionaryEntry {
                                            key: ident.to_single().unwrap().0.clone(),
                                            #[cfg(feature = "include_context_in_parse_errors")]
                                            context: literal!(static_current_context!()),
                                            source_span: *ident.get_source_span(),
                                        });
                                    }
                                }
                                (data.remove("path").unwrap_or_else(|| {
                                    errors.push(ParseError::MissingFlatDictionaryEntry {
                                        key: literal!("path"),
                                        #[cfg(feature = "include_context_in_parse_errors")]
                                        context: literal!(static_current_context!()),
                                        source_span: *value.get_source_span()
                                    });
                                    cst::Literal::String(EMPTY_ISTRING_REF.clone(), Default::default())
                                }).cast_to_string(), data)
                            },
                        };
                        let symbol_idx = symbols.len();
                        let previous_symbol = symbols.insert(
                            name.clone(),
                            match asset_type {
                                AssetDeclarationType::Costume => TargetSymbolDescriptor::Costume(
                                    CostumeDescriptor {
                                        name: name.clone(),
                                        canonical_name,
                                        source: path,
                                        rotation_center_x: extract_f64_entry(&mut data, &mut errors, ROTATION_CENTER_X_KEY.clone()),
                                        rotation_center_y: extract_f64_entry(&mut data, &mut errors, ROTATION_CENTER_Y_KEY.clone()),
                                        symbol_idx,
                                    }
                                ),
                                AssetDeclarationType::Backdrop => TargetSymbolDescriptor::Backdrop(
                                    BackdropDescriptor {
                                        name: name.clone(),
                                        canonical_name,
                                        source: path,
                                        rotation_center_x: extract_f64_entry(&mut data, &mut errors, ROTATION_CENTER_X_KEY.clone()),
                                        rotation_center_y: extract_f64_entry(&mut data, &mut errors, ROTATION_CENTER_Y_KEY.clone()),
                                        symbol_idx,
                                    }
                                ),
                                AssetDeclarationType::Sound => TargetSymbolDescriptor::Sound(
                                    SoundDescriptor {
                                        name: name.clone(),
                                        canonical_name,
                                        source: path,
                                        symbol_idx,
                                    }
                                ),
                            }
                        );
                        if let Some(previous_symbol) = previous_symbol {
                            symbols.insert(name.clone(), previous_symbol);
                            let single_identifier = identifier.to_single().unwrap();
                            errors.push(ParseError::ShadowedSymbol {
                                #[cfg(feature = "include_context_in_parse_errors")]
                                context: literal!(static_current_context!()),
                                symbol: single_identifier.0.clone(),
                                source_span: single_identifier.1,
                            });
                        }
                        if let Some((key, value)) = data.into_iter().next() {
                            errors.push(ParseError::UnknownFlatDictionaryEntry {
                                key,
                                #[cfg(feature = "include_context_in_parse_errors")]
                                context: literal!(static_current_context!()),
                                source_span: *value.get_source_span()
                            });
                        }
                    });
                    for error in errors {
                        context.successful = false;
                        emit_message(context, error.into(), GrazeMessageSetting::Errors);
                    }
                    declarations.push((
                        SingleAssetDeclaration(
                            canonical_identifier,
                            identifier,
                            value,
                            token_stream.span_from_previous_to_current(start_pos),
                        ),
                        {
                            match peek_token!(token_stream) {
                                Token::Comma => {
                                    skip_token!(token_stream);
                                    Some(from_stream_pos!(token_stream => Comma))
                                }
                                Token::RightParens => None,
                                _ => {
                                    let token = next_token!(token_stream);
                                    emit_unexpected_token!(
                                        token_stream,
                                        "Expected ',' or ')'.",
                                        "',' or ')'",
                                        token
                                    )
                                }
                            }
                        },
                    ));
                };
                Ok(AssetDeclaration::Multiple(
                    left_parens,
                    declarations,
                    right_parens,
                    token_stream.span_from_previous_to_current(start_pos),
                ))
            }
            Token::CanonicalIdentifier(_) => {
                let canonical_identifier = match next_token!(token_stream) {
                    Token::CanonicalIdentifier(name) => CanonicalIdentifier {
                        name,
                        source_span: get_token_source_span(token_stream),
                    },
                    _ => unreachable!(),
                };
                let start_pos = get_token_start(token_stream);
                let identifier = parse_single_identifier_as_identifier(token_stream, context)?;
                let value = match peek_token!(token_stream) {
                    Token::LeftParens => parse_simple_asset_value(token_stream, context)?,
                    Token::LeftBrace => parse_flat_directionary_asset_value(token_stream, context)?,
                    _ => {
                        let token = next_token!(token_stream);
                        emit_unexpected_token!(
                            token_stream,
                            "Expected '(' or '{'.",
                            "'(' or '{'",
                            token
                        );
                    }
                };
                let mut errors = Vec::new();
                with_mut_next_target!(context, target => {
                    use context::{TargetSymbolDescriptor, CostumeDescriptor, BackdropDescriptor, SoundDescriptor};
                    let symbols = target.borrow_symbols_mut();
                    let name = &identifier.to_single().unwrap().0;
                    if name.as_str() == "super" {
                        errors.push(ParseError::SymbolNamedSuper {
                            #[cfg(feature = "include_context_in_parse_errors")]
                            context: literal!(static_current_context!()),
                            source_span: identifier.to_single().unwrap().1,
                        });
                    }
                    let canonical_name = Some(canonical_identifier.name.clone());
                    let (path, mut data) = match &value {
                        cst::SingleAssetDeclarationValue::Simple(_, path, _, _) => (path.0.clone(), HashMap::new()),
                        cst::SingleAssetDeclarationValue::FlatDictionary(_, items, _, _) => {
                            let mut data = HashMap::with_capacity(items.len());
                            for (ident, _, value, _) in items {
                                if data.insert(ident.to_single().unwrap().0.clone(), value.clone()).is_some() {
                                    errors.push(ParseError::RepeatedFlatDictionaryEntry {
                                        key: ident.to_single().unwrap().0.clone(),
                                        #[cfg(feature = "include_context_in_parse_errors")]
                                        context: literal!(static_current_context!()),
                                        source_span: *ident.get_source_span(),
                                    });
                                }
                            }
                            (data.remove("path").unwrap_or_else(|| {
                                errors.push(ParseError::MissingFlatDictionaryEntry {
                                    key: literal!("path"),
                                    #[cfg(feature = "include_context_in_parse_errors")]
                                    context: literal!(static_current_context!()),
                                    source_span: *value.get_source_span()
                                });
                                cst::Literal::String(EMPTY_ISTRING_REF.clone(), Default::default())
                            }).cast_to_string(), data)
                        },
                    };
                    let symbol_idx = symbols.len();
                    let previous_symbol = symbols.insert(
                        name.clone(),
                        match asset_type {
                            AssetDeclarationType::Costume => TargetSymbolDescriptor::Costume(
                                CostumeDescriptor {
                                    name: name.clone(),
                                    canonical_name,
                                    source: path,
                                    rotation_center_x: extract_f64_entry(&mut data, &mut errors, ROTATION_CENTER_X_KEY.clone()),
                                    rotation_center_y: extract_f64_entry(&mut data, &mut errors, ROTATION_CENTER_Y_KEY.clone()),
                                    symbol_idx,
                                }
                            ),
                            AssetDeclarationType::Backdrop => TargetSymbolDescriptor::Backdrop(
                                BackdropDescriptor {
                                    name: name.clone(),
                                    canonical_name,
                                    source: path,
                                    rotation_center_x: extract_f64_entry(&mut data, &mut errors, ROTATION_CENTER_X_KEY.clone()),
                                    rotation_center_y: extract_f64_entry(&mut data, &mut errors, ROTATION_CENTER_Y_KEY.clone()),
                                    symbol_idx,
                                }
                            ),
                            AssetDeclarationType::Sound => TargetSymbolDescriptor::Sound(
                                SoundDescriptor {
                                    name: name.clone(),
                                    canonical_name,
                                    source: path,
                                    symbol_idx,
                                }
                            ),
                        }
                    );
                    if let Some(previous_symbol) = previous_symbol {
                        symbols.insert(name.clone(), previous_symbol);
                        let single_identifier = identifier.to_single().unwrap();
                        errors.push(ParseError::ShadowedSymbol {
                            #[cfg(feature = "include_context_in_parse_errors")]
                            context: literal!(static_current_context!()),
                            symbol: single_identifier.0.clone(),
                            source_span: single_identifier.1,
                        });
                    }
                    if let Some((key, value)) = data.into_iter().next() {
                        errors.push(ParseError::UnknownFlatDictionaryEntry {
                            key,
                            #[cfg(feature = "include_context_in_parse_errors")]
                            context: literal!(static_current_context!()),
                            source_span: *value.get_source_span()
                        });
                    }
                });
                for error in errors {
                    context.successful = false;
                    emit_message(context, error.into(), GrazeMessageSetting::Errors);
                }
                Ok(AssetDeclaration::Single(SingleAssetDeclaration(
                    Some(canonical_identifier),
                    identifier,
                    value,
                    token_stream.span_from_previous_to_current(start_pos),
                )))
            }
            Token::Identifier(_) => {
                let identifier = parse_single_identifier_as_identifier(token_stream, context)?;
                let start_pos = get_token_start(token_stream);
                let value = match peek_token!(token_stream) {
                    Token::LeftParens => parse_simple_asset_value(token_stream, context)?,
                    Token::LeftBrace => parse_flat_directionary_asset_value(token_stream, context)?,
                    _ => {
                        let token = next_token!(token_stream);
                        emit_unexpected_token!(
                            token_stream,
                            "Expected '(' or '{'.",
                            "'(' or '{'",
                            token
                        );
                    }
                };
                let mut errors = Vec::new();
                with_mut_next_target!(context, target => {
                    use context::{TargetSymbolDescriptor, CostumeDescriptor, BackdropDescriptor, SoundDescriptor};
                    let symbols = target.borrow_symbols_mut();
                    let name = &identifier.to_single().unwrap().0;
                    if name.as_str() == "super" {
                        errors.push(ParseError::SymbolNamedSuper {
                            #[cfg(feature = "include_context_in_parse_errors")]
                            context: literal!(static_current_context!()),
                            source_span: identifier.to_single().unwrap().1,
                        });
                    }
                    let canonical_name = None;
                    let (path, mut data) = match &value {
                        cst::SingleAssetDeclarationValue::Simple(_, path, _, _) => (path.0.clone(), HashMap::new()),
                        cst::SingleAssetDeclarationValue::FlatDictionary(_, items, _, _) => {
                            let mut data = HashMap::with_capacity(items.len());
                            for (ident, _, value, _) in items {
                                if data.insert(ident.to_single().unwrap().0.clone(), value.clone()).is_some() {
                                    errors.push(ParseError::RepeatedFlatDictionaryEntry {
                                        key: ident.to_single().unwrap().0.clone(),
                                        #[cfg(feature = "include_context_in_parse_errors")]
                                        context: literal!(static_current_context!()),
                                        source_span: *ident.get_source_span(),
                                    });
                                }
                            }
                            (data.remove("path").unwrap_or_else(|| {
                                errors.push(ParseError::MissingFlatDictionaryEntry {
                                    key: literal!("path"),
                                    #[cfg(feature = "include_context_in_parse_errors")]
                                    context: literal!(static_current_context!()),
                                    source_span: *value.get_source_span()
                                });
                                cst::Literal::String(EMPTY_ISTRING_REF.clone(), Default::default())
                            }).cast_to_string(), data)
                        },
                    };
                    let symbol_idx = symbols.len();
                    let previous_symbol = symbols.insert(
                        name.clone(),
                        match asset_type {
                            AssetDeclarationType::Costume => TargetSymbolDescriptor::Costume(
                                CostumeDescriptor {
                                    name: name.clone(),
                                    canonical_name,
                                    source: path,
                                    rotation_center_x: extract_f64_entry(&mut data, &mut errors, ROTATION_CENTER_X_KEY.clone()),
                                    rotation_center_y: extract_f64_entry(&mut data, &mut errors, ROTATION_CENTER_Y_KEY.clone()),
                                    symbol_idx,
                                }
                            ),
                            AssetDeclarationType::Backdrop => TargetSymbolDescriptor::Backdrop(
                                BackdropDescriptor {
                                    name: name.clone(),
                                    canonical_name,
                                    source: path,
                                    rotation_center_x: extract_f64_entry(&mut data, &mut errors, ROTATION_CENTER_X_KEY.clone()),
                                    rotation_center_y: extract_f64_entry(&mut data, &mut errors, ROTATION_CENTER_Y_KEY.clone()),
                                    symbol_idx,
                                }
                            ),
                            AssetDeclarationType::Sound => TargetSymbolDescriptor::Sound(
                                SoundDescriptor {
                                    name: name.clone(),
                                    canonical_name,
                                    source: path,
                                    symbol_idx,
                                }
                            ),
                        }
                    );
                    if let Some(previous_symbol) = previous_symbol {
                        symbols.insert(name.clone(), previous_symbol);
                        let single_identifier = identifier.to_single().unwrap();
                        errors.push(ParseError::ShadowedSymbol {
                            #[cfg(feature = "include_context_in_parse_errors")]
                            context: literal!(static_current_context!()),
                            symbol: single_identifier.0.clone(),
                            source_span: single_identifier.1,
                        });
                    }
                    if let Some((key, value)) = data.into_iter().next() {
                        errors.push(ParseError::UnknownFlatDictionaryEntry {
                            key,
                            #[cfg(feature = "include_context_in_parse_errors")]
                            context: literal!(static_current_context!()),
                            source_span: *value.get_source_span()
                        });
                    }
                });
                for error in errors {
                    context.successful = false;
                    emit_message(context, error.into(), GrazeMessageSetting::Errors);
                }
                Ok(AssetDeclaration::Single(SingleAssetDeclaration(
                    None,
                    identifier,
                    value,
                    token_stream.span_from_previous_to_current(start_pos),
                )))
            }
            _ => {
                let token = next_token!(token_stream);
                emit_unexpected_token!(
                    token_stream,
                    "Expected canonical identifier, identifier or '('.",
                    "canonical identifier, identifier or '('",
                    token
                )
            }
        }
    }

    pub fn parse_custom_block(
        token_stream: ParseIn,
        context: &mut ParseContext,
    ) -> ParseOut<(
        Option<WarpSpecifier>,
        ProcKeyword,
        Option<CanonicalIdentifier>,
        Identifier,
        LeftParens,
        Vec<(
            Option<CustomBlockParamKind>,
            Option<CanonicalIdentifier>,
            Identifier,
            Option<Comma>,
        )>,
        RightParens,
        CodeBlock,
        Option<Semicolon>,
        SourceSpan,
    )> {
        let (warp_specifier, start_pos) = match peek_token!(token_stream) {
            Token::WarpKeyword => (
                Some(WarpSpecifier {
                    is_warp: true,
                    source_span: {
                        skip_token!(token_stream);
                        get_token_source_span(token_stream)
                    },
                }),
                Some(get_token_start(token_stream)),
            ),
            Token::NowarpKeyword => (
                Some(WarpSpecifier {
                    is_warp: false,
                    source_span: {
                        skip_token!(token_stream);
                        get_token_source_span(token_stream)
                    },
                }),
                Some(get_token_start(token_stream)),
            ),
            _ => (None, None),
        };
        let proc_keyword = expect_token!(
            token_stream,
            Token::ProcKeyword => from_stream_pos!(token_stream => cst::ProcKeyword),
            "Expected \"proc\".",
            "\"proc\""
        );
        let start_pos = start_pos.unwrap_or_else(|| get_token_start(token_stream));
        let canonical_identifier = consume_and_use_if!(token_stream, Token::CanonicalIdentifier(value) => cst::CanonicalIdentifier {
            name: value,
            source_span: get_token_source_span(token_stream),
        });
        let identifier = parse_single_identifier_as_identifier(token_stream, context)?;
        let left_parens = expect_token!(
            token_stream,
            Token::LeftParens => from_stream_pos!(token_stream => cst::LeftParens),
            "Expected '('.",
            "'('"
        );
        let mut params = Vec::new();
        let right_parens = loop {
            consume_then_never_if!(token_stream, Token::RightParens => break from_stream_pos!(token_stream => cst::RightParens));
            let param_kind = if let Token::Identifier(ident) = peek_token!(token_stream) {
                match ident.as_str() {
                    "str" => Some(CustomBlockParamKindValue::String),
                    "bool" => Some(CustomBlockParamKindValue::Boolean),
                    "num" => Some(CustomBlockParamKindValue::Number),
                    _ => None,
                }
                .map(|value| {
                    Ok(CustomBlockParamKind {
                        kind: value,
                        source_span: {
                            skip_token!(token_stream);
                            get_token_source_span(token_stream)
                        },
                    })
                })
                .transpose()?
            } else {
                None
            };
            let canonical_identifier = consume_and_use_if!(token_stream, Token::CanonicalIdentifier(value) => cst::CanonicalIdentifier {
                name: value,
                source_span: get_token_source_span(token_stream),
            });
            let identifier = parse_single_identifier_as_identifier(token_stream, context)?;
            let comma = match peek_token!(token_stream) {
                Token::Comma => Some({
                    skip_token!(token_stream);
                    from_stream_pos!(token_stream => cst::Comma)
                }),
                Token::RightParens => None,
                _ => {
                    let token = next_token!(token_stream);
                    emit_unexpected_token!(
                        token_stream,
                        "Expected ',' or ')'.",
                        "',' or ')'",
                        token
                    )
                }
            };
            params.push((param_kind, canonical_identifier, identifier, comma));
        };
        let code_block = parse_code_block(token_stream, context)?;
        with_mut_next_target!(context, target => {
            use context::{TargetSymbolDescriptor, CustomBlockDescriptor, CustomBlockParamDescriptor};
            let symbols = target.borrow_symbols_mut();
            let name = &identifier.to_single().unwrap().0;
            if name.as_str() == "super" {
                return Err(ParseError::SymbolNamedSuper {
                    #[cfg(feature = "include_context_in_parse_errors")]
                    context: literal!(static_current_context!()),
                    source_span: identifier.to_single().unwrap().1,
                });
            }
            let previous_symbol = symbols.insert(
                name.clone(),
                TargetSymbolDescriptor::CustomBlockDescriptor(CustomBlockDescriptor {
                    name: identifier.to_single().unwrap().0.clone(),
                    canonical_name: canonical_identifier.as_ref().map(|value| &value.name).cloned(),
                    args: params.iter().map(|value| {
                        CustomBlockParamDescriptor {
                            name: value.2.to_single().unwrap().0.clone(),
                            canonical_name: value.1.as_ref().map(|value| &value.name).cloned(),
                            kind: value.0.as_ref().map(|value| &value.kind).copied().unwrap_or(CustomBlockParamKindValue::String),
                        }
                    }).collect(),
                    is_warp: warp_specifier.as_ref().map(|value| value.is_warp).unwrap_or(false)
                })
            );
            if let Some(previous_symbol) = previous_symbol {
                symbols.insert(name.clone(), previous_symbol);
                let single_identifier = identifier.to_single().unwrap();
                return Err(ParseError::ShadowedSymbol {
                    #[cfg(feature = "include_context_in_parse_errors")]
                    context: literal!(static_current_context!()),
                    symbol: single_identifier.0.clone(),
                    source_span: single_identifier.1,
                });
            }
        });
        Ok((
            warp_specifier,
            proc_keyword,
            canonical_identifier,
            identifier,
            left_parens,
            params,
            right_parens,
            code_block,
            consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon)),
            token_stream.span_from_previous_to_current(start_pos),
        ))
    }

    pub fn find_statement_end(token_stream: ParseIn) -> Result<(), ParseError> {
        let mut layers = 0_i32;
        let mut step_back = false;
        find_next_token(token_stream, |token| match token {
            Token::Semicolon => layers == 0,
            Token::LeftBrace => {
                layers += 1;
                false
            }
            Token::RightBrace(lexer::LexedRightBrace::Normal) => {
                // Example 1:
                // ```
                // {
                //     misformed-identifier
                // //  ^ Error is encountered here
                // }
                // ```
                // ^ Invalid statement ends here
                //
                // Example 2:
                // ```
                // {
                //     misformed-identifier {
                // //  ^ Error is encountered here
                //         something
                //     }
                // //  ^ Invalid statement ends here
                // }
                // ```
                // That is the reason for the one instead of a zero here.
                if layers > 1 {
                    layers -= 1;
                    false
                } else {
                    if layers == 0 {
                        step_back = true;
                    }
                    true
                }
            }
            _ => false,
        })?;
        if step_back {
            token_stream.step_back_if_unpeeked();
        }
        Ok(())
    }

    pub fn find_top_level_statement_end(token_stream: ParseIn) -> Result<(), ParseError> {
        let mut layers = 0_i32;
        find_next_token(token_stream, |token| match token {
            Token::Semicolon => layers == 0,
            Token::LeftBrace => {
                layers += 1;
                false
            }
            Token::RightBrace(lexer::LexedRightBrace::Normal) => {
                if layers > 1 {
                    layers -= 1;
                    false
                } else {
                    true
                }
            }
            _ => false,
        })?;
        Ok(())
    }
}

/// Statements do not include semicolons.
pub fn parse_statement(token_stream: ParseIn, context: &mut ParseContext) -> ParseOut<Statement> {
    let start = get_token_end(token_stream);
    match peek_token!(token_stream) {
        Token::LetKeyword => Ok(try_or_emit_message!(
            statement::parse_statement_data_declaration_fully(token_stream, context),
            context,
            find_statement_end_and_return_invalid!(token_stream, start, Statement)
        )),
        Token::Identifier(_) | Token::StageKeyword | Token::VarsKeyword | Token::ListsKeyword => {
            let identifier = try_or_emit_message!(
                parse_full_identifier(token_stream, context),
                context,
                find_statement_end_and_return_invalid!(token_stream, start, Statement)
            );
            Ok(try_or_emit_message!(
                statement::parse_statement_after_identifier(token_stream, context, identifier),
                context,
                find_statement_end_and_return_invalid!(token_stream, start, Statement)
            ))
        }
        Token::Semicolon => Ok(Statement::EmptyStatement(expect_token!(
            token_stream,
            Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon),
            "Expected ';'.",
            "';'"
        ))),
        _ => {
            let token = next_token!(token_stream);
            try_or_emit_message!(
                Err(create_unexpected_token_error(
                    token_stream,
                    literal!("Expected ';', \"let\" or an identifier."),
                    literal!("';', \"let\" or an identifier"),
                    #[cfg(feature = "include_context_in_parse_errors")]
                    literal!(static_current_context!()),
                    token
                )),
                context,
                find_statement_end_and_return_invalid!(token_stream, start, Statement)
            )
        }
    }
}

pub fn parse_sprite_statement(
    token_stream: ParseIn,
    context: &mut ParseContext,
) -> ParseOut<SpriteStatement> {
    match peek_token!(token_stream) {
        Token::CostumeKeyword => {
            skip_token!(token_stream);
            let costume_keyword = from_stream_pos!(token_stream => cst::CostumeKeyword);
            let start_pos = get_token_start(token_stream);
            Ok(SpriteStatement::CostumeDeclaration(
                costume_keyword,
                try_or_emit_message!(
                    statement::parse_asset_declaration(
                        token_stream,
                        context,
                        statement::AssetDeclarationType::Costume,
                    ),
                    context,
                    find_statement_end_and_return_invalid!(
                        token_stream,
                        start_pos,
                        SpriteStatement
                    )
                ),
                expect_token_or_message!(
                    token_stream,
                    context,
                    Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon),
                    "Expected ';'.",
                    "';'",
                    find_statement_end_and_return_invalid!(token_stream, start_pos, SpriteStatement)
                ),
                token_stream.span_from_previous_to_current(start_pos),
            ))
        }
        Token::SoundKeyword => {
            skip_token!(token_stream);
            let sound_keyword = from_stream_pos!(token_stream => cst::SoundKeyword);
            let start_pos = get_token_start(token_stream);
            Ok(SpriteStatement::SoundDeclaration(
                sound_keyword,
                try_or_emit_message!(
                    statement::parse_asset_declaration(
                        token_stream,
                        context,
                        statement::AssetDeclarationType::Sound,
                    ),
                    context,
                    find_statement_end_and_return_invalid!(
                        token_stream,
                        start_pos,
                        SpriteStatement
                    )
                ),
                expect_token_or_message!(
                    token_stream,
                    context,
                    Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon),
                    "Expected ';'.",
                    "';'",
                    find_statement_end_and_return_invalid!(token_stream, start_pos, SpriteStatement)
                ),
                token_stream.span_from_previous_to_current(start_pos),
            ))
        }
        Token::LetKeyword => {
            let start_pos = get_token_end(token_stream);
            Ok(try_or_emit_message!(
                statement::parse_sprite_data_declaration_fully(token_stream, context),
                context,
                find_statement_end_and_return_invalid!(token_stream, start_pos, SpriteStatement)
            ))
        }
        Token::NowarpKeyword | Token::WarpKeyword | Token::ProcKeyword => {
            let start_pos = get_token_end(token_stream);
            let (
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
            ) = try_or_emit_message!(
                statement::parse_custom_block(token_stream, context),
                context,
                find_statement_end_and_return_invalid!(token_stream, start_pos, SpriteStatement)
            );
            Ok(SpriteStatement::CustomBlockDefinition(
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
            ))
        }
        Token::Identifier(_) => {
            #[inline]
            fn parse_sprite_rest_of_single_input_control(
                token_stream: ParseIn,
                context: &mut ParseContext,
                identifier: Identifier,
                expression: Expression,
                start_pos: (usize, usize),
            ) -> ParseOut<SpriteStatement> {
                Ok(SpriteStatement::SingleInputHatStatement(
                    identifier,
                    expression,
                    try_or_emit_message!(
                        parse_code_block(token_stream, context),
                        context,
                        find_statement_end_and_return_invalid!(
                            token_stream,
                            start_pos,
                            SpriteStatement
                        )
                    ),
                    consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon)),
                    token_stream.span_from_previous_to_current(start_pos),
                ))
            }
            let start_pos = get_token_end(token_stream);
            let identifier = try_or_emit_message!(
                parse_full_identifier(token_stream, context),
                context,
                find_statement_end_and_return_invalid!(token_stream, start_pos, SpriteStatement)
            );
            let start_pos = identifier.source_span.0.0;
            match peek_token!(token_stream) {
                Token::LeftParens => {
                    let (left_parens, expressions, right_parens) = try_or_emit_message!(
                        parse_expression_list(token_stream, context),
                        context,
                        find_statement_end_and_return_invalid!(
                            token_stream,
                            start_pos,
                            SpriteStatement
                        )
                    );
                    if expressions.is_empty() {
                        let source_span = (left_parens.0.0, right_parens.0.1);
                        return parse_sprite_rest_of_single_input_control(
                            token_stream,
                            context,
                            identifier,
                            Expression::Literal(cst::Literal::EmptyExpression(
                                left_parens,
                                right_parens,
                                source_span,
                            )),
                            start_pos,
                        );
                    }
                    if expressions.len() == 1 && expressions[0].1.is_none() {
                        let source_span = (left_parens.0.0, right_parens.0.1);
                        return parse_sprite_rest_of_single_input_control(
                            token_stream,
                            context,
                            identifier,
                            Expression::Parentheses(
                                left_parens,
                                Box::new(expressions.into_iter().next().unwrap().0),
                                right_parens,
                                source_span,
                            ),
                            start_pos,
                        );
                    }
                    Ok(SpriteStatement::MultiInputHatStatement(
                        identifier,
                        left_parens,
                        expressions,
                        right_parens,
                        try_or_emit_message!(
                            parse_code_block(token_stream, context),
                            context,
                            find_statement_end_and_return_invalid!(
                                token_stream,
                                start_pos,
                                SpriteStatement
                            )
                        ),
                        consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon)),
                        token_stream.span_from_previous_to_current(start_pos),
                    ))
                }
                Token::LeftBrace => Ok(SpriteStatement::NoInputHatStatement(
                    identifier,
                    try_or_emit_message!(
                        parse_code_block(token_stream, context),
                        context,
                        find_statement_end_and_return_invalid!(
                            token_stream,
                            start_pos,
                            SpriteStatement
                        )
                    ),
                    consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon)),
                    token_stream.span_from_previous_to_current(start_pos),
                )),
                _ => {
                    let expression = try_or_emit_message!(
                        token_stream.substitude_unexpected_token_message(
                            |token_stream| parse_expression(token_stream, context),
                            literal!("Expected '(', '{' or an expression."),
                            literal!("'(', '{' or an expression"),
                        ),
                        context,
                        find_statement_end_and_return_invalid!(
                            token_stream,
                            start_pos,
                            SpriteStatement
                        )
                    );
                    Ok(try_or_emit_message!(
                        parse_sprite_rest_of_single_input_control(
                            token_stream,
                            context,
                            identifier,
                            expression,
                            start_pos,
                        ),
                        context,
                        find_statement_end_and_return_invalid!(
                            token_stream,
                            start_pos,
                            SpriteStatement
                        )
                    ))
                }
            }
        }
        Token::LeftBrace => {
            let start_pos = get_token_end(token_stream);
            let code_block = try_or_emit_message!(
                parse_code_block(token_stream, context),
                context,
                find_statement_end_and_return_invalid!(token_stream, start_pos, SpriteStatement)
            );
            let start_pos = code_block.source_span.0.0;
            Ok(SpriteStatement::IsolatedBlock(
                code_block,
                consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon)),
                token_stream.span_from_previous_to_current(start_pos),
            ))
        }
        Token::LeftParens => {
            skip_token!(token_stream);
            let left_parens = from_stream_pos!(token_stream => cst::LeftParens);
            let start_pos = get_token_start(token_stream);
            let expression = try_or_emit_message!(
                parse_expression(token_stream, context),
                context,
                find_statement_end_and_return_invalid!(token_stream, start_pos, SpriteStatement)
            );
            let right_parens = expect_token_or_message!(
                token_stream,
                context,
                Token::RightParens => from_stream_pos!(token_stream => cst::RightParens),
                "Expected ')'.",
                "')'",
                find_statement_end_and_return_invalid!(token_stream, start_pos, SpriteStatement)
            );
            Ok(SpriteStatement::IsolatedExpression(
                left_parens,
                expression,
                right_parens,
                consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon)),
                token_stream.span_from_previous_to_current(start_pos),
            ))
        }
        Token::Semicolon => {
            skip_token!(token_stream);
            Ok(SpriteStatement::EmptyStatement(
                from_stream_pos!(token_stream => cst::Semicolon),
            ))
        }
        _ => {
            let token = next_token!(token_stream);
            let start = get_token_start(token_stream);
            try_or_emit_message!(
                Err(create_unexpected_token_error(
                    token_stream,
                    literal!(
                        "Expected hat statement, '{', '(', ';', \"let\", \"nowarp\", \"warp\", \"proc\", \"sound\" or \"costume\"."
                    ),
                    literal!(
                        "hat statement, '{', '(', ';', \"let\", \"nowarp\", \"warp\", \"proc\", \"sound\" or \"costume\""
                    ),
                    #[cfg(feature = "include_context_in_parse_errors")]
                    literal!(static_current_context!()),
                    token
                )),
                context,
                find_statement_end_and_return_invalid!(token_stream, start, SpriteStatement)
            )
        }
    }
}

pub fn parse_stage_statement(
    token_stream: ParseIn,
    context: &mut ParseContext,
) -> ParseOut<StageStatement> {
    match peek_token!(token_stream) {
        Token::CostumeKeyword | Token::BackdropKeyword => {
            skip_token!(token_stream);
            let backdrop_keyword = from_stream_pos!(token_stream => cst::BackdropKeyword);
            let start_pos = get_token_start(token_stream);
            Ok(StageStatement::BackdropDeclaration(
                backdrop_keyword,
                try_or_emit_message!(
                    statement::parse_asset_declaration(
                        token_stream,
                        context,
                        statement::AssetDeclarationType::Backdrop,
                    ),
                    context,
                    find_statement_end_and_return_invalid!(token_stream, start_pos, StageStatement)
                ),
                expect_token_or_message!(
                    token_stream,
                    context,
                    Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon),
                    "Expected ';'.",
                    "';'",
                    find_statement_end_and_return_invalid!(token_stream, start_pos, StageStatement)
                ),
                token_stream.span_from_previous_to_current(start_pos),
            ))
        }
        Token::SoundKeyword => {
            skip_token!(token_stream);
            let sound_keyword = from_stream_pos!(token_stream => cst::SoundKeyword);
            let start_pos = get_token_start(token_stream);
            Ok(StageStatement::SoundDeclaration(
                sound_keyword,
                try_or_emit_message!(
                    statement::parse_asset_declaration(
                        token_stream,
                        context,
                        statement::AssetDeclarationType::Sound,
                    ),
                    context,
                    find_statement_end_and_return_invalid!(token_stream, start_pos, StageStatement)
                ),
                expect_token_or_message!(
                    token_stream,
                    context,
                    Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon),
                    "Expected ';'.",
                    "';'",
                    find_statement_end_and_return_invalid!(token_stream, start_pos, StageStatement)
                ),
                token_stream.span_from_previous_to_current(start_pos),
            ))
        }
        Token::LetKeyword => {
            let start_pos = get_token_end(token_stream);
            Ok(try_or_emit_message!(
                statement::parse_stage_data_declaration_fully(token_stream, context),
                context,
                find_statement_end_and_return_invalid!(token_stream, start_pos, StageStatement)
            ))
        }
        Token::NowarpKeyword | Token::WarpKeyword | Token::ProcKeyword => {
            let start_pos = get_token_end(token_stream);
            let (
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
            ) = try_or_emit_message!(
                statement::parse_custom_block(token_stream, context),
                context,
                find_statement_end_and_return_invalid!(token_stream, start_pos, StageStatement)
            );
            Ok(StageStatement::CustomBlockDefinition(
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
            ))
        }
        Token::Identifier(_) => {
            #[inline]
            fn parse_stage_rest_of_single_input_control(
                token_stream: ParseIn,
                context: &mut ParseContext,
                identifier: Identifier,
                expression: Expression,
                start_pos: (usize, usize),
            ) -> ParseOut<StageStatement> {
                Ok(StageStatement::SingleInputHatStatement(
                    identifier,
                    expression,
                    try_or_emit_message!(
                        parse_code_block(token_stream, context),
                        context,
                        find_statement_end_and_return_invalid!(
                            token_stream,
                            start_pos,
                            StageStatement
                        )
                    ),
                    consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon)),
                    token_stream.span_from_previous_to_current(start_pos),
                ))
            }
            let start_pos = get_token_end(token_stream);
            let identifier = try_or_emit_message!(
                parse_full_identifier(token_stream, context),
                context,
                find_statement_end_and_return_invalid!(token_stream, start_pos, StageStatement)
            );
            let start_pos = identifier.source_span.0.0;
            match peek_token!(token_stream) {
                Token::LeftParens => {
                    let (left_parens, expressions, right_parens) = try_or_emit_message!(
                        parse_expression_list(token_stream, context),
                        context,
                        find_statement_end_and_return_invalid!(
                            token_stream,
                            start_pos,
                            StageStatement
                        )
                    );
                    if expressions.is_empty() {
                        let source_span = (left_parens.0.0, right_parens.0.1);
                        return parse_stage_rest_of_single_input_control(
                            token_stream,
                            context,
                            identifier,
                            Expression::Literal(cst::Literal::EmptyExpression(
                                left_parens,
                                right_parens,
                                source_span,
                            )),
                            start_pos,
                        );
                    }
                    if expressions.len() == 1 && expressions[0].1.is_none() {
                        let source_span = (left_parens.0.0, right_parens.0.1);
                        return parse_stage_rest_of_single_input_control(
                            token_stream,
                            context,
                            identifier,
                            Expression::Parentheses(
                                left_parens,
                                Box::new(expressions.into_iter().next().unwrap().0),
                                right_parens,
                                source_span,
                            ),
                            start_pos,
                        );
                    }
                    Ok(StageStatement::MultiInputHatStatement(
                        identifier,
                        left_parens,
                        expressions,
                        right_parens,
                        try_or_emit_message!(
                            parse_code_block(token_stream, context),
                            context,
                            find_statement_end_and_return_invalid!(
                                token_stream,
                                start_pos,
                                StageStatement
                            )
                        ),
                        consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon)),
                        token_stream.span_from_previous_to_current(start_pos),
                    ))
                }
                Token::LeftBrace => Ok(StageStatement::NoInputHatStatement(
                    identifier,
                    try_or_emit_message!(
                        parse_code_block(token_stream, context),
                        context,
                        find_statement_end_and_return_invalid!(
                            token_stream,
                            start_pos,
                            StageStatement
                        )
                    ),
                    consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon)),
                    token_stream.span_from_previous_to_current(start_pos),
                )),
                _ => {
                    let expression = try_or_emit_message!(
                        token_stream.substitude_unexpected_token_message(
                            |token_stream| parse_expression(token_stream, context),
                            literal!("Expected '(', '{' or an expression."),
                            literal!("'(', '{' or an expression"),
                        ),
                        context,
                        find_statement_end_and_return_invalid!(
                            token_stream,
                            start_pos,
                            StageStatement
                        )
                    );
                    parse_stage_rest_of_single_input_control(
                        token_stream,
                        context,
                        identifier,
                        expression,
                        start_pos,
                    )
                }
            }
        }
        Token::LeftBrace => {
            let start_pos = get_token_end(token_stream);
            let code_block = try_or_emit_message!(
                parse_code_block(token_stream, context),
                context,
                find_statement_end_and_return_invalid!(token_stream, start_pos, StageStatement)
            );
            let start_pos = code_block.source_span.0.0;
            Ok(StageStatement::IsolatedBlock(
                code_block,
                consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon)),
                token_stream.span_from_previous_to_current(start_pos),
            ))
        }
        Token::LeftParens => {
            skip_token!(token_stream);
            let left_parens = from_stream_pos!(token_stream => cst::LeftParens);
            let start_pos = get_token_start(token_stream);
            let expression = try_or_emit_message!(
                parse_expression(token_stream, context),
                context,
                find_statement_end_and_return_invalid!(token_stream, start_pos, StageStatement)
            );
            let right_parens = expect_token_or_message!(
                token_stream,
                context,
                Token::RightParens => from_stream_pos!(token_stream => cst::RightParens),
                "Expected ')'.",
                "')'",
                find_statement_end_and_return_invalid!(token_stream, start_pos, StageStatement)
            );
            Ok(StageStatement::IsolatedExpression(
                left_parens,
                expression,
                right_parens,
                consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon)),
                token_stream.span_from_previous_to_current(start_pos),
            ))
        }
        Token::Semicolon => {
            skip_token!(token_stream);
            Ok(StageStatement::EmptyStatement(
                from_stream_pos!(token_stream => cst::Semicolon),
            ))
        }
        _ => {
            let token = next_token!(token_stream);
            let start = get_token_start(token_stream);
            try_or_emit_message!(
                Err(create_unexpected_token_error(
                    token_stream,
                    literal!(
                        "Expected hat statement, '{', '(', ';', \"let\", \"nowarp\", \"warp\", \"proc\", \"sound\", \"backdrop\" or \"costume\"."
                    ),
                    literal!(
                        "hat statement, '{', '(', ';', \"let\", \"nowarp\", \"warp\", \"proc\", \"sound\", \"backdrop\" or \"costume\""
                    ),
                    #[cfg(feature = "include_context_in_parse_errors")]
                    literal!(static_current_context!()),
                    token
                )),
                context,
                find_statement_end_and_return_invalid!(token_stream, start, StageStatement)
            )
        }
    }
}

pub fn parse_top_level_statement(
    token_stream: ParseIn,
    context: &mut ParseContext,
) -> ParseOut<TopLevelStatement> {
    match next_token!(token_stream) {
        Token::StageKeyword => {
            let stage_keyword = from_stream_pos!(token_stream => cst::StageKeyword);
            let start_pos = get_token_start(token_stream);
            context.next_target = Some(context.parsed_targets.swap_remove_front(0).unwrap());
            let return_value = Ok(TopLevelStatement::Stage(
                stage_keyword,
                try_or_emit_message!(
                    parse_stage_code_block(token_stream, context),
                    context,
                    find_top_level_statement_end_and_return_invalid!(
                        token_stream,
                        start_pos,
                        TopLevelStatement
                    )
                ),
                consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon)),
                token_stream.span_from_previous_to_current(start_pos),
            ));
            let stage = context.next_target.take().unwrap();
            context.parsed_targets.push_front(stage);
            return_value
        }
        Token::SpriteKeyword => {
            let sprite_keyword = from_stream_pos!(token_stream => cst::SpriteKeyword);
            let start_pos = get_token_start(token_stream);
            let canonical_identifier = consume_and_use_if!(
                token_stream,
                Token::CanonicalIdentifier(name) => cst::CanonicalIdentifier {
                    name,
                    source_span: get_token_source_span(token_stream)
                }
            );
            let identifier = try_or_emit_message!(
                parse_single_identifier_as_identifier(token_stream, context),
                context,
                find_top_level_statement_end_and_return_invalid!(
                    token_stream,
                    start_pos,
                    TopLevelStatement
                )
            );
            context.next_target = Some(context::Target::new_sprite(
                identifier.to_single().unwrap().0.clone(),
                canonical_identifier
                    .as_ref()
                    .map(|value| value.name.clone()),
            ));
            let return_val = Ok(TopLevelStatement::Sprite(
                sprite_keyword,
                canonical_identifier,
                identifier,
                try_or_emit_message!(
                    parse_sprite_code_block(token_stream, context),
                    context,
                    find_top_level_statement_end_and_return_invalid!(
                        token_stream,
                        start_pos,
                        TopLevelStatement
                    )
                ),
                consume_if!(token_stream, Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon)),
                token_stream.span_from_previous_to_current(start_pos),
            ));
            let target = context.next_target.take().unwrap();
            context.parsed_targets.push_back(target);
            return_val
        }
        Token::BroadcastKeyword => {
            let broadcast_keyword = from_stream_pos!(token_stream => cst::BroadcastKeyword);
            let start_pos = get_token_start(token_stream);
            let canonical_identifier = consume_and_use_if!(
                token_stream,
                Token::CanonicalIdentifier(name) => cst::CanonicalIdentifier {
                    name,
                    source_span: get_token_source_span(token_stream)
                }
            );
            let identifier = try_or_emit_message!(
                parse_single_identifier_as_identifier(token_stream, context),
                context,
                find_top_level_statement_end_and_return_invalid!(
                    token_stream,
                    start_pos,
                    TopLevelStatement
                )
            );
            let name = &identifier.to_single().unwrap().0;
            if name.as_str() == "super" {
                return Err(ParseError::SymbolNamedSuper {
                    #[cfg(feature = "include_context_in_parse_errors")]
                    context: literal!(static_current_context!()),
                    source_span: identifier.to_single().unwrap().1,
                });
            }
            let canonical_name = canonical_identifier
                .as_ref()
                .map(|value| value.name.clone());
            context.broadcasts.insert(
                name.clone(),
                BroadcastDescriptor {
                    name: name.clone(),
                    canonical_name,
                },
            );
            let return_val = Ok(TopLevelStatement::BroadcastDeclaration(
                broadcast_keyword,
                canonical_identifier,
                identifier,
                expect_token_or_message!(
                    token_stream,
                    context,
                    Token::Semicolon => from_stream_pos!(token_stream => cst::Semicolon),
                    "Expected ';'.",
                    "';'",
                    find_top_level_statement_end_and_return_invalid!(token_stream, start_pos, TopLevelStatement)
                ),
                token_stream.span_from_previous_to_current(start_pos),
            ));
            context.next_target = None;
            return_val
        }
        Token::Semicolon => Ok(TopLevelStatement::EmptyStatement(
            from_stream_pos!(token_stream => cst::Semicolon),
        )),
        token => {
            let start = get_token_start(token_stream);
            try_or_emit_message!(
                Err(create_unexpected_token_error(
                    token_stream,
                    literal!("Expected \"stage\", \"sprite\", \"broadcast\" or ';'."),
                    literal!("\"stage\", \"sprite\", \"broadcast\" or ';'"),
                    #[cfg(feature = "include_context_in_parse_errors")]
                    literal!(static_current_context!()),
                    token
                )),
                context,
                find_top_level_statement_end_and_return_invalid!(
                    token_stream,
                    start,
                    TopLevelStatement
                )
            )
        }
    }
}

pub fn parse_code_block(token_stream: ParseIn, context: &mut ParseContext) -> ParseOut<CodeBlock> {
    let left_brace = expect_token!(
        token_stream,
        Token::LeftBrace => from_stream_pos!(token_stream => cst::LeftBrace),
        "Expected '{'.",
        "'{'"
    );
    let start_pos = get_token_start(token_stream);
    let mut statements = Vec::<Statement>::new();
    let right_brace = loop {
        consume_then_never_if!(
            token_stream,
            Token::RightBrace(lexer::LexedRightBrace::Normal) => break from_stream_pos!(token_stream => cst::RightBrace)
        );
        statements.push(token_stream.substitude_unexpected_token_message(
            |token_stream| parse_statement(token_stream, context),
            literal!("Expected ';', '}', \"let\" or an identifier."),
            literal!("';', '}', \"let\" or an identifier"),
        )?);
    };
    Ok(CodeBlock {
        left_brace,
        statements,
        right_brace,
        source_span: token_stream.span_from_previous_to_current(start_pos),
    })
}

pub fn parse_sprite_code_block(
    token_stream: ParseIn,
    context: &mut ParseContext,
) -> ParseOut<SpriteCodeBlock> {
    expect_token!(token_stream, Token::LeftBrace => (), "Expected '{'.", "'{'");
    let start_pos = get_token_start(token_stream);
    let left_brace = from_stream_pos!(token_stream => cst::LeftBrace);
    let mut statements = Vec::<SpriteStatement>::new();
    let right_brace = loop {
        consume_then_never_if!(
            token_stream,
            Token::RightBrace(lexer::LexedRightBrace::Normal) => break from_stream_pos!(token_stream => cst::RightBrace)
        );
        statements.push(
            token_stream.substitude_unexpected_token_message(
                |token_stream| parse_sprite_statement(token_stream, context),
                literal!("Expected hat statement, '{', '}', '(', ';', \"let\", \"nowarp\", \"warp\", \"proc\", \"sound\" or \"costume\"."),
                literal!("hat statement, '{', '}', '(', ';', \"let\", \"nowarp\", \"warp\", \"proc\", \"sound\" or \"costume\""),
            )
            ?);
    };
    Ok(SpriteCodeBlock {
        left_brace,
        statements,
        right_brace,
        source_span: token_stream.span_from_previous_to_current(start_pos),
    })
}

pub fn parse_stage_code_block(
    token_stream: ParseIn,
    context: &mut ParseContext,
) -> ParseOut<StageCodeBlock> {
    expect_token!(token_stream, Token::LeftBrace => (), "Expected '{'.", "'{'");
    let start_pos = get_token_start(token_stream);
    let left_brace = from_stream_pos!(token_stream => cst::LeftBrace);
    let mut statements = Vec::<StageStatement>::new();
    let right_brace = loop {
        consume_then_never_if!(
            token_stream,
            Token::RightBrace(lexer::LexedRightBrace::Normal) => break from_stream_pos!(token_stream => cst::RightBrace)
        );
        statements.push(
            token_stream.substitude_unexpected_token_message(
                |token_stream| parse_stage_statement(token_stream, context),
                literal!("Expected hat statement, '{', '}', '(', ';', \"let\", \"nowarp\", \"warp\", \"proc\", \"sound\", \"backdrop\" or \"costume\"."),
                literal!("hat statement, '{', '}', '(', ';', \"let\", \"nowarp\", \"warp\", \"proc\", \"sound\", \"backdrop\" or \"costume\""),
            )?
        );
    };
    Ok(StageCodeBlock {
        left_brace,
        statements,
        right_brace,
        source_span: token_stream.span_from_previous_to_current(start_pos),
    })
}

pub mod expression {
    use std::collections::VecDeque;

    use crate::parser::cst::{
        Associativity, FormattedStringContent, GetPos, LeftBracket, LeftParens, RightBracket,
        RightParens,
    };

    use super::*;
    pub fn parse_expression_without_binops(
        token_stream: ParseIn,
        context: &mut ParseContext,
    ) -> ParseOut<Expression> {
        pub fn parse_expression_after_identifier(
            token_stream: ParseIn,
            context: &mut ParseContext,
            value: IString,
            token_source_span: SourceSpan,
        ) -> ParseOut<Expression> {
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
                        token_stream.span_from_previous_to_current(token_source_span.0.0),
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
                        token_stream.span_from_previous_to_current(token_source_span.0.0),
                    ))
                }
                _ => Ok(Expression::Identifier(identifier)),
            }
        }
        let token = next_token!(token_stream);
        let token_position = get_token_source_span(token_stream);
        use super::super::cst::Literal as LLiteral;
        use super::super::cst::UnOp;
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
            Token::Plus => {
                let token = next_token!(token_stream);
                if get_token_start(token_stream) != token_position.0.1 {
                    emit_unexpected_token!(
                        token_stream,
                        "Expected a decimal integer or decimal float immediately following the plus.",
                        "a decimal integer or decimal float immediately following the plus",
                        token
                    );
                }
                match token {
                    Token::DecimalInt(value) => Ok(ELiteral(LLiteral::DecimalInt(
                        arcstr::format!("+{}", value.as_str()),
                        token_stream.span_from_previous_to_current(token_position.0.0),
                    ))),
                    Token::DecimalFloat(value) => Ok(ELiteral(LLiteral::DecimalFloat(
                        arcstr::format!("+{}", value.as_str()),
                        token_stream.span_from_previous_to_current(token_position.0.0),
                    ))),
                    token => {
                        emit_unexpected_token!(
                            token_stream,
                            "Expected a decimal integer or decimal float.",
                            "a decimal integer or decimal float",
                            token
                        );
                    }
                }
            }
            Token::Minus => Ok(
                if matches!(
                    peek_token!(token_stream),
                    Token::DecimalFloat(_) | Token::DecimalInt(_)
                ) {
                    let new_token = next_token!(token_stream);
                    if token_position.0.1 == get_token_start(token_stream) {
                        match new_token {
                            Token::DecimalFloat(value) => ELiteral(LLiteral::DecimalFloat(
                                arcstr::format!("-{}", value.as_str()),
                                (
                                    (token_position.0.0, get_token_end(token_stream)),
                                    token_position.1,
                                ),
                            )),
                            Token::DecimalInt(value) => ELiteral(LLiteral::DecimalInt(
                                arcstr::format!("-{}", value.as_str()),
                                (
                                    (token_position.0.0, get_token_end(token_stream)),
                                    token_position.1,
                                ),
                            )),
                            _ => unreachable!(),
                        }
                    } else {
                        match new_token {
                            Token::DecimalFloat(value) => Expression::UnOp(
                                UnOp::Minus(token_position),
                                Box::new(ELiteral(LLiteral::DecimalFloat(
                                    value,
                                    get_token_source_span(token_stream),
                                ))),
                                token_stream.span_from_previous_to_current(token_position.0.0),
                            ),
                            Token::DecimalInt(value) => Expression::UnOp(
                                UnOp::Minus(token_position),
                                Box::new(ELiteral(LLiteral::DecimalInt(
                                    value,
                                    get_token_source_span(token_stream),
                                ))),
                                token_stream.span_from_previous_to_current(token_position.0.0),
                            ),
                            _ => unreachable!(),
                        }
                    }
                } else {
                    Expression::UnOp(
                        UnOp::Minus(token_position),
                        Box::new(parse_expression_without_binops(token_stream, context)?),
                        token_stream.span_from_previous_to_current(token_position.0.0),
                    )
                },
            ),
            Token::Not => Ok(Expression::UnOp(
                UnOp::Not(token_position),
                Box::new(parse_expression_without_binops(token_stream, context)?),
                token_stream.span_from_previous_to_current(token_position.0.0),
            )),
            Token::Exp => Ok(Expression::UnOp(
                UnOp::Exp(token_position),
                Box::new(parse_expression_without_binops(token_stream, context)?),
                token_stream.span_from_previous_to_current(token_position.0.0),
            )),
            Token::Pow => Ok(Expression::UnOp(
                UnOp::Pow(token_position),
                Box::new(parse_expression_without_binops(token_stream, context)?),
                token_stream.span_from_previous_to_current(token_position.0.0),
            )),
            Token::Identifier(value) => {
                parse_expression_after_identifier(token_stream, context, value, token_position)
            }
            Token::StageKeyword => {
                let value = literal!("stage");
                parse_expression_after_identifier(token_stream, context, value, token_position)
            }
            Token::VarsKeyword => {
                let value = literal!("vars");
                parse_expression_after_identifier(token_stream, context, value, token_position)
            }
            Token::ListsKeyword => {
                let value = literal!("lists");
                parse_expression_after_identifier(token_stream, context, value, token_position)
            }
            Token::LeftParens => {
                let left_parens = LeftParens(token_position);
                consume_then_never_if!(token_stream, Token::RightParens => {
                    return Ok(ELiteral(LLiteral::EmptyExpression(
                        left_parens,
                        from_stream_pos!(token_stream => RightParens),
                        token_stream.span_from_previous_to_current(token_position.0.0),
                    )));
                });
                let expr = parse_expression(token_stream, context)?;
                expect_token!(token_stream, Token::RightParens => (), "Expected ')'.", "')'");
                Ok(Expression::Parentheses(
                    left_parens,
                    Box::new(expr),
                    from_stream_pos!(token_stream => RightParens),
                    token_stream.span_from_previous_to_current(token_position.0.0),
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
                                    get_token_source_span(token_stream),
                                ));
                            }
                        }
                        Token::RightBrace(lexer::LexedRightBrace::RightFormattedString(string)) => {
                            if !string.is_empty() {
                                expressions.push(FormattedStringContent::String(
                                    string,
                                    get_token_source_span(token_stream),
                                ));
                            }
                            break;
                        }
                        token => {
                            emit_unexpected_token!(token_stream, "Expected '}'.", "'}'", token);
                        }
                    }
                }
                Ok(Expression::FormattedString(
                    expressions,
                    token_stream.span_from_previous_to_current(token_position.0.0),
                ))
            }
            token => emit_unexpected_token!(
                token_stream,
                "Expected an expression.",
                "an expression",
                token
            ),
        }
    }

    pub fn parse_binary_operation(
        token_stream: ParseIn,
        _context: &mut ParseContext,
    ) -> ParseOut<Option<BinOp>> {
        use super::super::cst::BinOp;
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
                let b_pos = b.as_ref().get_source_span().1;
                let a = Box::new(output_stack.pop().unwrap());
                let a_pos = a.as_ref().get_source_span().0;
                let op = operator_stack.pop().unwrap();
                output_stack.push(Expression::BinOp(a, op, b, (a_pos, b_pos)));
            }
            operator_stack.push(current_op);
        }
        output_stack.push(expressions.pop_front().unwrap());
        while !operator_stack.is_empty() {
            let b = Box::new(output_stack.pop().unwrap());
            let b_pos = b.as_ref().get_source_span().1;
            let a = Box::new(output_stack.pop().unwrap());
            let a_pos = a.as_ref().get_source_span().0;
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
        while let Some(left_bracket) = consume_if!(token_stream, Token::LetterAccessLeftBracket => from_stream_pos!(token_stream => cst::LetterAccessLeftBracket))
        {
            let string_expression = expressions.pop_back().unwrap();
            let inner_expression = parse_expression(token_stream, context)?;
            let right_bracket = expect_token!(
                token_stream,
                Token::RightBracket => from_stream_pos!(token_stream => cst::RightBracket),
                "Expected ']'.",
                "']'"
            );
            let source_span = string_expression.range_to_end(get_token_end(token_stream));
            expressions.push_back(Expression::GetLetter(
                Box::new(string_expression),
                left_bracket,
                Box::new(inner_expression),
                right_bracket,
                source_span,
            ));
        }
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
) -> ParseOut<(
    cst::LeftParens,
    Vec<(Expression, Option<cst::Comma>)>,
    cst::RightParens,
)> {
    expect_token!(token_stream, Token::LeftParens => (), "Expected '('.", "'('");
    let left_parens = from_stream_pos!(token_stream => cst::LeftParens);
    let mut expressions = Vec::<(Expression, Option<cst::Comma>)>::new();
    loop {
        consume_then_never_if!(token_stream, Token::RightParens => {
            break;
        });
        expressions.push((
            token_stream.substitude_unexpected_token_message(
                |token_stream| parse_expression(token_stream, context),
                literal!("Expected ')' or an expression."),
                literal!("')' or an expression"),
            )?,
            match peek_token!(token_stream) {
                Token::Comma => {
                    skip_token!(token_stream);
                    Some(cst::Comma(get_token_source_span(token_stream)))
                }
                Token::RightParens => None,
                _ => {
                    let token = next_token!(token_stream);
                    emit_unexpected_token!(
                        token_stream,
                        "Expected a comma or ')'.",
                        "a comma or ')'",
                        token
                    );
                }
            },
        ));
    }
    Ok((
        left_parens,
        expressions,
        from_stream_pos!(token_stream => cst::RightParens),
    ))
}
