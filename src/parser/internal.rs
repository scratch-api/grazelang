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

macro_rules! expect_token {
    ($peekable:expr, $variant:path) => {
        match $peekable.0.peek() {
            Some(Ok($variant(data))) => {
                let data = data.clone();
                skip_token!($peekable);
                Ok(Some(data))
            }
            Some(Err(_)) => Err(ParseError::LexerStuck(get_token_position!($peekable))),
            _ => Ok(None),
        }
    };
    ($peekable:expr, $variant:path, |$data:ident| $body:expr) => {
        match $peekable.0.peek() {
            Some(Ok($variant(data))) => {
                let $data = data.clone();
                skip_token!($peekable);
                Ok(Some($body))
            }
            Some(Err(_)) => Err(ParseError::LexerStuck(get_token_position!($peekable))),
            _ => Ok(None),
        }
    };
}

macro_rules! peek_token {
    ($peekable:expr) => {
        match $peekable.0.peek() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return { Err(ParseError::LexerStuck(get_token_position!($peekable))) },
            None => return Err(ParseError::UnexpectedEndOfInput),
        }
    };
    (optional $peekable:expr) => {
        match $peekable.0.peek() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return Err(ParseError::LexerStuck(get_token_position!($peekable))),
            None => return Ok(None),
        }
    };
}

macro_rules! next_token {
    ($peekable:expr) => {{
        $peekable.1.next();
        match $peekable.0.next() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return Err(ParseError::LexerStuck(get_token_position!($peekable))),
            None => return Err(ParseError::UnexpectedEndOfInput),
        }
    }};
    (optional $peekable:expr) => {{
        $peekable.1.next();
        match $peekable.0.next() {
            Some(Ok(value)) => value,
            Some(Err(_)) => return Err(ParseError::LexerStuck(get_token_position!($peekable))),
            None => return Ok(None),
        }
    }};
}

macro_rules! skip_token {
    ($peekable:expr) => {{
        $peekable.1.next();
        if let Some(Err(_)) = $peekable.0.next() {
            return Err(ParseError::LexerStuck(get_token_position!($peekable)));
        }
    }};
}

macro_rules! get_token_position {
    ($peekable:expr) => {
        internal_get_pos_range($peekable.1, $peekable.1.span())
    };
}

macro_rules! get_token_start {
    ($peekable:expr) => {
        internal_get_position($peekable.1, $peekable.1.span().start)
    };
}

macro_rules! get_token_end {
    ($peekable:expr) => {
        internal_get_position($peekable.1, $peekable.1.span().end)
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

pub fn parse_lexed_entrypoint(peekable: ParseIn, namespace: &mut Namespace) {
    todo!()
}

pub fn parse_single_identifier(peekable: ParseIn, namespace: &mut Namespace) -> ParseOut<IString> {
    if let Token::Identifier(value) = next_token!(peekable) {
        return Ok(value);
    }
    Err(ParseError::UnexpectedToken(
        "Expected an identifier.",
        "an identifier",
        get_token_position!(peekable),
    ))
}

pub fn parse_full_identifier(peekable: ParseIn, namespace: &mut Namespace) -> ParseOut<Identifier> {
    let mut names: Vec<(IString, PosRange)> =
        vec![if let Token::Identifier(value) = next_token!(peekable) {
            (value, get_token_position!(peekable))
        } else {
            return Err(ParseError::UnexpectedToken(
                "Expected an identifier.",
                "an identifier",
                get_token_position!(peekable),
            ));
        }];
    let start_pos = get_token_start!(peekable);
    let mut scope: Option<Vec<(IString, PosRange)>> = None;
    loop {
        match peek_token!(peekable) {
            Token::ScopeResolution => {
                if let Some(_) = scope {
                    skip_token!(peekable);
                    return Err(ParseError::UnexpectedToken(
                        "Expected a dot.",
                        "a dot",
                        get_token_position!(peekable),
                    ));
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
        skip_token!(peekable);
        if let Token::Identifier(value) = next_token!(peekable) {
            names.push((value, get_token_position!(peekable)));
        } else {
            return Err(ParseError::UnexpectedToken(
                "Expected an identifier.",
                "an identifier",
                get_token_position!(peekable),
            ));
        }
    }
    if names.len() > 1 && scope == None {
        scope = Some(names);
        names = Vec::new();
    }
    Ok(Identifier {
        scope: scope.unwrap_or_default(),
        names,
        pos_range: (start_pos, get_token_end!(peekable)),
    })
}

pub fn parse_full_identifier_starting_with(
    peekable: ParseIn,
    namespace: &mut Namespace,
    value: IString,
) -> ParseOut<Identifier> {
    let mut names: Vec<(IString, PosRange)> = vec![(value, get_token_position!(peekable))];
    let start_pos = get_token_start!(peekable);
    let mut scope: Option<Vec<(IString, PosRange)>> = None;
    loop {
        match peek_token!(peekable) {
            Token::ScopeResolution => {
                if let Some(_) = scope {
                    return Err(ParseError::UnexpectedToken(
                        "Expected a dot.",
                        "a dot",
                        get_token_position!(peekable),
                    ));
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
        skip_token!(peekable);
        if let Token::Identifier(value) = next_token!(peekable) {
            names.push((value, get_token_position!(peekable)));
        } else {
            return Err(ParseError::UnexpectedToken(
                "Expected an identifier.",
                "an identifier",
                get_token_position!(peekable),
            ));
        }
    }
    if names.len() > 1 && scope == None {
        scope = Some(names);
        names = Vec::new();
    }
    Ok(Identifier {
        scope: scope.unwrap_or_default(),
        names,
        pos_range: (start_pos, get_token_end!(peekable)),
    })
}

/// `parse_*` `fn`s contained don't swallow semicolons.
pub mod statement {
    use crate::parser::ast::{
        CanonicalIdentifier, DataDeclarationScope, LeftParens, LetKeyword, ListsKeyword,
        MultiDataDeclaration, RightParens, SingleDataDeclaration, SingleDataDeclarationType,
        VarsKeyword,
    };

    use super::*;

    pub fn register_single_data_declaration(
        namespace: &mut Namespace,
        single_data_declaration: &SingleDataDeclaration,
    ) {
        todo!()
    }

    pub fn parse_literal(peekable: ParseIn, namespace: &mut Namespace) -> ParseOut<ast::Literal> {
        use Expression::Literal as ELiteral;
        use ast::Literal as LLiteral;
        match next_token!(peekable) {
            Token::SimpleString(value) => {
                Ok(LLiteral::String(value, get_token_position!(peekable)))
            }
            Token::DecimalInt(value) => {
                Ok(LLiteral::DecimalInt(value, get_token_position!(peekable)))
            }
            Token::DecimalFloat(value) => {
                Ok(LLiteral::DecimalFloat(value, get_token_position!(peekable)))
            }
            Token::HexadecimalInt(value) => Ok(LLiteral::HexadecimalInt(
                value,
                get_token_position!(peekable),
            )),
            Token::OctalInt(value) => Ok(LLiteral::OctalInt(value, get_token_position!(peekable))),
            Token::BinaryInt(value) => {
                Ok(LLiteral::BinaryInt(value, get_token_position!(peekable)))
            }
            Token::LeftParens => {
                if peek_token!(peekable) == &Token::RightParens {
                    let left_parens_position = get_token_position!(peekable);
                    return Ok(LLiteral::EmptyExpression(
                        LeftParens(left_parens_position),
                        {
                            skip_token!(peekable);
                            RightParens(get_token_position!(peekable))
                        },
                        (left_parens_position.0, get_token_end!(peekable)),
                    ));
                }
                let value = parse_literal(peekable, namespace)?;
                if next_token!(peekable) != Token::RightParens {
                    return Err(ParseError::UnexpectedToken(
                        "Expected ')'.",
                        "')'",
                        get_token_position!(peekable),
                    ));
                }
                Ok(value)
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    "Expected a literal.",
                    "a literal",
                    get_token_position!(peekable),
                ));
            }
        }
    }

    #[derive(PartialEq)]
    enum DataDeclarationType {
        ImplicitVar,
        Var(PosRange),
        List(PosRange),
        Vars(PosRange),
        Lists(PosRange),
    }

    impl DataDeclarationType {
        fn get_single_data_declaration_type(&self) -> Option<SingleDataDeclarationType> {
            match self {
                DataDeclarationType::ImplicitVar => Some(SingleDataDeclarationType::ImplicitVar),
                DataDeclarationType::Var(p) => Some(SingleDataDeclarationType::Var(*p)),
                DataDeclarationType::List(p) => Some(SingleDataDeclarationType::List(*p)),
                DataDeclarationType::Vars(_) => None,
                DataDeclarationType::Lists(_) => None,
            }
        }
        fn get_vars_keyword(&self) -> Option<VarsKeyword> {
            if let DataDeclarationType::Vars(p) = self {
                return Some(VarsKeyword(*p));
            }
            None
        }
        fn get_lists_keyword(&self) -> Option<ListsKeyword> {
            if let DataDeclarationType::Lists(p) = self {
                return Some(ListsKeyword(*p));
            }
            None
        }
    }

    fn parse_inner_single_declarations(
        peekable: ParseIn,
        namespace: &mut Namespace,
        scope: DataDeclarationScope,
        dec_type: DataDeclarationType,
    ) {
        todo!()
    }

    pub fn parse_data_declaration(
        peekable: ParseIn,
        namespace: &mut Namespace,
    ) -> ParseOut<Statement> {
        use DataDeclarationType as DDS;
        if next_token!(peekable) != Token::LetKeyword {
            return Err(ParseError::UnexpectedToken(
                "Data declaration needs to start with \"let\".",
                "\"let\"",
                get_token_position!(peekable),
            ));
        }
        let let_keyword_position = get_token_position!(peekable);
        let start_token = next_token!(peekable);
        let start_position = get_token_position!(peekable);
        let mut current_token = start_token.clone();
        let mut scope = DataDeclarationScope::ImplicitLocal;
        let mut dec_type = DDS::ImplicitVar;
        let mut canonical_identifier = Option::<CanonicalIdentifier>::None;
        loop {
            match current_token {
                Token::LeftParens => {
                    let mut declarations =
                        Vec::<(SingleDataDeclarationType, SingleDataDeclaration)>::new();
                    if peek_token!(peekable) == &Token::RightParens {
                        skip_token!(peekable);
                        return Ok(Statement::DataDeclaration(
                            LetKeyword(let_keyword_position),
                            MultiDataDeclaration::Mixed(
                                LeftParens(start_position),
                                declarations,
                                RightParens(get_token_position!(peekable)),
                                (start_position.0, get_token_end!(peekable)),
                            ),
                            (let_keyword_position.0, get_token_end!(peekable)),
                        ));
                    }
                    loop {
                        declarations.push(todo!());
                        match next_token!(peekable) {
                            Token::Comma => (),
                            Token::RightParens => break,
                            _ => {
                                return Err(ParseError::UnexpectedToken(
                                    "Expected ',' or ')'.",
                                    "',' or ')'",
                                    get_token_position!(peekable),
                                ));
                            }
                        }
                    }
                    return Ok(Statement::DataDeclaration(
                        LetKeyword(let_keyword_position),
                        MultiDataDeclaration::Mixed(
                            LeftParens(start_position),
                            declarations,
                            RightParens(get_token_position!(peekable)),
                            (start_position.0, get_token_end!(peekable)),
                        ),
                        (let_keyword_position.0, get_token_end!(peekable)),
                    ));
                }
                Token::Identifier(identifier) => {
                    match dec_type {
                        DDS::Vars(_) => {
                            return Err(ParseError::UnexpectedToken(
                                "A vars declaration cannot be completed via assignment. Use \"let vars {<declarations>};\" instead.",
                                "'{'",
                                get_token_position!(peekable),
                            ));
                        }
                        DDS::Lists(_) => {
                            return Err(ParseError::UnexpectedToken(
                                "A lists declaration cannot be completed via assignment. Use \"let lists {<declarations>};\" instead.",
                                "'{'",
                                get_token_position!(peekable),
                            ));
                        }
                        _ => (),
                    }
                    match peek_token!(peekable) {
                        Token::Assign => {
                            if let DDS::List(_) = dec_type {
                                return Err(ParseError::UnexpectedToken(
                                    "Cannot declare a list via assignment. Use \"let list <list_name>{<elements>};\" instead.",
                                    "'{'",
                                    get_token_position!(peekable),
                                ));
                            }
                            skip_token!(peekable);
                            let assignment_operator_pos = get_token_position!(peekable);
                            let value = parse_literal(peekable, namespace)?;
                            return Ok(Statement::DataDeclaration(
                                LetKeyword(let_keyword_position),
                                MultiDataDeclaration::Single(
                                    dec_type.get_single_data_declaration_type().unwrap(),
                                    SingleDataDeclaration::Variable(
                                        scope,
                                        canonical_identifier,
                                        Identifier {
                                            scope: Vec::new(),
                                            names: vec![(identifier.clone(), start_position)],
                                            pos_range: start_position,
                                        },
                                        ast::AssignmentOperator::Set(assignment_operator_pos),
                                        value,
                                        (start_position.0, get_token_end!(peekable)),
                                    ),
                                ),
                                (let_keyword_position.0, get_token_end!(peekable)),
                            ));
                        }
                        Token::Semicolon => {
                            return Ok(Statement::DataDeclaration(
                                LetKeyword(let_keyword_position),
                                MultiDataDeclaration::Single(
                                    dec_type.get_single_data_declaration_type().unwrap(),
                                    SingleDataDeclaration::EmptyVariable(
                                        ast::DataDeclarationScope::ImplicitLocal,
                                        canonical_identifier,
                                        Identifier {
                                            scope: Vec::new(),
                                            names: vec![(identifier.clone(), start_position)],
                                            pos_range: start_position,
                                        },
                                        (start_position.0, get_token_end!(peekable)),
                                    ),
                                ),
                                (let_keyword_position.0, get_token_end!(peekable)),
                            ));
                        }
                        _ => {
                            return Err(ParseError::UnexpectedToken(
                                "Expected an assignment operator or ';'.",
                                "an assignment operator or ';'.",
                                get_token_position!(peekable),
                            ));
                        }
                    }
                }
                Token::GlobalKeyword => {
                    if scope != DataDeclarationScope::ImplicitLocal {
                        return Err(ParseError::UnexpectedToken(
                            "Cannot set the scope of a variable or list multiple times.",
                            "a canonical name, an identifier, \"vars\" or \"lists\"",
                            get_token_position!(peekable),
                        ));
                    }
                    scope = DataDeclarationScope::Global(get_token_position!(peekable));
                }
                Token::LocalKeyword => {
                    if scope != DataDeclarationScope::ImplicitLocal {
                        return Err(ParseError::UnexpectedToken(
                            "Cannot set the scope of a variable or list multiple times.",
                            "a canonical name, an identifier, \"vars\" or \"lists\"",
                            get_token_position!(peekable),
                        ));
                    }
                    scope = DataDeclarationScope::Local(get_token_position!(peekable));
                }
                Token::CloudKeyword => {
                    if scope != DataDeclarationScope::ImplicitLocal {
                        return Err(ParseError::UnexpectedToken(
                            "Cannot set the scope of a variable or list multiple times.",
                            "a canonical name, an identifier, \"var\", \"list\", \"vars\" or \"lists\"",
                            get_token_position!(peekable),
                        ));
                    }
                    scope = DataDeclarationScope::Cloud(get_token_position!(peekable));
                }
                Token::VarKeyword => {
                    if dec_type != DDS::ImplicitVar {
                        return Err(ParseError::UnexpectedToken(
                            "Cannot set the type of a declaration multiple times.",
                            "a canonical name, an identifier, a modifier or '('",
                            get_token_position!(peekable),
                        ));
                    }
                    dec_type = DDS::Var(get_token_position!(peekable));
                }
                Token::ListKeyword => {
                    if dec_type != DDS::ImplicitVar {
                        return Err(ParseError::UnexpectedToken(
                            "Cannot set the type of a declaration multiple times.",
                            "a canonical name, an identifier, a modifier or '('",
                            get_token_position!(peekable),
                        ));
                    }
                    dec_type = DDS::List(get_token_position!(peekable));
                }
                Token::VarsKeyword => {
                    if dec_type != DDS::ImplicitVar {
                        return Err(ParseError::UnexpectedToken(
                            "Cannot set the type of a declaration multiple times.",
                            "a canonical name, an identifier, a modifier or '('",
                            get_token_position!(peekable),
                        ));
                    }
                    dec_type = DDS::Vars(get_token_position!(peekable));
                }
                Token::ListsKeyword => {
                    if dec_type != DDS::ImplicitVar {
                        return Err(ParseError::UnexpectedToken(
                            "Cannot set the type of a declaration multiple times.",
                            "a canonical name, an identifier, a modifier or '('",
                            get_token_position!(peekable),
                        ));
                    }
                    dec_type = DDS::Lists(get_token_position!(peekable));
                }
                Token::LeftBrace => {
                    todo!()
                }
                Token::CanonicalName(value) => {
                    if canonical_identifier != None {
                        return Err(ParseError::UnexpectedToken(
                            "Cannot set the canonical name of a declaration multiple times.",
                            "an identifier, a modifier, '(', \"var\", \"list\", \"vars\" or \"lists\"",
                            get_token_position!(peekable),
                        ));
                    }
                    canonical_identifier = Some(CanonicalIdentifier {
                        name: value,
                        pos_range: get_token_position!(peekable),
                    })
                }
                _ => {
                    return Err(ParseError::UnexpectedToken(
                        "Expected a canonical name, an identifier, a modifier, '(', \"var\", \"list\", \"vars\" or \"lists\".",
                        "a canonical name, an identifier, a modifier, '(', \"var\", \"list\", \"vars\" or \"lists\"",
                        get_token_position!(peekable),
                    ));
                }
            }
            current_token = next_token!(peekable);
        }
    }
}

pub fn parse_statement(peekable: ParseIn, namespace: &mut Namespace) -> ParseOut<Statement> {
    todo!()
}

pub mod expression {
    use std::collections::VecDeque;

    use crate::parser::ast::{
        Associativity, FormattedStringContent, GetPos, LeftBracket, LeftParens, RightBracket,
        RightParens,
    };

    use super::*;
    pub fn parse_expression_without_binops(
        peekable: ParseIn,
        namespace: &mut Namespace,
    ) -> ParseOut<Expression> {
        let token = next_token!(peekable);
        let token_position = get_token_position!(peekable);
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
                Box::new(parse_expression_without_binops(peekable, namespace)?),
                (token_position.0, get_token_end!(peekable)),
            )),
            Token::Not => Ok(Expression::UnOp(
                UnOp::Not(token_position),
                Box::new(parse_expression_without_binops(peekable, namespace)?),
                (token_position.0, get_token_end!(peekable)),
            )),
            Token::Exp => Ok(Expression::UnOp(
                UnOp::Exp(token_position),
                Box::new(parse_expression_without_binops(peekable, namespace)?),
                (token_position.0, get_token_end!(peekable)),
            )),
            Token::Pow => Ok(Expression::UnOp(
                UnOp::Pow(token_position),
                Box::new(parse_expression_without_binops(peekable, namespace)?),
                (token_position.0, get_token_end!(peekable)),
            )),
            Token::Identifier(value) => {
                let identifier = parse_full_identifier_starting_with(peekable, namespace, value)?;
                match peek_token!(peekable) {
                    Token::LeftParens => {
                        let (left_parens, expressions, right_parens) =
                            parse_expression_list(peekable, namespace)?;
                        Ok(Expression::Call(
                            identifier,
                            left_parens,
                            expressions,
                            right_parens,
                            (token_position.0, get_token_end!(peekable)),
                        ))
                    }
                    Token::LeftBracket => {
                        skip_token!(peekable);
                        Ok(Expression::GetItem(
                            identifier,
                            LeftBracket(get_token_position!(peekable)),
                            Box::new(parse_expression(peekable, namespace)?),
                            {
                                if next_token!(peekable) != Token::RightBracket {
                                    return Err(ParseError::UnexpectedToken(
                                        "Expected ']'.",
                                        "']'",
                                        get_token_position!(peekable),
                                    ));
                                }
                                RightBracket(get_token_position!(peekable))
                            },
                            (token_position.0, get_token_end!(peekable)),
                        ))
                    }
                    _ => Ok(Expression::Identifier(identifier)),
                }
            }
            Token::LeftParens => {
                let left_parens = LeftParens(token_position);
                if peek_token!(peekable) == &Token::RightParens {
                    skip_token!(peekable);
                    return Ok(ELiteral(LLiteral::EmptyExpression(
                        left_parens,
                        RightParens(get_token_position!(peekable)),
                        (token_position.0, get_token_end!(peekable)),
                    )));
                }
                let expr = parse_expression(peekable, namespace)?;
                let end_pos = if let Token::RightParens = next_token!(peekable) {
                    get_token_end!(peekable)
                } else {
                    return Err(ParseError::UnexpectedToken(
                        "Excected ')'.",
                        "')'",
                        get_token_position!(peekable),
                    ));
                };
                Ok(Expression::Parentheses(
                    left_parens,
                    Box::new(expr),
                    RightParens(get_token_position!(peekable)),
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
                        peekable, namespace,
                    )?));
                    match next_token!(peekable) {
                        Token::MiddleFormattedString(string) => {
                            if !string.is_empty() {
                                expressions.push(FormattedStringContent::String(
                                    string,
                                    get_token_position!(peekable),
                                ));
                            }
                        }
                        Token::RightFormattedString(string) => {
                            if !string.is_empty() {
                                expressions.push(FormattedStringContent::String(
                                    string,
                                    get_token_position!(peekable),
                                ));
                            }
                            break;
                        }
                        _ => {
                            return Err(ParseError::UnexpectedToken(
                                "Expected '}'.",
                                "'}'",
                                get_token_position!(peekable),
                            ));
                        }
                    }
                }
                Ok(Expression::FormattedString(
                    expressions,
                    (token_position.0, get_token_end!(peekable)),
                ))
            }
            _ => Err(ParseError::UnexpectedToken(
                "Expected an expression.",
                "an expression",
                get_token_position!(peekable),
            )),
        }
    }

    pub fn parse_binary_operation(
        peekable: ParseIn,
        namespace: &mut Namespace,
    ) -> ParseOut<Option<BinOp>> {
        let token = peek_token!(optional peekable);
        use super::super::ast::BinOp;
        let result = match token {
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
            _ => return Ok(None),
        };
        skip_token!(peekable);
        Ok(Some(result(get_token_position!(peekable))))
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

pub fn parse_expression(peekable: ParseIn, namespace: &mut Namespace) -> ParseOut<Expression> {
    use expression::*;
    let mut expressions = VecDeque::from([parse_expression_without_binops(peekable, namespace)?]);
    let mut binops = VecDeque::<BinOp>::new();

    loop {
        binops.push_back(match parse_binary_operation(peekable, namespace)? {
            Some(value) => value,
            None => break,
        });
        expressions.push_back(parse_expression_without_binops(peekable, namespace)?);
    }

    Ok(order_operations(expressions, binops))
}

pub fn parse_expression_list(
    peekable: ParseIn,
    namespace: &mut Namespace,
) -> ParseOut<(ast::LeftParens, Vec<Expression>, ast::RightParens)> {
    if next_token!(peekable) != Token::LeftParens {
        return Err(ParseError::UnexpectedToken(
            "Expected '('.",
            "'('",
            get_token_position!(peekable),
        ));
    }
    let left_parens = ast::LeftParens(get_token_position!(peekable));
    let mut expressions = Vec::<Expression>::new();
    if peek_token!(peekable) == &Token::RightParens {
        skip_token!(peekable);
        return Ok((
            left_parens,
            expressions,
            ast::RightParens(get_token_position!(peekable)),
        ));
    }
    loop {
        expressions.push(parse_expression(peekable, namespace)?);
        match next_token!(peekable) {
            Token::Comma => (),
            Token::RightParens => break,
            _ => {
                return Err(ParseError::UnexpectedToken(
                    "Expected a comma or ')'.",
                    "a comma or ')'",
                    get_token_position!(peekable),
                ));
            }
        }
    }
    Ok((
        left_parens,
        expressions,
        ast::RightParens(get_token_position!(peekable)),
    ))
}
