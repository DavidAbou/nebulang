//
// EPITECH PROJECT, 2023
// nebulang [WSL : Ubuntu]
// File description:
// parser
//

use std::fmt::Display;

use crate::utils::tokenizer::{Token, TokenKind};

pub enum ParseErrorKind {
    UnexpectedToken,
    SyntaxError,
    TypeError
}

pub struct ParseError {
    kind: ParseErrorKind,
    pub message: String
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, message: String) -> Self {
        Self {
            kind,
            message
        }
    }

    pub fn unexpected_token(message: String) -> Self {
        Self::new(ParseErrorKind::UnexpectedToken, message)
    }

    pub fn syntax_error(message: String) -> Self {
        Self::new(ParseErrorKind::SyntaxError, message)
    }

    pub fn type_error(message: String) -> Self {
        Self::new(ParseErrorKind::TypeError, message)
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            ParseErrorKind::UnexpectedToken => write!(f, "ParseError: Unexpected token: {}", self.message),
            ParseErrorKind::SyntaxError => write!(f, "ParseError: Syntax error: {}", self.message),
            ParseErrorKind::TypeError => write!(f, "ParseError: Type error: {}", self.message)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnotation {
    Int,
    Char,
    String,
    Bool,
    Sequence(Box<TypeAnnotation>),
    Void
}

#[derive(Debug, Clone)]
pub enum AstNode {
    Program(Vec<AstNode>),
    EmitStmt(Box<AstNode>),
    IntLiteral(i32),
    CharLiteral(char),
    StringLiteral(String),
    BoolLiteral(bool),
    SequenceLiteral(Vec<AstNode>),
    Identifier(String),
    SetStmt(String, TypeAnnotation, Box<AstNode>),
    AssignmentStmt(Box<AstNode>, Box<AstNode>),
    BinaryExpr(Box<AstNode>, BinaryOperator, Box<AstNode>),
    ScopeStmt(Vec<AstNode>),
    IfStmt(Box<AstNode>, Box<AstNode>, Option<Box<AstNode>>),
    UnlessStmt(Box<AstNode>, Box<AstNode>, Option<Box<AstNode>>),
    WhileStmt(Box<AstNode>, Box<AstNode>),
    IterStmt {
        variable: (String, TypeAnnotation),
        sequence: Box<AstNode>,
        body: Box<AstNode>
    },
    ExperimentDecl {
        name: String,
        parameters: Vec<(String, TypeAnnotation)>,
        exp_type: TypeAnnotation,
        body: Box<AstNode>
    },
    ExperimentCall {
        name: String,
        arguments: Vec<AstNode>
    },
    InjectStmt {
        experiments: Vec<String>,
        file: String
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Is,
    Not,
    Inf,
    Sup,
    Leq,
    Geq
}

impl BinaryOperator {
    pub fn precedence(&self) -> u8 {
        match self {
            BinaryOperator::Add => 1,
            BinaryOperator::Sub => 1,
            BinaryOperator::Mul => 2,
            BinaryOperator::Div => 2,
            BinaryOperator::Mod => 2,
            BinaryOperator::Pow => 3,
            BinaryOperator::Is => 4,
            BinaryOperator::Not => 4,
            BinaryOperator::Inf => 5,
            BinaryOperator::Sup => 5,
            BinaryOperator::Leq => 5,
            BinaryOperator::Geq => 5
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0
        }
    }

    pub fn parse(&mut self) -> Result<AstNode, ParseError> {
        let mut nodes = Vec::new();

        while !self.is_at_end() {
            nodes.push(self.parse_statement()?);
        }

        let nodes = nodes.into_iter()
            .filter_map(|node| node)
            .collect();

        Ok(AstNode::Program(nodes))
    }

    fn parse_type_annotation(&mut self) -> Result<TypeAnnotation, ParseError> {
        let token = self.advance();

        match &token.kind {
            TokenKind::Identifier => {
                match token.text.as_ref().map(String::as_str) {
                    Some("Int") => Ok(TypeAnnotation::Int),
                    Some("String") => Ok(TypeAnnotation::String),
                    Some("Char") => Ok(TypeAnnotation::Char),
                    Some("Bool") => Ok(TypeAnnotation::Bool),
                    Some("Seq") => {
                        self.expect(TokenKind::OpenBracket)?;
                        let inner_type = self.parse_type_annotation()?;
                        self.expect(TokenKind::CloseBracket)?;
                        Ok(TypeAnnotation::Sequence(Box::new(inner_type)))
                    },
                    Some("Void") => Ok(TypeAnnotation::Void),
                    _ => Err(ParseError::type_error(format!("Unknown type: {:?}", token)))
                }
            },
            _ => Err(ParseError::type_error(format!("Expected type annotation, found {:?}", token)))
        }
    }

    fn parse_emit_stmt(&mut self) -> Result<AstNode, ParseError> {
        let expression = self.parse_expression(0)?;

        self.expect(TokenKind::Semi)?;

        return Ok(AstNode::EmitStmt(Box::new(expression)));
    }

    fn parse_set_stmt(&mut self) ->Result<AstNode, ParseError> {
        let identifier = match self.advance() {
            Token { kind: TokenKind::Identifier, text: Some(text) } => text.to_string(),
            _ => return Err(ParseError::unexpected_token(format!("Expected identifier after set keyword")))
        };

        self.expect(TokenKind::Colon)?;

        let type_annotation = self.parse_type_annotation()?;

        self.expect(TokenKind::Eq)?;

        let expression = self.parse_expression(0)?;

        self.expect(TokenKind::Semi)?;

        Ok(AstNode::SetStmt(identifier, type_annotation, Box::new(expression)))
    }

    fn parse_scope_stmt(&mut self) -> Result<AstNode, ParseError> {
        let mut nodes = Vec::new();

        while let Some(token) = self.peek(0) {
            if token.kind == TokenKind::CloseCurly {
                self.advance();
                break;
            } else {
                nodes.push(self.parse_statement()?);
            }
        }

        let nodes = nodes.into_iter()
            .filter_map(|node| node)
            .collect();

        Ok(AstNode::ScopeStmt(nodes))
    }

    fn parse_exp_stmt(&mut self) -> Result<AstNode, ParseError> {
        let name = self.advance();

        if name.kind != TokenKind::Identifier {
            return Err(ParseError::unexpected_token(format!("Expected identifier after exp keyword")));
        }

        let name = name.text.as_ref()
            .map(|t| t.to_string())
            .ok_or_else(|| ParseError::unexpected_token(format!("Invalid identifier: {:?}", name)))?;

        self.expect(TokenKind::OpenParen)?;

        let mut parameters = Vec::new();

        while let Some(token) = self.peek(0) {
            if token.kind == TokenKind::CloseParen {
                self.advance();
                break;
            } else if token.kind == TokenKind::Identifier {
                let name = self.advance();

                if name.kind != TokenKind::Identifier {
                    return Err(ParseError::unexpected_token(format!("Expected identifier after exp keyword")));
                }

                let name = name.text.as_ref()
                    .map(|t| t.to_string())
                    .ok_or_else(|| ParseError::unexpected_token(format!("Invalid identifier: {:?}", name)))?;

                self.expect(TokenKind::Colon)?;

                let type_annotation = self.parse_type_annotation()?;

                if let Some(token) = self.peek(0) {
                    if token.kind == TokenKind::Comma {
                        self.advance();
                    }
                }

                parameters.push((name, type_annotation));
            } else {
                return Err(ParseError::unexpected_token(format!("Expected identifier after exp keyword")));
            }
        }
        self.expect(TokenKind::Colon)?;

        let type_annotation = self.parse_type_annotation()?;

        self.expect(TokenKind::OpenCurly)?;

        let scope = self.parse_scope_stmt()?;

        Ok(AstNode::ExperimentDecl {
            name,
            parameters,
            exp_type: type_annotation,
            body: Box::new(scope)
        })
    }

    fn parse_if_stmt(&mut self) -> Result<AstNode, ParseError> {
        let condition = self.parse_expression(0)?;

        self.expect(TokenKind::OpenCurly)?;

        let if_scope = self.parse_scope_stmt()?;

        let otherwise = if let Some(token) = self.peek(0) {
            if token.kind == TokenKind::Otherwise {
                self.advance();
                self.expect(TokenKind::OpenCurly)?;

                let scope = self.parse_scope_stmt()?;

                Some(Box::new(scope))
            } else {
                None
            }
        } else {
            None
        };

        Ok(AstNode::IfStmt(Box::new(condition), Box::new(if_scope), otherwise))
    }

    fn parse_unless_stmt(&mut self) -> Result<AstNode, ParseError> {
        let condition = self.parse_expression(0)?;

        self.expect(TokenKind::OpenCurly)?;

        let unless_scope = self.parse_scope_stmt()?;

        let otherwise = if let Some(token) = self.peek(0) {
            if token.kind == TokenKind::Otherwise {
                self.advance();
                self.expect(TokenKind::OpenCurly)?;

                let scope = self.parse_scope_stmt()?;

                Some(Box::new(scope))
            } else {
                None
            }
        } else {
            None
        };

        Ok(AstNode::IfStmt(Box::new(condition), Box::new(unless_scope), otherwise))
    }

    fn parse_while_stmt(&mut self) -> Result<AstNode, ParseError> {
        let condition = self.parse_expression(0)?;

        self.expect(TokenKind::OpenCurly)?;

        let scope = self.parse_scope_stmt()?;

        Ok(AstNode::WhileStmt(Box::new(condition), Box::new(scope)))
    }

    fn parse_inject_stmt(&mut self) -> Result<AstNode, ParseError> {
        let mut experiments = Vec::new();

        self.expect(TokenKind::OpenParen)?;

        while let Some(token) = self.peek(0) {
            if token.kind == TokenKind::CloseParen {
                self.advance();
                break;
            } else {
                if let Some(token) = self.peek(0) {
                    if token.kind == TokenKind::Comma {
                        self.advance();
                    }
                }
                experiments.push(match self.advance() {
                    Token { kind: TokenKind::Identifier, text: Some(text) } => text.to_string(),
                    _ => return Err(ParseError::unexpected_token(format!("Expected identifier after inject keyword")))
                });
            }
        }

        self.expect(TokenKind::From)?;

        let file = match self.parse_expression(0)? {
            AstNode::StringLiteral(string) => string,
            _ => return Err(ParseError::unexpected_token(format!("Expected string literal after inject keyword")))
        };

        self.expect(TokenKind::Semi)?;

        Ok(AstNode::InjectStmt {
            experiments,
            file
        })
    }

    fn parse_iter_stmt(&mut self) -> Result<AstNode, ParseError> {
        self.expect(TokenKind::OpenParen)?;

        let variable = match self.advance() {
            Token { kind: TokenKind::Identifier, text: Some(text) } => text.to_string(),
            _ => return Err(ParseError::unexpected_token(format!("Expected identifier after iter keyword")))
        };

        self.expect(TokenKind::Colon)?;

        let type_annotation = self.parse_type_annotation()?;

        self.expect(TokenKind::In)?;

        let sequence = self.parse_expression(0)?;

        self.expect(TokenKind::CloseParen)?;

        self.expect(TokenKind::OpenCurly)?;

        let scope = self.parse_scope_stmt()?;

        Ok(AstNode::IterStmt {
            variable: (variable, type_annotation),
            sequence: Box::new(sequence),
            body: Box::new(scope)
        })
    }

    fn parse_identifier_stmt(&mut self, current_token: Token) -> Result<AstNode, ParseError> {
        let name = current_token.text.as_ref()
            .map(|t| t.to_string())
            .ok_or_else(|| ParseError::unexpected_token(format!("Invalid identifier: {:?}", current_token)))?;

        let token = self.advance();

        if token.kind == TokenKind::OpenParen {
            let mut arguments = Vec::new();

            while let Some(token) = self.peek(0) {
                if token.kind == TokenKind::CloseParen {
                    self.advance();
                    break;
                } else {
                    if let Some(token) = self.peek(0) {
                        if token.kind == TokenKind::Comma {
                            self.advance();
                        }
                    }
                    arguments.push(self.parse_expression(0)?);
                }
            }

            self.expect(TokenKind::Semi)?;

            Ok(AstNode::ExperimentCall {
                name,
                arguments
            })
        } else if token.kind == TokenKind::Eq {
            let expression = self.parse_expression(0)?;

            self.expect(TokenKind::Semi)?;

            Ok(AstNode::AssignmentStmt(Box::new(AstNode::Identifier(name)), Box::new(expression)))
        } else {
            Err(ParseError::unexpected_token(format!("{:?} in statement.", token)))
        }
    }

    fn parse_statement(&mut self) -> Result<Option<AstNode>, ParseError> {
        let token = self.advance();

        match token.kind {
            TokenKind::Emit => {
                let stmt = self.parse_emit_stmt()?;

                return Ok(Some(stmt));
            },
            TokenKind::Set => {
                let stmt = self.parse_set_stmt()?;

                return Ok(Some(stmt));
            },
            TokenKind::OpenCurly => {
                let stmt = self.parse_scope_stmt()?;

                return Ok(Some(stmt));
            },
            TokenKind::Exp => {
                let stmt = self.parse_exp_stmt()?;

                return Ok(Some(stmt));
            },
            TokenKind::If => {
                let stmt = self.parse_if_stmt()?;

                return Ok(Some(stmt));
            },
            TokenKind::Unless => {
                let stmt = self.parse_unless_stmt()?;

                return Ok(Some(stmt));
            },
            TokenKind::While => {
                let stmt = self.parse_while_stmt()?;

                return Ok(Some(stmt));
            },
            TokenKind::Inject => {
                let stmt = self.parse_inject_stmt()?;

                return Ok(Some(stmt));
            },
            TokenKind::Iter => {
                let stmt = self.parse_iter_stmt()?;

                return Ok(Some(stmt));
            },
            TokenKind::Identifier => {
                let current_token = token.clone();
                let stmt = self.parse_identifier_stmt(current_token)?;

                return Ok(Some(stmt));
            },
            _ => Err(ParseError::unexpected_token(format!("{:?} in statement.", token)))
        }
    }

    fn parse_expression(&mut self, min_precedence: u8) -> Result<AstNode, ParseError> {
        let mut left_expr = self.parse_primary()?;

        while let Some(op) = self.peek_operator() {
            let precedence = op.precedence();
            if precedence < min_precedence {
                break;
            }

            self.advance();

            let mut right_expr = self.parse_expression(precedence + 1)?;

            while let Some(next_op) = self.peek_operator() {
                let next_precedence = next_op.precedence();
                if precedence < next_precedence {
                    right_expr = self.parse_expression(next_precedence + 1)?;
                } else {
                    break;
                }
            }

            left_expr = AstNode::BinaryExpr(Box::new(left_expr), op, Box::new(right_expr));
        }

        Ok(left_expr)
    }

    fn parse_primary(&mut self) -> Result<AstNode, ParseError> {
        let token = self.advance();

        match token.kind {
            TokenKind::Int => {
                token.text.as_ref()
                    .and_then(|t| t.parse::<i32>().ok())
                    .map(AstNode::IntLiteral)
                    .ok_or_else(|| ParseError::unexpected_token(format!("Invalid integer literal: {:?}", token)))
            },
            TokenKind::Char => {
                token.text.as_ref()
                    .and_then(|t| t.chars().next())
                    .map(AstNode::CharLiteral)
                    .ok_or_else(|| ParseError::unexpected_token(format!("Invalid char literal: {:?}", token)))
            },
            TokenKind::String => {
                token.text.as_ref()
                    .map(|t| t.to_string())
                    .map(AstNode::StringLiteral)
                    .ok_or_else(|| ParseError::unexpected_token(format!("Invalid string literal: {:?}", token)))
            },
            TokenKind::Bool => {
                token.text.as_ref()
                    .and_then(|t| t.parse::<bool>().ok())
                    .map(AstNode::BoolLiteral)
                    .ok_or_else(|| ParseError::unexpected_token(format!("Invalid bool literal: {:?}", token)))
            },
            TokenKind::OpenParen => {
                let expression = self.parse_expression(0)?;

                self.expect(TokenKind::CloseParen)?;
                Ok(expression)
            },
            TokenKind::OpenBracket => {
                let mut values = Vec::new();

                while let Some(token) = self.peek(0) {
                    if token.kind == TokenKind::CloseBracket {
                        self.advance();
                        break;
                    } else {
                        if let Some(token) = self.peek(0) {
                            if token.kind == TokenKind::Comma {
                                self.advance();
                            }
                        }
                        values.push(self.parse_expression(0)?);
                    }
                }

                Ok(AstNode::SequenceLiteral(values))
            },
            TokenKind::Identifier => {
                let name = token.text.as_ref()
                    .map(|t| t.to_string())
                    .ok_or_else(|| ParseError::unexpected_token(format!("Invalid identifier: {:?}", token)))?;

                let token = self.peek(0).unwrap();

                if token.kind != TokenKind::OpenParen {
                    return Ok(AstNode::Identifier(name));
                }

                self.advance();

                let mut arguments = Vec::new();

                while let Some(token) = self.peek(0) {
                    if token.kind == TokenKind::CloseParen {
                        self.advance();
                        break;
                    } else {
                        if let Some(token) = self.peek(0) {
                            if token.kind == TokenKind::Comma {
                                self.advance();
                            }
                        }
                        arguments.push(self.parse_expression(0)?);
                    }
                }

                Ok(AstNode::ExperimentCall {
                    name,
                    arguments
                })
            },
            _ => Err(ParseError::unexpected_token(format!("{:?} in expression.", token)))
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        &self.tokens[self.current - 1]
    }

    fn peek_operator(&self) -> Option<BinaryOperator> {
        if let Some(token) = self.peek(0) {
            let op = match token.kind {
                TokenKind::Plus => Some(BinaryOperator::Add),
                TokenKind::Minus => Some(BinaryOperator::Sub),
                TokenKind::Star => Some(BinaryOperator::Mul),
                TokenKind::Slash => Some(BinaryOperator::Div),
                TokenKind::Percent => Some(BinaryOperator::Mod),
                TokenKind::Caret => Some(BinaryOperator::Pow),
                TokenKind::Is => Some(BinaryOperator::Is),
                TokenKind::Not => Some(BinaryOperator::Not),
                TokenKind::Inf => Some(BinaryOperator::Inf),
                TokenKind::Sup => Some(BinaryOperator::Sup),
                TokenKind::LtEq => Some(BinaryOperator::Leq),
                TokenKind::GtEq => Some(BinaryOperator::Geq),
                _ => None
            };

            return op;
        } else {
            return None;
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<(), ParseError> {
        let token = self.advance();

        if token.kind == kind {
            Ok(())
        } else {
            Err(ParseError::syntax_error(format!("Expected {:?} got {:?}", kind, token.kind)))
        }
    }

    fn peek(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.current + n)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }
}
