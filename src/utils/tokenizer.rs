//
// EPITECH PROJECT, 2023
// nebulang [WSL : Ubuntu]
// File description:
// tokenizer
//

use std::fmt::{Debug, Display};

pub struct TokenizerError {
    message: String
}

impl Display for TokenizerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TokenizerError: {}", self.message)
    }
}

impl TokenizerError {
    fn new(message: String) -> Self {
        Self {
            message
        }
    }
}

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub text: Option<String>,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.text {
            Some(text) => write!(f, "{:?}({})", self.kind, text),
            None => write!(f, "{:?}", self.kind)
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum TokenKind {
    Emit,
    Int,
    Char,
    String,
    Bool,
    Identifier,
    OpenParen,
    CloseParen,
    Set,
    Exp,
    Eq,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    OpenCurly,
    CloseCurly,
    If,
    Otherwise,
    Unless,
    While,
    Iter,
    In,
    Is,
    Not,
    Inf,
    LtEq,
    Sup,
    GtEq,
    OpenBracket,
    CloseBracket,
    Inject,
    From,
    Print,
    Comma,
    Colon,
    Semi,
}

impl Debug for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Emit => write!(f, "'emit'"),
            TokenKind::Int => write!(f, "Integer"),
            TokenKind::Char => write!(f, "Char"),
            TokenKind::String => write!(f, "String"),
            TokenKind::Bool => write!(f, "Bool"),
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::OpenParen => write!(f, "'('"),
            TokenKind::CloseParen => write!(f, "')'"),
            TokenKind::Set => write!(f, "'set'"),
            TokenKind::Exp => write!(f, "'exp'"),
            TokenKind::Eq => write!(f, "'='"),
            TokenKind::Plus => write!(f, "'+'"),
            TokenKind::Minus => write!(f, "'-'"),
            TokenKind::Star => write!(f, "'*'"),
            TokenKind::Slash => write!(f, "'/'"),
            TokenKind::Percent => write!(f, "'%'"),
            TokenKind::Caret => write!(f, "'^'"),
            TokenKind::OpenCurly => write!(f, "'{{'"),
            TokenKind::CloseCurly => write!(f, "'}}'"),
            TokenKind::If => write!(f, "'if'"),
            TokenKind::Otherwise => write!(f, "'otherwise'"),
            TokenKind::Unless => write!(f, "'unless'"),
            TokenKind::While => write!(f, "'while'"),
            TokenKind::Iter => write!(f, "'iter'"),
            TokenKind::In => write!(f, "'in'"),
            TokenKind::Is => write!(f, "'=='"),
            TokenKind::Not => write!(f, "'!='"),
            TokenKind::Inf => write!(f, "'<'"),
            TokenKind::LtEq => write!(f, "'<='"),
            TokenKind::Sup => write!(f, "'>'"),
            TokenKind::GtEq => write!(f, "'>='"),
            TokenKind::OpenBracket => write!(f, "'['"),
            TokenKind::CloseBracket => write!(f, "']'"),
            TokenKind::Inject => write!(f, "'inject'"),
            TokenKind::From => write!(f, "'from'"),
            TokenKind::Print => write!(f, "'print'"),
            TokenKind::Comma => write!(f, "','"),
            TokenKind::Colon => write!(f, "':'"),
            TokenKind::Semi => write!(f, "';'"),
        }
    }
}

pub struct Tokenizer<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable()
        }
    }

    pub fn next_token(&mut self) -> Option<Result<Token, TokenizerError>> {
        while let Some(&c) = self.chars.peek() {
            return if c.is_alphabetic() {
                Some(self.parse_identifier())
            } else if c.is_digit(10) {
                Some(self.parse_number())
            } else if c == '+' {
                self.chars.next();
                Some(Ok(Token {
                    kind: TokenKind::Plus,
                    text: None
                }))
            } else if c == '-' {
                self.chars.next();
                Some(Ok(Token {
                    kind: TokenKind::Minus,
                    text: None
                }))
            } else if c == '*' {
                self.chars.next();
                Some(Ok(Token {
                    kind: TokenKind::Star,
                    text: None
                }))
            } else if c == '#' {
                while let Some(&c) = self.chars.peek() {
                    if c == '\n' {
                        break;
                    }
                    self.chars.next();
                }
                continue;
            } else if c == '/' {
                self.chars.next();
                Some(Ok(Token {
                    kind: TokenKind::Slash,
                    text: None
                }))
            } else if c == '%' {
                self.chars.next();
                Some(Ok(Token {
                    kind: TokenKind::Percent,
                    text: None
                }))
            } else if c == '^' {
                self.chars.next();
                Some(Ok(Token {
                    kind: TokenKind::Caret,
                    text: None
                }))
            } else if c == '(' {
                self.chars.next();
                Some(Ok(Token {
                    kind: TokenKind::OpenParen,
                    text: None
                }))
            } else if c == ')' {
                self.chars.next();
                Some(Ok(Token {
                    kind: TokenKind::CloseParen,
                    text: None
                }))
            } else if c == '{' {
                self.chars.next();
                Some(Ok(Token {
                    kind: TokenKind::OpenCurly,
                    text: None
                }))
            } else if c == '}' {
                self.chars.next();
                Some(Ok(Token {
                    kind: TokenKind::CloseCurly,
                    text: None
                }))
            } else if c == '<' {
                self.chars.next();
                if let Some(&'=') = self.chars.peek() {
                    self.chars.next();
                    Some(Ok(Token {
                        kind: TokenKind::LtEq,
                        text: None
                    }))
                } else {
                    Some(Ok(Token {
                        kind: TokenKind::Inf,
                        text: None
                    }))
                }
            } else if c == '>' {
                self.chars.next();
                if let Some(&'=') = self.chars.peek() {
                    self.chars.next();
                    Some(Ok(Token {
                        kind: TokenKind::GtEq,
                        text: None
                    }))
                } else {
                    Some(Ok(Token {
                        kind: TokenKind::Sup,
                        text: None
                    }))
                }
            } else if c == '!' {
                self.chars.next();
                if let Some(&'=') = self.chars.peek() {
                    self.chars.next();
                    Some(Ok(Token {
                        kind: TokenKind::Not,
                        text: None
                    }))
                } else {
                    Some(Err(TokenizerError::new(format!("Unexpected character '{}'.", c))))
                }
            } else if c == '=' {
                self.chars.next();
                if let Some(&'=') = self.chars.peek() {
                    self.chars.next();
                    Some(Ok(Token {
                        kind: TokenKind::Is,
                        text: None
                    }))
                } else {
                    Some(Ok(Token {
                        kind: TokenKind::Eq,
                        text: None
                    }))
                }
            } else if c == '[' {
                self.chars.next();
                Some(Ok(Token {
                    kind: TokenKind::OpenBracket,
                    text: None
                }))
            } else if c == ']' {
                self.chars.next();
                Some(Ok(Token {
                    kind: TokenKind::CloseBracket,
                    text: None
                }))
            } else if c == '"' {
                let mut string = String::new();

                self.chars.next();
                while let Some(c) = self.chars.next() {
                    if c == '"' {
                        break;
                    } else if c == '\\' {
                        if let Some('n') = self.chars.peek() {
                            self.chars.next();
                            string.push('\n');
                        } else if let Some('t') = self.chars.peek() {
                            self.chars.next();
                            string.push('\t');
                        } else if let Some('\\') = self.chars.peek() {
                            self.chars.next();
                            string.push('\\');
                        } else if let Some('\'') = self.chars.peek() {
                            self.chars.next();
                            string.push('\'');
                        } else if let Some('"') = self.chars.peek() {
                            self.chars.next();
                            string.push('"');
                        } else {
                            return Some(Err(TokenizerError::new(format!("Unexpected character '{}'.", c))));
                        }
                    } else {
                        string.push(c);
                    }
                }

                Some(Ok(Token {
                    kind: TokenKind::String,
                    text: Some(string)
                }))
            } else if c == '\'' {
                self.chars.next();
                let char = match self.chars.next() {
                    Some('\\') => {
                        if let Some('n') = self.chars.peek() {
                            self.chars.next();
                            '\n'
                        } else if let Some('t') = self.chars.peek() {
                            self.chars.next();
                            '\t'
                        } else if let Some('\\') = self.chars.peek() {
                            self.chars.next();
                            '\\'
                        } else if let Some('\'') = self.chars.peek() {
                            self.chars.next();
                            '\''
                        } else {
                            return Some(Err(TokenizerError::new(format!("Unexpected character '{}'.", c))));
                        }
                    },
                    Some(c) => c,
                    None => return Some(Err(TokenizerError::new(format!("Unexpected character '{}'.", c))))
                };

                if let Some('\'') = self.chars.next() {
                    Some(Ok(Token {
                        kind: TokenKind::Char,
                        text: Some(char.to_string())
                    }))
                } else {
                    Some(Err(TokenizerError::new(format!("Unexpected character '{}'.", c))))
                }
            } else if c == ',' {
                self.chars.next();
                Some(Ok(Token {
                    kind: TokenKind::Comma,
                    text: None
                }))
            } else if c == ':' {
                self.chars.next();
                Some(Ok(Token {
                    kind: TokenKind::Colon,
                    text: None
                }))
            } else if c == ';' {
                self.chars.next();
                Some(Ok(Token {
                    kind: TokenKind::Semi,
                    text: None
                }))
            } else if c.is_whitespace() {
                self.chars.next();
                continue;
            } else {
                Some(Err(TokenizerError::new(format!("Unexpected character '{}'.", c))))
            }
        }
        None
    }

    fn parse_identifier(&mut self) -> Result<Token, TokenizerError> {
        let mut identifier = String::new();

        while let Some(&c) = self.chars.peek() {
            if c.is_alphanumeric() {
                identifier.push(c);
                self.chars.next();
            } else {
                break;
            }
        }

        match identifier.as_str() {
            "emit" => Ok(Token {
                kind: TokenKind::Emit,
                text: None
            }),
            "set" => Ok(Token {
                kind: TokenKind::Set,
                text: None
            }),
            "exp" => Ok(Token {
                kind: TokenKind::Exp,
                text: None
            }),
            "if" => Ok(Token {
                kind: TokenKind::If,
                text: None
            }),
            "otherwise" => Ok(Token {
                kind: TokenKind::Otherwise,
                text: None
            }),
            "unless" => Ok(Token {
                kind: TokenKind::Unless,
                text: None
            }),
            "while" => Ok(Token {
                kind: TokenKind::While,
                text: None
            }),
            "iter" => Ok(Token {
                kind: TokenKind::Iter,
                text: None
            }),
            "in" => Ok(Token {
                kind: TokenKind::In,
                text: None
            }),
            "inject" => Ok(Token {
                kind: TokenKind::Inject,
                text: None
            }),
            "from" => Ok(Token {
                kind: TokenKind::From,
                text: None
            }),
            "print" => Ok(Token {
                kind: TokenKind::Print,
                text: None
            }),
            "true" => Ok(Token {
                kind: TokenKind::Bool,
                text: Some(identifier)
            }),
            "false" => Ok(Token {
                kind: TokenKind::Bool,
                text: Some(identifier)
            }),
            _ => Ok(Token {
                kind: TokenKind::Identifier,
                text: Some(identifier)
            })
        }
    }

    fn parse_number(&mut self) -> Result<Token, TokenizerError> {
        let mut number = String::new();

        while let Some(&c) = self.chars.peek() {
            if c.is_digit(10) {
                number.push(c);
                self.chars.next();
            } else {
                break;
            }
        }

        Ok(Token {
            kind: TokenKind::Int,
            text: Some(number)
        })
    }
}