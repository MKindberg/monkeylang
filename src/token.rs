use std::hash::Hash;
use std::hash::Hasher;

#[derive(Debug, Clone, Eq)]
pub enum Token {
    ILLEGAL(String),
    EOF,

    // Identifiers + literals
    IDENT(Option<String>),
    INT(Option<i64>),

    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NEQ,

    // Delimiters
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

impl Token {
    pub fn lookup_ident(ident: &str) -> Token {
        match ident {
            "fn" => Token::FUNCTION,
            "let" => Token::LET,
            "true" => Token::TRUE,
            "false" => Token::FALSE,
            "if" => Token::IF,
            "else" => Token::ELSE,
            "return" => Token::RETURN,
            _ => Token::IDENT(Some(ident.to_string())),
        }
    }
}

impl ToString for Token {
    fn to_string(&self) -> String {
        match self {
            Token::ILLEGAL(token) => "ILLEGAL:".to_string() + &token.to_string(),
            Token::EOF => "EOF".to_string(),
            Token::IDENT(Some(token)) => "Identifier:".to_string() + &token.to_string(),
            Token::IDENT(None) => "Identifier:\"\"".to_string(),
            Token::INT(Some(token)) => token.to_string(),
            Token::INT(None) => "NaN".to_string(),
            Token::ASSIGN => "=".to_string(),
            Token::PLUS => "+".to_string(),
            Token::MINUS => "-".to_string(),
            Token::BANG => "!".to_string(),
            Token::ASTERISK => "*".to_string(),
            Token::SLASH => "/".to_string(),
            Token::LT => "<".to_string(),
            Token::GT => ">".to_string(),
            Token::EQ => "==".to_string(),
            Token::NEQ => "!=".to_string(),
            Token::COMMA => ",".to_string(),
            Token::SEMICOLON => ";".to_string(),
            Token::LPAREN => "(".to_string(),
            Token::RPAREN => ")".to_string(),
            Token::LBRACE => "{".to_string(),
            Token::RBRACE => "}".to_string(),
            Token::FUNCTION => "fn".to_string(),
            Token::LET => "let".to_string(),
            Token::TRUE => "true".to_string(),
            Token::FALSE => "false".to_string(),
            Token::IF => "if".to_string(),
            Token::ELSE => "else".to_string(),
            Token::RETURN => "return".to_string(),
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Token::IDENT(Some(a)), Token::IDENT(Some(b))) => a == b,
            (Token::IDENT(_), Token::IDENT(_)) => true,
            (Token::INT(Some(a)), Token::INT(Some(b))) => a == b,
            (Token::INT(_), Token::INT(_)) => true,
            (Token::ASSIGN, Token::ASSIGN) => true,
            (Token::PLUS, Token::PLUS) => true,
            (Token::MINUS, Token::MINUS) => true,
            (Token::BANG, Token::BANG) => true,
            (Token::ASTERISK, Token::ASTERISK) => true,
            (Token::SLASH, Token::SLASH) => true,
            (Token::LT, Token::LT) => true,
            (Token::GT, Token::GT) => true,
            (Token::EQ, Token::EQ) => true,
            (Token::NEQ, Token::NEQ) => true,
            (Token::COMMA, Token::COMMA) => true,
            (Token::SEMICOLON, Token::SEMICOLON) => true,
            (Token::LPAREN, Token::LPAREN) => true,
            (Token::RPAREN, Token::RPAREN) => true,
            (Token::LBRACE, Token::LBRACE) => true,
            (Token::RBRACE, Token::RBRACE) => true,
            (Token::FUNCTION, Token::FUNCTION) => true,
            (Token::LET, Token::LET) => true,
            (Token::TRUE, Token::TRUE) => true,
            (Token::FALSE, Token::FALSE) => true,
            (Token::IF, Token::IF) => true,
            (Token::ELSE, Token::ELSE) => true,
            (Token::RETURN, Token::RETURN) => true,
            (Token::EOF, Token::EOF) => true,
            (Token::ILLEGAL(_), Token::ILLEGAL(_)) => true,
            _ => false,
        }
    }
}

impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Token::IDENT(_) => "IDENTIFIER".hash(state),
            Token::INT(_) => "INTEGER".hash(state),
            _ => self.to_string().hash(state),
        };
    }
}
