#[derive(PartialEq, Debug)]
pub enum Token {
    ILLEGAL(String),
    EOF,

    // Identifiers + literals
    IDENT(String),
    INT(i64),

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

impl Token{
    pub fn lookup_ident(ident: &str) -> Token{
        match ident {
            "fn" => Token::FUNCTION,
            "let" => Token::LET,
            "true" => Token::TRUE,
            "false" => Token::FALSE,
            "if" => Token::IF,
            "else" => Token::ELSE,
            "return" => Token::RETURN,
            _ => Token::IDENT(ident.to_string()),
        }
    }
}
