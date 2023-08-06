use crate::token::Token;

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let c = input.chars().nth(0);
        Self {
            input,
            position: 0,
            read_position: 1,
            ch: c,
        }
    }

    fn read_char(&mut self) {
        self.ch = self.input.chars().nth(self.read_position);
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> Option<char> {
        self.input.chars().nth(self.read_position)
    }

    fn is_letter(ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while let Some(c) = self.ch {
            if !Self::is_letter(c) {
                break;
            }
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.ch {
            if !c.is_whitespace() {
                break;
            }
            self.read_char();
        }
    }

    fn read_number(&mut self) -> i64 {
        let position = self.position;
        while let Some(c) = self.ch {
            if !c.is_numeric() {
                break;
            }
            self.read_char();
        }
        i64::from_str_radix(&self.input[position..self.position], 10).unwrap()
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;
        self.read_char();
        while let Some(c) = self.ch {
            if c == '"' {
                break;
            }
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            Some('+') => Token::PLUS,
            Some('-') => Token::MINUS,
            Some('/') => Token::SLASH,
            Some('*') => Token::ASTERISK,
            Some('<') => Token::LT,
            Some('>') => Token::GT,
            Some(';') => Token::SEMICOLON,
            Some('(') => Token::LPAREN,
            Some(')') => Token::RPAREN,
            Some(',') => Token::COMMA,
            Some('{') => Token::LBRACE,
            Some('}') => Token::RBRACE,
            Some('[') => Token::LBRACKET,
            Some(']') => Token::RBRACKET,
            Some(':') => Token::COLON,
            Some('"') => Token::STRING(Some(self.read_string())),
            Some('=') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::EQ
                } else {
                    Token::ASSIGN
                }
            }
            Some('!') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::NEQ
                } else {
                    Token::BANG
                }
            }
            Some(c) => {
                if Self::is_letter(c) {
                    let ident = self.read_identifier();
                    return Token::lookup_ident(&ident);
                } else if c.is_numeric() {
                    return Token::INT(Some(self.read_number()));
                } else {
                    Token::ILLEGAL(c.to_string())
                }
            }
            None => Token::EOF,
        };
        self.read_char();
        return tok;
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::*;

    #[test]
    fn test_next_token() {
        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}
10 == 10;
10 != 9;
"foobar"
"foo bar"
[1, 2];
{"foo": "bar"}
"#;

        let expected: Vec<Token> = vec![
            Token::LET,
            Token::IDENT(Some("five".to_string())),
            Token::ASSIGN,
            Token::INT(Some(5)),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(Some("ten".to_string())),
            Token::ASSIGN,
            Token::INT(Some(10)),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(Some("add".to_string())),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT(Some("x".to_string())),
            Token::COMMA,
            Token::IDENT(Some("y".to_string())),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT(Some("x".to_string())),
            Token::PLUS,
            Token::IDENT(Some("y".to_string())),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(Some("result".to_string())),
            Token::ASSIGN,
            Token::IDENT(Some("add".to_string())),
            Token::LPAREN,
            Token::IDENT(Some("five".to_string())),
            Token::COMMA,
            Token::IDENT(Some("ten".to_string())),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::INT(Some(5)),
            Token::SEMICOLON,
            Token::INT(Some(5)),
            Token::LT,
            Token::INT(Some(10)),
            Token::GT,
            Token::INT(Some(5)),
            Token::SEMICOLON,
            Token::IF,
            Token::LPAREN,
            Token::INT(Some(5)),
            Token::LT,
            Token::INT(Some(10)),
            Token::RPAREN,
            Token::LBRACE,
            Token::RETURN,
            Token::TRUE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::ELSE,
            Token::LBRACE,
            Token::RETURN,
            Token::FALSE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::INT(Some(10)),
            Token::EQ,
            Token::INT(Some(10)),
            Token::SEMICOLON,
            Token::INT(Some(10)),
            Token::NEQ,
            Token::INT(Some(9)),
            Token::SEMICOLON,
            Token::STRING(Some("foobar".to_string())),
            Token::STRING(Some("foo bar".to_string())),
            Token::LBRACKET,
            Token::INT(Some(1)),
            Token::COMMA,
            Token::INT(Some(2)),
            Token::RBRACKET,
            Token::SEMICOLON,
            Token::LBRACE,
            Token::STRING(Some("foo".to_string())),
            Token::COLON,
            Token::STRING(Some("bar".to_string())),
            Token::RBRACE,
            Token::EOF,
        ];

        let mut l = Lexer::new(input.to_string());
        for e in expected.iter() {
            let token = l.next_token();
            assert_eq!(token, *e);
        }
    }
}
