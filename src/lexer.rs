use crate::token::Token;

struct Lexer {
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
            read_position: 0,
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
        while Self::is_letter(self.ch.unwrap()) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.map(char::is_whitespace).unwrap_or(false) {
            self.read_char();
        }
    }

    fn read_number(&mut self) -> i64 {
        let position = self.position;
        while self.ch.unwrap().is_numeric() {
            self.read_char();
        }
        i64::from_str_radix(&self.input[position..self.position], 10).unwrap()
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
                    return Token::INT(self.read_number());
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
        let input = "let five = 5;
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
";

        let expected: Vec<Token> = vec![
            Token::LET,
            Token::IDENT("five".to_string()),
            Token::ASSIGN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("ten".to_string()),
            Token::ASSIGN,
            Token::INT(10),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("add".to_string()),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT("x".to_string()),
            Token::COMMA,
            Token::IDENT("y".to_string()),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT("x".to_string()),
            Token::PLUS,
            Token::IDENT("y".to_string()),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("result".to_string()),
            Token::ASSIGN,
            Token::IDENT("add".to_string()),
            Token::LPAREN,
            Token::IDENT("five".to_string()),
            Token::COMMA,
            Token::IDENT("ten".to_string()),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::INT(5),
            Token::SEMICOLON,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::GT,
            Token::INT(5),
            Token::SEMICOLON,
            Token::IF,
            Token::LPAREN,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
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
            Token::INT(10),
            Token::EQ,
            Token::INT(10),
            Token::SEMICOLON,
            Token::INT(10),
            Token::NEQ,
            Token::INT(9),
            Token::SEMICOLON,
            Token::EOF,
        ];

        let mut l = Lexer::new(input.to_string());
        for e in expected.iter() {
            let token = l.next_token();
            assert_eq!(token, *e);
        }
    }
}
