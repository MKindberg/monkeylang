use crate::lexer::Lexer;
use crate::token::Token;
use std::io::Write;

const PROMPT: &str = ">> ";

pub fn start() {
    loop {
        let mut input = String::new();
        print!("{}", PROMPT);
        std::io::stdout().flush().unwrap();
        let read = std::io::stdin().read_line(&mut input).unwrap_or(0);
        if read == 0 {
            break;
        }
        let mut lexer = Lexer::new(input.trim().to_string());
        let mut token = lexer.next_token();
        while token != Token::EOF {
            println!("{:?}", token);
            token = lexer.next_token();
        }
    }
}
