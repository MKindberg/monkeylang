use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::Token;
use std::io::Write;
use crate::evaluator;

const PROMPT: &str = ">> ";

const MONKEY_FACE: &str = r#"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

pub fn start() {
    loop {
        let mut input = String::new();
        print!("{}", PROMPT);
        std::io::stdout().flush().unwrap();
        let read = std::io::stdin().read_line(&mut input).unwrap_or(0);
        if read == 0 {
            break;
        }
        let lexer = Lexer::new(input.trim().to_string());
        let mut parser = crate::parser::Parser::new(lexer);
        let program = parser.parse_program();
        let errors = parser.errors();
        if errors.len() > 0 {
            println!("{}", MONKEY_FACE);
            println!("Woops! We ran into some monkey business here!");
            println!("parser errors:");
            for e in errors.iter() {
                println!("{}", e);
            }
        }
        let evaluated = evaluator::eval(&program);
        if let Some(evaluated) = evaluated {
            println!("{}", evaluated);
        }
    }
}
