use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::object::{Environment, Object, BuiltinFunction};
use crate::parser::Parser;
use crate::symbol_table::SymbolTable;
use crate::{evaluator, vm::VM};
use std::io::Write;

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
    let env = Environment::new();
    let mut constants: Vec<Object> = vec![];
    let mut globals = vec![Object::Null; crate::vm::GLOBALS_SIZE];
    let mut symbol_table = SymbolTable::new();
    for (i, b) in BuiltinFunction::list().iter().enumerate() {
        symbol_table.define_builtin(i, b.to_name());
    }

    loop {
        let mut input = String::new();
        print!("{}", PROMPT);
        std::io::stdout().flush().unwrap();
        let read = std::io::stdin().read_line(&mut input).unwrap_or(0);
        if read == 0 {
            break;
        }
        let lexer = Lexer::new(input.trim().to_string());
        let mut parser = Parser::new(lexer);
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
        // let evaluated = evaluator::eval(&program, &mut env);
        let mut comp = Compiler::new_with_state(symbol_table, constants);
        if let Err(s) = comp.compile(program) {
            println!("Compilation error: {}", s);
            symbol_table = comp.symbol_table.clone();
            constants = comp.constants.clone();
            continue;
        }
        symbol_table = comp.symbol_table.clone();
        constants = comp.constants.clone();

        let mut machine = VM::new_with_gobals_store(comp.bytecode(), globals);

        if let Err(s) = machine.run() {
            println!("Bytecode execution failed: {}", s);
            globals = machine.globals.clone();
            continue;
        }
        globals = machine.globals.clone();
        let evaluated = machine.last_popped_stack_elem();
        println!("{}", evaluated);
    }
}
