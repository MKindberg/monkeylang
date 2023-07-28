mod token;
mod lexer;
mod repl;
mod ast;
mod parser;
mod object;
mod evaluator;
mod code;
mod compiler;
mod vm;
mod symbol_table;

fn main() {
    println!("Hello, this is the Monkey programming language!");
    println!("Feel free to type in commands");

    repl::start();
}
