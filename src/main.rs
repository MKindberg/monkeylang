#![feature(test)]
mod ast;
mod code;
mod compiler;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod symbol_table;
mod token;
mod vm;

fn main() {
    println!("Hello, this is the Monkey programming language!");
    println!("Feel free to type in commands");

    repl::start();
}

extern crate test;
#[cfg(test)]
mod tests {
    use std::time::Instant;
    use test::Bencher;

    use super::*;

    #[bench]
    fn benchmark_compiled_fibonacci(b: &mut Bencher) {
        let input = "let fibonacci = fn(x) {
  if (x == 0) {
    0
  } else {
    if (x == 1) {
      return 1;
    } else {
      fibonacci(x - 1) + fibonacci(x - 2);
    }
  }
};
fibonacci(15);";
        let lexer = lexer::Lexer::new(input.to_string());
        let mut parser = parser::Parser::new(lexer);
        let program = parser.parse_program();

        // vm
        let mut compiler = compiler::Compiler::new();
        if let Err(e) = compiler.compile(program) {
            println!("Compilation error: {}", e);
            assert!(false);
        }

        let mut machine = vm::VM::new(compiler.bytecode());

        b.iter(|| 
        if let Err(e) = machine.run() {
            println!("VM error: {}", e);
            assert!(false);
        });
    }

    #[bench]
    fn benchmark_evaluated_fibonacci(b: &mut Bencher) {
        let input = "let fibonacci = fn(x) {
  if (x == 0) {
    0
  } else {
    if (x == 1) {
      return 1;
    } else {
      fibonacci(x - 1) + fibonacci(x - 2);
    }
  }
};
fibonacci(15);";
        let lexer = lexer::Lexer::new(input.to_string());
        let mut parser = parser::Parser::new(lexer);
        let program = parser.parse_program();

        // vm
        let mut compiler = compiler::Compiler::new();
        if let Err(e) = compiler.compile(program.clone()) {
            println!("Compilation error: {}", e);
            assert!(false);
        }

        let mut machine = vm::VM::new(compiler.bytecode());

        b.iter(|| 
        if let Err(e) = machine.run() {
            println!("VM error: {}", e);
            assert!(false);
        });

        b.iter(|| evaluator::eval(&program, &mut object::Environment::new()));
    }
}
