use crate::ast;
use crate::code::{Instructions, Opcode};
use crate::object::{CompiledFunction, Object};
use crate::symbol_table::*;

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EmittedInstruction {
    pub opcode: Opcode,
    pub position: usize,
}

pub struct CompilationScope {
    instructions: Instructions,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

pub struct Compiler {
    pub constants: Vec<Object>,
    pub symbol_table: SymbolTable,
    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            constants: vec![],
            symbol_table: SymbolTable::new(),
            scopes: vec![CompilationScope {
                instructions: Instructions::new(),
                last_instruction: None,
                previous_instruction: None,
            }],
            scope_index: 0,
        }
    }

    pub fn new_with_state(s: SymbolTable, constants: Vec<Object>) -> Compiler {
        let mut c = Compiler::new();
        c.symbol_table = s;
        c.constants = constants;
        return c;
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.scopes[self.scope_index].instructions.clone(),
            constants: self.constants.clone(),
        }
    }

    fn current_instructions(&mut self) -> &mut Instructions {
        &mut self.scopes[self.scope_index].instructions
    }
    fn current_last_instruction(&mut self) -> &mut Option<EmittedInstruction> {
        &mut self.scopes[self.scope_index].last_instruction
    }
    fn current_previous_instruction(&mut self) -> &mut Option<EmittedInstruction> {
        &mut self.scopes[self.scope_index].previous_instruction
    }

    fn enter_scope(&mut self) {
        self.scopes.push(CompilationScope {
            instructions: Instructions::new(),
            last_instruction: None,
            previous_instruction: None,
        });
        self.scope_index += 1;
    }

    fn leave_scope(&mut self) -> Instructions {
        self.scope_index -= 1;
        let scope = self.scopes.pop().unwrap();
        return scope.instructions;
    }

    pub fn compile(&mut self, code: ast::Program) -> Result<(), String> {
        return self.compile_program(code);
    }

    fn compile_program(&mut self, program: ast::Program) -> Result<(), String> {
        for statement in &program.statements {
            self.compile_statement(statement)?;
        }

        Ok(())
    }

    fn compile_statement(&mut self, statement: &ast::Statement) -> Result<(), String> {
        match statement {
            ast::Statement::ExpressionStatement(e) => {
                self.compile_expression(&e.expression)?;
                self.emit(Opcode::Pop, &[]);
                Ok(())
            }
            ast::Statement::BlockStatement(b) => self.compile_block_statement(b),
            ast::Statement::LetStatement(l) => self.compile_let_statement(l),
            ast::Statement::ReturnStatement(r) => {
                self.compile_expression(&r.value)?;
                self.emit(Opcode::ReturnValue, &[]);
                Ok(())
            }
        }
    }

    fn compile_block_statement(&mut self, block: &ast::BlockStatement) -> Result<(), String> {
        for statement in &block.statements {
            self.compile_statement(statement)?;
        }
        Ok(())
    }

    fn compile_let_statement(&mut self, statement: &ast::LetStatement) -> Result<(), String> {
        self.compile_expression(&statement.value)?;
        let symbol = self.symbol_table.define(&statement.name.value);
        self.emit(Opcode::SetGlobal, &[symbol.index]);
        Ok(())
    }

    fn compile_expression(&mut self, expression: &ast::Expression) -> Result<(), String> {
        match expression {
            ast::Expression::IntegerLiteral(i) => self.compile_integer(i),
            ast::Expression::Boolean(b) => {
                if b.value {
                    self.emit(Opcode::True, &[])
                } else {
                    self.emit(Opcode::False, &[])
                };
                Ok(())
            }
            ast::Expression::InfixExpression(i) => self.compile_infix_expression(i),
            ast::Expression::PrefixExpression(p) => self.compile_prefix_expression(p),
            ast::Expression::IfExpression(i) => self.compile_if_expression(i),
            ast::Expression::Identifier(i) => self.compile_identifier(i),
            ast::Expression::StringLiteral(s) => self.compile_string_literal(s),
            ast::Expression::ArrayLiteral(a) => self.compile_array_literal(a),
            ast::Expression::HashLiteral(h) => self.compile_hash_literal(h),
            ast::Expression::IndexExpression(i) => self.compile_index_expression(i),
            ast::Expression::FunctionLiteral(f) => self.compile_function_literal(f),
            ast::Expression::CallExpression(c) => self.compile_call_expression(c),
        }
    }

    fn compile_call_expression(&mut self, call: &ast::CallExpression) -> Result<(), String> {
        self.compile_expression(&call.function)?;
        self.emit(Opcode::Call, &[]);
        Ok(())
    }

    fn last_instruction_is(&mut self, opcode: Opcode) -> bool {
        self.current_instructions().len() > 0
            && self.current_last_instruction().as_ref().unwrap().opcode == opcode
    }

    fn replace_last_pop_with_return(&mut self) {
        let last_pos = self.current_last_instruction().as_ref().unwrap().position;
        self.replace_instruction(last_pos, Opcode::ReturnValue.to_instruction(&[]));
        self.current_last_instruction().as_mut().unwrap().opcode = Opcode::ReturnValue;
    }

    fn compile_function_literal(&mut self, f: &ast::FunctionLiteral) -> Result<(), String> {
        self.enter_scope();
        self.compile_block_statement(&f.body)?;

        if self.last_instruction_is(Opcode::Pop) {
            self.replace_last_pop_with_return();
        }
        if !self.last_instruction_is(Opcode::ReturnValue) {
            self.emit(Opcode::Return, &[]);
        }

        let instructions = self.leave_scope();

        let func = Object::CompiledFunction(CompiledFunction { instructions });
        let c = self.add_constant(func);
        self.emit(Opcode::Constant, &[c]);

        Ok(())
    }

    fn compile_index_expression(&mut self, i: &ast::IndexExpression) -> Result<(), String> {
        self.compile_expression(&i.left)?;
        self.compile_expression(&i.index)?;
        self.emit(Opcode::Index, &[]);
        Ok(())
    }

    fn compile_hash_literal(&mut self, hash: &ast::HashLiteral) -> Result<(), String> {
        for (key, value) in &hash.pairs {
            self.compile_expression(key)?;
            self.compile_expression(value)?;
        }
        self.emit(Opcode::Hash, &[hash.pairs.len() * 2]);
        Ok(())
    }

    fn compile_array_literal(&mut self, array: &ast::ArrayLiteral) -> Result<(), String> {
        for element in &array.elements {
            self.compile_expression(element)?;
        }
        self.emit(Opcode::Array, &[array.elements.len()]);
        Ok(())
    }

    fn compile_string_literal(&mut self, s: &ast::StringLiteral) -> Result<(), String> {
        let string = Object::String(s.value.clone());
        let c = self.add_constant(string);
        self.emit(Opcode::Constant, &[c]);
        Ok(())
    }

    fn compile_identifier(&mut self, identifier: &ast::Identifier) -> Result<(), String> {
        let symbol = self.symbol_table.resolve(&identifier.value);
        if let Some(sym) = symbol {
            self.emit(Opcode::GetGlobal, &[sym.index]);
            return Ok(());
        } else {
            return Err(format!("Undefined variable {}", identifier.value));
        }
    }

    fn compile_infix_expression(
        &mut self,
        expression: &ast::InfixExpression,
    ) -> Result<(), String> {
        if expression.operator.as_str() == "<" {
            self.compile_expression(&expression.right)?;
            self.compile_expression(&expression.left)?;
            self.emit(Opcode::GreaterThan, &[]);
            return Ok(());
        }
        self.compile_expression(&expression.left)?;
        self.compile_expression(&expression.right)?;
        match expression.operator.as_str() {
            "+" => self.emit(Opcode::Add, &[]),
            "-" => self.emit(Opcode::Sub, &[]),
            "*" => self.emit(Opcode::Mul, &[]),
            "/" => self.emit(Opcode::Div, &[]),
            ">" => self.emit(Opcode::GreaterThan, &[]),
            "==" => self.emit(Opcode::Equal, &[]),
            "!=" => self.emit(Opcode::NotEqual, &[]),
            s => return Err(format!("unknown operation {}", s)),
        };
        Ok(())
    }

    fn compile_prefix_expression(
        &mut self,
        expression: &ast::PrefixExpression,
    ) -> Result<(), String> {
        self.compile_expression(&expression.right)?;

        match expression.operator.as_str() {
            "!" => self.emit(Opcode::Bang, &[]),
            "-" => self.emit(Opcode::Minus, &[]),
            _ => return Err(format!("Unknown operator {}", expression.operator)),
        };
        Ok(())
    }

    fn compile_if_expression(&mut self, expression: &ast::IfExpression) -> Result<(), String> {
        self.compile_expression(&expression.condition)?;
        let jump_not_truthy_pos = self.emit(Opcode::JumpNotThuthy, &[9999]); // Temporary value 9999
        self.compile_block_statement(&expression.consequence)?;
        self.remove_last_pop();

        let jump_pos = self.emit(Opcode::Jump, &[9999]); // Temporary value 9999

        let after_cons_pos = self.current_instructions().len();
        self.change_operand(jump_not_truthy_pos, after_cons_pos);

        if expression.alternative.is_none() {
            self.emit(Opcode::Null, &[]);
        } else {
            self.compile_block_statement(expression.alternative.as_ref().unwrap())?;
            self.remove_last_pop();
        }

        let after_alt_pos = self.current_instructions().len();
        self.change_operand(jump_pos, after_alt_pos);

        Ok(())
    }

    fn remove_last_pop(&mut self) {
        if self.current_last_instruction().is_some()
            && self.current_last_instruction().as_ref().unwrap().opcode == Opcode::Pop
        {
            self.current_instructions().pop();
            *self.current_last_instruction() = self.current_previous_instruction().clone();
        }
    }

    fn replace_instruction(&mut self, pos: usize, new: Instructions) {
        for i in 0..new.len() {
            self.current_instructions()[pos + i] = new[i];
        }
    }

    fn change_operand(&mut self, op_pos: usize, operand: usize) {
        let op = Opcode::lookup(self.current_instructions()[op_pos]).unwrap();
        let new_instruction = op.to_instruction(&[operand]);
        self.replace_instruction(op_pos, new_instruction);
    }

    fn compile_integer(&mut self, i: &ast::IntegerLiteral) -> Result<(), String> {
        let integer = Object::Integer(i.value);
        let c = self.add_constant(integer);
        self.emit(Opcode::Constant, &[c]);
        Ok(())
    }

    fn add_constant(&mut self, constant: Object) -> usize {
        self.constants.push(constant);
        return self.constants.len() - 1;
    }

    fn emit(&mut self, opcode: Opcode, operands: &[usize]) -> usize {
        let mut instruction = opcode.to_instruction(operands);
        let pos = self.add_instruction(&mut instruction);

        *self.current_previous_instruction() = self.current_last_instruction().take();
        *self.current_last_instruction() = Some(EmittedInstruction {
            opcode,
            position: pos,
        });

        return pos;
    }

    fn add_instruction(&mut self, instruction: &mut Instructions) -> usize {
        let pos = self.current_instructions().len();
        self.current_instructions().append(instruction);
        return pos;
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast,
        code::{Instructions, Opcode},
        compiler::Compiler,
        lexer::Lexer,
        object::{self, Object},
        parser::Parser,
    };

    struct CompilerTestCase {
        input: &'static str,
        expected_constants: Vec<object::Object>,
        expected_instructions: Vec<Instructions>,
    }

    fn parse(input: String) -> ast::Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        return parser.parse_program();
    }

    fn test_instructions(expected: Vec<Instructions>, instructions: Instructions) {
        assert_eq!(
            expected.iter().flatten().copied().collect::<Instructions>(),
            instructions
        );
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
        for test in tests {
            let program = parse(test.input.to_string());

            let mut compiler = Compiler::new();
            let _ = compiler
                .compile(program)
                .unwrap_or_else(|e| panic!("{}", e));

            let bytecode = compiler.bytecode();

            test_instructions(test.expected_instructions, bytecode.instructions);
            if test.expected_constants != bytecode.constants {
                println!("Input: {}", test.input);
            }
            assert_eq!(test.expected_constants, bytecode.constants);
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            CompilerTestCase {
                input: "1 + 2",
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    Opcode::Constant.to_instruction(&[0]),
                    Opcode::Constant.to_instruction(&[1]),
                    Opcode::Add.to_instruction(&[]),
                    Opcode::Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "1 - 2",
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    Opcode::Constant.to_instruction(&[0]),
                    Opcode::Constant.to_instruction(&[1]),
                    Opcode::Sub.to_instruction(&[]),
                    Opcode::Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "1 * 2",
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    Opcode::Constant.to_instruction(&[0]),
                    Opcode::Constant.to_instruction(&[1]),
                    Opcode::Mul.to_instruction(&[]),
                    Opcode::Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "2 / 1",
                expected_constants: vec![Object::Integer(2), Object::Integer(1)],
                expected_instructions: vec![
                    Opcode::Constant.to_instruction(&[0]),
                    Opcode::Constant.to_instruction(&[1]),
                    Opcode::Div.to_instruction(&[]),
                    Opcode::Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "1; 2",
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    Opcode::Constant.to_instruction(&[0]),
                    Opcode::Pop.to_instruction(&[]),
                    Opcode::Constant.to_instruction(&[1]),
                    Opcode::Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "-1",
                expected_constants: vec![Object::Integer(1)],
                expected_instructions: vec![
                    Opcode::Constant.to_instruction(&[0]),
                    Opcode::Minus.to_instruction(&[]),
                    Opcode::Pop.to_instruction(&[]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            CompilerTestCase {
                input: "true",
                expected_constants: vec![],
                expected_instructions: vec![
                    Opcode::True.to_instruction(&[0]),
                    Opcode::Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "false",
                expected_constants: vec![],
                expected_instructions: vec![
                    Opcode::False.to_instruction(&[0]),
                    Opcode::Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "1 > 2",
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    Opcode::Constant.to_instruction(&[0]),
                    Opcode::Constant.to_instruction(&[1]),
                    Opcode::GreaterThan.to_instruction(&[]),
                    Opcode::Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "1 < 2",
                expected_constants: vec![Object::Integer(2), Object::Integer(1)],
                expected_instructions: vec![
                    Opcode::Constant.to_instruction(&[0]),
                    Opcode::Constant.to_instruction(&[1]),
                    Opcode::GreaterThan.to_instruction(&[]),
                    Opcode::Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: " 1 == 2",
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    Opcode::Constant.to_instruction(&[0]),
                    Opcode::Constant.to_instruction(&[1]),
                    Opcode::Equal.to_instruction(&[]),
                    Opcode::Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: " 1 != 2",
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    Opcode::Constant.to_instruction(&[0]),
                    Opcode::Constant.to_instruction(&[1]),
                    Opcode::NotEqual.to_instruction(&[]),
                    Opcode::Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: " true == false",
                expected_constants: vec![],
                expected_instructions: vec![
                    Opcode::True.to_instruction(&[0]),
                    Opcode::False.to_instruction(&[1]),
                    Opcode::Equal.to_instruction(&[]),
                    Opcode::Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "true != false",
                expected_constants: vec![],
                expected_instructions: vec![
                    Opcode::True.to_instruction(&[0]),
                    Opcode::False.to_instruction(&[1]),
                    Opcode::NotEqual.to_instruction(&[]),
                    Opcode::Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "!true",
                expected_constants: vec![],
                expected_instructions: vec![
                    Opcode::True.to_instruction(&[]),
                    Opcode::Bang.to_instruction(&[]),
                    Opcode::Pop.to_instruction(&[]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_conditionals() {
        use Object::*;
        use Opcode::*;
        let tests = vec![
            CompilerTestCase {
                input: "if (true) { 10 }; 3333",
                expected_constants: vec![Integer(10), Integer(3333)],
                expected_instructions: vec![
                    True.to_instruction(&[]),
                    JumpNotThuthy.to_instruction(&[10]),
                    Constant.to_instruction(&[0]),
                    Jump.to_instruction(&[11]),
                    Opcode::Null.to_instruction(&[]),
                    Pop.to_instruction(&[]),
                    Constant.to_instruction(&[1]),
                    Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "if (true) { 10 } else { 20 }; 3333",
                expected_constants: vec![Integer(10), Integer(20), Integer(3333)],
                expected_instructions: vec![
                    True.to_instruction(&[]),
                    JumpNotThuthy.to_instruction(&[10]),
                    Constant.to_instruction(&[0]),
                    Jump.to_instruction(&[13]),
                    Constant.to_instruction(&[1]),
                    Pop.to_instruction(&[]),
                    Constant.to_instruction(&[2]),
                    Pop.to_instruction(&[]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_global_let_statements() {
        use Object::*;
        use Opcode::*;
        let tests = vec![
            CompilerTestCase {
                input: "let one = 1; let two = 2;",
                expected_constants: vec![Integer(1), Integer(2)],
                expected_instructions: vec![
                    Constant.to_instruction(&[0]),
                    SetGlobal.to_instruction(&[0]),
                    Constant.to_instruction(&[1]),
                    SetGlobal.to_instruction(&[1]),
                ],
            },
            CompilerTestCase {
                input: "let one = 1; one;",
                expected_constants: vec![Integer(1)],
                expected_instructions: vec![
                    Constant.to_instruction(&[0]),
                    SetGlobal.to_instruction(&[0]),
                    GetGlobal.to_instruction(&[0]),
                    Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "let one = 1; let two = one; two;",
                expected_constants: vec![Integer(1)],
                expected_instructions: vec![
                    Constant.to_instruction(&[0]),
                    SetGlobal.to_instruction(&[0]),
                    GetGlobal.to_instruction(&[0]),
                    SetGlobal.to_instruction(&[1]),
                    GetGlobal.to_instruction(&[1]),
                    Pop.to_instruction(&[]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_string_expressions() {
        use Opcode::*;
        let tests = vec![
            CompilerTestCase {
                input: r#""monkey""#,
                expected_constants: vec![Object::String("monkey".to_string())],
                expected_instructions: vec![Constant.to_instruction(&[0]), Pop.to_instruction(&[])],
            },
            CompilerTestCase {
                input: r#""mon" + "key""#,
                expected_constants: vec![
                    Object::String("mon".to_string()),
                    Object::String("key".to_string()),
                ],
                expected_instructions: vec![
                    Constant.to_instruction(&[0]),
                    Constant.to_instruction(&[1]),
                    Add.to_instruction(&[]),
                    Pop.to_instruction(&[]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_array_literals() {
        use Object::*;
        use Opcode::*;
        let tests = vec![
            CompilerTestCase {
                input: "[]",
                expected_constants: vec![],
                expected_instructions: vec![
                    Opcode::Array.to_instruction(&[0]),
                    Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "[1, 2, 3]",
                expected_constants: vec![Integer(1), Integer(2), Integer(3)],
                expected_instructions: vec![
                    Constant.to_instruction(&[0]),
                    Constant.to_instruction(&[1]),
                    Constant.to_instruction(&[2]),
                    Opcode::Array.to_instruction(&[3]),
                    Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "[1 + 2, 3 - 4, 5 * 6]",
                expected_constants: vec![
                    Integer(1),
                    Integer(2),
                    Integer(3),
                    Integer(4),
                    Integer(5),
                    Integer(6),
                ],
                expected_instructions: vec![
                    Constant.to_instruction(&[0]),
                    Constant.to_instruction(&[1]),
                    Add.to_instruction(&[]),
                    Constant.to_instruction(&[2]),
                    Constant.to_instruction(&[3]),
                    Sub.to_instruction(&[]),
                    Constant.to_instruction(&[4]),
                    Constant.to_instruction(&[5]),
                    Mul.to_instruction(&[]),
                    Opcode::Array.to_instruction(&[3]),
                    Pop.to_instruction(&[]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_hash_literals() {
        use Object::*;
        use Opcode::*;
        let tests = vec![
            CompilerTestCase {
                input: "{}",
                expected_constants: vec![],
                expected_instructions: vec![
                    Opcode::Hash.to_instruction(&[0]),
                    Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "{1: 2, 3: 4, 5: 6}",
                expected_constants: vec![
                    Integer(1),
                    Integer(2),
                    Integer(3),
                    Integer(4),
                    Integer(5),
                    Integer(6),
                ],
                expected_instructions: vec![
                    Constant.to_instruction(&[0]),
                    Constant.to_instruction(&[1]),
                    Constant.to_instruction(&[2]),
                    Constant.to_instruction(&[3]),
                    Constant.to_instruction(&[4]),
                    Constant.to_instruction(&[5]),
                    Opcode::Hash.to_instruction(&[6]),
                    Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "{1: 2 + 3, 4: 5 * 6}",
                expected_constants: vec![
                    Integer(1),
                    Integer(2),
                    Integer(3),
                    Integer(4),
                    Integer(5),
                    Integer(6),
                ],
                expected_instructions: vec![
                    Constant.to_instruction(&[0]),
                    Constant.to_instruction(&[1]),
                    Constant.to_instruction(&[2]),
                    Add.to_instruction(&[]),
                    Constant.to_instruction(&[3]),
                    Constant.to_instruction(&[4]),
                    Constant.to_instruction(&[5]),
                    Mul.to_instruction(&[]),
                    Opcode::Hash.to_instruction(&[4]),
                    Pop.to_instruction(&[]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_index_expressions() {
        use Object::*;
        use Opcode::*;
        let tests = vec![
            CompilerTestCase {
                input: "[1, 2, 3][1 + 1]",
                expected_constants: vec![
                    Integer(1),
                    Integer(2),
                    Integer(3),
                    Integer(1),
                    Integer(1),
                ],
                expected_instructions: vec![
                    Constant.to_instruction(&[0]),
                    Constant.to_instruction(&[1]),
                    Constant.to_instruction(&[2]),
                    Opcode::Array.to_instruction(&[3]),
                    Constant.to_instruction(&[3]),
                    Constant.to_instruction(&[4]),
                    Add.to_instruction(&[]),
                    Index.to_instruction(&[]),
                    Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "{1: 2}[2 - 1]",
                expected_constants: vec![Integer(1), Integer(2), Integer(2), Integer(1)],
                expected_instructions: vec![
                    Constant.to_instruction(&[0]),
                    Constant.to_instruction(&[1]),
                    Hash.to_instruction(&[2]),
                    Constant.to_instruction(&[2]),
                    Constant.to_instruction(&[3]),
                    Sub.to_instruction(&[]),
                    Index.to_instruction(&[]),
                    Pop.to_instruction(&[]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_functions() {
        use crate::object::CompiledFunction;
        use Object::Integer;
        use Opcode::*;
        let tests = vec![
            CompilerTestCase {
                input: "fn() { return 5 + 10 }",
                expected_constants: vec![
                    Integer(5),
                    Integer(10),
                    Object::CompiledFunction(CompiledFunction::new_from_array(&[
                        Constant.to_instruction(&[0]),
                        Constant.to_instruction(&[1]),
                        Add.to_instruction(&[]),
                        ReturnValue.to_instruction(&[]),
                    ])),
                ],
                expected_instructions: vec![Constant.to_instruction(&[2]), Pop.to_instruction(&[])],
            },
            CompilerTestCase {
                input: "fn() { 5 + 10 }",
                expected_constants: vec![
                    Integer(5),
                    Integer(10),
                    Object::CompiledFunction(CompiledFunction::new_from_array(&[
                        Constant.to_instruction(&[0]),
                        Constant.to_instruction(&[1]),
                        Add.to_instruction(&[]),
                        ReturnValue.to_instruction(&[]),
                    ])),
                ],
                expected_instructions: vec![Constant.to_instruction(&[2]), Pop.to_instruction(&[])],
            },
            CompilerTestCase {
                input: "fn() { 1; 2 }",
                expected_constants: vec![
                    Integer(1),
                    Integer(2),
                    Object::CompiledFunction(CompiledFunction::new_from_array(&[
                        Constant.to_instruction(&[0]),
                        Pop.to_instruction(&[]),
                        Constant.to_instruction(&[1]),
                        ReturnValue.to_instruction(&[]),
                    ])),
                ],
                expected_instructions: vec![Constant.to_instruction(&[2]), Pop.to_instruction(&[])],
            },
            CompilerTestCase {
                input: "fn() { }",
                expected_constants: vec![Object::CompiledFunction(
                    CompiledFunction::new_from_array(&[Return.to_instruction(&[])]),
                )],
                expected_instructions: vec![Constant.to_instruction(&[0]), Pop.to_instruction(&[])],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_compiler_scopes() {
        use super::*;
        let mut compiler = Compiler::new();
        assert_eq!(compiler.scope_index, 0);

        compiler.emit(Opcode::Mul, &[]);

        compiler.enter_scope();
        assert_eq!(compiler.scope_index, 1);

        compiler.emit(Opcode::Sub, &[]);
        assert_eq!(compiler.scopes[compiler.scope_index].instructions.len(), 1);

        assert_eq!(
            compiler.scopes[compiler.scope_index]
                .last_instruction
                .as_ref()
                .unwrap()
                .opcode,
            Opcode::Sub
        );

        compiler.leave_scope();

        assert_eq!(compiler.scope_index, 0);

        compiler.emit(Opcode::Add, &[]);
        assert_eq!(compiler.scopes[compiler.scope_index].instructions.len(), 2);

        assert_eq!(
            compiler.scopes[compiler.scope_index]
                .last_instruction
                .as_ref()
                .unwrap()
                .opcode,
            Opcode::Add
        );

        assert_eq!(
            compiler.scopes[compiler.scope_index]
                .previous_instruction
                .as_ref()
                .unwrap()
                .opcode,
            Opcode::Mul
        );
    }

    #[test]
    fn test_function_calls() {
        use crate::object::CompiledFunction;
        use Object::Integer;
        use Opcode::*;
        let tests = vec![
            CompilerTestCase {
                input: "fn() { 24 }();",
                expected_constants: vec![
                    Integer(24),
                    Object::CompiledFunction(CompiledFunction::new_from_array(&[
                        Constant.to_instruction(&[0]),
                        ReturnValue.to_instruction(&[]),
                    ])),
                ],
                expected_instructions: vec![
                    Constant.to_instruction(&[1]),
                    Call.to_instruction(&[]),
                    Pop.to_instruction(&[]),
                ],
            },
            CompilerTestCase {
                input: "let noArg = fn() { 24 }; noArg();",
                expected_constants: vec![
                    Integer(24),
                    Object::CompiledFunction(CompiledFunction::new_from_array(&[
                        Constant.to_instruction(&[0]),
                        ReturnValue.to_instruction(&[]),
                    ])),
                ],
                expected_instructions: vec![
                    Constant.to_instruction(&[1]),
                    SetGlobal.to_instruction(&[0]),
                    GetGlobal.to_instruction(&[0]),
                    Call.to_instruction(&[]),
                    Pop.to_instruction(&[]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }
}
