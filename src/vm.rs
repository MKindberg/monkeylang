use crate::{
    code::{Instructions, Opcode},
    compiler,
    object::CompiledFunction,
    object::Object,
};

macro_rules! read_arg {
    ($instructions:expr, $ip:expr, $type: ty) => {
        <$type>::from_be_bytes(
            $instructions[$ip..$ip + std::mem::size_of::<$type>()]
                .try_into()
                .unwrap(),
        ) as usize
    };
}

struct Frame {
    func: CompiledFunction,
    ip: usize,
}

impl Frame {
    fn new(func: CompiledFunction) -> Frame {
        Frame { func, ip: 0 }
    }

    fn instructions(&self) -> Instructions {
        self.func.instructions.clone()
    }
}

pub struct VM {
    constants: Vec<Object>,
    stack: Vec<Object>,
    pub globals: Vec<Object>,
    sp: usize,
    frames: Vec<Frame>,
    fames_index: usize,
}

const STACK_SIZE: usize = 2048;
pub const GLOBALS_SIZE: usize = 65536;
impl VM {
    pub fn new(bytecode: compiler::Bytecode) -> VM {
        VM {
            constants: bytecode.constants,
            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,
            globals: vec![Object::Null; GLOBALS_SIZE],
            frames: vec![Frame::new(CompiledFunction {
                instructions: bytecode.instructions,
            })],
            fames_index: 1,
        }
    }

    pub fn new_with_gobals_store(bytecode: compiler::Bytecode, globals: Vec<Object>) -> VM {
        let mut v = VM::new(bytecode);
        v.globals = globals;
        return v;
    }

    fn current_frame(&mut self) -> &mut Frame {
        return &mut self.frames[self.fames_index - 1];
    }

    fn push_frame(&mut self, frame: Frame) {
        self.fames_index += 1;
        self.frames.push(frame);
    }

    fn pop_frame(&mut self) -> Frame {
        self.fames_index -= 1;
        return self.frames.pop().unwrap();
    }

    pub fn last_popped_stack_elem(&self) -> &Object {
        return &self.stack[self.sp];
    }

    fn is_truthy(&self, operand: &Object) -> bool {
        match operand {
            Object::Boolean(b) => *b,
            Object::Null => false,
            _ => true,
        }
    }

    pub fn run(&mut self) -> Result<(), String> {
        let mut ip = 0;
        while ip < self.current_frame().instructions().len() {
            let op = Opcode::lookup(self.current_frame().instructions()[ip])?;
            ip += 1;

            match op {
                Opcode::Constant => {
                    let const_idx = read_arg!(self.current_frame().instructions(), ip, u16);
                    ip += 2;

                    self.push(self.constants[const_idx].clone())?;
                }
                Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                    self.execute_binary_expression(op)?;
                }
                Opcode::Pop => {
                    self.pop();
                }
                Opcode::True | Opcode::False => {
                    self.push(Object::Boolean(op == Opcode::True))?;
                }
                Opcode::Equal | Opcode::NotEqual | Opcode::GreaterThan => {
                    self.execute_comparison(op)?;
                }
                Opcode::Bang => {
                    let operand = self.pop();
                    self.push(Object::Boolean(!self.is_truthy(&operand)))?;
                }
                Opcode::Minus => {
                    let operand = self.pop();
                    if let Object::Integer(i) = operand {
                        self.push(Object::Integer(-i))?;
                    } else {
                        return Err(format!(
                            "Unsupported type for negation: {}",
                            operand.type_string()
                        ));
                    }
                }
                Opcode::JumpNotThuthy => {
                    let pos = read_arg!(self.current_frame().instructions(), ip, u16);
                    ip += 2;

                    let condition = self.pop();
                    if !self.is_truthy(&condition) {
                        ip = pos;
                    }
                }
                Opcode::Jump => {
                    let pos = read_arg!(self.current_frame().instructions(), ip, u16);
                    ip = pos;
                }
                Opcode::Null => {
                    self.push(Object::Null)?;
                }
                Opcode::SetGlobal => {
                    let global_idx = read_arg!(self.current_frame().instructions(), ip, u16);
                    ip += 2;
                    self.globals[global_idx] = self.pop();
                }
                Opcode::GetGlobal => {
                    let global_idx = read_arg!(self.current_frame().instructions(), ip, u16);
                    ip += 2;
                    self.push(self.globals[global_idx].clone())?;
                }
                Opcode::Array => {
                    let len = read_arg!(self.current_frame().instructions(), ip, u16);
                    ip += 2;

                    let elems = self.stack[self.sp - len..self.sp].to_owned();
                    self.sp -= len;
                    self.push(Object::Array(elems))?;
                }
                Opcode::Hash => {
                    let len = read_arg!(self.current_frame().instructions(), ip, u16);
                    ip += 2;

                    let pairs = self.stack[self.sp - len..self.sp]
                        .chunks(2)
                        .map(|c| (c[0].clone(), c[1].clone()))
                        .collect();
                    self.sp -= len;
                    self.push(Object::HashMap(pairs))?
                }
                Opcode::Index => {
                    let index = self.pop();
                    let left = self.pop();
                    match (&left, &index) {
                        (Object::Array(a), Object::Integer(i)) => {
                            if i >= &0 && i < &(a.len() as i64) {
                                self.push(a[*i as usize].clone())?
                            } else {
                                self.push(Object::Null)?
                            }
                        }
                        (Object::HashMap(m), _) => {
                            if !index.is_hashable() {
                                return Err(format!(
                                    "Unable to use as hash key: {}",
                                    index.type_string()
                                ));
                            }
                            if m.contains_key(&index) {
                                self.push(m[&index].clone())?
                            } else {
                                self.push(Object::Null)?
                            }
                        }
                        _ => {
                            return Err(format!(
                                "Index operator not supported for {}",
                                left.type_string()
                            ))
                        }
                    }
                }
                Opcode::Call => {
                    if let Object::CompiledFunction(func) = self.stack[self.sp - 1].clone() {
                        let frame = Frame::new(func);
                        self.push_frame(frame);
                        ip = 0;
                    }
                }
                Opcode::ReturnValue => {
                    let return_value = self.pop();
                    self.pop_frame();
                    self.pop();
                    self.push(return_value)?;
                    ip = self.current_frame().ip + 1;
                }
                Opcode::Return => {
                    self.pop_frame();
                    self.pop();
                    self.push(Object::Null)?;
                    ip = self.current_frame().ip + 1;
                }
                _ => return Err(format!("{} Not implemented", op.to_string())),
            }
            self.current_frame().ip = ip;
        }

        Ok(())
    }

    fn execute_binary_expression(&mut self, op: Opcode) -> Result<(), String> {
        let right = self.pop();
        let left = self.pop();
        match (&op, &left, &right) {
            (Opcode::Add, Object::Integer(l), Object::Integer(r)) => {
                self.push(Object::Integer(l + r))
            }
            (Opcode::Sub, Object::Integer(l), Object::Integer(r)) => {
                self.push(Object::Integer(l - r))
            }
            (Opcode::Mul, Object::Integer(l), Object::Integer(r)) => {
                self.push(Object::Integer(l * r))
            }
            (Opcode::Div, Object::Integer(l), Object::Integer(r)) => {
                self.push(Object::Integer(l / r))
            }
            (Opcode::Add, Object::String(l), Object::String(r)) => {
                self.push(Object::String(format!("{}{}", l, r)))
            }
            _ => {
                return Err(format!(
                    "Invalid operation {} {} {}",
                    left.type_string(),
                    op.to_string(),
                    right.type_string()
                ))
            }
        }
    }

    fn execute_comparison(&mut self, op: Opcode) -> Result<(), String> {
        let right = self.pop();
        let left = self.pop();

        match (op, &left, &right) {
            (Opcode::GreaterThan, Object::Integer(l), Object::Integer(r)) => {
                self.push(Object::Boolean(l > r))
            }
            (Opcode::Equal, _, _) => self.push(Object::Boolean(left == right)),
            (Opcode::NotEqual, _, _) => self.push(Object::Boolean(left != right)),
            _ => Err("Invalid comparison".to_string()),
        }
    }

    fn push(&mut self, obj: Object) -> Result<(), String> {
        if self.sp == STACK_SIZE {
            return Err("Stack overflow".to_string());
        }
        self.stack[self.sp] = obj;
        self.sp += 1;
        Ok(())
    }

    fn pop(&mut self) -> Object {
        self.sp -= 1;
        return self.stack[self.sp].clone();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast, compiler::Compiler, lexer::Lexer, object, parser::Parser};

    struct VmTestCase {
        input: &'static str,
        expected: object::Object,
    }
    impl VmTestCase {
        fn new(input: &'static str, expected: object::Object) -> VmTestCase {
            VmTestCase { input, expected }
        }
    }

    fn parse(input: String) -> ast::Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        return parser.parse_program();
    }

    fn run_vm_tests(tests: Vec<VmTestCase>) {
        for test in tests {
            println!("Input: {}", test.input);
            let program = parse(test.input.to_string());

            let mut compiler = Compiler::new();
            compiler.compile(program).unwrap();

            let mut vm = VM::new(compiler.bytecode());
            if let Err(e) = vm.run() {
                panic!("{}", e);
            }

            let stack_elem = vm.last_popped_stack_elem();
            assert_eq!(&test.expected, stack_elem);
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        use Object::Integer;
        let tests = vec![
            VmTestCase::new("1", Integer(1)),
            VmTestCase::new("2", Integer(2)),
            VmTestCase::new("1 + 2", Integer(3)),
            VmTestCase::new("1 - 2", Integer(-1)),
            VmTestCase::new("1 * 2", Integer(2)),
            VmTestCase::new("4 / 2", Integer(2)),
            VmTestCase::new("50 / 2 * 2 + 10 - 5", Integer(55)),
            VmTestCase::new("5 + 5 + 5 + 5 - 10", Integer(10)),
            VmTestCase::new("2 * 2 * 2 * 2 * 2", Integer(32)),
            VmTestCase::new("5 * 2 + 10", Integer(20)),
            VmTestCase::new("5 + 2 * 10", Integer(25)),
            VmTestCase::new("5 * (2 + 10)", Integer(60)),
            VmTestCase::new("-5", Integer(-5)),
            VmTestCase::new("-10", Integer(-10)),
            VmTestCase::new("-50 + 100 + -50", Integer(0)),
            VmTestCase::new("(5 + 10 * 2 + 15 / 3) * 2 + -10", Integer(50)),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        use Object::Boolean;
        let tests = vec![
            VmTestCase::new("true", Boolean(true)),
            VmTestCase::new("false", Boolean(false)),
            VmTestCase::new("1 < 2", Boolean(true)),
            VmTestCase::new("1 > 2", Boolean(false)),
            VmTestCase::new("1 < 1", Boolean(false)),
            VmTestCase::new("1 > 1", Boolean(false)),
            VmTestCase::new("1 == 1", Boolean(true)),
            VmTestCase::new("1 != 1", Boolean(false)),
            VmTestCase::new("1 == 2", Boolean(false)),
            VmTestCase::new("1 != 2", Boolean(true)),
            VmTestCase::new("true == true", Boolean(true)),
            VmTestCase::new("false == false", Boolean(true)),
            VmTestCase::new("true == false", Boolean(false)),
            VmTestCase::new("true != false", Boolean(true)),
            VmTestCase::new("false != true", Boolean(true)),
            VmTestCase::new("(1 < 2) == true", Boolean(true)),
            VmTestCase::new("(1 < 2) == false", Boolean(false)),
            VmTestCase::new("(1 > 2) == true", Boolean(false)),
            VmTestCase::new("(1 > 2) == false", Boolean(true)),
            VmTestCase::new("!true", Boolean(false)),
            VmTestCase::new("!false", Boolean(true)),
            VmTestCase::new("!5", Boolean(false)),
            VmTestCase::new("!!true", Boolean(true)),
            VmTestCase::new("!!false", Boolean(false)),
            VmTestCase::new("!!5", Boolean(true)),
            VmTestCase::new("!(if (false) { 5; })", Boolean(true)),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_conditionals() {
        use Object::Integer;
        let tests = vec![
            VmTestCase::new("if (true) { 10 }", Integer(10)),
            VmTestCase::new("if (true) { 10 } else { 20 }", Integer(10)),
            VmTestCase::new("if (false) { 10 } else { 20 } ", Integer(20)),
            VmTestCase::new("if (1) { 10 }", Integer(10)),
            VmTestCase::new("if (1 < 2) { 10 }", Integer(10)),
            VmTestCase::new("if (1 < 2) { 10 } else { 20 }", Integer(10)),
            VmTestCase::new("if (1 > 2) { 10 } else { 20 }", Integer(20)),
            VmTestCase::new("if (1 > 2) { 10 }", Object::Null),
            VmTestCase::new("if (false) { 10 }", Object::Null),
            VmTestCase::new("if ((if (false) { 10 })) { 10 } else { 20 }", Integer(20)),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_global_let_statements() {
        use Object::Integer;
        let tests = vec![
            VmTestCase::new("let one = 1; one;", Integer(1)),
            VmTestCase::new("let one = 1; let two = 2; one + two;", Integer(3)),
            VmTestCase::new("let one = 1; let two = one + one; one + two;", Integer(3)),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_string_expressions() {
        use Object::String;
        let tests = vec![
            VmTestCase::new(r#""monkey""#, String("monkey".to_string())),
            VmTestCase::new(r#""mon" + "key""#, String("monkey".to_string())),
            VmTestCase::new(
                r#""mon" + "key" + "banana""#,
                String("monkeybanana".to_string()),
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_array_literals() {
        use Object::*;
        let tests = vec![
            VmTestCase::new("[]", Array(vec![])),
            VmTestCase::new("[1, 2, 3]", Array(vec![Integer(1), Integer(2), Integer(3)])),
            VmTestCase::new(
                "[1 + 2, 3 * 4, 5 + 6]",
                Array(vec![Integer(3), Integer(12), Integer(11)]),
            ),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_hash_literals() {
        use std::collections::HashMap;
        use Object::Integer;
        let tests = vec![
            VmTestCase::new("{}", Object::HashMap(HashMap::new())),
            VmTestCase::new(
                "{1: 2, 3: 4}",
                Object::HashMap(HashMap::from([
                    (Integer(1), Integer(2)),
                    (Integer(3), Integer(4)),
                ])),
            ),
            VmTestCase::new(
                "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
                Object::HashMap(HashMap::from([
                    (Integer(2), Integer(4)),
                    (Integer(6), Integer(16)),
                ])),
            ),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_index_expressions() {
        use Object::{Integer, Null};
        let tests = vec![
            VmTestCase::new("[1, 2, 3][1]", Integer(2)),
            VmTestCase::new("[1, 2, 3][0 + 2]", Integer(3)),
            VmTestCase::new("[[1, 1, 1]][0][0]", Integer(1)),
            VmTestCase::new("[][0]", Null),
            VmTestCase::new("[1, 2, 3][99]", Null),
            VmTestCase::new("[1][-1]", Null),
            VmTestCase::new("{1: 1, 2: 2}[1]", Integer(1)),
            VmTestCase::new("{1: 1, 2: 2}[2]", Integer(2)),
            VmTestCase::new("{1: 1}[0]", Null),
            VmTestCase::new("{}[0]", Null),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_calling_functions_without_arguments() {
        use Object::Integer;
        let tests = vec![
            VmTestCase::new(
                "let fivePlusTen = fn() {5 + 10;}; fivePlusTen();",
                Integer(15),
            ),
            VmTestCase::new(
                "let one = fn() { 1; };
        let two = fn() { 2; };
        one() + two()",
                Integer(3),
            ),
            VmTestCase::new(
                "let a = fn() { 1 };
        let b = fn() { a() + 1 };
        let c = fn() { b() + 1 };
        c();",
                Integer(3),
            ),
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_functions_with_return_statements() {
        use Object::Integer;
        let tests = vec![
            VmTestCase::new(
                "let earlyExit = fn() { return 99; 100; };
        earlyExit();",
                Integer(99),
            ),
            VmTestCase::new(
                "let earlyExit = fn() { return 99; return 100; };
        earlyExit();",
                Integer(99),
            ),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_functions_without_return_value() {
        use Object::Null;
        let tests = vec![
            VmTestCase::new(
                "let noReturn = fn() { };
        noReturn();",
                Null,
            ),
            VmTestCase::new(
                "let noReturn = fn() { };
        let noReturnTwo = fn() { noReturn(); };
        noReturn();
        noReturnTwo();",
                Null,
            ),
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_first_class_functions() {
        let tests = vec![VmTestCase::new("let returnsOne = fn() { 1; };
        let returnsOneReturner = fn() { returnsOne; };
        returnsOneReturner()();", Object::Integer(1))];
        run_vm_tests(tests);
    }
}
