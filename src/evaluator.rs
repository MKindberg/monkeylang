use crate::ast;
use crate::ast::Expression;
use crate::ast::Statement;
use crate::object::BuiltinFunction;
use crate::object::Environment;
use crate::object::Function;
use crate::object::Object;

pub fn eval(program: &ast::Program, env: &mut Environment) -> Object {
    eval_program(&program.statements, env)
}

fn eval_statement(statement: &ast::Statement, env: &mut Environment) -> Object {
    match statement {
        Statement::ExpressionStatement(e) => eval_expression(&*e.expression, env),
        Statement::BlockStatement(b) => eval_block_statement(&*b.statements, env),
        Statement::ReturnStatement(r) => eval_return_statement(&*r.value, env),
        Statement::LetStatement(l) => eval_let_statement(&l, env),
    }
}

fn eval_program(statements: &[Statement], env: &mut Environment) -> Object {
    let mut result = Object::Null;
    for statement in statements {
        result = eval_statement(statement, env);
        if let Object::ReturnValue(r) = result {
            return *r;
        }
        if let Object::Error(_) = result {
            return result;
        }
    }
    result
}

fn eval_block_statement(statements: &[Statement], env: &mut Environment) -> Object {
    let mut result = Object::Null;
    for statement in statements {
        result = eval_statement(statement, env);
        if let Object::ReturnValue(_) = result {
            return result;
        }
        if let Object::Error(_) = result {
            return result;
        }
    }
    result
}

fn eval_return_statement(return_value: &Expression, env: &mut Environment) -> Object {
    let result = eval_expression(return_value, env);
    if let Object::Error(_) = result {
        return result;
    }
    Object::ReturnValue(Box::new(result))
}

fn eval_let_statement(let_stmt: &ast::LetStatement, env: &mut Environment) -> Object {
    let value = eval_expression(&let_stmt.value, env);
    if let Object::Error(_) = value {
        return value;
    }
    env.set(let_stmt.name.value.clone(), value);
    Object::Null
}

fn eval_expression(expression: &ast::Expression, env: &mut Environment) -> Object {
    match expression {
        Expression::IntegerLiteral(i) => Object::Integer(i.value),
        Expression::Boolean(b) => Object::Boolean(b.value),
        Expression::PrefixExpression(p) => eval_prefix_expression(&p.operator, &p.right, env),
        Expression::InfixExpression(i) => {
            eval_infix_expression(&i.left, &i.operator, &i.right, env)
        }
        Expression::IfExpression(i) => {
            eval_if_expression(&i.condition, &i.consequence, &i.alternative, env)
        }
        Expression::Identifier(i) => eval_identifier(&i.value, env),
        Expression::FunctionLiteral(f) => Object::Function(Function::new(
            f.parameters.clone(),
            f.body.clone(),
            env.clone(),
        )),
        Expression::CallExpression(c) => eval_call_expression(&c.function, &c.arguments, env),
        Expression::StringLiteral(s) => Object::String(s.value.clone()),
        Expression::ArrayLiteral(a) => eval_array_literal(&a.elements, env),
        Expression::IndexExpression(i) => eval_index_expression(&i.left, &i.index, env),
    }
}

fn eval_prefix_expression(operator: &str, right: &Expression, env: &mut Environment) -> Object {
    let r = eval_expression(right, env);
    if let Object::Error(_) = r {
        return r;
    }
    match (operator, &r) {
        ("!", _) => eval_bang_operator_expression(&r),
        ("-", Object::Integer(i)) => Object::Integer(-i),
        _ => Object::Error(format!("unknown operator: {}{}", operator, r.type_string())),
    }
}

fn eval_bang_operator_expression(right: &Object) -> Object {
    match right {
        Object::Boolean(b) => Object::Boolean(!b),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_infix_expression(
    left: &Expression,
    operator: &str,
    right: &Expression,
    env: &mut Environment,
) -> Object {
    let l = eval_expression(left, env);
    if let Object::Error(_) = l {
        return l;
    }
    let r = eval_expression(right, env);
    if let Object::Error(_) = r {
        return r;
    }
    match (&l, &r, operator) {
        (_, _, "==") => Object::Boolean(l == r),
        (_, _, "!=") => Object::Boolean(l != r),
        (Object::Integer(l), Object::Integer(r), _) => {
            eval_integer_infix_expression(&l, &r, operator)
        }
        (Object::String(l), Object::String(r), _) => eval_string_infix_expression(&l, &r, operator),
        (l, r, _) if l.type_string() != r.type_string() => Object::Error(format!(
            "type mismatch: {} {} {}",
            l.type_string(),
            operator,
            r.type_string()
        )),
        _ => Object::Error(format!(
            "unknown operator: {} {} {}",
            l.type_string(),
            operator,
            r.type_string()
        )),
    }
}

fn eval_integer_infix_expression(left: &i64, right: &i64, operator: &str) -> Object {
    match operator {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => Object::Boolean(left < right),
        ">" => Object::Boolean(left > right),
        _ => Object::Error(format!("unknown operator: INTEGER {} INTEGER", operator)),
    }
}

fn eval_string_infix_expression(left: &String, right: &String, operator: &str) -> Object {
    match operator {
        "+" => Object::String(format!("{}{}", left, right)),
        _ => Object::Error(format!("unknown operator: STRING {} STRING", operator)),
    }
}

fn eval_if_expression(
    condition: &Expression,
    consequence: &ast::BlockStatement,
    alternative: &Option<ast::BlockStatement>,
    env: &mut Environment,
) -> Object {
    let cond = eval_expression(condition, env);
    if let Object::Error(_) = cond {
        return cond;
    }

    if is_truthy(cond) {
        eval_statement(&Statement::BlockStatement(consequence.clone()), env)
    } else if let Some(block) = alternative {
        eval_statement(&Statement::BlockStatement(block.clone()), env)
    } else {
        Object::Null
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(b) => b,
        _ => true,
    }
}

fn eval_identifier(value: &str, env: &mut Environment) -> Object {
    if let Some(v) = env.get(value) {
        return v.clone();
    } else if let Some(v) = BuiltinFunction::lookup(value) {
        return Object::Builtin(v);
    } else {
        Object::Error(format!("identifier not found: {}", value))
    }
}

fn eval_call_expression(
    function: &Expression,
    arguments: &Vec<Expression>,
    env: &mut Environment,
) -> Object {
    let func = eval_expression(function, env);
    if let Object::Error(_) = func {
        return func;
    }
    let args = eval_expressions(arguments, env);
    if let Some(Object::Error(_)) = args.get(0) {
        return args[0].clone();
    }

    return apply_function(func, args);
}

fn eval_array_literal(elements: &Vec<Expression>, env: &mut Environment) -> Object {
    let e = eval_expressions(&elements, env);
    if let Some(Object::Error(_)) = e.get(0) {
        return e[0].clone();
    }
    return Object::Array(e);
}

fn eval_index_expression(left: &Expression, index: &Expression, env: &mut Environment) -> Object {
    let l = eval_expression(left, env);
    if let Object::Error(_) = l {
        return l;
    }
    let i = eval_expression(index, env);
    if let Object::Error(_) = i {
        return i;
    }
    return match (&l, &i) {
        (Object::Array(a), Object::Integer(i)) => eval_array_index_expression(&a, &i),
        _ => Object::Error(format!("index operator not supported: {} {}", l.type_string(), i.type_string())),
    }
}

fn eval_array_index_expression(arr: &Vec<Object>, index: &i64) -> Object {
    if *index < 0 || *index >= arr.len() as i64 {
        return Object::Null;
    }
    return arr[*index as usize].clone();
}

fn eval_expressions(expressions: &Vec<Expression>, env: &mut Environment) -> Vec<Object> {
    let mut result = Vec::new();
    for expr in expressions {
        let evaluated = eval_expression(expr, env);
        if let Object::Error(_) = evaluated {
            return vec![evaluated];
        }
        result.push(evaluated);
    }
    return result;
}

fn apply_function(func: Object, args: Vec<Object>) -> Object {
    if let Object::Function(f) = func {
        let mut extented_env = extended_function_env(&f, args);
        let evaluated = eval_block_statement(&f.body.statements, &mut extented_env);
        if let Object::ReturnValue(rv) = evaluated {
            return *rv;
        }
        return evaluated;
    } else if let Object::Builtin(f) = func {
        return f.execute(&args);
    } else {
        return Object::Error(format!("not a function: {}", func.type_string()));
    }
}

fn extended_function_env(f: &Function, args: Vec<Object>) -> Environment {
    let mut env = Environment::new_with_outer(f.env.clone());
    for (i, param) in f.parameters.iter().enumerate() {
        env.set(param.value.clone(), args[i].clone());
    }

    return env;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut env = Environment::new();

        return eval(&program, &mut env);
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        for (input, expected) in tests {
            assert_eq!(test_eval(input), Object::Integer(expected));
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];
        for (input, expected) in tests {
            assert_eq!(test_eval(input), Object::Boolean(expected));
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            assert_eq!(test_eval(input), Object::Boolean(expected));
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];

        for (input, expected) in tests {
            assert_eq!(test_eval(input), expected);
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", Object::Integer(10)),
            ("return 10; 9;", Object::Integer(10)),
            ("return 2 * 5; 9;", Object::Integer(10)),
            ("9; return 2 * 5; 9;", Object::Integer(10)),
            (
                "if (10 > 1) { if (10 > 1) { return 10; } return 1; }",
                Object::Integer(10),
            ),
        ];

        for (input, expected) in tests {
            assert_eq!(test_eval(input), expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true;", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar;", "identifier not found: foobar"),
            (r#""Hello" - "World""#, "unknown operator: STRING - STRING"),
        ];

        for (input, expected) in tests {
            if let Object::Error(e) = test_eval(input) {
                assert_eq!(e, expected);
            } else {
                panic!("Error parsing {}", input);
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in tests {
            if let Object::Integer(e) = test_eval(input) {
                assert_eq!(e, expected);
            } else {
                panic!("Error parsing {}", input);
            }
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";

        let evaluated = test_eval(input);
        if let Object::Function(f) = evaluated {
            assert_eq!(f.parameters.len(), 1);
            assert_eq!(&f.parameters[0].value, "x");
            assert_eq!(f.body.len(), 1);
            assert_eq!(f.body.to_string(), "(x + 2)");
        } else {
            panic!("not a function object");
        }
    }

    #[test]
    fn test_function_application() {
        let input = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, expected) in input {
            assert_eq!(test_eval(input), Object::Integer(expected));
        }
    }

    #[test]
    fn test_closures() {
        let input = "let newAdder = fn(x) { fn(y) { x + y; }; };
            let addTwo = newAdder(2);
            addTwo(2);";
        assert_eq!(test_eval(input), Object::Integer(4));
    }

    #[test]
    fn test_string_literal() {
        assert_eq!(
            test_eval(r#""hello world""#),
            Object::String("hello world".to_string())
        );
    }

    #[test]
    fn test_string_concatenation() {
        assert_eq!(
            test_eval(r#""hello" + " " + "world""#),
            Object::String("hello world".to_string())
        )
    }

    #[test]
    fn test_builtin_functions() {
        let input = vec![
            (r#"len("")"#, Object::Integer(0)),
            (r#"len("four")"#, Object::Integer(4)),
            (r#"len("hello world")"#, Object::Integer(11)),
            (
                r#"len(1)"#,
                Object::Error("argument to `len` not supported, got INTEGER".to_string()),
            ),
            (
                r#"len(true)"#,
                Object::Error("argument to `len` not supported, got BOOLEAN".to_string()),
            ),
        ];

        for (input, expected) in input {
            assert_eq!(test_eval(input), expected);
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        assert_eq!(
            test_eval(input),
            Object::Array(vec![
                Object::Integer(1),
                Object::Integer(4),
                Object::Integer(6),
            ])
        );
    }

    #[test]
    fn test_index_expressions() {
        let tests = vec![
            ("[1, 2, 3][0]", Object::Integer(1)),
            ("[1, 2, 3][1]", Object::Integer(2)),
            ("[1, 2, 3][2]", Object::Integer(3)),
            ("let i = 0; [1][i];", Object::Integer(1)),
            ("[1, 2, 3][1 + 1];", Object::Integer(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Object::Integer(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Object::Integer(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];",
                Object::Integer(2),
            ),
            ("[1, 2, 3][3]", Object::Null),
            ("[1, 2, 3][-1]", Object::Null),
        ];

        for (input, expected) in tests {
            assert_eq!(test_eval(input), expected);
        }
    }
}
