use crate::ast;
use crate::ast::Expression;
use crate::ast::Statement;
use crate::object::Environment;
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
        _ => Object::Null,
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
        (Object::Integer(l), Object::Integer(r), _) => {
            eval_integer_infix_expression(&l, &r, operator)
        }
        (_, _, "==") => Object::Boolean(l == r),
        (_, _, "!=") => Object::Boolean(l != r),
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
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => Object::Error(format!("unknown operator: INTEGER {} INTEGER", operator)),
    }
}

fn eval_if_expression(
    condition: &Expression,
    consequence: &Statement,
    alternative: &Option<Statement>,
    env: &mut Environment,
) -> Object {
    let cond = eval_expression(condition, env);

    if is_truthy(cond) {
        eval_statement(consequence, env)
    } else if let Some(block) = alternative {
        eval_statement(block, env)
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
    match env.get(value) {
        Some(v) => v.clone(),
        None => Object::Error(format!("identifier not found: {}", value)),
    }
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
            }
        }
    }
}
