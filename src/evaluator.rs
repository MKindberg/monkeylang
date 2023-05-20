use crate::ast;
use crate::ast::Expression;
use crate::ast::Statement;
use crate::object::Object;

const TRUE: bool = true;
const FALSE: bool = false;

pub fn eval(program: &ast::Program) -> Object {
    eval_program(&program.statements)
}

fn eval_statement(statement: &ast::Statement) -> Object {
    match statement {
        Statement::ExpressionStatement(e) => eval_expression(&*e.expression),
        Statement::BlockStatement(b) => eval_block_statement(&*b.statements),
        Statement::ReturnStatement(r) => eval_return_statement(&*r.value),
        _ => Object::Null,
    }
}

fn eval_program(statements: &[Statement]) -> Object {
    let mut result = Object::Null;
    for statement in statements {
        result = eval_statement(statement);
        if let Object::ReturnValue(r) = result {
            return *r;
        }
        if let Object::Error(_) = result {
            return result;
        }
    }
    result
}

fn eval_block_statement(statements: &[Statement]) -> Object {
    let mut result = Object::Null;
    for statement in statements {
        result = eval_statement(statement);
        if let Object::ReturnValue(_) = result {
            return result;
        }
        if let Object::Error(_) = result {
            return result;
        }
    }
    result
}

fn eval_return_statement(return_value: &Expression) -> Object {
    let result = eval_expression(return_value);
    if let Object::Error(_) = result {
        return result;
    }
    Object::ReturnValue(Box::new(result))
}

fn eval_expression(expression: &ast::Expression) -> Object {
    match expression {
        Expression::IntegerLiteral(i) => Object::Integer(i.value),
        Expression::Boolean(b) => Object::Boolean(b.value),
        Expression::PrefixExpression(p) => eval_prefix_expression(&p.operator, &p.right),
        Expression::InfixExpression(i) => eval_infix_expression(&i.left, &i.operator, &i.right),
        Expression::IfExpression(i) => {
            eval_if_expression(&i.condition, &i.consequence, &i.alternative)
        }
        _ => Object::Null,
    }
}

fn eval_prefix_expression(operator: &str, right: &Expression) -> Object {
    let r = eval_expression(right);
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

fn eval_infix_expression(left: &Expression, operator: &str, right: &Expression) -> Object {
    let l = eval_expression(left);
    if let Object::Error(_) = l {
        return l;
    }
    let r = eval_expression(right);
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
) -> Object {
    let cond = eval_expression(condition);

    if is_truthy(cond) {
        eval_statement(consequence)
    } else if let Some(block) = alternative {
        eval_statement(block)
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

        return eval(&program);
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
            if let e = test_eval(input) {
                assert_eq!(e, Object::Integer(expected));
            } else {
                panic!("Error parsing {}", input);
            }
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
            if let e = test_eval(input) {
                assert_eq!(e, Object::Boolean(expected));
            } else {
                panic!("Error parsing {}", input);
            }
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
            if let e = test_eval(input) {
                assert_eq!(e, Object::Boolean(expected));
            } else {
                panic!("Error parsing {}", input);
            }
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
            if let e = test_eval(input) {
                assert_eq!(e, expected);
            } else {
                panic!("Error parsing {}", input);
            }
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
            if let e = test_eval(input) {
                assert_eq!(e, expected);
            } else {
                panic!("Error parsing {}", input);
            }
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
        ];

        for (input, expected) in tests {
            if let Object::Error(e) = test_eval(input) {
                assert_eq!(e, expected);
            } else {
                panic!("Error parsing {}", input);
            }
        }
    }
}
