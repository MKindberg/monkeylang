use crate::ast;
use crate::ast::Expression;
use crate::ast::Statement;
use crate::object::Object;

const TRUE: bool = true;
const FALSE: bool = false;

pub fn eval(program: &ast::Program) -> Option<Object> {
    eval_program(&program.statements)
}

fn eval_statement(statement: &ast::Statement) -> Option<Object> {
    match statement {
        Statement::ExpressionStatement(e) => eval_expression(&*e.expression),
        Statement::BlockStatement(b) => eval_block_statement(&*b.statements),
        Statement::ReturnStatement(r) => eval_return_statement(&*r.value),
        _ => None,
    }
}

fn eval_program(statements: &[Statement]) -> Option<Object> {
    let mut result = None;
    for statement in statements {
        result = eval_statement(statement);
        if let Some(Object::ReturnValue(r)) = result {
            return Some(*r);
        }
    }
    result
}

fn eval_block_statement(statements: &[Statement]) -> Option<Object> {
    let mut result = None;
    for statement in statements {
        result = eval_statement(statement);
        if let Some(Object::ReturnValue(_)) = result {
            return result;
        }
    }
    result
}

fn eval_return_statement(return_value: &Expression) -> Option<Object> {
    let result = eval_expression(return_value)?;
    Some(Object::ReturnValue(Box::new(result)))
}

fn eval_expression(expression: &ast::Expression) -> Option<Object> {
    match expression {
        Expression::IntegerLiteral(i) => Some(Object::Integer(i.value)),
        Expression::Boolean(b) => Some(Object::Boolean(b.value)),
        Expression::PrefixExpression(p) => eval_prefix_expression(&p.operator, &p.right),
        Expression::InfixExpression(i) => eval_infix_expression(&i.left, &i.operator, &i.right),
        Expression::IfExpression(i) => {
            eval_if_expression(&i.condition, &i.consequence, &i.alternative)
        }
        _ => None,
    }
}

fn eval_prefix_expression(operator: &str, right: &Expression) -> Option<Object> {
    let r = eval_expression(right)?;
    match operator {
        "!" => eval_bang_operator_expression(&r),
        "-" => eval_minus_prefix_operator_expression(&r),
        _ => None,
    }
}

fn eval_bang_operator_expression(right: &Object) -> Option<Object> {
    match right {
        Object::Boolean(b) => Some(Object::Boolean(!b)),
        Object::Null => Some(Object::Boolean(true)),
        _ => Some(Object::Boolean(false)),
    }
}

fn eval_minus_prefix_operator_expression(right: &Object) -> Option<Object> {
    match right {
        Object::Integer(i) => Some(Object::Integer(-i)),
        _ => None,
    }
}

fn eval_infix_expression(left: &Expression, operator: &str, right: &Expression) -> Option<Object> {
    let l = eval_expression(left)?;
    let r = eval_expression(right)?;
    match (&l, &r, operator) {
        (Object::Integer(l), Object::Integer(r), _) => {
            eval_integer_infix_expression(&l, &r, operator)
        }
        (_, _, "==") => Some(Object::Boolean(l == r)),
        (_, _, "!=") => Some(Object::Boolean(l != r)),
        _ => None,
    }
}

fn eval_integer_infix_expression(left: &i64, right: &i64, operator: &str) -> Option<Object> {
    match operator {
        "+" => Some(Object::Integer(left + right)),
        "-" => Some(Object::Integer(left - right)),
        "*" => Some(Object::Integer(left * right)),
        "/" => Some(Object::Integer(left / right)),
        "<" => Some(Object::Boolean(left < right)),
        ">" => Some(Object::Boolean(left > right)),
        "==" => Some(Object::Boolean(left == right)),
        "!=" => Some(Object::Boolean(left != right)),
        _ => None,
    }
}

fn eval_if_expression(
    condition: &Expression,
    consequence: &Statement,
    alternative: &Option<Statement>,
) -> Option<Object> {
    let cond = eval_expression(condition)?;

    if is_truthy(cond) {
        eval_statement(consequence)
    } else if let Some(block) = alternative {
        eval_statement(block)
    } else {
        Some(Object::Null)
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

    fn test_eval(input: &str) -> Option<Object> {
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
            if let Some(e) = test_eval(input) {
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
            if let Some(e) = test_eval(input) {
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
            if let Some(e) = test_eval(input) {
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
            if let Some(e) = test_eval(input) {
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
            if let Some(e) = test_eval(input) {
                assert_eq!(e, expected);
            }
        }
    }
}
