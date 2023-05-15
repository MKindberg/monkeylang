use crate::ast;
use crate::lexer::Lexer;
use crate::token::Token;

use std::collections::HashMap;

type PrefixParseFn = fn(&mut Parser) -> Option<Box<dyn ast::Expression>>;
type InfixParseFn = fn(&mut Parser, Box<dyn ast::Expression>) -> Option<Box<dyn ast::Expression>>;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

impl Precedence {
    fn get_precedence(token: &Token) -> Precedence {
        match token {
            Token::EQ | Token::NEQ => Precedence::EQUALS,
            Token::LT | Token::GT => Precedence::LESSGREATER,
            Token::PLUS | Token::MINUS => Precedence::SUM,
            Token::SLASH | Token::ASTERISK => Precedence::PRODUCT,
            Token::LPAREN => Precedence::CALL,
            _ => Precedence::LOWEST,
        }
    }
}

struct Parser {
    l: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<Token, PrefixParseFn>,
    infix_parse_fns: HashMap<Token, InfixParseFn>,
}

impl Parser {
    fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            l,
            current_token: Token::EOF,
            peek_token: Token::EOF,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        p.next_token();
        p.next_token();
        p.register_prefix(Token::IDENT(None), Parser::parse_identifier);
        p.register_prefix(Token::INT(None), Parser::parse_integer_literal);
        p.register_prefix(Token::BANG, Parser::parse_prefix_expression);
        p.register_prefix(Token::MINUS, Parser::parse_prefix_expression);
        p.register_prefix(Token::TRUE, Parser::parse_boolean);
        p.register_prefix(Token::FALSE, Parser::parse_boolean);
        p.register_prefix(Token::LPAREN, Parser::parse_grouped_expression);
        p.register_prefix(Token::IF, Parser::parse_if_expression);
        p.register_prefix(Token::FUNCTION, Parser::parse_function_literal);

        p.register_infix(Token::PLUS, Parser::parse_infix_expression);
        p.register_infix(Token::MINUS, Parser::parse_infix_expression);
        p.register_infix(Token::SLASH, Parser::parse_infix_expression);
        p.register_infix(Token::ASTERISK, Parser::parse_infix_expression);
        p.register_infix(Token::EQ, Parser::parse_infix_expression);
        p.register_infix(Token::NEQ, Parser::parse_infix_expression);
        p.register_infix(Token::LT, Parser::parse_infix_expression);
        p.register_infix(Token::GT, Parser::parse_infix_expression);
        p.register_infix(Token::LPAREN, Parser::parse_call_expression);

        return p;
    }

    fn parse_identifier(&mut self) -> Option<Box<dyn ast::Expression>> {
        return Some(Box::new(ast::Identifier {
            token: self.current_token.clone(),
            value: self.current_token.to_string(),
        }));
    }

    fn parse_integer_literal(&mut self) -> Option<Box<dyn ast::Expression>> {
        return Some(Box::new(ast::IntegerLiteral {
            token: self.current_token.clone(),
            value: match self.current_token.clone() {
                Token::INT(n) => n.unwrap(),
                _ => {
                    self.errors.push(format!(
                        "Could not parse {} as integer",
                        self.current_token.to_string()
                    ));
                    return None;
                }
            },
        }));
    }

    fn parse_boolean(&mut self) -> Option<Box<dyn ast::Expression>> {
        return Some(Box::new(ast::Boolean {
            token: self.current_token.clone(),
            value: match self.current_token.clone() {
                Token::TRUE => true,
                Token::FALSE => false,
                _ => {
                    self.errors.push(format!(
                        "Could not parse {} as boolean",
                        self.current_token.to_string()
                    ));
                    return None;
                }
            },
        }));
    }

    fn parse_prefix_expression(&mut self) -> Option<Box<dyn ast::Expression>> {
        let token = self.current_token.clone();
        self.next_token();
        let right = self.parse_expression(Precedence::PREFIX)?;
        return Some(Box::new(ast::PrefixExpression {
            token: token.clone(),
            operator: token.to_string(),
            right,
        }));
    }

    fn parse_infix_expression(
        &mut self,
        left: Box<dyn ast::Expression>,
    ) -> Option<Box<dyn ast::Expression>> {
        let token = self.current_token.clone();
        let precedence = self.current_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        return Some(Box::new(ast::InfixExpression {
            token: token.clone(),
            left,
            operator: token.to_string(),
            right,
        }));
    }

    fn parse_grouped_expression(&mut self) -> Option<Box<dyn ast::Expression>> {
        self.next_token();

        let exp = self.parse_expression(Precedence::LOWEST)?;
        if !self.expect_peek(Token::RPAREN) {
            return None;
        }
        return Some(exp);
    }

    fn parse_block_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        let mut stmts = vec![];
        let token = self.current_token.clone();
        self.next_token();
        while self.current_token != Token::RBRACE && self.current_token != Token::EOF {
            if let Some(stmt) = self.parse_statement() {
                stmts.push(stmt);
            }
            self.next_token();
        }
        return Some(Box::new(ast::BlockStatement {
            token,
            statements: stmts,
        }));
    }

    fn parse_if_expression(&mut self) -> Option<Box<dyn ast::Expression>> {
        let token = self.current_token.clone();
        if !self.expect_peek(Token::LPAREN) {
            return None;
        }
        self.next_token();
        let condition = self.parse_expression(Precedence::LOWEST)?;
        if !self.expect_peek(Token::RPAREN) {
            return None;
        }
        if !self.expect_peek(Token::LBRACE) {
            return None;
        }

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token == Token::ELSE {
            self.next_token();
            if !self.expect_peek(Token::LBRACE) {
                return None;
            }
            Some(self.parse_block_statement()?)
        } else {
            None
        };
        return Some(Box::new(ast::IfExpression {
            token,
            condition,
            consequence,
            alternative,
        }));
    }

    fn parse_function_params(&mut self) -> Option<Vec<ast::Identifier>> {
        let mut params = vec![];
        if self.peek_token == Token::RPAREN {
            self.next_token();
            return Some(params);
        }

        self.next_token();

        params.push(ast::Identifier {
            token: self.current_token.clone(),
            value: self.current_token.to_string(),
        });
        while self.peek_token == Token::COMMA {
            self.next_token();
            self.next_token();
            params.push(ast::Identifier {
                token: self.current_token.clone(),
                value: self.current_token.to_string(),
            });
        }
        if !self.expect_peek(Token::RPAREN) {
            return None;
        }
        return Some(params);
    }

    fn parse_function_literal(&mut self) -> Option<Box<dyn ast::Expression>> {
        let token = self.current_token.clone();
        if !self.expect_peek(Token::LPAREN) {
            return None;
        }

        let params = self.parse_function_params()?;

        if !self.expect_peek(Token::LBRACE) {
            return None;
        }
        let body = self.parse_block_statement()?;
        return Some(Box::new(ast::FunctionLiteral {
            token,
            parameters: params,
            body,
        }));
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Box<dyn ast::Expression>>> {
        let mut args = vec![];
        if self.peek_token == Token::RPAREN {
            self.next_token();
            return Some(args);
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::LOWEST)?);

        while self.peek_token == Token::COMMA {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::LOWEST)?);
        }

        if !self.expect_peek(Token::RPAREN) {
            return None;
        }
        return Some(args);
    }

    fn parse_call_expression(
        &mut self,
        function: Box<dyn ast::Expression>,
    ) -> Option<Box<dyn ast::Expression>> {
        let token = self.current_token.clone();
        let arguments = self.parse_call_arguments()?;
        return Some(Box::new(ast::CallExpression {
            token,
            function,
            arguments,
        }));
    }

    fn register_prefix(&mut self, token: Token, prefix_parse_fn: PrefixParseFn) {
        self.prefix_parse_fns.insert(token, prefix_parse_fn);
    }

    fn register_infix(&mut self, token: Token, infix_parse_fn: InfixParseFn) {
        self.infix_parse_fns.insert(token, infix_parse_fn);
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    fn peek_error(&mut self, expected: Token) {
        self.errors.push(format!(
            "Expected next token to be '{}', got '{}'",
            expected.to_string(),
            self.peek_token.to_string()
        ));
    }
    fn errors(&self) -> Vec<String> {
        return self.errors.clone();
    }

    fn expect_peek(&mut self, t: Token) -> bool {
        if self.peek_token == t {
            self.next_token();
            return true;
        } else {
            self.peek_error(t);
        }
        return false;
    }

    fn peek_precedence(&self) -> Precedence {
        return Precedence::get_precedence(&self.peek_token);
    }

    fn current_precedence(&self) -> Precedence {
        return Precedence::get_precedence(&self.current_token);
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        if self.expect_peek(Token::IDENT(None)) && let Token::IDENT(Some(name)) = self.current_token.clone() {
            if self.expect_peek(Token::ASSIGN) {
                self.next_token();
                let value = self.parse_expression(Precedence::LOWEST)?;

                if self.peek_token == Token::SEMICOLON {
                    self.next_token();
                }

                return Some(Box::new(ast::LetStatement {
                    token: Token::LET,
                    name: ast::Identifier {
                        token: Token::IDENT(Some(name.to_string())),
                        value: name.to_string(),
                    },
                    value,
                }));
            }
        }
        return None;
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        self.next_token();

        let value = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token == Token::SEMICOLON {
            self.next_token();
        }

        return Some(Box::new(ast::ReturnStatement {
            token: Token::RETURN,
            value,
        }));
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn ast::Expression>> {
        let prefix = self.prefix_parse_fns.get(&self.current_token);
        let mut left = if let Some(prefix) = prefix {
            prefix(self)
        } else {
            self.errors.push(format!(
                "No prefix parse function for {} found",
                self.current_token.to_string()
            ));
            return None;
        };

        while self.peek_token != Token::SEMICOLON && precedence < self.peek_precedence() {
            if self.infix_parse_fns.contains_key(&self.peek_token) {
                self.next_token();
                left = self.infix_parse_fns.get(&self.current_token).unwrap()(self, left.unwrap());
            } else {
                return left;
            }
        }
        return left;
    }

    fn parse_expression_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        let tok = self.current_token.clone();
        let expr = self.parse_expression(Precedence::LOWEST);

        if self.peek_token == Token::SEMICOLON {
            self.next_token();
        }

        if let Some(e) = expr {
            return Some(Box::new(ast::ExpressionStatement {
                token: tok,
                expression: e,
            }));
        }
        return None;
    }

    fn parse_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        match self.current_token {
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_program(&mut self) -> Option<ast::Program> {
        let mut statements: Vec<Box<dyn ast::Statement>> = vec![];
        while self.current_token != Token::EOF {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                statements.push(stmt);
            }
            self.next_token();
        }
        return Some(ast::Program { statements });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expression, ExpressionStatement, LetStatement, Node, Program};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::token::Token;

    fn check_parser_errors(p: Parser) {
        let errors = p.errors();
        for e in errors.iter() {
            println!("{}", e);
        }
        assert_eq!(errors.len(), 0);
    }

    fn read_program(input: &str) -> Program {
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert!(program.is_some());
        check_parser_errors(p);
        return program.unwrap();
    }

    #[test]
    fn test_let_statements() {
        let input = "
        let x = 5;
        let y = true;
        let foobar = y;
";
        let program = read_program(input);

        assert_eq!(&program.statements.len(), &3);

        let s = program
            .statements
            .get(0)
            .unwrap()
            .as_any()
            .downcast_ref::<LetStatement>()
            .unwrap();
        assert_eq!(s.token_literal(), Token::LET.to_string());
        assert_eq!(s.name.value, "x".to_string());
        assert_eq!(
            s.value
                .as_any()
                .downcast_ref::<ast::IntegerLiteral>()
                .unwrap()
                .value,
            5
        );

        let s = program
            .statements
            .get(1)
            .unwrap()
            .as_any()
            .downcast_ref::<LetStatement>()
            .unwrap();
        assert_eq!(s.token_literal(), Token::LET.to_string());
        assert_eq!(s.name.value, "y".to_string());
        assert_eq!(
            s.value
                .as_any()
                .downcast_ref::<ast::Boolean>()
                .unwrap()
                .value,
            true
        );

        let s = program
            .statements
            .get(2)
            .unwrap()
            .as_any()
            .downcast_ref::<LetStatement>()
            .unwrap();
        assert_eq!(s.token_literal(), Token::LET.to_string());
        assert_eq!(s.name.value, "foobar".to_string());
        assert_eq!(
            s.value
                .as_any()
                .downcast_ref::<ast::Identifier>()
                .unwrap()
                .value,
            "y"
        );
    }

    fn test_integer_literal(input: &Box<dyn Expression>, value: i64) {
        let i = input
            .as_any()
            .downcast_ref::<ast::IntegerLiteral>()
            .unwrap();
        assert_eq!(i.value, value);
    }
    fn test_boolean(input: &Box<dyn Expression>, value: bool) {
        let i = input.as_any().downcast_ref::<ast::Boolean>().unwrap();
        assert_eq!(i.value, value);
    }

    fn test_identifier(input: &Box<dyn Expression>, value: &str) {
        let i = input.as_any().downcast_ref::<ast::Identifier>().unwrap();
        assert_eq!(i.value, value);
        assert_eq!(i.token_literal(), value);
    }

    trait TestExpr {
        fn test(&self, input: &Box<dyn Expression>);
    }
    impl TestExpr for i64 {
        fn test(&self, input: &Box<dyn Expression>) {
            test_integer_literal(input, *self);
        }
    }
    impl TestExpr for &str {
        fn test(&self, input: &Box<dyn Expression>) {
            test_identifier(input, *self);
        }
    }
    impl TestExpr for bool {
        fn test(&self, input: &Box<dyn Expression>) {
            test_boolean(input, *self);
        }
    }
    fn test_literal_expression<T: TestExpr>(input: &Box<dyn Expression>, value: &T) {
        value.test(input);
    }
    fn test_infix_expression<T: TestExpr>(
        input: &Box<dyn Expression>,
        left: &T,
        operator: &str,
        right: &T,
    ) {
        let i = input
            .as_any()
            .downcast_ref::<ast::InfixExpression>()
            .unwrap();
        test_literal_expression(&i.left, left);
        assert_eq!(i.operator, operator.to_string());
        test_literal_expression(&i.right, right);
    }

    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return 10;
return 993322;";
        let program = read_program(input);

        let values = [5, 10, 993322];
        assert_eq!(&program.statements.len(), &values.len());

        for (s, v) in program.statements.iter().zip(values.iter()) {
            assert_eq!(s.token_literal(), "return".to_string());
            let r = s.as_any().downcast_ref::<ast::ReturnStatement>().unwrap();
            assert_eq!(r.token, Token::RETURN);
            test_literal_expression(&r.value, v);
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let program = read_program(input);

        assert_eq!(program.statements.len(), 1);
        let s = program
            .statements
            .get(0)
            .unwrap()
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .unwrap();
        test_literal_expression(&s.expression, &"foobar");
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let program = read_program(input);

        assert_eq!(program.statements.len(), 1);
        let s = program
            .statements
            .get(0)
            .unwrap()
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .unwrap();
        test_literal_expression(&s.expression, &5);
    }

    #[test]
    fn test_boolean_literal_expression() {
        let input = "true;";

        let program = read_program(input);

        assert_eq!(program.statements.len(), 1);
        let s = program
            .statements
            .get(0)
            .unwrap()
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .unwrap();
        test_literal_expression(&s.expression, &true);
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct IntPrefixTest {
            input: String,
            operator: String,
            value: i64,
        }
        struct BoolPrefixTest {
            input: String,
            operator: String,
            value: bool,
        }
        let int_tests = vec![
            IntPrefixTest {
                input: "!5;".to_string(),
                operator: "!".to_string(),
                value: 5,
            },
            IntPrefixTest {
                input: "-15;".to_string(),
                operator: "-".to_string(),
                value: 15,
            },
        ];

        let bool_tests = vec![
            BoolPrefixTest {
                input: "!true;".to_string(),
                operator: "!".to_string(),
                value: true,
            },
            BoolPrefixTest {
                input: "!false;".to_string(),
                operator: "!".to_string(),
                value: false,
            },
        ];
        for t in int_tests.iter() {
            let program = read_program(&t.input);

            assert_eq!(program.statements.len(), 1);
            let e = program
                .statements
                .get(0)
                .unwrap()
                .as_any()
                .downcast_ref::<ast::ExpressionStatement>()
                .unwrap()
                .expression
                .as_any()
                .downcast_ref::<ast::PrefixExpression>()
                .unwrap();
            assert_eq!(e.operator, t.operator.to_string());

            test_literal_expression(&e.right, &t.value);
        }

        for t in bool_tests.iter() {
            let program = read_program(&t.input);

            assert_eq!(program.statements.len(), 1);
            let e = program
                .statements
                .get(0)
                .unwrap()
                .as_any()
                .downcast_ref::<ast::ExpressionStatement>()
                .unwrap()
                .expression
                .as_any()
                .downcast_ref::<ast::PrefixExpression>()
                .unwrap();
            assert_eq!(e.operator, t.operator.to_string());

            test_literal_expression(&e.right, &t.value);
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        struct InfixTestInt {
            input: String,
            left: i64,
            operator: String,
            right: i64,
        }
        struct InfixTestBool {
            input: String,
            left: bool,
            operator: String,
            right: bool,
        }
        macro_rules! infix_expression_test_int {
            ($op: literal) => {
                InfixTestInt {
                    input: format!("5 {} 5", $op),
                    left: 5,
                    operator: $op.to_string(),
                    right: 5,
                }
            };
        }
        macro_rules! infix_expression_test_bool {
            ($left: literal, $op: literal, $right: literal) => {
                InfixTestBool {
                    input: format!("{} {} {}", $left.to_string(), $op, $right.to_string()),
                    left: $left,
                    operator: $op.to_string(),
                    right: $right,
                }
            };
        }
        let int_tests = vec![
            infix_expression_test_int!("+"),
            infix_expression_test_int!("-"),
            infix_expression_test_int!("*"),
            infix_expression_test_int!("/"),
            infix_expression_test_int!(">"),
            infix_expression_test_int!("<"),
            infix_expression_test_int!("=="),
            infix_expression_test_int!("!="),
        ];
        let bool_tests = vec![
            infix_expression_test_bool!(true, "==", true),
            infix_expression_test_bool!(true, "!=", false),
            infix_expression_test_bool!(false, "==", false),
        ];

        for t in int_tests.iter() {
            let program = read_program(&t.input);

            assert_eq!(program.statements.len(), 1);
            let s = program.statements.get(0).unwrap();
            let e = &s
                .as_any()
                .downcast_ref::<ast::ExpressionStatement>()
                .unwrap()
                .expression;

            test_infix_expression(&e, &t.left, t.operator.as_str(), &t.right);
        }

        for t in bool_tests.iter() {
            let program = read_program(&t.input);

            assert_eq!(program.statements.len(), 1);
            let s = program.statements.get(0).unwrap();
            let e = &s
                .as_any()
                .downcast_ref::<ast::ExpressionStatement>()
                .unwrap()
                .expression;

            test_infix_expression(&e, &t.left, t.operator.as_str(), &t.right);
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];
        for t in tests.iter() {
            let program = read_program(t.0);

            let str = program
                .statements
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join("");
            assert_eq!(str.to_string(), t.1.to_string());
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let program = read_program(input);

        assert_eq!(program.statements.len(), 1);
        let s = program.statements.get(0).unwrap();
        assert_eq!(s.token_literal(), "if");
        let if_exp = &s
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .unwrap()
            .expression
            .as_any()
            .downcast_ref::<ast::IfExpression>()
            .unwrap();
        test_infix_expression(&if_exp.condition, &"x", &"<", &"y");

        let if_branch = if_exp
            .consequence
            .as_any()
            .downcast_ref::<ast::BlockStatement>()
            .unwrap();
        assert_eq!(if_branch.statements.len(), 1);
        let e = if_branch
            .statements
            .get(0)
            .unwrap()
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .unwrap();
        test_literal_expression(&e.expression, &"x");

        assert!(if_exp.alternative.is_none());
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let program = read_program(input);

        assert_eq!(program.statements.len(), 1);
        let s = program.statements.get(0).unwrap();
        assert_eq!(s.token_literal(), "if");
        let if_exp = &s
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .unwrap()
            .expression
            .as_any()
            .downcast_ref::<ast::IfExpression>()
            .unwrap();
        test_infix_expression(&if_exp.condition, &"x", &"<", &"y");

        let if_branch = if_exp
            .consequence
            .as_any()
            .downcast_ref::<ast::BlockStatement>()
            .unwrap();
        assert_eq!(if_branch.statements.len(), 1);
        let e = if_branch
            .statements
            .get(0)
            .unwrap()
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .unwrap();
        test_literal_expression(&e.expression, &"x");

        assert!(if_exp.alternative.is_some());
        let if_branch = if_exp
            .consequence
            .as_any()
            .downcast_ref::<ast::BlockStatement>()
            .unwrap();
        assert_eq!(if_branch.statements.len(), 1);
        let e = if_branch
            .statements
            .get(0)
            .unwrap()
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .unwrap();
        test_literal_expression(&e.expression, &"x");
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x + y; }";

        let program = read_program(input);

        assert_eq!(program.statements.len(), 1);
        let f = program
            .statements
            .get(0)
            .unwrap()
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .unwrap()
            .expression
            .as_any()
            .downcast_ref::<ast::FunctionLiteral>()
            .unwrap();

        assert_eq!(f.parameters.len(), 2);
        assert_eq!(&f.parameters.get(0).unwrap().value, &"x");
        assert_eq!(&f.parameters.get(1).unwrap().value, &"y");

        let b = f
            .body
            .as_any()
            .downcast_ref::<ast::BlockStatement>()
            .unwrap();
        assert_eq!(b.statements.len(), 1);
        let e = b
            .statements
            .get(0)
            .unwrap()
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap();
        test_infix_expression(&e.expression, &"x", &"+", &"y");
    }

    #[test]
    fn test_function_parameter_parsing() {
        let tests = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        for t in tests.iter() {
            let program = read_program(t.0);
            assert_eq!(program.statements.len(), 1);
            let f = program
                .statements
                .get(0)
                .unwrap()
                .as_any()
                .downcast_ref::<ast::ExpressionStatement>()
                .unwrap()
                .expression
                .as_any()
                .downcast_ref::<ast::FunctionLiteral>()
                .unwrap();
            assert_eq!(f.parameters.len(), t.1.len());
            for (i, p) in f.parameters.iter().enumerate() {
                assert_eq!(p.value, t.1[i]);
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let program = read_program(input);
        assert_eq!(program.statements.len(), 1);

        let e = program
            .statements
            .get(0)
            .unwrap()
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap()
            .expression
            .as_any()
            .downcast_ref::<ast::CallExpression>()
            .unwrap();

        test_literal_expression(&e.function, &"add");
        assert_eq!(e.arguments.len(), 3);

        test_literal_expression(&e.arguments.get(0).unwrap(), &1);
        test_infix_expression(&e.arguments.get(1).unwrap(), &2, &"*", &3);
        test_infix_expression(&e.arguments.get(2).unwrap(), &4, &"+", &5);
    }
}
