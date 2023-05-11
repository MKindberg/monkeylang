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

        p.register_infix(Token::PLUS, Parser::parse_infix_expression);
        p.register_infix(Token::MINUS, Parser::parse_infix_expression);
        p.register_infix(Token::SLASH, Parser::parse_infix_expression);
        p.register_infix(Token::ASTERISK, Parser::parse_infix_expression);
        p.register_infix(Token::EQ, Parser::parse_infix_expression);
        p.register_infix(Token::NEQ, Parser::parse_infix_expression);
        p.register_infix(Token::LT, Parser::parse_infix_expression);
        p.register_infix(Token::GT, Parser::parse_infix_expression);

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
                while self.peek_token != Token::SEMICOLON {
                    self.next_token();
                }
                self.next_token();
                return Some(Box::new(ast::LetStatement {
                    token: Token::LET,
                    name: ast::Identifier {
                        token: Token::IDENT(Some(name.to_string())),
                        value: name.to_string(),
                    },
                    value: Box::new(ast::Identifier {
                        token: Token::IDENT(None),
                        value: "".to_string(),
                    }),
                }));
            }
        }
        return None;
    }

    fn parse_return_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        self.next_token();

        while self.peek_token != Token::SEMICOLON {
            self.next_token();
        }
        self.next_token();

        return Some(Box::new(ast::ReturnStatement {
            token: Token::RETURN,
            value: Box::new(ast::Identifier {
                token: Token::IDENT(None),
                value: "".to_string(),
            }),
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
        let expr = self.parse_expression(Precedence::LOWEST);
        let tok = self.current_token.clone();

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
    use crate::ast::{Expression, LetStatement, Node};
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

    #[test]
    fn test_let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
";
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);
        assert!(program.is_some());
        let identifiers = ["x", "y", "foobar"];
        assert_eq!(
            &program.as_ref().unwrap().statements.len(),
            &identifiers.len()
        );

        for (s, i) in program.unwrap().statements.iter().zip(identifiers.iter()) {
            assert_eq!(s.token_literal(), Token::LET.to_string());
            let l = s.as_any().downcast_ref::<LetStatement>().unwrap();
            assert_eq!(l.name.value, i.to_string());
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return 10;
return 993322;";
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);
        assert!(program.is_some());
        assert_eq!(&program.as_ref().unwrap().statements.len(), &3);

        for s in program.unwrap().statements.iter() {
            assert_eq!(s.token_literal(), "return".to_string());
            let r = s.as_any().downcast_ref::<ast::ReturnStatement>().unwrap();
            assert_eq!(r.token, Token::RETURN);
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert!(program.is_some());
        check_parser_errors(p);

        assert_eq!(program.as_ref().unwrap().statements.len(), 1);
        let s = program.as_ref().unwrap().statements.get(0).unwrap();
        assert_eq!(s.token_literal(), "foobar".to_string());
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert!(program.is_some());
        check_parser_errors(p);

        assert_eq!(program.as_ref().unwrap().statements.len(), 1);
        let s = program.as_ref().unwrap().statements.get(0).unwrap();
        assert_eq!(s.token_literal(), "5".to_string());
    }

    fn test_integer_literal(input: &Box<dyn Expression>, value: i64) {
        let i = input
            .as_any()
            .downcast_ref::<ast::IntegerLiteral>()
            .unwrap();
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
    fn test_parsing_prefix_expressions() {
        struct PrefixTest {
            input: String,
            operator: String,
            value: i64,
        }
        let tests = vec![
            PrefixTest {
                input: "!5;".to_string(),
                operator: "!".to_string(),
                value: 5,
            },
            PrefixTest {
                input: "-15;".to_string(),
                operator: "-".to_string(),
                value: 15,
            },
        ];

        for t in tests.iter() {
            let l = Lexer::new(t.input.clone());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            assert!(program.is_some());
            check_parser_errors(p);

            assert_eq!(program.as_ref().unwrap().statements.len(), 1);
            let s = program.as_ref().unwrap().statements.get(0).unwrap();
            assert_eq!(s.token_literal(), t.value.to_string());
            let e = s
                .as_any()
                .downcast_ref::<ast::ExpressionStatement>()
                .unwrap()
                .expression
                .as_any()
                .downcast_ref::<ast::PrefixExpression>()
                .unwrap();
            assert_eq!(e.operator, t.operator.to_string());

            test_integer_literal(&e.right, t.value);
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        struct InfixTest {
            input: String,
            left: i64,
            operator: String,
            right: i64,
        }
        macro_rules! infix_expression_test {
            ($op: literal) => {
                InfixTest {
                    input: format!("5 {} 5", $op),
                    left: 5,
                    operator: $op.to_string(),
                    right: 5,
                }
            };
        }
        let tests = vec![
            infix_expression_test!("+"),
            infix_expression_test!("-"),
            infix_expression_test!("*"),
            infix_expression_test!("/"),
            infix_expression_test!(">"),
            infix_expression_test!("<"),
            infix_expression_test!("=="),
            infix_expression_test!("!="),
        ];

        for t in tests.iter() {
            let l = Lexer::new(t.input.clone());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            assert!(program.is_some());
            check_parser_errors(p);

            assert_eq!(program.as_ref().unwrap().statements.len(), 1);
            let s = program.as_ref().unwrap().statements.get(0).unwrap();
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
        ];
        for t in tests.iter() {
            let l = Lexer::new(t.0.to_string());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            assert!(program.is_some());
            check_parser_errors(p);

            let str = program
                .as_ref()
                .unwrap()
                .statements
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join("");
            assert_eq!(str.to_string(), t.1.to_string());
        }
    }
}
