use crate::ast;
use crate::lexer::Lexer;
use crate::token::Token;

use std::any::Any;
use std::collections::HashMap;

type PrefixParseFn = fn(&mut Parser) -> Option<Box<dyn ast::Expression>>;
type InfixParseFn = fn(Box<dyn ast::Expression>) -> Option<Box<dyn ast::Expression>>;

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
        p.register_prefix(Token::IDENT("".to_string()), Parser::parse_identifier);
        return p;
    }

    fn parse_identifier(&mut self) -> Option<Box<dyn ast::Expression>> {
        return Some(Box::new(ast::Identifier {
            token: self.current_token.clone(),
            value: self.current_token.to_string(),
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
        } else if t == Token::IDENT("".to_string()) && let Token::IDENT(_) = self.peek_token {
            return true;
        } else if t == Token::INT(0) && let Token::INT(_) = self.peek_token {
            return true;
        } else {
            self.peek_error(t);
        }
        return false;
    }

    fn parse_let_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        if self.expect_peek(Token::IDENT("".to_string())) && let Token::IDENT(name) = self.peek_token.clone() {
            self.next_token();
            if self.expect_peek(Token::ASSIGN) {
                self.next_token();
                while self.peek_token != Token::SEMICOLON {
                    self.next_token();
                }
                return Some(Box::new(ast::LetStatement {
                    token: Token::LET,
                    name: ast::Identifier {
                        token: Token::IDENT(name.to_string()),
                        value: name.to_string(),
                    },
                    value: Box::new(ast::Identifier {
                        token: Token::IDENT("".to_string()),
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

        return Some(Box::new(ast::ReturnStatement {
            token: Token::RETURN,
            value: Box::new(ast::Identifier {
                token: Token::IDENT("".to_string()),
                value: "".to_string(),
            }),
        }));
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn ast::Expression>> {
        let prefix = self.prefix_parse_fns.get(&Token::normalize_tok(&self.current_token));
        if let Some(prefix) = prefix {
            return prefix(self);
        }
        return None;
    }

    fn parse_expression_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        let expr = self.parse_expression(Precedence::LOWEST);
        let tok = self.current_token.clone();

        if self.peek_token == Token::SEMICOLON {
            self.next_token();
        }

        if let Some(expr) = expr {
            return Some(Box::new(ast::ExpressionStatement {
                token: tok,
                expression: expr,
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
    use std::ops::Deref;

    use super::*;
    use crate::ast::LetStatement;
    use crate::ast::Program;
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
        assert_eq!(s.token_literal(), "Identifier:foobar".to_string());
    }
}
