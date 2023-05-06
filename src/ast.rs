use crate::token::Token;
use std::any::Any;

pub trait Node {
    fn token_literal(&self) -> String;
    fn as_any(&self) -> &dyn Any;
    fn to_string(&self) -> String;
}

pub trait Statement: Node {
    // fn statement_node(&self);
}

pub trait Expression: Node {
    // fn expression_node(&self);
}

macro_rules! impl_traits {
    ($name:ty, $type:ty, $to_string:item) => {
        impl Node for $name {
            fn token_literal(&self) -> String {
                return self.token.to_string();
            }
            fn as_any(&self) -> &dyn Any {
                return self;
            }
            $to_string
        }
        impl $type for $name {}
    };
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Program {
    fn token_literal(&self) -> String {
        return self
            .statements
            .first()
            .map(|s| s.token_literal())
            .unwrap_or("".to_string());
    }

    fn to_string(&self) -> String {
        return self
            .statements
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join("\n");
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}
impl_traits!(
    LetStatement,
    Statement,
    fn to_string(&self) -> String {
        format!("let {} = {};", self.name.to_string(), self.value.to_string())
    }
);

pub struct Identifier {
    pub token: Token,
    pub value: String,
}
impl_traits!(
    Identifier,
    Expression,
    fn to_string(&self) -> String {
        self.value.clone()
    }
);

pub struct ReturnStatement {
    pub token: Token,
    pub value: Box<dyn Expression>,
}
impl_traits!(
    ReturnStatement,
    Statement,
    fn to_string(&self) -> String {
        format!("return {};", self.value.to_string())
    }
);

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Box<dyn Expression>,
}
impl_traits!(
    ExpressionStatement,
    Statement,
    fn to_string(&self) -> String {
        self.expression.to_string()
    }
);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![
                Box::new(LetStatement {
                    token: Token::LET,
                    name: Identifier {
                        token: Token::IDENT("myVar".to_string()),
                        value: "myVar".to_string(),
                    },
                    value: Box::new(Identifier {
                        token: Token::IDENT("anotherVar".to_string()),
                        value: "anotherVar".to_string(),
                    }),
                })
            ]
        };
        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }
}
