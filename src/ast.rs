use crate::token::Token;
use std::any::Any;

pub trait Node {
    fn token_literal(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

pub trait Statement: Node {
    // fn statement_node(&self);
}

pub trait Expression: Node {
    // fn expression_node(&self);
}

macro_rules! impl_traits {
    ($name:ty, $type:ty) => {
        impl Node for $name {
            fn token_literal(&self) -> String {
                return self.token.to_string();
            }
            fn as_any(&self) -> &dyn Any {
                return self;
            }
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
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}
impl_traits!(LetStatement, Statement);

pub struct Identifier {
    pub token: Token,
    pub value: String,
}
impl_traits!(Identifier, Expression);

pub struct ReturnStatement {
    pub token: Token,
    pub value: Box<dyn Expression>,
}
impl_traits!(ReturnStatement, Statement);
