use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn to_string(&self) -> String;
}

pub enum Expression {
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Boolean(Boolean),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
    Identifier(Identifier),
}
impl Expression {
    pub fn to_string(&self) -> String {
        match self {
            Expression::IntegerLiteral(i) => i.to_string(),
            Expression::PrefixExpression(p) => p.to_string(),
            Expression::InfixExpression(i) => i.to_string(),
            Expression::Boolean(b) => b.to_string(),
            Expression::IfExpression(i) => i.to_string(),
            Expression::FunctionLiteral(f) => f.to_string(),
            Expression::CallExpression(c) => c.to_string(),
            Expression::Identifier(i) => i.to_string(),
        }
    }
}

pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
}

impl Statement {
    pub fn to_string(&self) -> String {
        match self {
            Statement::LetStatement(s) => s.to_string(),
            Statement::ReturnStatement(s) => s.to_string(),
            Statement::ExpressionStatement(s) => s.to_string(),
            Statement::BlockStatement(s) => s.to_string(),
        }
    }
    pub fn token_literal(&self) -> String {
        match self {
            Statement::LetStatement(s) => s.token_literal(),
            Statement::ReturnStatement(s) => s.token_literal(),
            Statement::ExpressionStatement(s) => s.token_literal(),
            Statement::BlockStatement(s) => s.token_literal(),
        }
    }
}

macro_rules! impl_traits {
    ($name:ty, $to_string:item) => {
        impl Node for $name {
            fn token_literal(&self) -> String {
                return self.token.to_string();
            }
            $to_string
        }
    };
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    fn token_literal(&self) -> String {
        return self
            .statements
            .first()
            .map(|s| s.token_literal())
            .unwrap_or("".to_string());
    }

    pub fn to_string(&self) -> String {
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
    pub value: Box<Expression>,
}
impl_traits!(
    LetStatement,
    fn to_string(&self) -> String {
        format!(
            "let {} = {};",
            self.name.to_string(),
            self.value.to_string()
        )
    }
);

pub struct Identifier {
    pub token: Token,
    pub value: String,
}
impl_traits!(
    Identifier,
    fn to_string(&self) -> String {
        self.value.clone()
    }
);

pub struct ReturnStatement {
    pub token: Token,
    pub value: Box<Expression>,
}
impl_traits!(
    ReturnStatement,
    fn to_string(&self) -> String {
        format!("return {};", self.value.to_string())
    }
);

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Box<Expression>,
}
impl_traits!(
    ExpressionStatement,
    fn to_string(&self) -> String {
        self.expression.to_string()
    }
);

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}
impl_traits!(
    IntegerLiteral,
    fn to_string(&self) -> String {
        self.value.to_string()
    }
);

pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}
impl_traits!(
    PrefixExpression,
    fn to_string(&self) -> String {
        format!("({}{})", self.operator, self.right.to_string())
    }
);

pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}
impl_traits!(
    InfixExpression,
    fn to_string(&self) -> String {
        format!(
            "({} {} {})",
            self.left.to_string(),
            self.operator,
            self.right.to_string()
        )
    }
);

pub struct Boolean {
    pub token: Token,
    pub value: bool,
}
impl_traits!(
    Boolean,
    fn to_string(&self) -> String {
        self.value.to_string()
    }
);

pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}
impl_traits!(
    BlockStatement,
    fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join("\n")
    }
);

pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: Statement,
    pub alternative: Option<Statement>,
}
impl_traits!(
    IfExpression,
    fn to_string(&self) -> String {
        format!(
            "if ({}) {} {}",
            self.condition.to_string(),
            self.consequence.to_string(),
            if let Some(alternative) = &self.alternative {
                format!("else {}", alternative.to_string())
            } else {
                "".to_string()
            }
        )
    }
);

pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: Statement,
}
impl_traits!(
    FunctionLiteral,
    fn to_string(&self) -> String {
        format!(
            "fn ({}) {}",
            self.parameters
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            self.body.to_string()
        )
    }
);

pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}
impl_traits!(
    CallExpression,
    fn to_string(&self) -> String {
        format!(
            "{}({})",
            self.function.to_string(),
            self.arguments
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::LetStatement(LetStatement {
                token: Token::LET,
                name: Identifier {
                    token: Token::IDENT(Some("myVar".to_string())),
                    value: "myVar".to_string(),
                },
                value: Box::new(Expression::Identifier(Identifier {
                    token: Token::IDENT(Some("anotherVar".to_string())),
                    value: "anotherVar".to_string(),
                })),
            })],
        };
        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }
}
