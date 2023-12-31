use std::{hash::Hash, hash::Hasher};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Expression {
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Boolean(Boolean),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
    Identifier(Identifier),
    StringLiteral(StringLiteral),
    ArrayLiteral(ArrayLiteral),
    IndexExpression(IndexExpression),
    HashLiteral(HashLiteral),
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
            Expression::StringLiteral(s) => s.to_string(),
            Expression::ArrayLiteral(a) => a.to_string(),
            Expression::IndexExpression(i) => i.to_string(),
            Expression::HashLiteral(h) => h.to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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
}

macro_rules! impl_traits {
    ($name:ty, $to_string:item) => {
        impl ToString for $name {
            $to_string
        }
    };
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn to_string(&self) -> String {
        return self
            .statements
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join("\n");
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct LetStatement {
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Identifier {
    pub value: String,
}
impl_traits!(
    Identifier,
    fn to_string(&self) -> String {
        self.value.clone()
    }
);

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ReturnStatement {
    pub value: Box<Expression>,
}
impl_traits!(
    ReturnStatement,
    fn to_string(&self) -> String {
        format!("return {};", self.value.to_string())
    }
);

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ExpressionStatement {
    pub expression: Box<Expression>,
}
impl_traits!(
    ExpressionStatement,
    fn to_string(&self) -> String {
        self.expression.to_string()
    }
);

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct IntegerLiteral {
    pub value: i64,
}
impl_traits!(
    IntegerLiteral,
    fn to_string(&self) -> String {
        self.value.to_string()
    }
);

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct PrefixExpression {
    pub operator: String,
    pub right: Box<Expression>,
}
impl_traits!(
    PrefixExpression,
    fn to_string(&self) -> String {
        format!("({}{})", self.operator, self.right.to_string())
    }
);

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct InfixExpression {
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Boolean {
    pub value: bool,
}
impl_traits!(
    Boolean,
    fn to_string(&self) -> String {
        self.value.to_string()
    }
);

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct BlockStatement {
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

impl BlockStatement {
    pub fn len(&self) -> usize {
        self.statements.len()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct FunctionLiteral {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub name: String,
}
impl_traits!(
    FunctionLiteral,
    fn to_string(&self) -> String {
        format!(
            "fn {}({}) {}",
            self.name,
            self.parameters
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            self.body.to_string()
        )
    }
);

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct CallExpression {
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StringLiteral {
    pub value: String,
}
impl_traits!(
    StringLiteral,
    fn to_string(&self) -> String {
        self.value.clone()
    }
);

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
}
impl_traits!(
    ArrayLiteral,
    fn to_string(&self) -> String {
        format!(
            "[{}]",
            self.elements
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
);

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct IndexExpression {
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}
impl_traits!(
    IndexExpression,
    fn to_string(&self) -> String {
        format!("({}[{}])", self.left.to_string(), self.index.to_string())
    }
);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct HashLiteral {
    pub pairs: Vec<(Expression, Expression)>,
}
impl_traits!(
    HashLiteral,
    fn to_string(&self) -> String {
        format!(
            "{{{}}}",
            self.pairs
                .iter()
                .map(|(k, v)| format!("{}:{}", k.to_string(), v.to_string()))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
);
impl Hash for HashLiteral {
    fn hash<H: Hasher>(&self, _: &mut H) {
        panic!("Hash not implemented for HashLiteral");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::LetStatement(LetStatement {
                name: Identifier {
                    value: "myVar".to_string(),
                },
                value: Box::new(Expression::Identifier(Identifier {
                    value: "anotherVar".to_string(),
                })),
            })],
        };
        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }
}
