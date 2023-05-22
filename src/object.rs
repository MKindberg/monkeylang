use crate::ast::{BlockStatement, Identifier};
use std::fmt::{Debug, Display};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Function(Function),
    Builtin(BuiltinFunction),
    String(String),
    Error(String),
    Null,
}

impl Object {
    fn to_string(&self) -> String {
        match self {
            Object::Integer(i) => i.to_string(),
            Object::Boolean(b) => b.to_string(),
            Object::ReturnValue(v) => v.to_string(),
            Object::Function(f) => f.to_string(),
            Object::Builtin(f) => f.to_string(),
            Object::String(s) => s.to_string(),
            Object::Error(s) => s.to_string(),
            Object::Null => "null".to_string(),
        }
    }

    pub fn type_string(&self) -> &'static str {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::ReturnValue(_) => "RETURN_VALUE",
            Object::Function(_) => "FUNCTION",
            Object::Builtin(_) => "BUILTIN",
            Object::String(_) => "STRING",
            Object::Error(_) => "ERROR",
            Object::Null => "NULL",
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: std::collections::HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: std::collections::HashMap::new(),
            outer: None,
        }
    }

    pub fn new_with_outer(outer: Environment) -> Environment {
        Environment {
            store: std::collections::HashMap::new(),
            outer: Some(Box::new(outer)),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        let res = self.store.get(name);
        if res.is_none() && self.outer.is_some() {
            return self.outer.as_ref().unwrap().get(name);
        }
        return res;
    }

    pub fn set(&mut self, name: String, value: Object) -> Object {
        self.store.insert(name, value.clone());
        return value;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl Function {
    pub fn new(parameters: Vec<Identifier>, body: BlockStatement, env: Environment) -> Function {
        Function {
            parameters,
            body,
            env,
        }
    }

    pub fn to_string(&self) -> String {
        format!(
            "fn ({}) {{\n{}\n}}",
            self.parameters
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            self.body.to_string(),
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum BuiltinFunction {
    Len,
}
impl BuiltinFunction {
    pub fn execute(&self, args: &[Object]) -> Object {
        match self {
            BuiltinFunction::Len => {
                if args.len() != 1 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}, want=1",
                        &args.len()
                    ));
                } else {
                    match &args[0] {
                        Object::String(s) => Object::Integer(s.len() as i64),
                        _ => Object::Error(format!(
                            "argument to `len` not supported, got {}",
                            args[0].type_string()
                        )),
                    }
                }
            }
        }
    }
    pub fn lookup(name: &str) -> Option<BuiltinFunction> {
        match name {
            "len" => Some(BuiltinFunction::Len),
            _ => None,
        }
    }
    fn to_string(&self) -> String {
        match self {
            BuiltinFunction::Len => "builtin_len".to_string(),
        }
    }
}
