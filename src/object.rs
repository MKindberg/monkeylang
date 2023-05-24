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
    Array(Vec<Object>),
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
            Object::Array(a) => format!(
                "[{}]",
                a.iter()
                    .map(|o| o.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
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
            Object::Array(_) => "ARRAY",
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
    First,
    Last,
    Rest,
    Push,
}
impl BuiltinFunction {
    pub fn execute(&self, args: &[Object]) -> Object {
        match self {
            BuiltinFunction::Len => Self::builtin_len(args),
            BuiltinFunction::First => Self::builtin_first(args),
            BuiltinFunction::Last => Self::builtin_last(args),
            BuiltinFunction::Rest => Self::builtin_rest(args),
            BuiltinFunction::Push => Self::builtin_push(args),
        }
    }

    pub fn lookup(name: &str) -> Option<BuiltinFunction> {
        match name {
            "len" => Some(BuiltinFunction::Len),
            "first" => Some(BuiltinFunction::First),
            "last" => Some(BuiltinFunction::Last),
            "rest" => Some(BuiltinFunction::Rest),
            "push" => Some(BuiltinFunction::Push),
            _ => None,
        }
    }
    fn to_string(&self) -> String {
        match self {
            BuiltinFunction::Len => "builtin_len".to_string(),
            BuiltinFunction::First => "builtin_first".to_string(),
            BuiltinFunction::Last => "builtin_last".to_string(),
            BuiltinFunction::Rest => "builtin_rest".to_string(),
            BuiltinFunction::Push => "builtin_push".to_string(),
        }
    }

    fn builtin_len(args: &[Object]) -> Object {
        if args.len() != 1 {
            return Object::Error(format!(
                "wrong number of arguments. got={}, want=1",
                &args.len()
            ));
        } else {
            match &args[0] {
                Object::String(s) => Object::Integer(s.len() as i64),
                Object::Array(a) => Object::Integer(a.len() as i64),
                _ => Object::Error(format!(
                    "argument to `len` not supported, got {}",
                    args[0].type_string()
                )),
            }
        }
    }

    fn builtin_first(args: &[Object]) -> Object {
        if args.len() != 1 {
            return Object::Error(format!(
                "wrong number of arguments. got={}, want=1",
                &args.len()
            ));
        }
        match &args[0] {
            Object::Array(a) => {
                if a.len() == 0 {
                    return Object::Null;
                }
                return a[0].clone();
            }
            _ => Object::Error(format!(
                "argument to `first` must be ARRAY, got {}",
                args[0].type_string()
            )),
        }
    }

    fn builtin_last(args: &[Object]) -> Object {
        if args.len() != 1 {
            return Object::Error(format!(
                "wrong number of arguments. got={}, want=1",
                &args.len()
            ));
        }
        match &args[0] {
            Object::Array(a) => {
                if a.len() == 0 {
                    return Object::Null;
                }
                return a[a.len() - 1].clone();
            }
            _ => Object::Error(format!(
                "argument to `last` must be ARRAY, got {}",
                args[0].type_string()
            )),
        }
    }

    fn builtin_rest(args: &[Object]) -> Object {
        if args.len() != 1 {
            return Object::Error(format!(
                "wrong number of arguments. got={}, want=1",
                &args.len()
            ));
        }
        match &args[0] {
            Object::Array(a) => {
                if a.len() == 0 {
                    return Object::Null;
                }
                return Object::Array(a[1..].to_vec());
            }
            _ => Object::Error(format!(
                "argument to `rest` must be ARRAY, got {}",
                args[0].type_string()
            )),
        }
    }

    fn builtin_push(args: &[Object]) -> Object {
        if args.len() != 2 {
            return Object::Error(format!(
                "wrong number of arguments. got={}, want=2",
                &args.len()
            ));
        }
        match &args[0] {
            Object::Array(a) => {
                let mut a = a.clone();
                a.push(args[1].clone());
                return Object::Array(a);
            }
            _ => Object::Error(format!(
                "argument to `push` must be ARRAY, got {}",
                args[0].type_string()
            )),
        }
    }
}
