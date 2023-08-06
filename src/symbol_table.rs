use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolScope {
    Global,
    Local,
    Builtin,
    Free,
    Function,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: usize,
}

impl Symbol {
    pub fn new(name: &str, scope: SymbolScope, index: usize) -> Symbol {
        Symbol {
            name: name.to_string(),
            scope,
            index,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    pub outer: Option<Box<SymbolTable>>,
    store: HashMap<String, Symbol>,
    pub num_definitions: usize,
    pub free_symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
            free_symbols: vec![],
        }
    }

    pub fn new_enclosed(outer: SymbolTable) -> SymbolTable {
        let mut s = SymbolTable::new();
        s.outer = Some(Box::new(outer));
        s
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = if self.outer.is_none() {
            Symbol::new(name, SymbolScope::Global, self.num_definitions)
        } else {
            Symbol::new(name, SymbolScope::Local, self.num_definitions)
        };
        self.store.insert(name.to_string(), symbol.clone());
        self.num_definitions += 1;
        return symbol;
    }

    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        if self.store.contains_key(name) {
            return self.store.get(name).cloned();
        } else if self.outer.is_some() {
            let sym = &self.outer.as_mut().unwrap().resolve(name);
            if sym.is_none() {
                return sym.clone();
            }
            let sym = sym.clone().unwrap();
            if sym.scope == SymbolScope::Global || sym.scope == SymbolScope::Builtin {
                return Some(sym);
            }
            return Some(self.define_free(sym));
        } else {
            return None;
        }
    }

    pub fn define_builtin(&mut self, index: usize, name: &str) -> Symbol {
        let symbol = Symbol::new(name, SymbolScope::Builtin, index);
        self.store.insert(name.to_string(), symbol.clone());
        return symbol;
    }

    pub fn define_free(&mut self, original: Symbol) -> Symbol {
        let symbol = Symbol::new(&original.name, SymbolScope::Free, self.free_symbols.len());
        self.store.insert(original.name.clone(), symbol.clone());
        self.free_symbols.push(original);

        return symbol;
    }

    pub fn define_function_name(&mut self, name: &str) -> Symbol {
        let symbol = Symbol::new (name, SymbolScope::Function, 0);
        self.store.insert(name.to_string(), symbol.clone());
        return symbol;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define() {
        let expected = HashMap::from([
            ("a".to_string(), Symbol::new("a", SymbolScope::Global, 0)),
            ("b".to_string(), Symbol::new("b", SymbolScope::Global, 1)),
            ("c".to_string(), Symbol::new("c", SymbolScope::Local, 0)),
            ("d".to_string(), Symbol::new("d", SymbolScope::Local, 1)),
            ("e".to_string(), Symbol::new("e", SymbolScope::Local, 0)),
            ("f".to_string(), Symbol::new("f", SymbolScope::Local, 1)),
        ]);

        let mut global = SymbolTable::new();
        let a = global.define("a");
        assert_eq!(a, expected["a"]);
        let b = global.define("b");
        assert_eq!(b, expected["b"]);

        let mut first_local = SymbolTable::new_enclosed(global);
        let c = first_local.define("c");
        assert_eq!(c, expected["c"]);
        let d = first_local.define("d");
        assert_eq!(d, expected["d"]);

        let mut second_local = SymbolTable::new_enclosed(first_local);
        let e = second_local.define("e");
        assert_eq!(e, expected["e"]);
        let f = second_local.define("f");
        assert_eq!(f, expected["f"]);
    }

    #[test]
    fn test_resolve() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let expected = [
            Symbol {
                name: "a".to_string(),
                scope: SymbolScope::Global,
                index: 0,
            },
            Symbol {
                name: "b".to_string(),
                scope: SymbolScope::Global,
                index: 1,
            },
        ];

        for symbol in expected {
            assert_eq!(global.resolve(&symbol.name), Some(symbol));
        }
    }

    #[test]
    fn test_resolve_local() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let mut local = SymbolTable::new_enclosed(global);
        local.define("c");
        local.define("d");

        let expected = vec![
            Symbol::new("a", SymbolScope::Global, 0),
            Symbol::new("b", SymbolScope::Global, 1),
            Symbol::new("c", SymbolScope::Local, 0),
            Symbol::new("d", SymbolScope::Local, 1),
        ];

        for symbol in expected {
            assert_eq!(local.resolve(&symbol.name), Some(symbol));
        }
    }

    #[test]
    fn test_resolve_nested_local() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let mut first_local = SymbolTable::new_enclosed(global);
        first_local.define("c");
        first_local.define("d");

        let mut second_local = SymbolTable::new_enclosed(first_local.clone());
        second_local.define("e");
        second_local.define("f");

        let mut tests = vec![
            (
                &first_local,
                vec![
                    Symbol::new("a", SymbolScope::Global, 0),
                    Symbol::new("b", SymbolScope::Global, 1),
                    Symbol::new("c", SymbolScope::Local, 0),
                    Symbol::new("d", SymbolScope::Local, 1),
                ],
            ),
            (
                &second_local,
                vec![
                    Symbol::new("a", SymbolScope::Global, 0),
                    Symbol::new("b", SymbolScope::Global, 1),
                    Symbol::new("e", SymbolScope::Local, 0),
                    Symbol::new("f", SymbolScope::Local, 1),
                ],
            ),
        ];
        for test in tests.iter_mut() {
            let mut table = test.0.clone();
            let expected = &test.1;
            for symbol in expected {
                assert_eq!(table.resolve(&symbol.name).unwrap(), symbol.clone());
            }
        }
    }

    #[test]
    fn test_define_resolve_builtins() {
        let mut globals = SymbolTable::new();

        let expected = vec![
            Symbol::new("a", SymbolScope::Builtin, 0),
            Symbol::new("b", SymbolScope::Builtin, 1),
            Symbol::new("c", SymbolScope::Builtin, 2),
            Symbol::new("d", SymbolScope::Builtin, 3),
        ];

        for (i, f) in expected.iter().enumerate() {
            globals.define_builtin(i, &f.name);
        }

        for e in &expected {
            assert_eq!(&globals.resolve(&e.name).unwrap(), e);
        }

        let mut first_local = SymbolTable::new_enclosed(globals);
        for e in &expected {
            assert_eq!(&first_local.resolve(&e.name).unwrap(), e);
        }

        let mut second_local = SymbolTable::new_enclosed(first_local);
        for e in &expected {
            assert_eq!(&second_local.resolve(&e.name).unwrap(), e);
        }
    }

    #[test]
    fn test_resolve_free() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let mut first_local = SymbolTable::new_enclosed(global.clone());
        first_local.define("c");
        first_local.define("d");

        let mut second_local = SymbolTable::new_enclosed(first_local.clone());
        second_local.define("e");
        second_local.define("f");

        let tests = vec![
            (
                first_local,
                vec![
                    Symbol::new("a", SymbolScope::Global, 0),
                    Symbol::new("b", SymbolScope::Global, 1),
                    Symbol::new("c", SymbolScope::Local, 0),
                    Symbol::new("d", SymbolScope::Local, 1),
                ],
                vec![],
            ),
            (
                second_local,
                vec![
                    Symbol::new("a", SymbolScope::Global, 0),
                    Symbol::new("b", SymbolScope::Global, 1),
                    Symbol::new("c", SymbolScope::Free, 0),
                    Symbol::new("d", SymbolScope::Free, 1),
                    Symbol::new("e", SymbolScope::Local, 0),
                    Symbol::new("f", SymbolScope::Local, 1),
                ],
                vec![
                    Symbol::new("c", SymbolScope::Local, 0),
                    Symbol::new("d", SymbolScope::Local, 1),
                ],
            ),
        ];

        for mut t in tests {
            let table = &mut t.0;
            let expected_symbols = &t.1;
            let expected_free = &t.2;

            for sym in expected_symbols {
                assert_eq!(&table.resolve(&sym.name).unwrap(), sym);
            }

            assert_eq!(table.free_symbols.len(), expected_free.len());

            for (i, sym) in expected_free.iter().enumerate() {
                assert_eq!(&table.free_symbols[i], sym);
            }
        }
    }

    #[test]
    fn test_resolve_unresolvable_free() {
        let mut global = SymbolTable::new();
        global.define("a");

        let mut first_local = SymbolTable::new_enclosed(global);
        first_local.define("c");

        let mut second_local = SymbolTable::new_enclosed(first_local);
        second_local.define("e");
        second_local.define("f");

        let expected = vec![
            Symbol::new("a", SymbolScope::Global, 0),
            Symbol::new("c", SymbolScope::Free, 0),
            Symbol::new("e", SymbolScope::Local, 0),
            Symbol::new("f", SymbolScope::Local, 1),
        ];

        for sym in expected {
            assert_eq!(&second_local.resolve(&sym.name).unwrap(), &sym);
        }

        let expected_unresolvable = vec!["b", "d"];
        for name in expected_unresolvable {
            assert_eq!(second_local.resolve(&name), None);
        }
    }

    #[test]
    fn test_define_and_resolve_functin_name() {
        let mut global = SymbolTable::new();
        global.define_function_name("a");

        let expected = Symbol {
            name: "a".to_string(),
            scope: SymbolScope::Function,
            index: 0,
        };

        assert_eq!(global.resolve(&expected.name), Some(expected));
    }

    #[test]
    fn test_shadowing_function_name() {
        let mut global = SymbolTable::new();
        global.define_function_name("a");
        global.define("a");

        let expected = Symbol {name: "a".to_string(), scope: SymbolScope::Global, index: 0};
        assert_eq!(global.resolve(&expected.name), Some(expected));
    }
}
