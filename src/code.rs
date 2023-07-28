use std::ops::{Deref, DerefMut};

#[derive(PartialEq, Clone, Eq)]
pub struct Instructions(pub Vec<u8>);

impl Deref for Instructions {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Instructions {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl FromIterator<u8> for Instructions {
    fn from_iter<T: IntoIterator<Item = u8>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl IntoIterator for Instructions {
    type Item = u8;
    type IntoIter = std::vec::IntoIter<u8>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl<'a> IntoIterator for &'a Instructions {
    type Item = <std::slice::Iter<'a, u8> as Iterator>::Item;
    type IntoIter = std::slice::Iter<'a, u8>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.as_slice().into_iter()
    }
}

impl ToString for Instructions {
    fn to_string(&self) -> String {
        let mut i = 0;
        let mut res = String::new();
        while i < self.0.len() {
            let opcode = Opcode::lookup(self.0[i]).expect("Unknown opcode");
            let (operands, read) = opcode.read_operands(&self.0[i + 1..]);
            res += &format!(
                "{:04} {} {}\n",
                i,
                opcode.to_string(),
                operands
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            );
            i += 1 + read;
        }

        res
    }
}

impl std::fmt::Debug for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Instructions {
    pub fn new() -> Self {
        Self(Vec::new())
    }
    pub fn from_vec(vec: Vec<u8>) -> Self {
        Self(vec)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Opcode {
    Constant = 0,
    Add,
    Sub,
    Mul,
    Div,
    Pop,
    True,
    False,
    Equal,
    NotEqual,
    GreaterThan,
    Minus,
    Bang,
    JumpNotThuthy,
    Jump,
    Null,
    SetGlobal,
    GetGlobal,
    Array,
    Hash,
    Index,
    Call,
    ReturnValue,
    Return,
}

impl Opcode {
    pub fn to_string(&self) -> String {
        format!("Op{:?}", self)
    }
    fn operand_widths(&self) -> Vec<usize> {
        use Opcode::*;
        match self {
            Constant | JumpNotThuthy | Jump | SetGlobal | GetGlobal | Array | Hash => vec![2],
            _ => vec![],
        }
    }
    pub fn lookup(op: u8) -> Result<Opcode, &'static str> {
        use Opcode::*;
        let codes = [
            Constant,
            Add,
            Sub,
            Mul,
            Div,
            Pop,
            True,
            False,
            Equal,
            NotEqual,
            GreaterThan,
            Minus,
            Bang,
            JumpNotThuthy,
            Jump,
            Null,
            SetGlobal,
            GetGlobal,
            Array,
            Hash,
            Index,
            Call,
            ReturnValue,
            Return,
        ];
        for c in codes {
            if c as u8 == op {
                return Ok(c);
            }
        }
        Err("Unknown opcode")
    }
    pub fn to_instruction(&self, operands: &[usize]) -> Instructions {
        let mut instruction = Instructions::new();
        instruction.push(*self as u8);
        for (o, w) in operands.iter().zip(self.operand_widths()) {
            let o_bytes = o.to_be_bytes();
            instruction.extend_from_slice(&o_bytes[o_bytes.len() - w..]);
        }
        return instruction;
    }

    pub fn read_operands(&self, operands: &[u8]) -> (Vec<usize>, usize) {
        let operand_width = self.operand_widths();
        let mut i = 0;
        let mut read_operands = vec![];
        for w in &operand_width {
            match w {
                2 => read_operands
                    .push(u16::from_be_bytes(operands[i..i + w].try_into().unwrap()) as usize),
                _ => panic!("Unsupported operand width"),
            }
            i += w;
        }
        return (read_operands, i);
    }
}

pub fn instructions_to_string(instructions: Instructions) -> String {
    let mut i = 0;
    let mut res = String::new();
    while i < instructions.len() {
        let opcode = Opcode::lookup(instructions[i]).expect("Unknown opcode");
        let (operands, read) = opcode.read_operands(&instructions[i + 1..]);
        res += &format!(
            "{:04} {}\n",
            i,
            [opcode.to_string()]
                .iter()
                .cloned()
                .chain(operands.iter().map(|x| x.to_string()))
                .collect::<Vec<String>>()
                .join(" ")
        );
        i += 1 + read;
    }

    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_instruction() {
        let tests = [
            (
                Opcode::Constant,
                vec![65534],
                vec![Opcode::Constant as u8, 255, 254],
            ),
            (Opcode::Add, vec![], vec![Opcode::Add as u8]),
        ];
        for t in tests {
            let instruction = t.0.to_instruction(&t.1);
            assert_eq!(instruction.len(), t.2.len());
            for (i, e) in instruction.iter().zip(t.2.iter()) {
                dbg!((i, e));
                assert_eq!(i, e);
            }
        }
    }

    #[test]
    fn test_instruction_string() {
        let instructions: Instructions = vec![
            Opcode::Add.to_instruction(&vec![]),
            Opcode::Constant.to_instruction(&vec![2]),
            Opcode::Constant.to_instruction(&vec![65535]),
        ]
        .iter()
        .flatten()
        .copied()
        .collect();
        let expected = "0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65535
";

        assert_eq!(instructions_to_string(instructions), expected);
    }

    #[test]
    fn test_read_operands() {
        let tests = vec![(Opcode::Constant, vec![65535], 2)];

        for (op, expected_operands, expected_bytes_read) in tests {
            let instruction = op.to_instruction(&expected_operands);

            let (operands_read, n) = op.read_operands(&instruction[1..]);
            assert_eq!(n, expected_bytes_read);

            assert_eq!(operands_read, expected_operands);
        }
    }
}
