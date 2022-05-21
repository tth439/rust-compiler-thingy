// use std::borrow::Cow;
// use std::fmt::{Debug, Formatter};
// use std::collections::HashMap;
// use std::vec;
// use crate::parser::ast::{Stmt, Program, Expr, OpType, OrderType};

// type VariableMap = HashMap<String, usize>;

// fn unsignedLEB128(mut num: usize) -> Vec<u8> {
//     let mut output: Vec<u8> = Vec::new();
//     loop {
//         let mut byte = (num & 0x7f) as u8;
//         num >>= 7;
//         if num != 0 {
//             byte |= 0x80;
//         }
//         output.push(byte);

//         if num == 0 {
//             break;
//         }
//     }
//     return output;
// }

// enum Section {
//     Custom = 0,
//     Type = 1,
//     Import = 2,
//     Func = 3,
//     Table = 4,
//     Memory = 5,
//     Global = 6,
//     Export = 7,
//     Start = 8,
//     Element = 9,
//     Code = 10,
//     Data = 11
// }

// enum Valtype {
//     I32 = 0x7f,
//     F32 = 0x7d
// }

// enum Blocktype {
//     Void = 0x40
// }

// enum Opcodes {
//     Block = 0x02,
//     Loop = 0x03,
//     Br = 0x0c,
//     BrIf = 0x0d,
//     End = 0x0b,
//     Call = 0x10,
//     GetLocal = 0x20,
//     SetLocal = 0x21,
//     I32Store8 = 0x3a,
//     I32Const = 0x41,
//     F32Const = 0x43,
//     I32Eqz = 0x45,
//     I32Eq = 0x46,
//     F32Eq = 0x5b,
//     F32Ne = 0x5c,
//     F32Lt = 0x5d,
//     F32Gt = 0x5e,
//     F32Le = 0x5f,
//     F32Ge = 0x60,
//     I32And = 0x71,
//     F32Add = 0x92,
//     F32Sub = 0x93,
//     F32Mul = 0x94,
//     F32Div = 0x95,
//     I32TruncF32S = 0xa8
// }

// enum ExportType {
//     Func = 0x00,
//     Table = 0x01,
//     Mem = 0x02,
//     Global = 0x03
// }

// const functionType: u8 = 0x60;
// const emptyArray: u8 = 0x0;
// const magicModuleHeader: &'static [u8] = &[0x00, 0x61, 0x73, 0x6d];
// const moduleVersion: &'static [u8] = &[0x01, 0x00, 0x00, 0x00];

// impl From<&OpType> for Opcodes {
//     fn from(symbol: &OpType) -> Self {
//         match symbol {
//             OpType::Add => Opcodes::F32Add,
//             OpType::Sub => Opcodes::F32Sub,
//             OpType::Mul => Opcodes::F32Mul,
//             OpType::Div => Opcodes::F32Div,
//             _ => panic!("Operator invalid!"),
//         }
//     }
// }

// impl From<&OrderType> for Opcodes {
//     fn from(symbol: &OrderType) -> Self {
//         match symbol {
//             OrderType::Less => Opcodes::F32Lt,
//             OrderType::LessOrEqual => Opcodes::F32Le,
//             OrderType::Equal => Opcodes::F32Eq,
//             OrderType::GreaterOrEqual => Opcodes::F32Ge,
//             OrderType::Greater => Opcodes::F32Gt,
//             OrderType::Unequal => Opcodes::F32Ne,
//             _ => panic!("Operator invalid!"),
//         }
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct ProcContext {
//     outer_scope: VariableMap,
//     current_scope: VariableMap,
// }

// impl ProcContext {
//     fn get_var_from_current_scope(&mut self, name: &String) -> usize {
//         if let Some(index) = self.current_scope.get(name) {
//             return *index;
//         } else {
//             self.current_scope.insert(name.to_string(), self.current_scope.len());
//             return self.current_scope.len();
//         }
//     }

//     fn flatten_scopes<'a>(map1: &'a VariableMap, map2: &'a VariableMap) -> VariableMap {
//         let mut new_map = map1.clone();
//         for (k, &v) in map2 {
//             *new_map.entry(k.clone()).or_insert(0) = v;
//         }
//         new_map
//     }

//     pub fn generate_top_level(begin: Program) -> Vec<u8> {
//         let mut output = Vec::new();
//         if let Program::Function(Stmt::FuncDecl(name, statements)) = begin {

//         }
//         return output
//     }

//     fn generate_function(name: String, statements: Vec<Expr>) -> Vec<u8> {
//         let mut output = Vec::new();
//         return output;
//     }

//     fn generate_expr(&mut self, expr: &Expr) -> Vec<u8> {
//         let mut output: Vec<u8> = Vec::new();
//         match expr {
//             Expr::Num(i) => {
//                 output.push(Opcodes::F32Const as u8);
//                 i.to_be_bytes().map(|b: u8| output.push(b));
//             },
//             Expr::Id(id) => {
//                 output.push(Opcodes::GetLocal as u8);
//                 let index = self.get_var_from_current_scope(&id.to_string());
//                 let mut res = unsignedLEB128(index);
//                 output.append(&mut res);
//             },
//             Expr::OpExpr(opcode, lhs, rhs) => {
//                 output.append(&mut self.generate_expr(&lhs));
//                 output.append(&mut self.generate_expr(&rhs));
//                 let wasm_opcode = Opcodes::from(opcode);
//                 output.push(wasm_opcode as u8);
//             },
//             Expr::OrderExpr(ordercode, lhs, rhs) => {
//                 output.append(&mut self.generate_expr(&lhs));
//                 output.append(&mut self.generate_expr(&rhs));
//                 let wasm_opcode = Opcodes::from(ordercode);
//                 output.push(wasm_opcode as u8);
//             },
//         }
//         return output;
//     }
// }
