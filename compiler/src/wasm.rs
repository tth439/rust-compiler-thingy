// use walrus::ir::*;
// use walrus::{FunctionBuilder, Module, ModuleConfig, ValType};
// use parser::ast::{Stmt, Declaration, Expr, OpType, OrderType};

// // #[derive(PartialEq, Debug, Clone)]
// // pub enum Declaration<'a> {
// //     FuncDecl(Cow<'a, str>, Vec<&'a str>, Box<Stmt<'a>>),
// //     VarDecl(Box<Expr<'a>>, Option<Expr<'a>>),
// //     Statement(Box<Stmt<'a>>),
// // }

// struct ProcContext {
//     module: Module,
// }

// impl ProcContext {
//     pub fn generate_wasm_module(begin: Program) -> Self {
//         let config = ModuleConfig::new();
//         let mut module = Module::with_config(config);
//         let log_type = module.types.add(&[ValType::I32], &[]);
//         let (log, _) = module.add_import_func("env", "log", log_type);

//         ProcContext {
//             module
//         }
//     }

//     fn generate_decl(&self, name: String, statements: Vec<Expr>) -> FunctionBuilder {
//         let mut f = FunctionBuilder::new(&mut self::module.types);
//     }

//     fn generate_expr(&mut self, expr: &Expr) -> InstrSeqBuilder {
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
