use parser::ast::{
  Declaration, Expr, Literal, OpType, OrderType, Stmt, Visitable, Visitor,
};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug)]
enum Op {
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  And,
  Or,
  Print,
  Store,
  Alloc,
  Load,
  Label,
  CBranch,
  Branch,
  Call,
  Ret,
  Var,
  Mov,
  Declare,
}

impl From<&OpType> for Op {
  fn from(symbol: &OpType) -> Self {
    match symbol {
      Mul => Op::Mul,
      Add => Op::Add,
      Div => Op::Div,
      Sub => Op::Sub,
      Mod => Op::Mod,
      _ => panic!("Operator invalid!"),
    }
  }
}

impl From<&OrderType> for Op {
  fn from(symbol: &OrderType) -> Self {
    match symbol {
      OrderType::Less => ,
      OrderType::LessOrEqual => ,
      OrderType::Equal => ,
      OrderType::Unequal => ,
      OrderType::Greater => ,
      OrderType::GreaterOrEqual => ,
      OrderType::And => ,
      OrderType::Or => ,
    }
  }
}

#[derive(Clone, Debug)]
enum Type {
  Number(f32),
  Str(String),
  Boolean(bool),
  GotoLabel,
}

#[derive(Clone, Debug)]
struct Symbol {
  name: String,
  value: Option<Type>,
  size: isize,
}

#[derive(Clone, Debug)]
struct SymbolTable {
  table: HashMap<String, Symbol>,
  register_count: u32,
  label_count: u32,
}

#[derive(Clone, Debug)]
struct IRSeq {
  op_code: Op,
  arguments: Vec<String>,
  addr: Option<String>,
}

#[derive(Clone, Debug)]
struct IRBlock {
  current_scope: HashSet<String>,
  stack_index: isize,
  break_label: Option<String>,
  continue_label: Option<String>,
  code: Vec<IRSeq>,
}

struct IR {
  blocks: Vec<IRBlock>,
  sym_table: SymbolTable,
}

// impl SymbolTable {
//   pub(crate) fn new() -> Self {
//     Self {
//       table: HashMap::new(),
//       register_count: 0,
//       label_count: 0,
//     }
//   }

//   pub(crate) fn new_register(&mut self, value: Option<Type>) -> Symbol {
//     self.register_count += 1;
//     let register_name = format!("temp_{}", self.register_count);
//     let sym = Symbol {
//       name: register_name.clone(),
//       value,
//     };
//     self.table.insert(register_name, sym.clone());
//     return sym;
//   }

//   pub(crate) fn new_label(&mut self) -> Symbol {
//     self.label_count += 1;
//     let label_name = format!("label_{}", self.label_count);
//     let sym = Symbol {
//       name: label_name.clone(),
//       value: Some(Type::GotoLabel),
//     };
//     self.table.insert(label_name, sym.clone());
//     return sym;
//   }
// }

impl IRSeq {
  pub(crate) fn new(op_code: Op) -> Self {
    Self {
      op_code,
      arguments: Vec::new(),
      addr: None,
    }
  }
}

impl IR {
  pub(crate) fn new() -> Self {
    Self {
      blocks: Vec::new(),
      sym_table: SymbolTable::new(),
    }
  }
}

impl Visitor<IRSeq> for IR {
  fn visit_literal(&mut self, literal: &Literal) -> Option<IRSeq> {
    use Literal::*;
    let ret: IRSeq = match &literal {
      NumLiteral(i) => {
        let target = self.sym_table.new_register(None);
        let mut code = IRSeq::new(Op::Mov);
        code.arguments.push(i.to_string());
        code.addr = Some(target.name);
        code
      }

      StringLiteral(s) => {
        let target = self.sym_table.new_register(None);
        let mut code = IRSeq::new(Op::Mov);
        code.arguments.push(s.to_string());
        code.addr = Some(target.name);
        code
      }

      BoolLiteral(b) => {
        let target = self.sym_table.new_register(None);
        let mut code = IRSeq::new(Op::Mov);
        code.arguments.push((*b as i32).to_string());
        code.addr = Some(target.name);
        code
      }
    };
    return Some(ret);
  }

  fn visit_expr(&mut self, expr: &Expr) -> Option<IRSeq> {
    use Expr::*;

    let ret = match &expr {
      ExprLiteral(i) => i.accept(self),
      Id(s) => {
        let mut code = IRSeq::new(Op::Var);
        code.addr = Some(s.to_string());
        Some(code)
      }

      OpExpr(op, x, y) => {
        let mut code = IRSeq::new(Op::from(op));
        let target = self.sym_table.new_register(None);
        let code1 = x.accept(self);
        let code2 = y.accept(self);

        if let Some(seq) = code1 {
          code.arguments.push(seq.addr.unwrap())
        }

        if let Some(seq) = code2 {
          code.arguments.push(seq.addr.unwrap())
        }
        code.addr = Some(target);
        Some(code)
      }

      OrderExpr(op, x, y) => {
        let mut code = IRSeq::new(Op::from(op));
        let target = self.sym_table.new_register(None);        
        let code1 = x.accept(self);
        let code2 = y.accept(self);
        
        if let Some(seq) = code1 {
          code.arguments.push(seq.addr.unwrap())
        }

        if let Some(seq) = code2 {
          code.arguments.push(seq.addr.unwrap())
        }
        code.addr = Some(target);
        Some(code)
      }

      Assign(id, exp) => {
        let mut code = IRSeq::new(Op::Mov);
        let code1 = exp.accept(self);
        if let Some(seq) = code1 {
          code.arguments.push(seq.addr.unwrap());
        }
        code.addr = Some(id.to_string());
        Some(code)
      }

      UnaryExpr(op, exp) => {
        unimplemented!()
      }
    };
    return Some(ret);
  }

  fn visit_stmt(&mut self, stmt: &Stmt) -> Option<Symbol> {
    use Stmt::*;
    match &stmt {
      Block(decl_list) => {
        unimplemented!()
      }
      While(n1, n2) => {
        n1.accept(self);
        n2.accept(self);
      }
      Print(n) => {
        n.accept(self);
      }
      Call(name, params) => {}
      ExprStmt(expr) => {
        expr.accept(self);
      }
      Empty => {}
    }
  }

  fn visit_declaration(&mut self, decl: &Declaration) -> Option<Symbol> {
    use Declaration::*;
    match &decl {
      FuncDecl(name, params, stmt) => {
        stmt.accept(self);
      }
      VarDecl(id, exp, offset) => {
        id.accept(self);
        if let Some(e) = exp {
          e.accept(self);
        }
      }
      Statement(stmt) => {
        stmt.accept(self);
      }
    }
  }
}
