use parser::ast::{
  Declaration, Expr, Literal, OpType, OrderType, Stmt, UnaryType, Visitable, Visitor,
};
use std::collections::{HashMap, VecDeque};

enum Op {
  Mov,
  Add,
  Sub,
  Mul,
  Div,
  And,
  Or,
  Print,
  Store,
  Var,
  Alloc,
  Load,
  Label,
  Cbranch,
  Branch,
  Call,
  Ret,
}

enum Type {
  Number(f32),
  Str(String),
  Boolean(bool),
  GotoLabel,
}

enum Scope {
  Global,
  Local,
}

struct Symbol {
  name: String,
  value: Option<Type>,
}

struct SymbolTable<'a> {
  table: HashMap<String, &'a Symbol>,
  register_count: u32,
  label_count: u32,
}

impl<'a> SymbolTable<'a> {
  pub(crate) fn new() -> Self {
    Self {
      table: HashMap::new(),
      register_count: 0,
      label_count: 0,
    }
  }

  pub(crate) fn new_register(&mut self, value: Option<Type>) -> &Symbol {
    self.register_count += 1;
    let register_name = format!("new_temp_{}", self.register_count);
    let sym = &Symbol {
      name: register_name,
      value,
    };
    self.table.insert(register_name, sym);
    return &sym;
  }

  pub(crate) fn new_label(&mut self) -> &Symbol {
    self.label_count += 1;
    let label_name = format!("new_temp_{}", self.label_count);
    let sym = &Symbol {
      name: label_name,
      value: Some(Type::GotoLabel),
    };
    self.table.insert(label_name, sym);
    return sym;
  }
}

struct IRSeq {
  op_code: Op,
  arguments: Vec<String>,
}

impl IRSeq {
  pub(crate) fn new(op_code: Op) -> Self {
    Self {
      op_code,
      arguments: Vec::new(),
    }
  }
}
struct IR<'a> {
  code: Vec<IRSeq>,
  saved_registers: VecDeque<&'a Symbol>,
  global_scope: bool,
  sym_table: SymbolTable<'a>,
}

impl<'a> Visitor for IR<'a> {
  fn visit_literal(&mut self, literal: &Literal) {
    use Literal::*;
    match &literal {
      NumLiteral(i) => {
        let target = self.sym_table.new_register(None);
        let mut code = IRSeq::new(Op::Mov);
        code.arguments.push(i.to_string());
        code.arguments.push(target.name);
        self.code.push(code);
        self.saved_registers.push_front(target);
      }
      StringLiteral(s) => {}
      BoolLiteral(b) => {}
    }
  }

  fn visit_stmt(&mut self, stmt: &Stmt) {
    use Stmt::*;
    match &stmt {
      Block(decl_list) => {
        for s in decl_list.iter() {
          s.accept(self);
        }
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

  fn visit_expr(&mut self, expr: &Expr) {
    use Expr::*;

    match &expr {
      ExprLiteral(i) => i.accept(self),
      Id(s) => {}

      OpExpr(op, x, y) => {
        let key = match op {
          OpType::Add => String::from("+"),
          OpType::Mul => String::from("*"),
          OpType::Sub => String::from("-"),
          OpType::Div => String::from("/"),
          OpType::Mod => String::from("%"),
        };
        x.accept(self);
        y.accept(self);
      }

      OrderExpr(op, x, y) => {
        let key = match op {
          OrderType::Less => String::from("<"),
          OrderType::LessOrEqual => String::from("<="),
          OrderType::Equal => String::from("=="),
          OrderType::Unequal => String::from("!="),
          OrderType::Greater => String::from(">"),
          OrderType::GreaterOrEqual => String::from(">="),
          OrderType::And => String::from("and"),
          OrderType::Or => String::from("or"),
        };
        x.accept(self);
        y.accept(self);
      }

      Assign(n1, n2) => {
        n1.accept(self);
        n2.accept(self);
      }

      UnaryExpr(op, n) => {
        let key = match op {
          UnaryType::Not => String::from("Not"),
          UnaryType::Negative => String::from("-"),
        };
        n.accept(self);
      }
    }
  }

  fn visit_declaration(&mut self, decl: &Declaration) {
    use Declaration::*;
    match &decl {
      FuncDecl(name, params, stmt) => {
        stmt.accept(self);
      }
      VarDecl(id, exp) => {
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
