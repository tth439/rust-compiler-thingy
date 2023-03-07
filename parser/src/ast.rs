extern crate ptree;
use ptree::TreeBuilder;
use std::borrow::Cow;
use std::fmt::Debug;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompilerError {
  #[error("division by zero")]
  DivByZeroError,
  #[error("invalid instruction")]
  InvalidStatementError,
}

pub trait Visitor<T> {
  fn visit_literal(&mut self, literal: &Literal) -> Option<T>;
  fn visit_expr(&mut self, expr: &Expr) -> Option<T>;
  fn visit_stmt(&mut self, stmt: &Stmt) -> Option<T>;
  fn visit_declaration(&mut self, decl: &Declaration) -> Option<T>;
}

pub trait Visitable<T> {
  fn accept(&self, v: &mut dyn Visitor<T>) -> Option<T>;
}

pub struct PrintVisitor {
  pub root: Box<TreeBuilder>,
}

impl PrintVisitor {
  pub fn new(input_file: String) -> Self {
    let tree = Box::new(TreeBuilder::new(input_file.to_string()));
    PrintVisitor { root: tree }
  }
}

impl Visitor<()> for PrintVisitor {
  fn visit_literal(&mut self, literal: &Literal) -> Option<()> {
    use Literal::*;
    match &literal {
      NumLiteral(i) => {
        self
          .root
          .add_empty_child(format!("NUMBER: \x1b[32m{}\x1b[0m", i));
      }
      StringLiteral(s) => {
        self
          .root
          .add_empty_child(format!("STRING LITERAL: \x1b[32m{}\x1b[0m", s));
      }
      BoolLiteral(b) => {
        self
          .root
          .add_empty_child(format!("BOOLEAN LITERAL: \x1b[32m{}\x1b[0m", &b));
      }
    }
    return None;
  }

  fn visit_stmt(&mut self, stmt: &Stmt) -> Option<()> {
    use Stmt::*;
    match &stmt {
      Block(decl_list) => {
        self.root.begin_child("BLOCK".to_string());
        for s in decl_list.iter() {
          s.accept(self);
        }
        self.root.end_child();
      }
      While(n1, n2) => {
        self.root.begin_child("WHILE".to_string());
        n1.accept(self);
        n2.accept(self);
        self.root.end_child();
      }
      Print(n) => {
        self.root.begin_child("PRINT".to_string());
        n.accept(self);
        self.root.end_child();
      }
      Call(name, params) => {
        self.root.begin_child(format!(
          "\x1b[34FUNCTION CALL\x1b[0m: \x1b[32{}\x1b[0m",
          name
        ));
        self.root.end_child();
      }
      ExprStmt(expr) => {
        self.root.begin_child("Expression Statement".to_string());
        expr.accept(self);
        self.root.end_child();
      }
      Empty => {}
    }
    return None;
  }

  fn visit_expr(&mut self, expr: &Expr) -> Option<()> {
    use Expr::*;

    match &expr {
      ExprLiteral(i) => {
        i.accept(self);
      }
      Id(s) => {
        self
          .root
          .add_empty_child(format!("IDENTIFIER: \x1b[32m{}\x1b[0m", s));
      }

      OpExpr(op, x, y) => {
        let key = match op {
          OpType::Add => String::from("+"),
          OpType::Mul => String::from("*"),
          OpType::Sub => String::from("-"),
          OpType::Div => String::from("/"),
          OpType::Mod => String::from("%"),
        };
        self.root.begin_child(key);
        x.accept(self);
        y.accept(self);
        self.root.end_child();
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
        self.root.begin_child(key);
        x.accept(self);
        y.accept(self);
        self.root.end_child();
      }

      Assign(id, n2) => {
        self.root.begin_child("ASSIGN".to_string());
        self.root.add_empty_child(id.to_string());
        n2.accept(self);
        self.root.end_child();
      }

      UnaryExpr(op, n) => {
        let key = match op {
          UnaryType::Not => String::from("Not"),
          UnaryType::Negative => String::from("-"),
        };
        self.root.begin_child(key);
        n.accept(self);
        self.root.end_child();
      }
    }
    return None;
  }

  fn visit_declaration(&mut self, decl: &Declaration) -> Option<()> {
    use Declaration::*;
    match &decl {
      FuncDecl(name, params, stmt) => {
        self.root.begin_child(format!(
          "\x1b[34FUNCTION DECL\x1b[0m: \x1b[32{}\x1b[0m, \x1b[34PARAMS\x1b[0m: \x1b[32{:?}\x1b[0m",
          name, params
        ));
        stmt.accept(self);
        self.root.end_child();
      }
      VarDecl(id, exp, _) => {
        self.root.begin_child("VAR DECL".to_string());
        id.accept(self);
        if let Some(e) = exp {
          e.accept(self);
        }
        self.root.end_child();
      }
      Statement(stmt) => {
        self.root.begin_child("STMT DECL".to_string());
        stmt.accept(self);
        self.root.end_child();
      }
    }
    return None;
  }
}

#[derive(PartialEq, Debug, Clone)]
pub enum UnaryType {
  Not,
  Negative,
}

#[derive(PartialEq, Debug, Clone)]
pub enum OpType {
  Mul,
  Add,
  Div,
  Sub,
  Mod,
}

#[derive(PartialEq, Debug, Clone)]
pub enum OrderType {
  Less,
  LessOrEqual,
  Equal,
  Unequal,
  Greater,
  GreaterOrEqual,
  And,
  Or,
}

impl From<&str> for OrderType {
  fn from(symbol: &str) -> Self {
    match symbol {
      "<" => OrderType::Less,
      "<=" => OrderType::LessOrEqual,
      "==" => OrderType::Equal,
      ">=" => OrderType::GreaterOrEqual,
      ">" => OrderType::Greater,
      "!=" => OrderType::Unequal,
      "and" => OrderType::And,
      "or" => OrderType::Or,
      _ => panic!("Operator invalid!"),
    }
  }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal<'a> {
  NumLiteral(f32),
  StringLiteral(Cow<'a, str>),
  BoolLiteral(bool),
}

impl<'a, T> Visitable<T> for Literal<'a> {
  fn accept(&self, v: &mut dyn Visitor<T>) -> Option<T> {
    v.visit_literal(self)
  }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr<'a> {
  ExprLiteral(Literal<'a>),
  Id(Cow<'a, str>),
  OpExpr(OpType, Box<Self>, Box<Self>),
  OrderExpr(OrderType, Box<Self>, Box<Self>),
  Assign(Cow<'a, str>, Box<Self>),
  UnaryExpr(UnaryType, Box<Self>),
}

pub(crate) fn deduce_expr_type_offset(e: &Expr) -> usize {
  use Expr::*;
  use Literal::*;

  let offset = match e {
    ExprLiteral(NumLiteral(_)) => 32,
    ExprLiteral(BoolLiteral(_)) => 4,
    ExprLiteral(StringLiteral(s)) => s.len(),
    Id(_) => 0,
    OpExpr(_, _, _) => 32,
    OrderExpr(_, _, _) => 4,
    UnaryExpr(UnaryType::Negative, _) => 32,
    UnaryExpr(UnaryType::Not, _) => 32,
    _ => unreachable!(),
  };
  return offset;
}

impl<'a, T> Visitable<T> for Expr<'a> {
  fn accept(&self, v: &mut dyn Visitor<T>) -> Option<T> {
    v.visit_expr(self)
  }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Stmt<'a> {
  Empty,
  Block(Vec<Declaration<'a>>),
  While(Expr<'a>, Box<Self>),
  Print(Expr<'a>),
  Call(Cow<'a, str>, Vec<Expr<'a>>),
  ExprStmt(Expr<'a>),
}

impl<'a, T> Visitable<T> for Stmt<'a> {
  fn accept(&self, v: &mut dyn Visitor<T>) -> Option<T> {
    v.visit_stmt(self)
  }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Declaration<'a> {
  FuncDecl(Cow<'a, str>, Vec<&'a str>, Box<Stmt<'a>>),
  VarDecl(Box<Expr<'a>>, Option<Expr<'a>>, usize),
  Statement(Box<Stmt<'a>>),
}

impl<'a, T> Visitable<T> for Declaration<'a> {
  fn accept(&self, v: &mut dyn Visitor<T>) -> Option<T> {
    v.visit_declaration(self)
  }
}
