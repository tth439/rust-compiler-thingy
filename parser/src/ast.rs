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

pub type VResult = Result<(), Box<CompilerError>>;

pub trait Visitor {
    fn start_literal(&mut self, _t: &mut Literal) -> VResult {
        Ok(())
    }
    
    fn start_stmt(&mut self, _t: &mut Stmt) -> VResult {
        Ok(())
    }

    fn post_stmt(&mut self, _t: &mut Stmt) -> VResult {
        Ok(())
    }

    fn start_expr(&mut self, _t: &mut Expr) -> VResult {
        Ok(())
    }

    fn post_expr(&mut self, _t: &mut Expr) -> VResult {
        Ok(())
    }

    fn start_decl(&mut self, _t: &mut Declaration) -> VResult {
        Ok(())
    }

    fn post_decl(&mut self, _t: &mut Declaration) -> VResult {
        Ok(())
    }
}

pub struct PrintVisitor {
    pub root: Box<TreeBuilder>,
}

impl PrintVisitor {
    pub fn new(input_file: String) -> Self {
        let tree = Box::new(TreeBuilder::new(input_file.to_string()));
        PrintVisitor {root: tree}
    }
}

impl Visitor for PrintVisitor {
    fn start_literal(&mut self, literal: &mut Literal) -> VResult {
        use Literal::*;
        match &literal {
            NumLiteral(i) => {
                self.root.add_empty_child(format!("NUMBER: \x1b[32m{}\x1b[0m", i));
            }
            StringLiteral(s) => {
                self.root.add_empty_child(format!("STRING LITERAL: \x1b[32m{}\x1b[0m", s));
            }
            BoolLiteral(b) => {
                self.root.add_empty_child(format!("BOOLEAN LITERAL: \x1b[32m{}\x1b[0m", &b));
            }
        }
        Ok(())
    }
    
    fn start_stmt(&mut self, stmt: &mut Stmt) -> VResult {
        use Stmt::*;
        match &stmt {
            Block(_) => {
                self.root.begin_child("BLOCK".to_string());
            },
            While(_, _) => {
                self.root.begin_child("WHILE".to_string());
            },
            Print(_) => {
                self.root.begin_child("PRINT".to_string());
            },
            Call(name, _) => {
                self.root.begin_child(format!(
                    "\x1b[34FUNCTION CALL\x1b[0m: \x1b[32{}\x1b[0m",
                    name
                ));
            },
            ExprStmt(_) => {
                self.root.begin_child("Expression Statement".to_string());
            }
            Empty => {}
        }
        Ok(())
    }

    fn post_stmt(&mut self, _t: &mut Stmt) -> VResult {
        self.root.end_child();
        Ok(())
    }

    fn start_expr(&mut self, expr: &mut Expr) -> VResult {
        use Expr::*;

        match &expr {
            ExprLiteral(_) => {},
            Id(s) => {
                self.root.add_empty_child(format!("IDENTIFIER: \x1b[32m{}\x1b[0m", s));
            }

            OpExpr(op, _, _) => {
                let key = match op {
                    OpType::Add => String::from("+"), 
                    OpType::Mul => String::from("*"),
                    OpType::Sub => String::from("-"),
                    OpType::Div => String::from("/"),
                    OpType::Mod => String::from("%"),
                };
                self.root.begin_child(key);
            }

            OrderExpr(op, _, _) => {
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
            }

            Assign(_, _) => {
                self.root.begin_child("ASSIGN".to_string());
            },

            UnaryExpr(op, _) => {
                let key = match op {
                    UnaryType::Not => String::from("Not"),
                    UnaryType::Negative => String::from("-"),
                };
                self.root.begin_child(key);
            }
        }
        Ok(())
    }

    fn post_expr(&mut self, expr: &mut Expr) -> VResult {
        use Expr::*;
        match &expr {
            ExprLiteral(_) => (),
            Id(_) => (),
            OpExpr(_, _, _) => {self.root.end_child();},
            OrderExpr(_, _, _) => {self.root.end_child();}
            Assign(_, _) => {self.root.end_child();}
            UnaryExpr(_, _) => {self.root.end_child();}
        }
        Ok(())
    }

    fn start_decl(&mut self, decl: &mut Declaration) -> VResult {
        use Declaration::*;
        match &decl {
            FuncDecl(name, params, _) => {
                self.root.begin_child(format!(
                    "\x1b[34FUNCTION DECL\x1b[0m: \x1b[32{}\x1b[0m, \x1b[34PARAMS\x1b[0m: \x1b[32{:?}\x1b[0m",
                    name, params
                ));
            },
            VarDecl(_, _) => {self.root.begin_child("VAR DECL".to_string());},
            Statement(_) => {self.root.begin_child("STMT DECL".to_string());},
        }
        Ok(())
    }

    fn post_decl(&mut self, _t: &mut Declaration) -> VResult {
        self.root.end_child();
        Ok(())
    }
}

pub trait Visitable {
    fn visit(&mut self, v: &mut dyn Visitor) -> VResult;
}

#[derive(PartialEq, Debug, Clone)]
pub enum UnaryType {
    Not,
    Negative
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

impl<'a> Visitable for Literal<'a> {
    fn visit(&mut self, v: &mut dyn Visitor) -> VResult {
        v.start_literal(self)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr<'a> {
    ExprLiteral(Literal<'a>),
    Id(Cow<'a, str>),
    OpExpr(OpType, Box<Self>, Box<Self>),
    OrderExpr(OrderType, Box<Self>, Box<Self>),
    Assign(Box<Self>, Box<Self>),
    UnaryExpr(UnaryType, Box<Self>)
}

impl<'a> Visitable for Expr<'a> {
    fn visit(&mut self, v: &mut dyn Visitor) -> VResult {
        use Expr::*;
        v.start_expr(self)?;

        match self {
            ExprLiteral(i) => i.visit(v)?,
            Id(_) => {},
            OpExpr(_, x, y) => {
                x.visit(v)?;
                y.visit(v)?;
            }

            OrderExpr(_, x, y) => {
                x.visit(v)?;
                y.visit(v)?;
            }

            Assign(n1, n2) => {
                n1.visit(v)?;
                n2.visit(v)?;
            },

            UnaryExpr(_, n) => n.visit(v)?,
        }
        v.post_expr(self)    
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

impl<'a> Visitable for Stmt<'a> {
    fn visit(&mut self, v: &mut dyn Visitor) -> VResult {
        use Stmt::*;
        v.start_stmt(self)?;
        match self {
            Block(decl_list) => {
                for s in decl_list {
                    s.visit(v)?;
                }
            }

            While(n1, n2) => {
                n1.visit(v)?;
                n2.visit(v)?;
            }

            Print(n) => n.visit(v)?,
            Call(_, params) => { 
                for s in params {
                    s.visit(v)?;
                }
            },

            ExprStmt(expr) => expr.visit(v)?,
            Empty => ()
        }
        v.post_stmt(self)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Declaration<'a> {
    FuncDecl(Cow<'a, str>, Vec<&'a str>, Box<Stmt<'a>>),
    VarDecl(Box<Expr<'a>>, Option<Expr<'a>>),
    Statement(Box<Stmt<'a>>),
}

impl<'a> Visitable for Declaration<'a> {
    fn visit(&mut self, v: &mut dyn Visitor) -> VResult {
        use Declaration::*;
        v.start_decl(self)?;

        match self {
            FuncDecl(_, _, stmt) => stmt.visit(v)?,
            VarDecl(id, exp) => {
                id.visit(v)?;
                if let Some(e) = exp {
                    e.visit(v)?;   
                }
            },
            Statement(stmt) => stmt.visit(v)?,
        }
        v.post_decl(self)
    }
}