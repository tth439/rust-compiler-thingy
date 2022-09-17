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
pub trait AST {
    fn print_node(&self, root: &mut TreeBuilder);
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

impl<'a> AST for Literal<'a> {
    fn print_node(&self, root: &mut TreeBuilder) {
        use Literal::*;
        match &self {
            NumLiteral(i) => {
                root.add_empty_child(format!("NUMBER: \x1b[32m{}\x1b[0m", i));
            }
            StringLiteral(s) => {
                root.add_empty_child(format!("STRING LITERAL: \x1b[32m{}\x1b[0m", s));
            }
            BoolLiteral(b) => {
                root.add_empty_child(format!("BOOLEAN LITERAL: \x1b[32m{}\x1b[0m", &b));
            }
        }
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

impl<'a> AST for Expr<'a> {
    fn print_node(&self, root: &mut TreeBuilder) {
        use Expr::*;
        match &self {
            ExprLiteral(i) => i.print_node(root),
            Id(s) => {
                root.add_empty_child(format!("IDENTIFIER: \x1b[32m{}\x1b[0m", s));
            }

            OpExpr(op, x, y) => {
                let key = match op {
                    OpType::Add => String::from("+"),
                    OpType::Mul => String::from("*"),
                    OpType::Sub => String::from("-"),
                    OpType::Div => String::from("/"),
                    OpType::Mod => String::from("%"),
                };
                root.begin_child(key);
                x.print_node(root);
                y.print_node(root);
                root.end_child();
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
                root.begin_child(key);
                x.print_node(root);
                y.print_node(root);
                root.end_child();
            }

            Assign(n1, n2) => {
                root.begin_child("ASSIGN".to_string());
                n1.print_node(root);
                n2.print_node(root);
                root.end_child();
            },

            UnaryExpr(op, n) => {
                let key = match op {
                    UnaryType::Not => String::from("Not"),
                    UnaryType::Negative => String::from("-"),
                };
                root.begin_child(key);
                n.print_node(root);
                root.end_child();
            }
        }
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

impl<'a> AST for Stmt<'a> {
    fn print_node(&self, root: &mut TreeBuilder) {
        use Stmt::*;
        match &self {
            Block(decl_list) => {
                root.begin_child("BLOCK".to_string());
                for s in decl_list.iter() {
                    s.print_node(root);
                }
                root.end_child();
            }

            While(n1, n2) => {
                root.begin_child("WHILE".to_string());
                n1.print_node(root);
                n2.print_node(root);
                root.end_child();
            }

            Print(n) => {
                root.begin_child("PRINT".to_string());
                n.print_node(root);
                root.end_child();
            }

            Call(name, params) => {
                root.begin_child(format!(
                    "\x1b[34FUNCTION CALL\x1b[0m: \x1b[32{}\x1b[0m, \x1b[34PARAMS\x1b[0m: \x1b[32{:?}\x1b[0m",
                    name, params
                ));
                root.end_child();
            },

            ExprStmt(expr) => {
                root.begin_child("Expression Statement".to_string());
                expr.print_node(root);
                root.end_child();
            }
            Empty => {}
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Declaration<'a> {
    FuncDecl(Cow<'a, str>, Vec<&'a str>, Box<Stmt<'a>>),
    VarDecl(Box<Expr<'a>>, Option<Expr<'a>>),
    Statement(Box<Stmt<'a>>),
}

impl<'a> AST for Declaration<'a> {
    fn print_node(&self, root: &mut TreeBuilder) {
        use Declaration::*;
        match &self {
            FuncDecl(name, params, stmt) => {
                root.begin_child(format!(
                    "\x1b[34FUNCTION\x1b[0m: \x1b[32{}\x1b[0m, \x1b[34PARAMS\x1b[0m: \x1b[32{:?}\x1b[0m",
                    name, params
                ));
                stmt.print_node(root);
                root.end_child();
            },

            VarDecl(id, exp) => {
                root.begin_child("VAR DECL".to_string());
                id.print_node(root);
                if let Some(e) = exp {
                    e.print_node(root);    
                }
                root.end_child();
            },

            Statement(stmt) => {
                root.begin_child("STMT DECL".to_string());
                stmt.print_node(root);
                root.end_child();
            }
        }
    }
}