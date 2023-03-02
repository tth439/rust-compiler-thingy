
struct TAC {
    op_code: Op;
    value;
    target: String;
}

struct IR {
    register_count: 0,
    label_count: 0,
    code: Vec<TAC>;
    saved_registers: Stack;
    global_scope: bool;
}

impl Visitor for IR {
    fn new_register () {}

    fn visit_literal(&mut self, literal: &mut Literal) {
        use Literal::*;
        match &literal {
            NumLiteral(i) => {
                let target = new_register();
                let code = TAC{op_code: Opcode::MOV, i, target};
                self.code.push(code);
                self.registers.push(target);
            },
            StringLiteral(s) => {
            },
            BoolLiteral(b) => {
            }
        }
    }
    
    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        use Stmt::*;
        match &stmt {
            Block(decl_list) => {
                for s in decl_list.iter() {
                    s.accept(self);
                }
            },
            While(n1, n2) => {
                n1.accept(self);
                n2.accept(self);
            },
            Print(n) => {
                n.accept(self);
            },
            Call(name, params) => {
            },
            ExprStmt(expr) => {
                expr.accept(self);
            }
            Empty => {}
        }
    }

    fn start_expr(&mut self, expr: &mut Expr) {
        use Expr::*;

        match &expr {
            ExprLiteral(i) => i.accept(self),
            Id(s) => {
            }

            OpExpr(op, x, y) => {
                let key = match op {
                    OpType::Add => String::from("+"), 
                    OpType::Mul => String::from("*"),
                    OpType::Sub => String::from("-"),
                    OpType::Div => String::from("/"),
                    OpType::Mod => String::from("%"),
                };
                key);
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
            },

            UnaryExpr(op, n) => {
                let key = match op {
                    UnaryType::Not => String::from("Not"),
                    UnaryType::Negative => String::from("-"),
                };
                key);
                n.accept(self);
            }
        }
    }

    fn start_decl(&mut self, decl: &mut Declaration) {
        use Declaration::*;
        match &decl {
            FuncDecl(name, params, stmt) => {
                stmt.accept(self);
            },
            VarDecl(id, exp) => {
                id.accept(self);
                if let Some(e) = exp {
                    e.accept(self);
                }
            },
            Statement(stmt) => {
                stmt.accept(self);
            },
        }
    }
}