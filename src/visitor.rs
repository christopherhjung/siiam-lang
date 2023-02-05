use crate::ast::*;

//struct Visitor;

pub trait Visitor{
    fn enter_module(&mut self, module: &mut Module){}
    fn exit_module(&mut self, module: &mut Module){}

    fn bind_decl(&mut self, decl: &mut Decl){}
    fn exit_decl(&mut self, decl: &mut Decl){}

    fn enter_expr(&mut self, expr: &mut Expr){}
    fn exit_expr(&mut self, expr: &mut Expr){}

    fn enter_stmt(&mut self, stmt: &mut Stmt){}
    fn exit_stmt(&mut self, stmt: &mut Stmt){}

    fn visit_module(&mut self, module: &mut Module){
        self.enter_module(module);
        for mut item in &mut module.items{
            self.visit_decl(item)
        }
        self.exit_module(module)
    }

    fn visit_decl(&mut self, decl: &mut Decl){
        self.bind_decl(decl);
        match &mut decl.kind {
            DeclKind::FnDecl(fn_decl) => {
                for mut param in &mut fn_decl.params{
                    self.visit_decl(param);
                }

                self.visit_expr(&mut fn_decl.body)
            }
            DeclKind::StructDecl(struct_decl) => {
                for member in &mut struct_decl.members{
                    self.visit_decl(member);
                }
            }
            DeclKind::LocalDecl(_) |
            DeclKind::MemberDecl(_) => {},
            _ => unreachable!()
        }
        self.exit_decl(decl);
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt){
        self.enter_stmt(stmt);
        match stmt {
            Stmt::Expr(expr) => self.visit_expr(&mut expr.expr),
            Stmt::Let(let_stmt) => {
                if let Some(init) = &mut let_stmt.init{
                    self.visit_expr(init);
                }

                self.visit_decl(&mut let_stmt.local_decl);
            },
        }
        self.enter_stmt(stmt);
    }

    fn visit_expr(&mut self, expr: &mut Expr){
        self.enter_expr(expr);
        match &mut expr.kind {
            ExprKind::Infix(infix) => {
                self.visit_expr(&mut infix.lhs);
                self.visit_expr(&mut infix.rhs);
            }
            ExprKind::Postfix(postfix) => {
                self.visit_expr(&mut postfix.expr);
            }
            ExprKind::Prefix(prefix) => {
                self.visit_expr(&mut prefix.expr);
            }
            ExprKind::Ret(ret_expr) => {
                if let Some(expr) = &mut ret_expr.expr{
                    self.visit_expr(expr);
                }
            }
            ExprKind::Ident(ident) => {
                //self.visit_ident_expr( ident)
            }
            ExprKind::Block(block) => {
                for mut stmt in &mut block.stmts{
                    self.visit_stmt(stmt);
                }
            }
            ExprKind::FnCall(fn_call) => {
                self.visit_expr(&mut fn_call.callee);
                for mut arg in &mut fn_call.args{
                    self.visit_expr(arg);
                }
            }
            ExprKind::If(if_expr) => {
                self.visit_expr(&mut if_expr.condition);
                self.visit_expr(&mut if_expr.true_branch);
                if let Some(false_branch) = &mut if_expr.false_branch{
                    self.visit_expr(false_branch);
                }
            }
            ExprKind::While(while_expr) => {
                self.visit_expr(&mut while_expr.condition);
                self.visit_expr(&mut while_expr.body);
                if let Some(else_branch) = &mut while_expr.else_branch{
                    self.visit_expr(else_branch);
                }
            }
            _ => return
        }
        self.exit_expr(expr);
    }
}