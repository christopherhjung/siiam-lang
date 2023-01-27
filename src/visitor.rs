use crate::ast::*;

//struct Visitor;

pub trait Visitor{
    fn enter_module(&mut self, module: &mut Module){}
    fn exit_module(&mut self, module: &mut Module){}

    fn enter_decl(&mut self, decl: &mut Decl){}
    fn exit_decl(&mut self, decl: &mut Decl){}

    fn enter_block(&mut self, decl: &mut Block){}
    fn exit_block(&mut self, decl: &mut Block){}

    fn visit_ident_expr(&mut self, ident_expr: &mut IdentExpr){}

    fn visit_module(&mut self, module: &mut Module){
        self.enter_module(module);
        for mut item in &mut module.items{
            self.visit_decl(item)
        }
        self.exit_module(module)
    }

    fn visit_decl(&mut self, decl: &mut Decl){
        self.enter_decl(decl);
        match &mut decl.kind {
            DeclKind::FnDecl( kind) => {
                for mut param in &mut kind.params{
                    self.visit_decl(param);
                }

                self.visit_expr(&mut kind.body)
            },
            DeclKind::LetDecl( kind) => {

            },
            DeclKind::StructDecl( kind) => {

            },
            DeclKind::MemberDecl(kind) => {

            },
            _ => unreachable!()
        }
        self.exit_decl(decl);
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt){
        match stmt {
            Stmt::Expr(expr) => self.visit_expr(&mut expr.expr),
            Stmt::Let(let_stmt) => {
                if let Some(init) = &mut let_stmt.init{
                    self.visit_expr(init);
                }

                self.visit_decl(&mut let_stmt.local_decl);
            },
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr){
        match &mut expr.kind {
            ExprKind::Infix(infix) => {
                self.visit_expr(&mut infix.lhs);
                self.visit_expr(&mut infix.rhs);
            },
            ExprKind::Postfix(postfix) => {
                self.visit_expr(&mut postfix.expr);
            },
            ExprKind::Prefix(prefix) => {
                self.visit_expr(&mut prefix.expr);
            },
            ExprKind::Ident(ident) => {
                self.visit_ident_expr(ident)
            },
            ExprKind::Block(block) => {
                self.enter_block(block);
                for mut stmt in &mut block.stmts{
                    self.visit_stmt(stmt);
                }
                self.exit_block(block);
            }
            _ => return
        }
    }
}