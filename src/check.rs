use std::borrow::{Borrow, BorrowMut};
use std::cell::{Cell, Ref, RefCell};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::rc::Rc;

use strum::IntoEnumIterator;

use crate::{ast, Decl, Module, Sym};
use crate::ast::{ASTTy, Block, DeclKind, Expr, ExprKind, IdentExpr, Literal, LocalDecl, Op, PrimTy, Stmt, StructDecl, ASTStructTy};
use crate::check::Ty::Prim;
use crate::visitor::Visitor;

pub type TyRef = Rc<RefCell<Ty>>;

#[derive(Debug)]
pub struct StructTy {
    pub name: Sym,
    members : Vec<TyRef>
}

#[derive(Debug)]
pub struct FnTy {
    pub params : Vec<TyRef>,
    pub ret_ty : Option<TyRef>
}

#[derive(Debug)]
pub enum Ty {
    Prim(PrimTy),
    Struct(StructTy),
    Fn(FnTy),
    Err,
}

pub struct TyTable {
    prim_tys: HashMap<PrimTy, TyRef>,
    err_ty : TyRef
}

impl TyTable {
    pub fn prim_ty(&self, ty: PrimTy) -> TyRef {
        self.prim_tys.get(&ty).unwrap().clone()
    }
    pub fn err_ty(&self) -> TyRef {
        self.err_ty.clone()
    }

    fn struct_ty(&mut self, ty: &ASTStructTy) -> TyRef {
        if let Some(decl) = ty.ident_use.decl {
            self.decl_ty(decl)
        } else {
            unreachable!()
        }
    }
    fn infer(&mut self, ast_ty: &ASTTy) -> TyRef {
        match ast_ty {
            ASTTy::Prim(prim) => self.prim_ty(prim.clone()),
            ASTTy::Struct(struct_ty) => self.struct_ty(struct_ty),
            _ => unreachable!()
        }
    }

    pub fn decl_ty(&mut self, decl_ptr: *const Decl) -> TyRef {
        let mut_decl_ptr = decl_ptr as *mut Decl;
        let decl = unsafe { &mut *mut_decl_ptr };

        if let Some(ty) = &decl.ty {
            return ty.clone();
        }

        match &decl.kind {
            DeclKind::StructDecl(struct_decl) => {
                decl.ty = Some(Rc::new(RefCell::new(Ty::Struct(StructTy{ name: decl.ident.sym, members: Vec::new() }))));

                for member in &struct_decl.members {
                    let member_ty = self.decl_ty(member.as_ref() as *const Decl);
                    let mut struct_ty = RefCell::borrow_mut(decl.ty.as_ref().unwrap());

                    if let Ty::Struct(struct_ty) = &mut *struct_ty{
                        struct_ty.members.push(member_ty);
                    }
                }

                decl.ty.as_ref().unwrap().clone()
            }
            DeclKind::MemberDecl(member_decl) => {
                let result = self.infer(&member_decl.ast_type);
                decl.ty = Some(result.clone());
                result
            }
            _ => unreachable!()
        }
    }

    pub fn new() -> TyTable {
        let mut prim_tys = HashMap::new();

        for ty in PrimTy::iter() {
            prim_tys.insert(ty, Rc::new(RefCell::new(Ty::Prim(ty))));
        }

        TyTable {
            prim_tys,
            err_ty : Rc::new(RefCell::new(Ty::Err))
        }
    }
}

pub struct TypeChecker {
    ty_table: Rc<RefCell<TyTable>>,
}

impl TypeChecker {
    pub fn new( ty_table : Rc<RefCell<TyTable>> ) -> TypeChecker {
        TypeChecker {
            ty_table
        }
    }

    pub fn coerce_option(&mut self, expr: &mut Expr, ty: Option<TyRef>){
        if let Some(some_ty) = &ty{
            self.coerce(expr, some_ty);
        }
    }

    pub fn coerce(&mut self, expr: &mut Expr, ty: &TyRef){
        if let Some(expr_ty) = &expr.ty{
            if expr_ty.as_ptr() != ty.as_ptr() {
                expr.ty = Some(self.err_ty())
            }
        }else{
            expr.ty = Some(ty.clone());
        }
    }

    pub fn coerce_decl(&mut self, decl: &mut Decl, ty: &TyRef){
        if let Some(decl_ty) = &decl.ty{
            if decl_ty.as_ptr() != ty.as_ptr() {
                decl.ty = Some(self.err_ty())
            }
        }else{
            decl.ty = Some(ty.clone());
        }
    }

    pub fn prim_ty(&self, prim_ty: PrimTy) -> TyRef{
        RefCell::borrow(&self.ty_table).prim_ty(prim_ty)
    }

    pub fn err_ty(&self) -> TyRef{
        RefCell::borrow(&self.ty_table).err_ty()
    }

    pub fn infer(&mut self, ast_ty: &ASTTy) -> TyRef{
        RefCell::borrow_mut(&mut self.ty_table).infer(ast_ty)
    }

    pub fn decl_ty(&mut self, decl: *const Decl) -> TyRef{
        RefCell::borrow_mut(&mut self.ty_table).decl_ty(decl)
    }

    pub fn join_ty(&mut self, lhs: &TyRef, rhs: &TyRef) -> TyRef{
        if lhs.as_ptr() != rhs.as_ptr(){
            self.err_ty()
        }else{
            lhs.clone()
        }
    }

    pub fn join_option_ty(&mut self, lhs: &Option<TyRef>, rhs: &Option<TyRef>) -> Option<TyRef>{
        Some(if let Some(lhs_ty) = lhs{
            if let Some(rhs_ty) = rhs{
                self.join_ty(lhs_ty, rhs_ty)
            }else{
                lhs_ty.clone()
            }
        }else if let Some(rhs_ty) = rhs{
            rhs_ty.clone()
        }else {
            return None
        })
        //RefCell::borrow_mut(&mut self.ty_table).decl_ty(decl)
    }

    pub fn infer_or_unit(&mut self, ty: &Option<Box<ASTTy>>) -> TyRef{
        if let Some(return_ty) = ty{
            self.infer(return_ty)
        }else{
            self.prim_ty(PrimTy::Unit)
        }
    }
}

impl Visitor for TypeChecker {
    fn enter_decl(&mut self, decl: &mut Decl) {
        match &mut decl.kind {
            DeclKind::StructDecl(_) |
            DeclKind::MemberDecl(_) =>{
                self.decl_ty(decl as *const Decl);
            },
            DeclKind::FnDecl(fn_decl) => {
                let ret_ty = self.infer_or_unit(&fn_decl.ret_ty);
                /*let param_tys = Vec::new();
                for param in fn_decl.params{
                    param_tys.push(param.ty);
                }*/

                decl.ty = Some(Rc::new(RefCell::new(Ty::Fn(FnTy{ params: vec![], ret_ty: Some(ret_ty.clone()) }))));
                fn_decl.body.ty = Some(ret_ty);
            },
            _ => return
        }
    }

    fn enter_expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
            //Expr::Literal(lit) => self.ty_table.prim_ty(lit.
            ExprKind::Infix(infix) => {
                match infix.op {
                    Op::And | Op::Or => {
                        let bool = self.prim_ty(PrimTy::Bool);
                        self.coerce(&mut infix.lhs, &bool);
                        self.coerce(&mut infix.rhs, &bool);
                        self.coerce(expr, &bool);
                    },
                    Op::Eq | Op::Ne => {
                        self.coerce(expr, &self.prim_ty(PrimTy::Bool))
                    },
                    Op::Add | Op::Sub => {
                        let join_option_ty = self.join_option_ty(&infix.lhs.ty, &expr.ty);
                        let join_option_ty = self.join_option_ty(&join_option_ty, &infix.rhs.ty);

                        if let Some(join_ty) = join_option_ty{
                            self.coerce(&mut infix.lhs, &join_ty);
                            self.coerce(&mut infix.rhs, &join_ty);
                            self.coerce(expr, &join_ty);
                        }
                    },
                    _ => return
                }
            },
            ExprKind::Literal(lit) => {
                match lit {
                    Literal::Str(_) => self.coerce(expr, &self.prim_ty(PrimTy::Str)),
                    Literal::Bool(_) => self.coerce(expr, &self.prim_ty(PrimTy::Bool)),
                    Literal::Char(_) => self.coerce(expr, &self.prim_ty(PrimTy::Char)),
                    _ => {}
                }

                println!("{:?} !!!", expr.ty);
            },
            ExprKind::Ident(ident_expr) => {
                if let Some(ty) = &expr.ty{
                    println!("{:?} ----", ty);
                    if let Some(decl) = ident_expr.ident_use.decl{
                        self.coerce_decl(unsafe{&mut *(decl as *mut Decl)}, ty);
                    }
                }
            },
            ExprKind::Block(block) => {
                if let Some(ret_ty) = &expr.ty{
                    let last_stmt_opt = block.stmts.last_mut();

                    if let Some(last_stmt) = last_stmt_opt{
                        if let Stmt::Expr(expr_stmt) = last_stmt.as_mut(){
                            self.coerce(&mut expr_stmt.expr, ret_ty);
                        }
                    }
                }
            }
            _ => return
        }
    }

    fn enter_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Let(let_expr) =>{
                if let Some(init) = &mut let_expr.init{
                    if let DeclKind::LocalDecl(local_decl) = &let_expr.local_decl.kind{
                        if let Some(ast_ty) = &local_decl.ast_type{
                            init.ty = Some(self.infer(ast_ty));
                        }
                    }
                }
            },
            _ => return
        }
    }
}
