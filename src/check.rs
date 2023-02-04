use std::borrow::{Borrow, BorrowMut};
use std::cell::{Cell, Ref, RefCell};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::hash::Hash;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use std::sync::atomic::AtomicPtr;

use strum::IntoEnumIterator;

use crate::{ast, Decl, Module, Sym};
use crate::ast::{ASTTy, Block, DeclKind, Expr, ExprKind, IdentExpr, Literal, LocalDecl, Op, PrimTy, Stmt, StructDecl, ASTStructTy};
use crate::check::Ty::Prim;
use crate::visitor::Visitor;

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct TyRef{
    ptr : *const Ty
}

impl Debug for TyRef{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", "sss")
    }
}

impl Deref for TyRef {
    type Target = Ty;

    fn deref(&self) -> &Self::Target {
        unsafe {&*self.ptr}
    }
}

impl DerefMut for TyRef {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {&mut *(self.ptr as *mut Ty)}
    }
}

impl TyRef {
    pub fn from( val : &Ty ) -> TyRef{
        TyRef{
            ptr: val
        }
    }
}

#[derive(Debug)]
pub struct StructTy {
    pub name: Sym,
    pub members : Vec<TyRef>
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
    prim_tys: HashMap<PrimTy, Box<Ty>>,
    decl_tys: HashMap<*const Decl, Box<Ty>>,
    err_ty : Box<Ty>
}

impl TyTable {
    pub fn prim_ty(&self, ty: PrimTy) -> TyRef {
        TyRef::from(self.prim_tys.get(&ty).unwrap())
    }
    pub fn err_ty(&self) -> TyRef {
        TyRef::from(&self.err_ty)
    }

    fn struct_ty(&mut self, ty: &ASTStructTy) -> TyRef {
        if let Some(decl) = ty.ident_use.decl {
            self.decl_ty(decl).unwrap()
        } else {
            unreachable!()
        }
    }

    pub fn infer_or_unit(&mut self, ty: &Option<Box<ASTTy>>) -> TyRef{
        if let Some(return_ty) = ty{
            self.infer(return_ty)
        }else{
            self.prim_ty(PrimTy::Unit)
        }
    }

    fn infer(&mut self, ast_ty: &ASTTy) -> TyRef {
        match ast_ty {
            ASTTy::Prim(prim) => self.prim_ty(prim.clone()),
            ASTTy::Struct(struct_ty) => self.struct_ty(struct_ty),
            _ => unreachable!()
        }
    }

    pub fn decl_ty(&mut self, decl_ptr: *const Decl) -> Option<TyRef> {
        let mut_decl_ptr = decl_ptr as *mut Decl;
        let decl = unsafe { &mut *mut_decl_ptr };

        if let Some(ty) = &decl.ty {
            return Some(*ty);
        }

        let result = match &mut decl.kind {
            DeclKind::StructDecl(struct_decl) => {
                let struct_ty = Box::new(Ty::Struct(StructTy{ name: decl.ident.sym, members: Vec::new() }));
                decl.ty = Some(TyRef::from(&*struct_ty));

                for member in &struct_decl.members {
                    let member_ty = self.decl_ty(member.as_ref() as *const Decl).unwrap();
                    let mut struct_ty = decl.ty.unwrap();

                    if let Ty::Struct(struct_ty) = &mut *struct_ty{
                        struct_ty.members.push(member_ty);
                    }
                }

                self.decl_tys.insert(decl, struct_ty);
                decl.ty
            },
            DeclKind::MemberDecl(member_decl) => {
                Some(self.infer(&member_decl.ast_ty))
            },
            DeclKind::LocalDecl(local_decl) => {
                if let Some(ast_ty) = &local_decl.ast_ty {
                    let result = self.infer(ast_ty);
                    Some(result.clone())
                }else{
                    None
                }
            },
            DeclKind::FnDecl(fn_decl) => {
                let mut param_tys = Vec::new();
                for mut param in &mut fn_decl.params{
                    self.decl_ty(&**param);
                    param_tys.push(param.ty.clone().unwrap());
                }

                let ret_ty = self.infer_or_unit(&fn_decl.ret_ty);

                let fn_ty = Box::new(Ty::Fn(FnTy{ params: param_tys, ret_ty: Some(ret_ty.clone()) }));
                let result = Some(TyRef::from(&*fn_ty));
                fn_decl.body.ty = Some(ret_ty);
                self.decl_tys.insert(decl, fn_ty);
                result
            }
            _ => None
        };
        decl.ty = result;
        result
    }

    pub fn new() -> TyTable {
        let mut prim_tys = HashMap::new();

        for ty in PrimTy::iter() {
            prim_tys.insert(ty, Box::new(Ty::Prim(ty)));
        }

        TyTable {
            prim_tys,
            decl_tys: HashMap::new(),
            err_ty : Box::new(Ty::Err)
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

    pub fn coerce_option(&mut self, expr: &mut Expr, ty: &Option<TyRef>){
        if let Some(some_ty) = &ty{
            self.coerce(expr, some_ty);
        }
    }

    pub fn coerce_option_decl(&mut self, decl: &mut Decl, ty: &Option<TyRef>){
        if let Some(some_ty) = &ty{
            self.coerce_decl(decl, some_ty);
        }
    }

    pub fn coerce(&mut self, expr: &mut Expr, ty: &TyRef){
        if let Some(expr_ty) = &expr.ty{
            if expr_ty != ty {
                expr.ty = Some(self.err_ty())
            }
        }else{
            expr.ty = Some(ty.clone());
        }
    }

    pub fn coerce_decl(&mut self, decl: &mut Decl, ty: &TyRef){
        if let Some(decl_ty) = &decl.ty{
            if decl_ty != ty {
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

    pub fn decl_ty(&mut self, decl: *const Decl) -> Option<TyRef>{
        RefCell::borrow_mut(&mut self.ty_table).decl_ty(decl)
    }

    pub fn join_ty(&mut self, lhs: &TyRef, rhs: &TyRef) -> TyRef{
        if lhs != rhs{
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
    }
}

impl Visitor for TypeChecker {
    fn enter_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Let(let_stmt) =>{
                if let DeclKind::LocalDecl(local_decl) = &let_stmt.local_decl.kind {
                    if let Some(ast_ty) = &local_decl.ast_ty {
                        let ty = self.infer(ast_ty);
                        self.coerce_decl(&mut let_stmt.local_decl, &ty);
                    }
                }

                if let Some(init) = &mut let_stmt.init{
                    self.coerce_option(init, &let_stmt.local_decl.ty);
                    self.coerce_option_decl(&mut let_stmt.local_decl, &init.ty);
                }
            }
            Stmt::Expr(expr_stmt) =>{
                self.visit_expr(&mut expr_stmt.expr);
            },
            _ => {}
        }
    }

    fn visit_decl(&mut self, decl: &mut Decl) {
        match &decl.kind{
            DeclKind::MemberDecl(_) => {
                self.decl_ty(decl as *const Decl);
            },
            DeclKind::StructDecl(_) => {
                self.decl_ty(decl as *const Decl);

                if let DeclKind::StructDecl(struct_decl) = &mut decl.kind{
                    for member in &mut struct_decl.members{
                        self.visit_decl(member);
                    }
                }
            },
            DeclKind::FnDecl(_) => {
                if let DeclKind::FnDecl(fn_decl) = &mut decl.kind{
                    for mut param in &mut fn_decl.params{
                        self.visit_decl(param);
                    }
                }

                self.decl_ty(decl as *const Decl);

                if let DeclKind::FnDecl(fn_decl) = &mut decl.kind{
                    self.visit_expr(&mut fn_decl.body);
                }
            },
            _ => {}
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
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
                    Op::Add | Op::Sub | Op::Mul | Op::Div => {

                        let join_option_ty = self.join_option_ty(&expr.ty, &infix.lhs.ty );
                        let join_option_ty = self.join_option_ty(&join_option_ty, &infix.rhs.ty);

                        if let Some(join_ty) = &join_option_ty{
                            self.coerce(&mut infix.lhs, &join_ty);
                            self.coerce(&mut infix.rhs, &join_ty);
                        }

                        self.visit_expr(&mut infix.lhs);
                        self.visit_expr(&mut infix.rhs);

                        if let Some(join_ty) = &join_option_ty{
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
            },
            ExprKind::Ident(ident_expr) => {
                if let Some(ty) = &expr.ty{
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

                    for stmt in block.stmts.iter_mut().rev(){
                        self.visit_stmt(stmt);
                    }
                }
            }
            _ => return
        }
    }
}
