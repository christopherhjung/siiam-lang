use std::borrow::{Borrow, BorrowMut};
use std::cell::{Cell, Ref, RefCell};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::rc::Rc;

use strum::IntoEnumIterator;

use crate::{ast, Decl, Module};
use crate::ast::{ASTTy, Block, DeclKind, Expr, ExprKind, IdentExpr, Literal, Op, PrimTy, Stmt, StructDecl, StructTy};
use crate::sema::Ty::Prim;
use crate::visitor::Visitor;

pub type TyRef = Rc<RefCell<Ty>>;

#[derive(Debug)]
pub enum Ty {
    Prim(PrimTy),
    Struct{members : Vec<TyRef>},
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

    fn struct_ty(&mut self, ty: &StructTy) -> TyRef {
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
                decl.ty = Some(Rc::new(RefCell::new(Ty::Struct { members: Vec::new() })));

                for member in &struct_decl.members {
                    let member_ty = self.decl_ty(member.as_ref() as *const Decl);
                    let mut struct_ty = RefCell::borrow_mut(decl.ty.as_ref().unwrap());

                    if let Ty::Struct { members } = &mut *struct_ty{
                        members.push(member_ty);
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
                fn_decl.body.ty = Some(self.infer_or_unit(&fn_decl.return_type));
            }
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
                        println!("{:?}", expr)
                        //self.coerce_option(expr, infix.lhs.ty);
                        //self.coerce_option(expr, infix.rhs.ty);
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
            ExprKind::Block(block) => {
                if let Some(ret_ty) = &expr.ty{
                    let last_stmt_opt = block.stmts.last_mut();

                    if let Some(last_stmt) = last_stmt_opt{
                        if let Stmt::Expr(expr_stmt) = last_stmt.as_mut(){
                            self.coerce(&mut expr_stmt.expr, ret_ty);
                        }
                    }
                }

                println!("{:?}", expr.ty);
            }
            _ => return
        }
    }
}
