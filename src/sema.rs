use std::cell::{Cell, Ref, RefCell};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::rc::Rc;

use strum::IntoEnumIterator;

use crate::{ast, Decl, Module};
use crate::ast::{Block, DeclKind, Expr, ExprKind, IdentExpr, Op, PrimTy, Stmt, StructDecl, StructTy};
use crate::visitor::Visitor;

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Ty {
    Prim(PrimTy),
    Struct,
    Err,
}

pub struct TyTable {
    prim_tys: HashMap<PrimTy, Rc<Ty>>,
}

impl TyTable {
    pub fn prim_ty(&mut self, ty: &PrimTy) -> Rc<Ty> {
        self.prim_tys.get(ty).unwrap().clone()
    }


    fn struct_ty(&mut self, ty: &StructTy) -> TyRef {
        if let Some(decl) = ty.ident_use.decl {
            self.decl_ty(decl)
        } else {
            unreachable!()
        }
    }
    /*
          fn infer(&mut self, ty: &ast::Ty) -> TyRef{
              match ty {
                  ast::Ty::Prim(prim) => self.prim_ty(prim),
                  ast::Ty::Struct(struct_ty) => self.struct_ty(struct_ty),
                  _ => unreachable!()
              }
          }

          fn decl_ty(&mut self, decl_ptr: *const Decl ) -> TyRef{
              let decl = unsafe {&*decl_ptr};

              if let Some(ty_ref) = self.decl_types.get(&decl_ptr) {
                  return ty_ref;
              }

              match &decl.kind {
                  DeclKind::StructDecl(struct_decl) => {
                      self.decl_types.insert(decl_ptr, Ty::Struct { members: Vec::new() });

                      for member in &struct_decl.members {
                          let member_ty = self.decl_ty(member.as_ref() as *const Decl);
                          let mut ty = self.decl_types.get_mut(&decl_ptr).unwrap();
                          let Ty::Struct{ members } = &mut ty else {unreachable!()};
                          members.push(member_ty);
                      }

                      self.decl_types.get_mut(&decl_ptr).unwrap()
                  },
                  DeclKind::MemberDecl(member_decl) => {
                      self.infer(&member_decl.ast_type)
                  },
                  _ => unreachable!()
              }
          }*/

    pub fn new() -> TyTable {
        let mut prim_tys = HashMap::new();

        for ty in PrimTy::iter() {
            prim_tys.insert(ty, Rc::new(Ty::Prim(ty)));
        }

        TyTable {
            prim_tys
        }
    }
}

struct SemanticAnalysis {
    ty_table: Rc<RefCell<TyTable>>,
}

impl Visitor for SemanticAnalysis {
    fn visit_expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
            //Expr::Literal(lit) => self.ty_table.prim_ty(lit.
            ExprKind::Infix(infix) => {
                match infix.op {
                    Op::And | Op::Or => {
                        //self.ty_table.set_ty(PrimTy::Bool);
                        //self.ty_table.infer(infix, &infix.lhs);
                        //self.ty_table.infer(infix, &infix.rhs);
                    }
                    _ => return
                }
            }
            _ => return
        }
    }
}
