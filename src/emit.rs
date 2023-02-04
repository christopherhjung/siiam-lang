use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use crate::{Array, Builder, Decl, Module, Sym, SymTable, Visitor, World};
use crate::ast::{DeclKind, Expr, ExprKind, Literal, Op, PrimTy, Stmt};
use crate::check::{Ty, TyRef};
use crate::def::Def;

pub struct HirEmitter {
    fns: HashMap<String, Def>,
    //expr2value: HashMap<*const Expr, String>,
    //decl2ptr: HashMap<*const Decl, Def>,
    decl2val: HashMap<*const Decl, Def>,
    sym_table : Rc<RefCell<SymTable>>,
    current_index : usize,
    world : World,
    b : Builder,
    curr_mem: Option<Def>,
    cur_bb: Option<Def>
}

impl HirEmitter {
    pub fn get_fn(&mut self, name : &String) -> Def{
        let def = self.fns.get(name).unwrap();
        self.b.construct_def(def)
    }

    fn name(&self, sym: Sym) -> String{
        let sym_table = self.sym_table.borrow();
        let name = SymTable::get(&sym_table, sym);
        name
    }

    fn map_decl(&mut self, decl: &Decl, val: Def){
        self.decl2val.insert(decl as *const Decl, val);
    }

    fn emit_opt_ty(&mut self, ty_ref: Option<TyRef> ) -> Def{
        match ty_ref {
            Some(ty) => self.emit_ty(ty),
            None => self.b.ty_unit()
        }
    }

    fn emit_ty(&mut self, ty_ref: TyRef ) -> Def{
        match &*ty_ref {
            Ty::Prim(prim_ty) => {
                match prim_ty {
                    PrimTy::I32 => self.b.ty_int(32),
                    PrimTy::I64 => self.b.ty_int(64),
                    PrimTy::F32 => self.b.ty_real(32),
                    PrimTy::F64 => self.b.ty_real(64),
                    _ => panic!()
                }
            }
            Ty::Fn(fn_ty) => {
                let arr = Array::new(fn_ty.params.len());
                for (idx, param) in fn_ty.params.iter().enumerate(){
                    let ty = self.emit_ty(*param);
                    arr.set(idx, ty.link);
                }

                let arg_ty = self.b.sigma_arr(arr);
                let ret_ty = self.emit_opt_ty(fn_ty.ret_ty);

                self.b.pi(&arg_ty, &ret_ty)
            }
            Ty::Struct(struct_ty) => {
                self.b.bot()
            }
            _ => {
                println!("{:?}", *ty_ref);
                panic!()
            }
        }
    }

    pub fn new(sym_table: Rc<RefCell<SymTable>>, world: World) -> HirEmitter {
        let world= World::new();
        let builder = world.builder();
        HirEmitter {
            fns : HashMap::new(),
            //expr2value: HashMap::new(),
            decl2val: HashMap::new(),
            current_index: 0,
            sym_table,
            world,
            b: builder,
            curr_mem: None,
            cur_bb: None
        }
    }

    fn handle_mem_result(&mut self, def : &Def) -> Def{
        let zero = self.b.lit_idx(2, 0);
        let one = self.b.lit_idx(2, 1);
        let mem = self.b.extract(def, &zero);
        let res = self.b.extract(def, &one);
        self.curr_mem = Some(mem);
        res
    }

    fn mem(&self) -> &Def{
        match &self.curr_mem {
            Some(mem) => mem,
            None => panic!()
        }
    }

    pub fn emit_module(&mut self, module: &Module){
        for mut item in &module.items{
            self.emit_decl(item);
        }
    }

    fn emit_decl(&mut self, decl: &Decl) -> Def {
        let name = self.name(decl.ident.sym);
        match &decl.kind {
            DeclKind::FnDecl(fn_decl) => {
                let fnc_ty = self.emit_ty(decl.ty.unwrap());
                
                let fnc = self.b.lam(&fnc_ty);
                let res = fnc.clone();
                let var = self.b.var(&fnc);

                let mut idx = 0;
                let arity = self.b.ty_idx(fn_decl.params.len() as u32);

                for param in &fn_decl.params{
                    let pos = self.b.lit(idx, &arity);
                    let val = self.b.extract(&var, &pos);
                    self.map_decl(param, val);
                    idx+=1;
                }

                self.fns.insert(name, fnc);
                let body = self.remit_expr(&fn_decl.body);
                self.b.set_body(&res, &body);
                res
            }
            _ => self.b.bot()
        }
    }

    fn emit_stmt(&mut self, stmt: &Stmt) -> Def {
        match stmt {
            Stmt::Expr(expr_stmt) => {
                self.remit_expr(&expr_stmt.expr)
            },
            Stmt::Let(let_stmt) =>{
                //let ty = self.emit_ty(let_stmt.local_decl.ty.unwrap());

                if let Some(init) = &let_stmt.init{
                    let init_val = self.remit_expr(init);
                    self.map_decl(&let_stmt.local_decl, init_val);
                }else{
                    panic!()
                }

                self.b.unit()
            }
        }
    }

    fn remit_expr(&mut self, expr: &Expr) -> Def {
        match &expr.kind {
            ExprKind::Literal(lit) => {
                match lit {
                    Literal::Int(int_val) => self.b.lit_int(32, *int_val as u32),
                    _ => panic!()
                }
            },
            ExprKind::Infix(infix_expr) => {
                let ty = self.emit_ty(expr.ty.unwrap());
                let lhs_val = self.remit_expr(&infix_expr.lhs);
                let rhs_val = self.remit_expr(&infix_expr.rhs);

                match infix_expr.op {
                    Op::Add => self.b.add(&lhs_val, &rhs_val),
                    Op::Sub => self.b.sub(&lhs_val, &rhs_val),
                    Op::Mul => self.b.mul(&lhs_val, &rhs_val),
                    Op::Div => self.b.div(&lhs_val, &rhs_val),
                    _ => panic!()
                }
            },
            ExprKind::Ident(ident_expr) => {
                let decl =  unsafe{&*ident_expr.ident_use.decl.unwrap()};
                let ty = self.emit_ty(decl.ty.unwrap());
                let val = self.decl2val.get(&ident_expr.ident_use.decl.unwrap()).unwrap();
                val.clone()
            },
            ExprKind::Block(block_expr) => {
                let mut val = self.b.bot();
                for stmt in &block_expr.stmts{
                    val = self.emit_stmt(stmt);
                }
                val
            },
            _ => {
                println!("{:?}", expr);
                panic!()
            }
        }
    }
}
