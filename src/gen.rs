use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use crate::{Builder, Decl, Module, Sym, SymTable, Visitor, World};
use crate::ast::{DeclKind, Expr, ExprKind, Literal, Op, PrimTy, Stmt};
use crate::check::{Ty, TyRef};
use crate::def::Def;

pub struct CodeGen {
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

impl CodeGen {
    fn name(&self, sym: Sym) -> String{
        let sym_table = self.sym_table.borrow();
        let name = SymTable::get(&sym_table, sym);
        name
    }
    /*
        pub fn map_expr(&mut self, expr: &mut Expr, val: &Option<String>){
            if val.is_some(){
                self.expr2value.insert(expr as *const Expr, val.clone().unwrap());
            }
        }
    */
    pub fn map_decl(&mut self, decl: &Decl, val: Def){
        self.decl2val.insert(decl as *const Decl, val);
    }

    pub fn emit_ty(&mut self, ty_ref: TyRef ) -> Def{
        match *ty_ref {
            Ty::Prim(prim_ty) => {
                match prim_ty {
                    PrimTy::I32 => self.b.ty_int(32),
                    PrimTy::I64 => self.b.ty_int(64),
                    PrimTy::F32 => self.b.ty_real(32),
                    PrimTy::F64 => self.b.ty_real(64),
                    _ => panic!()
                }
            },
            _ => panic!()
        }
    }

    pub fn new(sym_table: Rc<RefCell<SymTable>>) -> CodeGen {
        let world= World::new();
        let builder = world.builder();
        CodeGen {
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

    fn emit_module(&mut self, module: &Module){
        for mut item in &module.items{
            self.emit_decl(item)
        }
    }

    fn emit_decl(&mut self, decl: &Decl) {
        let name = self.name(decl.ident.sym);
        match &decl.kind {
            DeclKind::FnDecl(fn_decl) => {
                let fnc_ty = self.emit_ty(decl.ty.unwrap());
                
                let fnc = self.b.lam(&fnc_ty);
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
                self.remit_expr(&fn_decl.body);
            },
            _ => {}
        }
    }

    fn emit_stmt(&mut self, stmt: &Stmt) -> Def {
        
        match stmt {
            Stmt::Expr(expr_stmt) => {
                self.remit_expr(&expr_stmt.expr)
            },
            Stmt::Let(let_stmt) =>{
                self.remit_expr( let_stmt.init.as_ref().unwrap());

                let ty = self.emit_ty(let_stmt.local_decl.ty.unwrap());

                if let Some(init) = &let_stmt.init{
                    let init_val = self.remit_expr(init);
                    self.map_decl(&let_stmt.local_decl, init_val);
                }else{
                    panic!()
                }

                self.b.bot()
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

                self.b.add(&lhs_val, &rhs_val)
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
            _ => panic!()
        }
    }
}

impl Op{
    fn llvm_op(&self) -> String{
        String::from(match self {
            Op::Add => "add",
            Op::Sub => "sub",
            Op::Mul => "mul",
            Op::Div => "div",
            _ => panic!()
        })
    }
}