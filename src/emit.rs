use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use crate::{Array, Decl, Module, Sym, SymTable, Visitor, World};
use crate::ast::{DeclKind, Expr, ExprKind, IfExpr, Literal, Op, PrimTy, Stmt, WhileExpr};
use crate::builder::Builder;
use crate::check::{Ty, TyRef};
use crate::def::Def;

#[derive(PartialEq, Eq)]
enum HirEmitterMode{
    Decl,
    Def
}

pub struct HirEmitter {
    fns: HashMap<String, Def>,
    structs: HashMap<String, Def>,
    struct2def: HashMap<TyRef, Def>,
    decl2def: HashMap<*const Decl, Def>,
    sym_table : Rc<RefCell<SymTable>>,
    world : World,
    b : Builder,
    curr_mem: Option<Def>,
    cur_bb: Option<Def>,
    mode : HirEmitterMode
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
        let name = self.name(decl.ident.sym);
        match &decl.kind {
            DeclKind::FnDecl(_) => {
                self.fns.insert(name, val.clone());
            }
            DeclKind::StructDecl(_) => {
                self.structs.insert(name, val.clone());
            }
            _ => {}
        }

        self.decl2def.insert(decl as *const Decl, val);
    }

    fn get_decl(&self, decl: &Decl) -> Option<Def>{
        self.decl2def.get(&(decl as *const Decl)).cloned()
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
                if let Some(def) = self.struct2def.get(&ty_ref){
                    def.clone()
                }else{
                    let member_tys = Array::new(struct_ty.members.len());
                    let placeholder = self.b.placeholder();
                    for idx in 0 .. member_tys.len(){
                        member_tys.set(idx, placeholder.link);
                    }

                    let sigma = self.b.sigma_arr(member_tys);
                    self.struct2def.insert(ty_ref, sigma.clone());
                    //self.structs.insert(self.name(struct_ty.name), sigma.clone());

                    for (idx, member_ty) in struct_ty.members.iter().enumerate(){
                        let mem_ty_def = self.emit_ty(*member_ty);
                        sigma.set_op(idx, &mem_ty_def);
                    }

                    sigma
                }
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
            structs : HashMap::new(),
            decl2def: HashMap::new(),
            struct2def: HashMap::new(),
            sym_table,
            world,
            b: builder,
            curr_mem: None,
            cur_bb: None,
            mode: HirEmitterMode::Decl
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
        self.mode = HirEmitterMode::Decl;
        for mut item in &module.items{
            self.emit_decl(item);
        }
        self.mode = HirEmitterMode::Def;
        for mut item in &module.items{
            self.emit_decl(item);
        }
    }

    fn emit_decl(&mut self, decl: &Decl){
        if self.mode == HirEmitterMode::Decl{
            match &decl.kind {
                DeclKind::FnDecl(_) => {
                    let fnc_ty = self.emit_ty(decl.ty.unwrap());
                    let fnc = self.b.lam(&fnc_ty);
                    self.map_decl(decl, fnc);
                }
                DeclKind::StructDecl(_) => {
                    if let Some(struct_ty) = decl.ty{
                        let struct_ty_def = self.emit_ty(struct_ty);
                        self.map_decl(decl, struct_ty_def);
                    }else{
                        self.b.bot();
                    }
                }
                _ => {}
            }
        }else{
            match &decl.kind {
                DeclKind::FnDecl(fn_decl) => {
                    let fnc = if let Some(fnc) = self.get_decl(decl){
                        fnc
                    }else{
                        let fnc_ty = self.emit_ty(decl.ty.unwrap());
                        let fnc = self.b.lam(&fnc_ty);
                        self.map_decl(decl, fnc.clone());
                        fnc
                    };

                    let var = self.b.var(&fnc);
                    let arity = self.b.ty_idx(fn_decl.params.len() as u32);

                    for (idx, param) in fn_decl.params.iter().enumerate(){
                        let pos = self.b.lit(idx as u32, &arity);
                        let val = self.b.extract(&var, &pos);
                        self.map_decl(param, val);
                    }

                    self.cur_bb = Some(fnc.clone());
                    let body = self.remit_expr(&fn_decl.body);

                    let ret = self.b.ret(&fnc);
                    let ret_app = self.b.app(&ret, &body);
                    self.b.set_body(&self.cur_bb.clone().unwrap(), &ret_app);
                }
                _ => {}
            }
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
            }
            ExprKind::Infix(infix_expr) => {
                //let ty = self.emit_ty(expr.ty.unwrap());
                let lhs_val = self.remit_expr(&infix_expr.lhs);
                let rhs_val = self.remit_expr(&infix_expr.rhs);

                match &infix_expr.op {
                    Op::Add => self.b.add(&lhs_val, &rhs_val),
                    Op::Sub => self.b.sub(&lhs_val, &rhs_val),
                    Op::Mul => self.b.mul(&lhs_val, &rhs_val),
                    Op::Div => self.b.div(&lhs_val, &rhs_val),
                    Op::Gt => self.b.gt(&lhs_val, &rhs_val),
                    Op::Ne => self.b.ne(&lhs_val, &rhs_val),
                    op => {
                        println!("{:?}", op);
                        panic!()
                    }
                }
            }
            ExprKind::Ident(ident_expr) => {
                let decl =  unsafe{&*ident_expr.ident_use.decl.unwrap()};
                //let ty = self.emit_ty(decl.ty.unwrap());
                let val = self.decl2def.get(&ident_expr.ident_use.decl.unwrap()).unwrap();
                val.clone()
            }
            ExprKind::Block(block_expr) => {
                let mut val = self.b.bot();
                for stmt in &block_expr.stmts{
                    val = self.emit_stmt(stmt);
                }
                val
            }
            ExprKind::FnCall(call_expr) => {
                let callee = self.remit_expr(&*call_expr.callee);

                let arr = Array::new(call_expr.args.len());
                for (idx, arg) in call_expr.args.iter().enumerate(){
                    let arg_def = self.remit_expr(arg);
                    arr.set(idx, arg_def.link);
                }

                let arg_def = self.b.tuple_arr(arr);
                self.b.app(&callee, &arg_def)
            }
            ExprKind::If(if_expr) => {
                self.remit_if_expr(expr, if_expr)
            }
            ExprKind::While(while_expr) => {
                self.remit_while_expr(expr, while_expr)
            }
            _ => {
                println!("{:?}", expr);
                panic!()
            }
        }
    }

    fn remit_if_expr(&mut self, expr: &Expr, if_expr: &IfExpr) -> Def{
        let ty_unit = self.b.ty_unit();
        let unit =  self.b.unit();
        let bot =  self.b.bot();

        let if_ty = self.b.pi(&unit, &bot);

        let true_fn = self.b.lam(&if_ty);
        let false_fn = self.b.lam(&if_ty);

        let cmp = self.remit_expr(&if_expr.condition);
        let branches = self.b.tuple([&true_fn, &false_fn]);
        let callee = self.b.extract(&branches, &cmp);

        let app = self.b.app(&callee, &unit);
        self.finish_bb(&app, &true_fn);

        let left_val = self.remit_expr(&if_expr.true_branch);

        if let Some(false_branch) = &if_expr.false_branch{
            let val_ty = self.emit_opt_ty(expr.ty);
            let join_ty = self.b.pi(&val_ty, &bot);
            let join_fn = self.b.lam(&join_ty);

            let app = self.b.app(&join_fn, &left_val);
            self.finish_bb(&app, &false_fn);

            let right_val = self.remit_expr(false_branch);
            let app = self.b.app(&join_fn, &right_val);
            self.finish_bb(&app, &join_fn);

            let join_var = self.b.var(&join_fn);
            join_var
        }else{
            let app = self.b.app(&false_fn, &unit);
            self.finish_bb(&app, &false_fn);
            unit
        }
    }

    fn remit_while_expr(&mut self, expr: &Expr, while_expr: &WhileExpr) -> Def{
        let ty_unit = self.b.ty_unit();
        let unit =  self.b.unit();
        let bot =  self.b.bot();

        let while_ty = self.b.pi(&unit, &bot);

        let body_fn = self.b.lam(&while_ty);
        let exit_fn = self.b.lam(&while_ty);

        let cmp = self.remit_expr(&while_expr.condition);
        let branches = self.b.tuple([&body_fn, &exit_fn]);
        let callee = self.b.extract(&branches, &cmp);

        let app = self.b.app(&callee, &unit);
        self.finish_bb(&app, &body_fn);

        self.remit_expr(&while_expr.body);

        let cmp2 = self.remit_expr(&while_expr.condition);
        let branches2 = self.b.tuple([&body_fn, &exit_fn]);
        let callee2 = self.b.extract(&branches2, &cmp2);

        self.finish_bb(&callee2, &exit_fn);
        unit
    }

    fn finish_bb(&mut self, body: &Def, next: &Def){
        let prev = self.cur_bb.replace(next.clone());
        self.b.set_body(&prev.unwrap(), &body);
        self.cur_bb = Some(next.clone());
    }

    pub fn list(&mut self){
        println!("-------------------------------------------------");
        println!("Structs:");
        for (name, def) in &self.structs{
            let new = self.b.construct_def(&def);
            println!("    {} {:?}", name, new.sign())
        }
        println!("Functions:");
        for (name, def) in &self.fns{
            let new = self.b.construct_def(&def);
            println!("    {} {:?}", name, new.sign())
        }
        println!("-------------------------------------------------");
    }
}
