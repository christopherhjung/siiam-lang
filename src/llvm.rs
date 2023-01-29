use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use crate::{Decl, Module, Sym, SymTable, Visitor};
use crate::ast::{DeclKind, Expr, ExprKind, Literal, Op, PrimTy, Stmt};
use crate::check::{Ty, TyRef};

pub struct Value{
    content : String
}

pub struct Block{
    name: String,
    phis: Vec<String>,
    instrs: Vec<String>
}

pub struct Function{
    name: String,
    blocks: HashMap<String, Rc<RefCell<Block>>>
}

pub struct CodeGen {
    fns: HashMap<String, Box<Function>>,
    //expr2value: HashMap<*const Expr, String>,
    decl2ptr: HashMap<*const Decl, String>,
    sym_table : Rc<RefCell<SymTable>>,
    current_block : Option<Rc<RefCell<Block>>>,
    current_value : Option<String>,
    current_index : usize
}

impl CodeGen {
    fn name(&self, sym: Sym) -> String{
        let sym_table = self.sym_table.borrow();
        let name = SymTable::get(&sym_table, sym);
        name
    }

    pub fn print(&mut self, instr: String){
        let mut current_block = RefCell::borrow_mut(self.current_block.as_mut().unwrap());
        current_block.instrs.push(instr);
    }
/*
    pub fn map_expr(&mut self, expr: &mut Expr, val: &Option<String>){
        if val.is_some(){
            self.expr2value.insert(expr as *const Expr, val.clone().unwrap());
        }
    }
*/
    pub fn map_decl(&mut self, decl: &Decl, val: String){
        self.decl2ptr.insert(decl as *const Decl, val.clone());
    }

    pub fn remit(&mut self, expr: &mut Expr) -> Option<String>{
/*
        let cached = self.expr2value.get(&(expr as *const Expr));

        if let Some(cached_val) = cached{
            return Some(cached_val.clone());
        }*/

        self.current_value = None;
        self.visit_expr(expr);
        let result = self.current_value.take();
        //self.map_expr(expr, &result);
        result
    }

    pub fn emit_ty(&mut self, ty_ref: &TyRef ) -> String{
        let ty = RefCell::borrow(ty_ref);

        String::from(match *ty {
            Ty::Prim(prim_ty) => {
                match prim_ty {
                    PrimTy::I32 => "i32",
                    PrimTy::I64 => "i64",
                    PrimTy::F32 => "f32",
                    PrimTy::F64 => "f64",
                    _ => panic!()
                }
            },
            _ => panic!()
        })
    }

    pub fn new_value(&mut self) -> String{
        let index = self.current_index;
        self.current_index += 1;
        let val = format!("%{}", index);
        //assert!(self.current_value.is_none());
        self.current_value = Some(val.clone());
        val
    }

    pub fn new(sym_table: Rc<RefCell<SymTable>>) -> CodeGen {
        CodeGen {
            fns : HashMap::new(),
            //expr2value: HashMap::new(),
            decl2ptr: HashMap::new(),
            current_block: None,
            current_value: None,
            current_index: 0,
            sym_table
        }
    }

    pub fn emit(&self){
        for (key, fun) in &self.fns{
            println!("{}", key);
            Self::emit_fn(fun);
        }
    }

    pub fn emit_fn( fun : &Box<Function> ){
        for (key, block) in &fun.blocks{
            Self::emit_block(block);
        }
    }

    pub fn emit_block( block : &RefCell<Block> ){
        let block = block.borrow();
        println!("{}", block.name);
        for instr in &block.instrs{
            println!("{}", instr);
        }
    }
}

impl Visitor for CodeGen {
    fn visit_decl(&mut self, decl: &mut Decl) {
        let name = self.name(decl.ident.sym);
        match &mut decl.kind {
            DeclKind::FnDecl(fn_decl) => {
                let entry_name = String::from("entry");
                let mut fnc = Box::new(Function{ name: name.clone(), blocks: HashMap::new() });
                let entry_block = Rc::new(RefCell::new(Block{
                    name : entry_name.clone(),
                    phis: Vec::new(),
                    instrs: Vec::new()
                }));
                self.current_block = Some(Rc::clone(&entry_block));

                for param in &mut fn_decl.params{
                    let ty = self.emit_ty(param.ty.as_ref().unwrap());
                    let val = self.new_value();
                    self.print(format!("{val} = alloca {ty}, align 8"));
                    self.map_decl(param, val);
                }

                fnc.blocks.insert(entry_name, entry_block);
                self.fns.insert(name, fnc);
                self.visit_expr(&mut fn_decl.body);
            },
            _ => {}
        }
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Expr(expr_stmt) => {
                self.visit_expr(&mut expr_stmt.expr);
            },
            Stmt::Let(let_stmt) =>{
                self.visit_expr( let_stmt.init.as_mut().unwrap());

                let ty = self.emit_ty(let_stmt.local_decl.ty.as_ref().unwrap());
                let val = self.new_value();
                self.print(format!("{val} = alloca {ty}, align 8"));
                self.map_decl(&let_stmt.local_decl, val.clone());

                if let Some(init) = &mut let_stmt.init{
                    let init_val = self.remit(init).unwrap();
                    self.print(format!("store {ty} {init_val}, {ty}* {val}, align 8"));
                }
            }
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
            ExprKind::Literal(lit) => {
                self.current_value = match lit {
                    Literal::Int(int_val) => Some(int_val.to_string()),
                    _ => None
                }
            },
            ExprKind::Infix(infix_expr) => {
                let ty = self.emit_ty(expr.ty.as_ref().unwrap());
                let lhs_val = self.remit(&mut infix_expr.lhs).unwrap();
                let rhs_val = self.remit(&mut infix_expr.rhs).unwrap();
                let val = self.new_value();
                let op = infix_expr.op.llvm_op();
                self.print(format!("{val} = {op} {ty} {lhs_val}, {rhs_val}"));
            },
            ExprKind::Ident(ident_expr) => {
                let decl =  unsafe{&*ident_expr.ident_use.decl.unwrap()};
                let ty = self.emit_ty(decl.ty.as_ref().unwrap());
                let val = self.new_value();
                let ptr = self.decl2ptr.get(&ident_expr.ident_use.decl.unwrap()).unwrap();
                self.print(format!("{val} = load {ty} ({ty})*, {ty} ({ty})** {ptr}, align 8"));
            },
            ExprKind::Block(block_expr) => {
                for stmt in &mut block_expr.stmts{
                    self.visit_stmt(stmt);
                }
            },
            _ => {}
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