#![feature(hash_set_entry)]
#![feature(let_else)]
#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(map_try_insert)]

use std::borrow::BorrowMut;
use std::cell::{Cell, RefCell};
use std::ptr;
use std::rc::Rc;

use crate::ast::{Decl, Module};
use crate::bind::NameBinder;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::check::{TypeChecker, TyTable};
use crate::world::{Axiom, DepCheck, World};
use crate::llvm::CodeGen;
use crate::source::Source;
use crate::sym::*;
use crate::visitor::Visitor;
use crate::print::ProgramPrinter;
use crate::array::Array;

mod lexer;
mod source;
mod parser;
mod bind;
mod ast;
mod sym;
mod visitor;
mod check;
mod token;
mod print;
mod llvm;
mod world;
mod sign;
mod array;
mod def;
mod utils;

pub fn main() {
    let mut sym_table = Rc::new(RefCell::new(SymTable::new()));
    let mut source = Source::new(String::from("test.si"));
    let mut lexer = Lexer::new(source, sym_table.clone());

    let mut parser = Parser::new(lexer, sym_table.clone());
    let mut module = parser.parse_module();

    let mut binder = NameBinder::new();
    binder.resolve(&mut module);
    println!("{:#?}", module);

    let mut ty_table = Rc::new(RefCell::new(TyTable::new()));
    let mut checker = TypeChecker::new(ty_table);
    checker.visit_module(&mut module);

    let mut printer = ProgramPrinter::new(sym_table.clone());
    printer.visit_module(&mut module);
    println!("{}", printer.result);
/*
    let mut code_gen = CodeGen::new(sym_table);
    code_gen.visit_module(&mut module);
    code_gen.emit();*/

    let mut world = World::new_boxed();
/*
    let zero = world.lit_int(0);
    let one = world.lit_int(1);

    let int_ty = world.ty_int(32);
    let bot = world.bot();
    let pi = world.pi(int_ty, bot);*/

    let bot = world.bot();
    let mut cn = world.lam(bot);
    let var = world.var(cn);
    world.set_body(cn, var);

    println!("{:?}", cn.sign());

    let bot2 = world.bot();
    let mut cn2 = world.lam(bot2);
    let var2 = world.var(cn2);
    world.set_body(cn2, var2);

    println!("{:?}", cn.sign());
}




/*
use std::ptr::eq;
use std::rc::Rc;
use std::time::{Duration, Instant};
use hex::ToHex;
use rand::{distributions::Alphanumeric, Rng};
use sha2::{Sha256, Sha512, Sha384, Digest};

use sha256::digest;
use crate::hash::Hash;
*/

/*
struct Def{
    ops : Vec<Rc<Def>>,
    data : Vec<u8>
}

fn test(arr: &mut [u8; 4]){
    arr[0] = 92;
}

struct Node{
    value: i32,
    next: Link
}

type Link = Option<Box<Node>>;

struct List{
    head: Link
}

impl List{
    fn add( &mut self,  x : i32 ){
        let prev = self.head.take();
        self.head = Some(Box::new(Node { value: x, next: prev }))
    }
}

fn main() {


    let mut list = List {head : None};
    list.add(10);
    list.add(33);

    if let Some(x) = list.head {
        println!("{}", x.value);
    }


    /*let start = Instant::now();

    let hash = Hash::random();
    let hash2 = Hash::random();
    println!("{}", hash.toHex());
    println!("{}", hash2.toHex());
    println!("{}", hash == hash);
    println!("{}", hash == hash2);

    let mut a = Rc::new(Def{ops : vec!(), data : vec!()});
    let mut b = Rc::new(Def{ops : vec!(), data : vec!()});

    //6bbec93bef20083b489da2450e2e2b83714e9205
    //945a2f39ea3294a0a6f65c5083c4a8332ed5d712391ca61edb1100cf191f5809
    let duration = start.elapsed();
    println!("Time elapsed in expensive_function() is: {:?}", duration);*/
}
*/




/*
pub struct List<T> {
    head: Link<T>,
}

type Link<T> = Option<Box<Node<T>>>;

struct Node<T> {
    elem: T,
    next: Link<T>,
}


struct B{
    val : i32
}

struct A{
    b: B
}


impl<T> List<T> {
    pub fn new() -> Self {
        List { head: None }
    }

    pub fn push(&mut self, elem: T) {
        let new_node = Box::new(Node {
            elem: elem,
            next: self.head.take(),
        });

        self.head = Some(new_node);
    }

    pub fn pop(&mut self) -> Option<T> {
        self.head.take().map(|node| {
            self.head = node.next;
            node.elem
        })
    }
}

impl<T> Drop for List<T> {
    fn drop(&mut self) {
        let mut cur_link = self.head.take();
        while let Some(mut boxed_node) = cur_link {
            cur_link = boxed_node.next.take();
        }
    }
}
*/