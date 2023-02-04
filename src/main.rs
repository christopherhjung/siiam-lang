#![feature(hash_set_entry)]
#![feature(let_else)]
#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(map_try_insert)]

use std::borrow::BorrowMut;
use std::cell::{Cell, RefCell};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;
use std::time::{Duration, Instant};
use sha2::digest::{FixedOutput, Update};
use sha2::{Digest, Sha224, Sha256, Sha512};

use crate::ast::{Decl, Module};
use crate::bind::NameBinder;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::check::{TypeChecker, TyTable};
use crate::world::{Axiom, World, WorldImpl};
use crate::source::Source;
use crate::sym::*;
use crate::visitor::Visitor;
use crate::print::ProgramPrinter;
use crate::array::Array;
use crate::data::Data;
use crate::emit::HirEmitter;
use crate::sign::Signature;

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
mod world;
mod sign;
mod array;
mod def;
mod utils;
mod data;
mod emit;
mod builder;
mod tools;

pub fn main() {

    let mut sym_table = Rc::new(RefCell::new(SymTable::new()));

    let mut source = Source::new(String::from("test.si"));
    let mut lexer = Lexer::new(source, sym_table.clone());

    let start_parse = Instant::now();
    let mut parser = Parser::new(lexer, sym_table.clone());
    let mut module = parser.parse_module();
    let duration_parse = start_parse.elapsed();
    println!("Parser: {:?}", duration_parse);

    println!("{:#?}", module);

    let start_parse = Instant::now();
    let mut binder = NameBinder::new();
    binder.resolve(&mut module);
    let duration_parse = start_parse.elapsed();
    println!("Binder: {:?}", duration_parse);

    let mut ty_table = Rc::new(RefCell::new(TyTable::new()));

    let start_parse = Instant::now();
    let mut checker = TypeChecker::new(ty_table);
    checker.visit_module(&mut module);
    let duration_parse = start_parse.elapsed();
    println!("Checker: {:?}", duration_parse);

    let mut printer = ProgramPrinter::new(sym_table.clone());
    printer.visit_module(&mut module);
    println!("{}", printer.result);

    let start_emit = Instant::now();
    let world = World::new();
    let mut code_gen = HirEmitter::new(sym_table, world);
    code_gen.emit_module(&mut module);
    let fnc = code_gen.get_fn(&String::from("main"));
    let duration_parse = start_emit.elapsed();
    println!("Emitter: {:?}", duration_parse);

    println!("-----");
    println!("{:?}", fnc.sign());
    println!("-----");



/*
    let start = Instant::now();
    let mut world = World::new();
/*
    let zero = world.lit_int(0);

    let int_ty = world.ty_int(32);
    let bot = world.bot();
    let pi = world.pi(int_ty, bot);*/

    println!("-----------------");
    println!("-----------------");
    println!("-----------------");
    println!("-----------------");

    let cn2 = {
        let mut builder = world.builder();

        let i32_ty = builder.ty_int(32);
        let one = builder.lit(1, &i32_ty);
        let one2 = builder.lit(1, &i32_ty);

        let pi = builder.pi(&i32_ty, &i32_ty);
        let cn = builder.lam(&pi);
        let mut test = Box::new(builder.var(&cn));
        let mut test2 = Box::new(builder.var(&cn));

        for _ in 0 .. 300{
            test2 = Box::new(builder.add(&one, &one2));
        }

        builder.set_body(&cn, &test2);
        builder.construct_def(&cn)
    };

    println!("-----");
    println!("{:?}", cn2.sign());
    println!("-----");

    println!("sss");
    let duration = start.elapsed();
    println!("Time elapsed in expensive_function() is: {:?}", duration);

    let start = Instant::now();
    let duration = start.elapsed();
    println!("Time elapsed in expensive_function() is: {:?}", duration);*/
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