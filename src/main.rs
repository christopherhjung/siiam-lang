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
use crate::bind::Binder;
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

    //println!("{:#?}", module);

    let start_parse = Instant::now();
    let mut binder = Binder::new();
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
    let mut world = World::new();
    let mut code_gen = HirEmitter::new(sym_table, world.clone());
    code_gen.emit_module(&mut module);

    code_gen.list();

    let duration_parse = start_emit.elapsed();
    println!("Emitter: {:?}", duration_parse);

}
