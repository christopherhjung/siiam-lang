use std::borrow::BorrowMut;
use std::ptr;
use std::rc::Rc;
use crate::lexer::{Lexer, TokenVariant};
use crate::parser::Parser;
use crate::source::Source;

mod lexer;
mod source;
mod parser;
mod referencing;
mod ast;

struct B{
    test : i32
}

struct A{
    child : Option<Rc<A>>
}

pub fn main() {
    let mut source = Source::new(String::from("test.si"));
    let mut lexer = Lexer::new(source);

    let mut parser = Parser::new(lexer);
    let module = parser.parse_module();
    println!("{:?}", "finish");


    let strong = Rc::new("hello".to_owned());
    let weak = Rc::downgrade(&strong);
    assert!(ptr::eq(&*strong, weak.as_ptr()));
    assert_eq!("hello", unsafe { &*weak.as_ptr() });


    /*
        loop{
            let token = lexer.next_token();
            println!("{:?}", token);
            if token.variant == TokenVariant::Eof || token.variant == TokenVariant::Error{
                break
            }
        }*/

    //let mut f = BufReader::new(File::open("src/main.rs").expect("open failed"));

    //let mut buffer = [0; 1];
    //f.read(&mut buffer[..]);

    //let val = buffer[0];
    //println!("{val}")
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