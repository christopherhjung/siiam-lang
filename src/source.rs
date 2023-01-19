
use std::io::{BufRead, BufReader, Read};
use std::fs::File;
use std::ops::Add;
use crate::lexer::{Loc, Pos};

pub struct Source{
    reader : BufReader<File>,
    buffer : [u8; 1],
    eof : bool,

    pub last: Pos,
    pub current: Pos,
    pub peek: Pos,

    last_char: char,
}

impl Source{
    pub fn peek(&mut self) -> Option<char> {
        if self.eof{
            None
        }else{
            Some(self.buffer[0] as char)
        }
    }

    pub fn next(&mut self) -> Option<char> {
        let prev = self.peek();
        self.current = self.peek;

        if let Some(c) = prev{
            self.updatePosition(c);
            if let Ok(size) = self.reader.read(&mut self.buffer[..]){
                if size == 0 {
                    self.eof = true;
                }
            }
        }


        prev
    }

    pub fn acceptStr( &mut self, str: &mut String, pred : impl Fn(char) -> bool) -> bool{
        if let Some(cha) = self.peek() {
            if pred(cha) {
                str.push(cha);
                self.next();
                return true;
            }
        }

        return false;
    }

    pub fn accept( &mut self,  pred : impl Fn(char) -> bool ) -> bool{
        if let Some(cha) = self.peek() {
            if pred(cha) {
                self.next();
                return true;
            }
        }

        return false;
    }

    fn updatePosition(&mut self, c: char) {
        if c == '\r' || c == '\n' {
            if self.last_char as i32 == (('\n' as i32 ^ '\r' as i32) ^ c as i32) {
                self.last_char = '\0';
            } else {
                self.peek.col = 1;
                self.peek.line += 1;
                self.last_char = c;
            }
        } else {
            self.peek.col += 1;
            self.last_char = c;
        }
    }

    pub fn resetLoc(&mut self){
        self.last = self.peek;
    }

    pub fn loc(&mut self) -> Loc {
        Loc {
            file: String::from(""),
            begin: self.last,
            end: self.current,
        }
    }

    pub fn new( file : String ) -> Source{
        let mut res = Source{
            reader : BufReader::new(File::open(file ).expect("open failed")),
            buffer : [0; 1],
            eof : false,
            last: Pos { line: 0, col: 0 },
            current: Pos { line: 0, col: 0 },
            peek: Pos { line: 0, col: 0 },
            last_char: '\0',
        };

        res.next();

        res
    }
}