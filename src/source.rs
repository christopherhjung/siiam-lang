
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

    last_char: char,
}

impl Source{
    pub fn peek(&self) -> Option<char> {
        if self.eof{
            None
        }else{
            Some(self.buffer[0] as char)
        }
    }

    pub fn next(&mut self) -> Option<char> {
        let prev = self.peek();

        if let Some(c) = prev{
            self.update_position(c);
            if let Ok(size) = self.reader.read(&mut self.buffer[..]){
                if size == 0 {
                    self.eof = true;
                }
            }else{
                self.eof = true;
            }
        }

        prev
    }

    pub fn accept_str_pred(&mut self, str: &mut String, pred : impl Fn(char) -> bool) -> bool{
        self.accept(|ch: char| {
            if pred(ch) {
                str.push(ch);
                true
            }else{
                false
            }
        })
    }

    pub fn accept_str( &mut self, str: &mut String ) -> bool{
        if let Some(ch) = self.peek() {
            str.push(ch);
            self.next();
            return true;
        }

        return false;
    }

    pub fn accept( &mut self,  mut pred : impl FnMut(char) -> bool ) -> bool{
        if let Some(ch) = self.peek() {
            if pred(ch) {
                self.next();
                return true;
            }
        }

        return false;
    }

    fn update_position(&mut self, c: char) {
        if c == '\r' || c == '\n' {
            if self.last_char as i32 == (('\n' as i32 ^ '\r' as i32) ^ c as i32) {
                self.last_char = '\0';
            } else {
                self.current.col = 1;
                self.current.line += 1;
                self.last_char = c;
            }
        } else {
            self.current.col += 1;
            self.last_char = c;
        }
    }

    pub fn reset_loc(&mut self){
        self.last = self.current;
    }

    pub fn loc(&mut self) -> Loc {
        Loc {
            file: String::new(),
            begin: self.last,
            end: self.current,
        }
    }

    pub fn new( file : String ) -> Source{
        let mut res = Source{
            reader : BufReader::new(File::open(file ).expect("open failed")),
            buffer : [0; 1],
            eof : false,
            last: Pos{ line : 1, col : 0 },
            current: Pos{ line : 1, col : 0 },
            last_char: '\0',
        };

        res.next();
        res
    }
}