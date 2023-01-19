use std::rc::Rc;
use crate::{Lexer, TokenVariant};
use crate::lexer::Token;

const LOOKAHEAD_SIZE: usize = 3;

struct Parser {
    lexer: Lexer,
    lookahead: [Token; LOOKAHEAD_SIZE],
    lookahead_idx: usize,
    new_lines: usize,
}
/*

struct Module{
    items : Vec<Rc<Item>>
}

trait Expr{

}*/


impl Parser {
    fn shift(&mut self) {
        let mut new_token = self.lexer.nextToken();

        self.lookahead_idx = (self.lookahead_idx + 1) % LOOKAHEAD_SIZE;

        self.new_lines <<= 1;
        if new_token.variant == TokenVariant::NL {
            new_token = self.lexer.nextToken();
            self.new_lines |= 1;
        }

        self.lookahead[self.lookahead_idx] = new_token;
    }


    fn new(mut lexer: Lexer) -> Parser {
        let lookahead = [lexer.nextToken(), lexer.nextToken(), lexer.nextToken()];

        Parser {
            lexer,
            lookahead,
            lookahead_idx: 0,
            new_lines: 0,
        }
    }
}