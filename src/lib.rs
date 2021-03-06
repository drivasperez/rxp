use expr::{BlankExpr, ChoiceExpr, Expr, PrimitiveExpr, RepetitionExpr, SequenceExpr};
use expr::{DigitExpr, OneOrMoreExpr};
use scanner::Token;
use scanner::Tokens;
use std::iter::Peekable;

mod expr;
pub mod graphviz;
mod scanner;
mod state;
mod vm;

pub use scanner::Scanner;
pub use state::{Compiler, DfaCompiler, DfaState, State};
pub use vm::{
    compile::{instructions_graphviz, vm_graphviz},
    VirtualMachine,
};

use color_eyre::eyre::{eyre, Result};

// Grammar:
//
// <regex>  ::= <term> '|' <regex>
//            | <term>
//
// <term>   ::= { <factor> }
//
// <factor> ::= <base> { '*' }
//
// <base>   ::= <char>
//            | '\' <char>
//            | '(' <regex> ')'
//

pub struct Parser<'a> {
    id_counter: usize,
    input: Peekable<Tokens<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(scanner: &'a Scanner<'a>) -> Self {
        Self {
            id_counter: 0,
            input: scanner.tokens().peekable(),
        }
    }

    fn get_id(&mut self) -> usize {
        let id = self.id_counter;
        self.id_counter += 1;
        id
    }

    pub fn parse(&mut self) -> Result<Expr> {
        self.regex()
    }
}

impl<'a> Parser<'a> {
    fn sequence_expr(&mut self, exprs: Vec<Expr<'a>>) -> Expr<'a> {
        let id = self.get_id();
        Expr::Sequence(SequenceExpr::new(id, exprs))
    }

    fn choice_expr(&mut self, start: Expr<'a>, end: Expr<'a>) -> Expr<'a> {
        let id = self.get_id();
        Expr::Choice(ChoiceExpr::new(id, start, end))
    }

    fn repetition_expr(&mut self, n: Expr<'a>) -> Expr<'a> {
        let id = self.get_id();
        Expr::Repetition(RepetitionExpr::new(id, n))
    }

    fn one_or_more_expr(&mut self, n: Expr<'a>) -> Expr<'a> {
        let id = self.get_id();
        Expr::OneOrMore(OneOrMoreExpr::new(id, n))
    }

    fn primitive_expr(&mut self, t: Token<'a>) -> Expr<'a> {
        let id = self.get_id();
        Expr::Primitive(PrimitiveExpr::new(id, t))
    }

    fn digit_expr(&mut self) -> Expr<'a> {
        let id = self.get_id();
        Expr::Digit(DigitExpr::new(id))
    }

    fn blank_expr(&mut self) -> Expr<'a> {
        let id = self.get_id();
        Expr::Blank(BlankExpr::new(id))
    }
}

impl<'a> Parser<'a> {
    fn peek(&mut self) -> Option<Token<'a>> {
        self.input.peek().cloned()
    }

    fn eat(&mut self, expected: Token) -> Result<Token> {
        match self.peek() {
            Some(c) if c == expected => {
                let next = self.input.next().unwrap();
                Ok(next)
            }
            Some(other) => Err(eyre!("Expected {expected:?}, got {other:?}")),
            None => Err(eyre!("Expected {expected:?}, string empty")),
        }
    }

    fn next(&mut self) -> Option<Token<'a>> {
        self.input.next()
    }

    fn more(&mut self) -> bool {
        self.peek().is_some()
    }

    fn regex(&mut self) -> Result<Expr<'a>> {
        let term = self.term()?;

        if self.eat(Token::Pipe).is_ok() {
            let regex = self.regex()?;
            Ok(self.choice_expr(term, regex))
        } else {
            Ok(term)
        }
    }

    fn term(&mut self) -> Result<Expr<'a>> {
        if !self.more() {
            return Ok(self.blank_expr());
        }

        let mut factor = None;
        let mut last = None;
        while let Some(token) = self.peek() {
            if matches!(token, Token::RightParen) {
                last = Some(token);
                break;
            }
            if self.eat(Token::Pipe).is_ok() {
                let regex = self.regex()?;
                let blank = self.blank_expr();
                factor = Some(self.choice_expr(factor.unwrap_or(blank), regex));
                break;
            }
            let next_factor = self.factor()?;
            if factor.is_none() {
                factor = Some(next_factor)
            } else if let Some(Expr::Sequence(seq)) = &mut factor {
                seq.exprs.push(next_factor);
            } else if let Some(expr) = factor {
                factor = Some(self.sequence_expr(vec![expr, next_factor]));
            }
        }

        factor.ok_or_else(|| eyre!("Unexpected character: {last:?}"))
    }

    fn factor(&mut self) -> Result<Expr<'a>> {
        let mut base = self.base()?;

        while self.eat(Token::Star).is_ok() {
            base = self.repetition_expr(base);
        }

        while self.eat(Token::Plus).is_ok() {
            base = self.one_or_more_expr(base);
        }

        Ok(base)
    }

    fn base(&mut self) -> Result<Expr<'a>> {
        if let Some(peeked) = self.peek() {
            match peeked {
                Token::LeftParen => {
                    self.eat(Token::LeftParen)?;
                    let r = self.regex()?;
                    self.eat(Token::RightParen)?;
                    Ok(r)
                }
                Token::BackSlash => {
                    self.eat(Token::BackSlash)?;
                    let escaped = self
                        .next()
                        .ok_or_else(|| eyre!("Ended before escaped character"))?;

                    match escaped.lexeme() {
                        "d" => Ok(self.digit_expr()),
                        _ => Ok(self.primitive_expr(escaped)),
                    }
                }
                Token::RightParen => Err(eyre!("Unmatched close paren")),

                c => {
                    self.eat(c).unwrap();
                    Ok(self.primitive_expr(peeked))
                }
            }
        } else {
            Err(eyre!("Input was empty!"))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! test_round_trip {
        ($n:tt, $s:tt) => {
            #[test]
            fn $n() {
                let scanner = Scanner::new($s);
                let mut parser = Parser::new(&scanner);
                let re = parser.parse().unwrap();

                assert_eq!(re.fmt($s), $s);
            }
        };
    }

    test_round_trip! {parse_char, "a"}
    test_round_trip! {parse_sequence, "ab"}
    test_round_trip! {parse_blank, ""}
    test_round_trip! {parse_choice, "a|b"}
    test_round_trip! {parse_repetition, "abc(de)*"}
}
