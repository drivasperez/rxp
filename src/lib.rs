use expr::Expr;
use scanner::Token;
use scanner::Tokens;
use std::iter::Peekable;

mod expr;
mod scanner;
mod state;

pub use scanner::Scanner;
pub use state::Compiler;

use anyhow::anyhow;
use anyhow::Result;

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

/// Generate a unique ID
fn gen_id() -> usize {
    use std::cell::Cell;
    thread_local!(static NODE_ID: Cell<usize> = Cell::new(0));
    NODE_ID.with(|thread_id| {
        let id = thread_id.get();
        thread_id.set(id + 1);
        id
    })
}

pub struct Parser<'a> {
    input: Peekable<Tokens<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(scanner: &'a Scanner<'a>) -> Self {
        Self {
            input: scanner.tokens().peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Expr> {
        self.regex()
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
            Some(other) => Err(anyhow!("Expected {expected:?}, got {other:?}")),
            None => Err(anyhow!("Expected {expected:?}, string empty")),
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
            Ok(Expr::choice(term, regex))
        } else {
            Ok(term)
        }
    }

    fn term(&mut self) -> Result<Expr<'a>> {
        if !self.more() {
            return Ok(Expr::blank());
        }

        let mut factor = None;
        let mut last = None;
        while let Some(token) = self.peek() {
            if matches!(token, Token::RightParen | Token::Pipe) {
                last = Some(token);
                break;
            }
            let next_factor = self.factor()?;
            if let Some(r) = factor {
                factor = Some(Expr::sequence(r, next_factor));
            } else {
                factor = Some(next_factor);
            }
        }

        Ok(factor.ok_or_else(|| anyhow!("Unexpected character: {last:?}"))?)
    }

    fn factor(&mut self) -> Result<Expr<'a>> {
        let mut base = self.base()?;

        while self.eat(Token::Star).is_ok() {
            base = Expr::repetition(base);
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
                        .ok_or(anyhow!("Ended before escaped character"))?;
                    Ok(Expr::primitive(escaped))
                }
                Token::RightParen => Err(anyhow!("Unmatched close paren")),

                c => {
                    self.eat(c).unwrap();
                    Ok(Expr::primitive(peeked))
                }
            }
        } else {
            Err(anyhow!("Input was empty!"))
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
