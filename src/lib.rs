use scanner::Token;
use scanner::TokenKind;
use scanner::Tokens;
use std::cell::Cell;
use std::iter::Peekable;

mod scanner;
pub use scanner::Scanner;

use anyhow::anyhow;
use anyhow::Result;
/*
   <regex> ::= <term> '|' <regex>
            |  <term>

   <term> ::= { <factor> }

   <factor> ::= <base> { '*' }

   <base> ::= <char>
           |  '\' <char>
           |  '(' <regex> ')'
*/

pub trait ToGraphviz {
    fn graphviz(&self, graph_name: &str, source: &str) -> String;
}

impl ToGraphviz for Regex {
    fn graphviz(&self, graph_name: &str, source: &str) -> String {
        let mut edges = Vec::new();

        self.visit(&mut |r, _level| {
            let id = r.id;

            match &r.kind {
                RegexKind::Choice(a, b) => {
                    edges.push(format!("  {id} [label=\"Choice\"]"));
                    edges.push(format!("  {id} -> {};", a.id));
                    edges.push(format!("  {id} -> {};", b.id));
                }
                RegexKind::Sequence(a, b) => {
                    edges.push(format!("  {id} [label=\"Sequence\"]"));
                    edges.push(format!("  {id} -> {};", a.id));
                    edges.push(format!("  {id} -> {};", b.id));
                }
                RegexKind::Repetition(a) => {
                    edges.push(format!("  {id} [label=\"Repetition\"]"));
                    edges.push(format!("  {id} -> {};", a.id));
                }
                RegexKind::Primitive(c) => {
                    let lexeme = c.lexeme(source);
                    edges.push(format!("  {id} [label=\"Primitive ({lexeme})\"]"));
                }
                RegexKind::Blank => {
                    edges.push(format!("  {id} [label=\"Blank\"]"));
                }
            }
        });

        // edges.reverse();
        let edges = edges.join("\n");

        format!(
            "digraph {graph_name} {{\n\
            {edges}\n\
            }}"
        )
    }
}

thread_local!(static REGEX_NODE_ID: Cell<usize> = Cell::new(0));

#[derive(Debug)]
pub struct Regex {
    id: usize,
    pub kind: RegexKind,
}

impl PartialEq for Regex {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for Regex {}

#[derive(Debug, PartialEq, Eq)]
pub enum RegexKind {
    Choice(Box<Regex>, Box<Regex>),
    Sequence(Box<Regex>, Box<Regex>),
    Repetition(Box<Regex>),
    Primitive(Token),
    Blank,
}

impl Regex {
    pub fn new(kind: RegexKind) -> Self {
        REGEX_NODE_ID.with(|thread_id| {
            let id = thread_id.get();
            thread_id.set(id + 1);

            Self { id, kind }
        })
    }
    pub fn visit(&self, f: &mut dyn FnMut(&Regex, usize)) {
        self._visit(f, 0)
    }

    fn _visit(&self, f: &mut dyn FnMut(&Regex, usize), level: usize) {
        match &self.kind {
            RegexKind::Choice(a, b) => {
                f(self, level);
                a._visit(f, level + 1);
                b._visit(f, level + 1);
            }
            RegexKind::Sequence(a, b) => {
                f(self, level);
                a._visit(f, level + 1);
                b._visit(f, level + 1);
            }
            RegexKind::Repetition(n) => {
                f(self, level);
                n._visit(f, level + 1);
            }
            RegexKind::Primitive(_) => f(self, level),
            RegexKind::Blank => f(self, level),
        }
    }

    pub fn sequence(a: Regex, b: Regex) -> Self {
        Self::new(RegexKind::Sequence(Box::new(a), Box::new(b)))
    }

    pub fn choice(a: Regex, b: Regex) -> Self {
        Self::new(RegexKind::Choice(Box::new(a), Box::new(b)))
    }

    pub fn repetition(n: Regex) -> Self {
        Self::new(RegexKind::Repetition(Box::new(n)))
    }

    pub fn primitive(t: Token) -> Self {
        Self::new(RegexKind::Primitive(t))
    }

    pub fn blank() -> Self {
        Self::new(RegexKind::Blank)
    }
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

    pub fn parse(&mut self) -> Result<Regex> {
        self.regex()
    }
}

impl<'a> Parser<'a> {
    fn peek(&mut self) -> Option<&Token> {
        self.input.peek()
    }

    fn eat(&mut self, expected: TokenKind) -> Result<Token> {
        match self.peek() {
            Some(c) if c.kind == expected => {
                let next = self.input.next().unwrap();
                Ok(next)
            }
            Some(other) => Err(anyhow!("Expected {expected:?}, got {other:?}")),
            None => Err(anyhow!("Expected {expected:?}, string empty")),
        }
    }
    fn next(&mut self) -> Option<Token> {
        self.input.next()
    }

    fn more(&mut self) -> bool {
        self.peek().is_some()
    }

    fn regex(&mut self) -> Result<Regex> {
        let term = self.term()?;

        if self.more() && self.peek().unwrap().kind == TokenKind::Pipe {
            self.eat(TokenKind::Pipe)?;
            let regex = self.regex()?;
            Ok(Regex::choice(term, regex))
        } else {
            Ok(term)
        }
    }

    fn term(&mut self) -> Result<Regex> {
        let mut factor = None;

        while self.more()
            && self.peek().unwrap().kind != TokenKind::RightParen
            && self.peek().unwrap().kind != TokenKind::Pipe
        {
            let next_factor = self.factor()?;
            if let Some(r) = factor {
                factor = Some(Regex::sequence(r, next_factor));
            } else {
                factor = Some(next_factor);
            }
        }

        Ok(factor.unwrap_or(Regex::blank()))
    }

    fn factor(&mut self) -> Result<Regex> {
        let mut base = self.base()?;

        while self.more() && self.peek().unwrap().kind == TokenKind::Star {
            self.eat(TokenKind::Star)?;
            base = Regex::repetition(base);
        }

        Ok(base)
    }

    fn base(&mut self) -> Result<Regex> {
        if let Some(peeked) = self.peek().cloned() {
            match peeked.kind {
                TokenKind::LeftParen => {
                    self.eat(TokenKind::LeftParen)?;
                    let r = self.regex()?;
                    self.eat(TokenKind::RightParen)?;
                    Ok(r)
                }
                TokenKind::BackSlash => {
                    self.eat(TokenKind::BackSlash)?;
                    let escaped = self
                        .next()
                        .ok_or(anyhow!("Ended before escaped character"))?;
                    Ok(Regex::primitive(escaped))
                }
                TokenKind::RightParen => Err(anyhow!("Unmatched close paren")),

                c => {
                    self.eat(c).unwrap();
                    Ok(Regex::primitive(peeked))
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

    #[test]
    fn parse_blank() {
        let test_string = "";
        let scanner = Scanner::new(test_string);
        let mut parser = Parser::new(&scanner);
        let re = parser.parse().unwrap();

        assert_eq!(re, Regex::blank());
    }

    //     #[test]
    //     fn parse_char() {
    //         let test_string = "a";
    //         let scanner = Scanner::new(test_string);
    //         let mut parser = Parser::new(&scanner);
    //         let re = parser.parse().unwrap();

    //         assert!(re, Regex::primitive('a'));
    //     }
    //     #[test]
    //     fn parse_sequence() {
    //         let test_string: Vec<char> = "ab".chars().collect();
    //         let mut parser = Parser::new(&test_string);
    //         let re = parser.parse().unwrap();

    //         assert_eq!(
    //             re,
    //             Regex::sequence(Regex::primitive('a'), Regex::primitive('b'))
    //         );
    //     }

    //     #[test]
    //     fn parse_choice() {
    //         let test_string: Vec<char> = "a|b".chars().collect();
    //         let mut parser = Parser::new(&test_string);
    //         let re = parser.parse().unwrap();

    //         assert_eq!(
    //             re,
    //             Regex::choice(Regex::primitive('a'), Regex::primitive('b'),)
    //         );
    //     }
}
