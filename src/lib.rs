use std::cell::Cell;

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
    fn graphviz(&self, graph_name: &str) -> String;
}

impl ToGraphviz for Regex {
    fn graphviz(&self, graph_name: &str) -> String {
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
                    edges.push(format!("  {id} [label=\"Primitive ({c})\"]"));
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
    Primitive(char),
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

    pub fn primitive(c: char) -> Self {
        Self::new(RegexKind::Primitive(c))
    }

    pub fn blank() -> Self {
        Self::new(RegexKind::Blank)
    }
}

pub struct Parser<'a> {
    input: &'a [char],
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a [char]) -> Self {
        Self { input }
    }

    pub fn parse(&mut self) -> Result<Regex> {
        self.regex()
    }
}

impl<'a> Parser<'a> {
    fn peek(&self) -> Option<char> {
        self.input.first().copied()
    }
    fn eat(&mut self, expected: char) -> Result<char> {
        match self.peek() {
            Some(c) if c == expected => {
                self.input = &self.input[1..];
                Ok(c)
            }
            Some(other) => Err(anyhow!("Expected {expected}, got {other}")),
            None => Err(anyhow!("Expected {expected}, string empty")),
        }
    }
    fn next(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.eat(c).unwrap();
        Some(c)
    }

    fn more(&self) -> bool {
        !self.input.is_empty()
    }

    fn regex(&mut self) -> Result<Regex> {
        let term = self.term()?;

        if self.more() && self.peek().unwrap() == '|' {
            self.eat('|')?;
            let regex = self.regex()?;
            Ok(Regex::choice(term, regex))
        } else {
            Ok(term)
        }
    }

    fn term(&mut self) -> Result<Regex> {
        let mut factor = None;

        while self.more() && self.peek().unwrap() != ')' && self.peek().unwrap() != '|' {
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

        while self.more() && self.peek().unwrap() == '*' {
            self.eat('*')?;
            base = Regex::repetition(base);
        }

        Ok(base)
    }

    fn base(&mut self) -> Result<Regex> {
        match self.peek() {
            Some('(') => {
                self.eat('(')?;
                let r = self.regex()?;
                self.eat(')')?;
                Ok(r)
            }
            Some('\\') => {
                self.eat('\\')?;
                let escaped = self
                    .next()
                    .ok_or(anyhow!("Ended before escaped character"))?;
                Ok(Regex::primitive(escaped))
            }

            Some(c) => {
                self.eat(c).unwrap();
                Ok(Regex::primitive(c))
            }
            None => Err(anyhow!("Input was empty!")),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_blank() {
        let test_string: Vec<char> = "".chars().collect();
        let mut parser = Parser::new(&test_string);
        let re = parser.parse().unwrap();

        assert_eq!(re, Regex::blank());
    }

    #[test]
    fn parse_char() {
        let test_string: Vec<char> = "a".chars().collect();
        let mut parser = Parser::new(&test_string);
        let re = parser.parse().unwrap();

        assert_eq!(re, Regex::primitive('a'));
    }
    #[test]
    fn parse_sequence() {
        let test_string: Vec<char> = "ab".chars().collect();
        let mut parser = Parser::new(&test_string);
        let re = parser.parse().unwrap();

        assert_eq!(
            re,
            Regex::sequence(Regex::primitive('a'), Regex::primitive('b'))
        );
    }

    #[test]
    fn parse_choice() {
        let test_string: Vec<char> = "a|b".chars().collect();
        let mut parser = Parser::new(&test_string);
        let re = parser.parse().unwrap();

        assert_eq!(
            re,
            Regex::choice(Regex::primitive('a'), Regex::primitive('b'),)
        );
    }
}
