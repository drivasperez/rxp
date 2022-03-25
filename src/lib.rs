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
        let mut idx = 0;
        let mut edges = String::new();

        self.visit(&mut |r| {
            idx += 1;

            match r {
                Regex::Choice(_, _) => {
                    edges.push_str(&format!("  {idx} [label=\"Choice\"]\n"));
                    edges.push_str(&format!("  {idx} -> {};\n", idx + 1));
                    edges.push_str(&format!("  {idx} -> {};\n", idx + 2));
                }
                Regex::Sequence(_, _) => {
                    edges.push_str(&format!("  {idx} [label=\"Sequence\"]\n"));
                    edges.push_str(&format!("  {idx} -> {};\n", idx + 1));
                    edges.push_str(&format!("  {idx} -> {};\n", idx + 2));
                }
                Regex::Repetition(_) => {
                    edges.push_str(&format!("  {idx} [label=\"Repitition\"]\n"));
                    edges.push_str(&format!("  {idx} -> {};\n", idx + 1));
                }
                Regex::Primitive(c) => {
                    edges.push_str(&format!("  {idx} [label=\"Primitive ({c})\"]\n"));
                }
                Regex::Blank => {
                    edges.push_str(&format!("  {idx} [label=\"Blank\"]\n"));
                }
            }
        });

        format!(
            "digraph {graph_name} {{\n\
            {edges}\
            }}"
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Regex {
    Choice(Box<Regex>, Box<Regex>),
    Sequence(Box<Regex>, Box<Regex>),
    Repetition(Box<Regex>),
    Primitive(char),
    Blank,
}

impl Regex {
    pub fn visit(&self, f: &mut dyn FnMut(&Regex)) {
        match self {
            Regex::Choice(a, b) => {
                f(self);
                a.visit(f);
                b.visit(f);
            }
            Regex::Sequence(a, b) => {
                f(self);
                a.visit(f);
                b.visit(f);
            }
            Regex::Repetition(n) => {
                f(self);
                n.visit(f);
            }
            Regex::Primitive(_) => f(self),
            Regex::Blank => f(self),
        }
    }

    pub fn sequence(a: Regex, b: Regex) -> Self {
        Self::Sequence(Box::new(a), Box::new(b))
    }

    pub fn choice(a: Regex, b: Regex) -> Self {
        Self::Choice(Box::new(a), Box::new(b))
    }

    pub fn repitition(n: Regex) -> Self {
        Self::Repetition(Box::new(n))
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
            Ok(Regex::Choice(Box::new(term), Box::new(regex)))
        } else {
            Ok(term)
        }
    }

    fn term(&mut self) -> Result<Regex> {
        let mut factor = None;

        while self.more() && self.peek().unwrap() != ')' && self.peek().unwrap() != '|' {
            let next_factor = self.factor()?;
            if let Some(r) = factor {
                factor = Some(Regex::Sequence(Box::new(r), Box::new(next_factor)));
            } else {
                factor = Some(next_factor);
            }
        }

        Ok(factor.unwrap_or(Regex::Blank))
    }

    fn factor(&mut self) -> Result<Regex> {
        let mut base = self.base()?;

        while self.more() && self.peek().unwrap() == '*' {
            self.eat('*')?;
            base = Regex::Repetition(Box::new(base));
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
                Ok(Regex::Primitive(escaped))
            }

            Some(c) => {
                self.eat(c).unwrap();
                Ok(Regex::Primitive(c))
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

        assert_eq!(re, Regex::Blank);
    }

    #[test]
    fn parse_char() {
        let test_string: Vec<char> = "a".chars().collect();
        let mut parser = Parser::new(&test_string);
        let re = parser.parse().unwrap();

        assert_eq!(re, Regex::Primitive('a'));
    }
    #[test]
    fn parse_sequence() {
        let test_string: Vec<char> = "ab".chars().collect();
        let mut parser = Parser::new(&test_string);
        let re = parser.parse().unwrap();

        assert_eq!(
            re,
            Regex::sequence(Regex::Primitive('a'), Regex::Primitive('b'))
        );
    }

    #[test]
    fn parse_choice() {
        let test_string: Vec<char> = "a|b".chars().collect();
        let mut parser = Parser::new(&test_string);
        let re = parser.parse().unwrap();

        assert_eq!(
            re,
            Regex::choice(Regex::Primitive('a'), Regex::Primitive('b'),)
        );
    }

    #[test]
    fn visit() {
        let test_string: Vec<char> = "(a|b)*".chars().collect();
        let mut parser = Parser::new(&test_string);
        let re = parser.parse().unwrap();
        re.visit(&mut |r| println!("{r:?}"));
        assert_eq!(0, 1);
    }

    #[test]
    fn graphviz() {
        let test_string: Vec<char> = "(a|b)*".chars().collect();
        let mut parser = Parser::new(&test_string);
        let graph = parser.parse().unwrap().graphviz("G");

        println!("{graph}");

        assert_eq!(0, 1);
    }
}
