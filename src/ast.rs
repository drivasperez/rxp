use crate::graphviz::{DiGraph, Style};
use crate::scanner::Token;

#[derive(Debug)]
pub enum Ast<'a> {
    Choice(Choice<'a>),
    Sequence(Sequence<'a>),
    Repetition(Repetition<'a>),
    OneOrMore(OneOrMore<'a>),
    Primitive(Primitive<'a>),
    Digit(Digit),
    Blank(Blank),
}

impl<'a> Ast<'a> {
    pub fn id(&self) -> usize {
        match self {
            Self::Choice(Choice { id, .. }) => *id,
            Self::Sequence(Sequence { id, .. }) => *id,
            Self::Repetition(Repetition { id, .. }) => *id,
            Self::OneOrMore(OneOrMore { id, .. }) => *id,
            Self::Primitive(Primitive { id, .. }) => *id,
            Self::Blank(Blank { id, .. }) => *id,
            Self::Digit(Digit { id }) => *id,
        }
    }
}

#[derive(Debug)]
pub struct Choice<'a> {
    pub id: usize,
    pub a: Box<Ast<'a>>,
    pub b: Box<Ast<'a>>,
}

impl<'a> Choice<'a> {
    pub fn new(id: usize, a: Ast<'a>, b: Ast<'a>) -> Self {
        Self {
            id,
            a: Box::new(a),
            b: Box::new(b),
        }
    }
}

#[derive(Debug)]
pub struct Sequence<'a> {
    pub id: usize,
    pub exprs: Vec<Ast<'a>>,
}

impl<'a> Sequence<'a> {
    pub fn new(id: usize, exprs: Vec<Ast<'a>>) -> Self {
        Self { id, exprs }
    }
}

#[derive(Debug)]
pub struct Repetition<'a> {
    pub id: usize,
    pub term: Box<Ast<'a>>,
}

impl<'a> Repetition<'a> {
    pub fn new(id: usize, term: Ast<'a>) -> Self {
        Self {
            id,
            term: Box::new(term),
        }
    }
}

#[derive(Debug)]
pub struct OneOrMore<'a> {
    pub id: usize,
    pub term: Box<Ast<'a>>,
}

impl<'a> OneOrMore<'a> {
    pub fn new(id: usize, term: Ast<'a>) -> Self {
        Self {
            id,
            term: Box::new(term),
        }
    }
}

#[derive(Debug)]
pub struct Primitive<'a> {
    pub id: usize,
    pub token: Token<'a>,
}

impl<'a> Primitive<'a> {
    pub fn new(id: usize, token: Token<'a>) -> Self {
        Self { id, token }
    }
}

#[derive(Debug)]
pub struct Blank {
    pub id: usize,
}

impl Blank {
    pub fn new(id: usize) -> Self {
        Self { id }
    }
}

#[derive(Debug)]
pub struct Digit {
    pub id: usize,
}

impl Digit {
    pub fn new(id: usize) -> Self {
        Self { id }
    }
}

impl<'a> Ast<'a> {
    pub fn visit(&self, f: &mut dyn FnMut(&Ast<'a>, usize)) {
        self._visit(f, 0)
    }

    fn _visit(&self, f: &mut dyn FnMut(&Ast<'a>, usize), level: usize) {
        match &self {
            Self::Choice(Choice { a, b, .. }) => {
                f(self, level);
                a._visit(f, level + 1);
                b._visit(f, level + 1);
            }
            Self::Sequence(Sequence { exprs, .. }) => {
                f(self, level);
                for expr in exprs {
                    expr._visit(f, level + 1);
                }
            }
            Self::Repetition(Repetition { term, .. }) | Self::OneOrMore(OneOrMore { term, .. }) => {
                f(self, level);
                term._visit(f, level + 1);
            }
            Self::Primitive(_) => f(self, level),
            Self::Digit(_) => f(self, level),
            Self::Blank(_) => f(self, level),
        }
    }

    /// Given a source string for the Regex AST, reproduce that source string.
    /// Basically useless except for testing.
    pub fn fmt(&self, source: &str) -> String {
        // TODO: bit buggy around parentheses

        match &self {
            Self::Choice(Choice { a, b, .. }) => format!(
                "{}|{}",
                match **a {
                    Self::Primitive(_) => a.fmt(source),
                    _ => format!("({})", a.fmt(source)),
                },
                match **b {
                    Self::Primitive(_) => b.fmt(source),
                    _ => format!("({})", b.fmt(source)),
                },
            ),
            Self::Sequence(Sequence { exprs, .. }) => {
                let mut s = String::new();
                for expr in exprs {
                    s.push_str(&expr.fmt(source));
                }
                s
            }
            Self::Repetition(Repetition { term, .. }) => format!(
                "{}*",
                match **term {
                    Self::Primitive(_) => term.fmt(source),
                    _ => format!("({})", term.fmt(source)),
                },
            ),
            Self::OneOrMore(OneOrMore { term, .. }) => format!(
                "{}+",
                match **term {
                    Self::Primitive(_) => term.fmt(source),
                    _ => format!("({})", term.fmt(source)),
                },
            ),
            Self::Primitive(Primitive { token, .. }) => token.lexeme().to_string(),
            Self::Digit(_) => "\\d".to_string(),
            Self::Blank(_) => String::new(),
        }
    }

    pub fn sequence(id: usize, exprs: Vec<Ast<'a>>) -> Self {
        Self::Sequence(Sequence::new(id, exprs))
    }

    pub fn choice(id: usize, start: Ast<'a>, end: Ast<'a>) -> Self {
        Self::Choice(Choice::new(id, start, end))
    }

    pub fn repetition(id: usize, n: Ast<'a>) -> Self {
        Self::Repetition(Repetition::new(id, n))
    }

    pub fn primitive(id: usize, t: Token<'a>) -> Self {
        Self::Primitive(Primitive::new(id, t))
    }

    pub fn blank(id: usize) -> Self {
        Self::Blank(Blank::new(id))
    }
}

impl<'a> Ast<'a> {
    pub fn graphviz(&'a self, graph_name: &str) -> String {
        let mut digraph = DiGraph::new(graph_name);

        self.visit(&mut |r, _level| match &r {
            Self::Choice(Choice { id, a, b }) => {
                digraph
                    .vertex(id, Style::new().label("Choice").fontname("Monospace"))
                    .edge(id, a.id(), None)
                    .edge(id, b.id(), None);
            }
            Self::Sequence(Sequence { id, exprs }) => {
                digraph.vertex(id, Style::new().label("Sequence").fontname("Monospace"));
                for expr in exprs {
                    digraph.edge(id, expr.id(), None);
                }
            }
            Self::Repetition(Repetition { id, term }) => {
                digraph
                    .vertex(id, Style::new().label("Repetition").fontname("Monospace"))
                    .edge(id, term.id(), None);
            }
            Self::OneOrMore(OneOrMore { id, term }) => {
                digraph
                    .vertex(id, Style::new().label("OneOrMore").fontname("Monospace"))
                    .edge(id, term.id(), None);
            }
            Self::Primitive(Primitive { id, token }) => {
                let lexeme = token.lexeme();
                digraph.vertex(
                    id,
                    Style::new()
                        .label(format!("Primitive ({lexeme})"))
                        .fontname("Monospace"),
                );
            }
            Self::Digit(Digit { id }) => {
                digraph.vertex(id, Style::new().label("\\\\d").fontname("Monospace"));
            }
            Self::Blank(Blank { id }) => {
                digraph.vertex(id, Style::new().label("Blank").fontname("Monospace"));
            }
        });

        digraph.to_string()
    }
}
