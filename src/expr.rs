use crate::graphviz::{DiGraph, Style};
use crate::scanner::Token;

#[derive(Debug)]
pub enum Expr<'a> {
    Choice(ChoiceExpr<'a>),
    Sequence(SequenceExpr<'a>),
    Repetition(RepetitionExpr<'a>),
    OneOrMore(OneOrMoreExpr<'a>),
    Primitive(PrimitiveExpr<'a>),
    Digit(DigitExpr),
    Blank(BlankExpr),
}

impl<'a> Expr<'a> {
    pub fn id(&self) -> usize {
        match self {
            Self::Choice(ChoiceExpr { id, .. }) => *id,
            Self::Sequence(SequenceExpr { id, .. }) => *id,
            Self::Repetition(RepetitionExpr { id, .. }) => *id,
            Self::OneOrMore(OneOrMoreExpr { id, .. }) => *id,
            Self::Primitive(PrimitiveExpr { id, .. }) => *id,
            Self::Blank(BlankExpr { id, .. }) => *id,
            Self::Digit(DigitExpr { id }) => *id,
        }
    }
}

#[derive(Debug)]
pub struct ChoiceExpr<'a> {
    pub id: usize,
    pub a: Box<Expr<'a>>,
    pub b: Box<Expr<'a>>,
}

impl<'a> ChoiceExpr<'a> {
    pub fn new(id: usize, a: Expr<'a>, b: Expr<'a>) -> Self {
        Self {
            id,
            a: Box::new(a),
            b: Box::new(b),
        }
    }
}

#[derive(Debug)]
pub struct SequenceExpr<'a> {
    pub id: usize,
    pub exprs: Vec<Expr<'a>>,
}

impl<'a> SequenceExpr<'a> {
    pub fn new(id: usize, exprs: Vec<Expr<'a>>) -> Self {
        Self { id, exprs }
    }
}

#[derive(Debug)]
pub struct RepetitionExpr<'a> {
    pub id: usize,
    pub term: Box<Expr<'a>>,
}

impl<'a> RepetitionExpr<'a> {
    pub fn new(id: usize, term: Expr<'a>) -> Self {
        Self {
            id,
            term: Box::new(term),
        }
    }
}

#[derive(Debug)]
pub struct OneOrMoreExpr<'a> {
    pub id: usize,
    pub term: Box<Expr<'a>>,
}

impl<'a> OneOrMoreExpr<'a> {
    pub fn new(id: usize, term: Expr<'a>) -> Self {
        Self {
            id,
            term: Box::new(term),
        }
    }
}

#[derive(Debug)]
pub struct PrimitiveExpr<'a> {
    pub id: usize,
    pub token: Token<'a>,
}

impl<'a> PrimitiveExpr<'a> {
    pub fn new(id: usize, token: Token<'a>) -> Self {
        Self { id, token }
    }
}

#[derive(Debug)]
pub struct BlankExpr {
    pub id: usize,
}

impl BlankExpr {
    pub fn new(id: usize) -> Self {
        Self { id }
    }
}

#[derive(Debug)]
pub struct DigitExpr {
    pub id: usize,
}

impl DigitExpr {
    pub fn new(id: usize) -> Self {
        Self { id }
    }
}

impl<'a> Expr<'a> {
    pub fn visit(&self, f: &mut dyn FnMut(&Expr<'a>, usize)) {
        self._visit(f, 0)
    }

    fn _visit(&self, f: &mut dyn FnMut(&Expr<'a>, usize), level: usize) {
        match &self {
            Self::Choice(ChoiceExpr { a, b, .. }) => {
                f(self, level);
                a._visit(f, level + 1);
                b._visit(f, level + 1);
            }
            Self::Sequence(SequenceExpr { exprs, .. }) => {
                f(self, level);
                for expr in exprs {
                    expr._visit(f, level + 1);
                }
            }
            Self::Repetition(RepetitionExpr { term, .. })
            | Self::OneOrMore(OneOrMoreExpr { term, .. }) => {
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
            Self::Choice(ChoiceExpr { a, b, .. }) => format!(
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
            Self::Sequence(SequenceExpr { exprs, .. }) => {
                let mut s = String::new();
                for expr in exprs {
                    s.push_str(&expr.fmt(source));
                }
                s
            }
            Self::Repetition(RepetitionExpr { term, .. }) => format!(
                "{}*",
                match **term {
                    Self::Primitive(_) => term.fmt(source),
                    _ => format!("({})", term.fmt(source)),
                },
            ),
            Self::OneOrMore(OneOrMoreExpr { term, .. }) => format!(
                "{}+",
                match **term {
                    Self::Primitive(_) => term.fmt(source),
                    _ => format!("({})", term.fmt(source)),
                },
            ),
            Self::Primitive(PrimitiveExpr { token, .. }) => token.lexeme().to_string(),
            Self::Digit(_) => "\\d".to_string(),
            Self::Blank(_) => String::new(),
        }
    }

    pub fn sequence(id: usize, exprs: Vec<Expr<'a>>) -> Self {
        Self::Sequence(SequenceExpr::new(id, exprs))
    }

    pub fn choice(id: usize, start: Expr<'a>, end: Expr<'a>) -> Self {
        Self::Choice(ChoiceExpr::new(id, start, end))
    }

    pub fn repetition(id: usize, n: Expr<'a>) -> Self {
        Self::Repetition(RepetitionExpr::new(id, n))
    }

    pub fn primitive(id: usize, t: Token<'a>) -> Self {
        Self::Primitive(PrimitiveExpr::new(id, t))
    }

    pub fn blank(id: usize) -> Self {
        Self::Blank(BlankExpr::new(id))
    }
}

impl<'a> Expr<'a> {
    pub fn graphviz(&'a self, graph_name: &str) -> String {
        let mut digraph = DiGraph::new(graph_name);

        self.visit(&mut |r, _level| match &r {
            Self::Choice(ChoiceExpr { id, a, b }) => {
                digraph
                    .vertex(id, Style::new().label("Choice").fontname("Monospace"))
                    .edge(id, a.id(), None)
                    .edge(id, b.id(), None);
            }
            Self::Sequence(SequenceExpr { id, exprs }) => {
                digraph.vertex(id, Style::new().label("Sequence").fontname("Monospace"));
                for expr in exprs {
                    digraph.edge(id, expr.id(), None);
                }
            }
            Self::Repetition(RepetitionExpr { id, term }) => {
                digraph
                    .vertex(id, Style::new().label("Repetition").fontname("Monospace"))
                    .edge(id, term.id(), None);
            }
            Self::OneOrMore(OneOrMoreExpr { id, term }) => {
                digraph
                    .vertex(id, Style::new().label("OneOrMore").fontname("Monospace"))
                    .edge(id, term.id(), None);
            }
            Self::Primitive(PrimitiveExpr { id, token }) => {
                let lexeme = token.lexeme();
                digraph.vertex(
                    id,
                    Style::new()
                        .label(format!("Primitive ({lexeme})"))
                        .fontname("Monospace"),
                );
            }
            Self::Digit(DigitExpr { id }) => {
                digraph.vertex(id, Style::new().label("\\\\d").fontname("Monospace"));
            }
            Self::Blank(BlankExpr { id }) => {
                digraph.vertex(id, Style::new().label("Blank").fontname("Monospace"));
            }
        });

        digraph.to_string()
    }
}
