use crate::gen_id;
use crate::scanner::Token;
use crate::ToGraphviz;

#[derive(Debug)]
pub enum Expr {
    Choice(ChoiceExpr),
    Sequence(SequenceExpr),
    Repetition(RepetitionExpr),
    Primitive(PrimitiveExpr),
    Blank(BlankExpr),
}

impl Expr {
    pub fn id(&self) -> usize {
        match self {
            Self::Choice(ChoiceExpr { id, .. }) => *id,
            Self::Sequence(SequenceExpr { id, .. }) => *id,
            Self::Repetition(RepetitionExpr { id, .. }) => *id,
            Self::Primitive(PrimitiveExpr { id, .. }) => *id,
            Self::Blank(BlankExpr { id, .. }) => *id,
        }
    }
}

#[derive(Debug)]
pub struct ChoiceExpr {
    id: usize,
    pub a: Box<Expr>,
    pub b: Box<Expr>,
}

impl ChoiceExpr {
    pub fn new(a: Expr, b: Expr) -> Self {
        Self {
            id: gen_id(),
            a: Box::new(a),
            b: Box::new(b),
        }
    }
}

#[derive(Debug)]
pub struct SequenceExpr {
    id: usize,
    pub start: Box<Expr>,
    pub end: Box<Expr>,
}

impl SequenceExpr {
    pub fn new(start: Expr, end: Expr) -> Self {
        Self {
            id: gen_id(),
            start: Box::new(start),
            end: Box::new(end),
        }
    }
}

#[derive(Debug)]
pub struct RepetitionExpr {
    id: usize,
    pub term: Box<Expr>,
}

impl RepetitionExpr {
    pub fn new(term: Expr) -> Self {
        Self {
            id: gen_id(),
            term: Box::new(term),
        }
    }
}

#[derive(Debug)]
pub struct PrimitiveExpr {
    id: usize,
    pub token: Token,
}

impl PrimitiveExpr {
    pub fn new(token: Token) -> Self {
        Self {
            id: gen_id(),
            token,
        }
    }
}

#[derive(Debug)]
pub struct BlankExpr {
    id: usize,
}

impl BlankExpr {
    pub fn new() -> Self {
        Self { id: gen_id() }
    }
}

impl Expr {
    pub fn visit(&self, f: &mut dyn FnMut(&Expr, usize)) {
        self._visit(f, 0)
    }

    fn _visit(&self, f: &mut dyn FnMut(&Expr, usize), level: usize) {
        match &self {
            Self::Choice(ChoiceExpr { a, b, .. }) => {
                f(self, level);
                a._visit(f, level + 1);
                b._visit(f, level + 1);
            }
            Self::Sequence(SequenceExpr { start, end, .. }) => {
                f(self, level);
                start._visit(f, level + 1);
                end._visit(f, level + 1);
            }
            Self::Repetition(RepetitionExpr { term, .. }) => {
                f(self, level);
                term._visit(f, level + 1);
            }
            Self::Primitive(_) => f(self, level),
            Self::Blank(_) => f(self, level),
        }
    }

    /// Given a source string for the Regex AST, reproduce that source string.
    /// Basically useless except for testing.
    pub fn fmt(&self, source: &str) -> String {
        // TODO: bit buggy around parentheses
        format!(
            "{}",
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
                Self::Sequence(SequenceExpr { start, end, .. }) =>
                    format!("{}{}", start.fmt(source), end.fmt(source)),
                Self::Repetition(RepetitionExpr { term, .. }) => format!(
                    "{}*",
                    match **term {
                        Self::Primitive(_) => term.fmt(source),
                        _ => format!("({})", term.fmt(source)),
                    },
                ),
                Self::Primitive(PrimitiveExpr { token, .. }) => format!("{}", token.lexeme(source)),
                Self::Blank(_) => format!(""),
            }
        )
    }

    pub fn sequence(a: Expr, b: Expr) -> Self {
        Self::Sequence(SequenceExpr::new(a, b))
    }

    pub fn choice(start: Expr, end: Expr) -> Self {
        Self::Choice(ChoiceExpr::new(start, end))
    }

    pub fn repetition(n: Expr) -> Self {
        Self::Repetition(RepetitionExpr::new(n))
    }

    pub fn primitive(t: Token) -> Self {
        Self::Primitive(PrimitiveExpr::new(t))
    }

    pub fn blank() -> Self {
        Self::Blank(BlankExpr::new())
    }
}

impl ToGraphviz for Expr {
    fn graphviz(&self, graph_name: &str, source: &str) -> String {
        let mut edges = Vec::new();

        self.visit(&mut |r, _level| match &r {
            Self::Choice(ChoiceExpr { id, a, b }) => {
                edges.push(format!("  {id} [label=\"Choice\"]"));
                edges.push(format!("  {id} -> {};", a.id()));
                edges.push(format!("  {id} -> {};", b.id()));
            }
            Self::Sequence(SequenceExpr { id, start, end }) => {
                edges.push(format!("  {id} [label=\"Sequence\"]"));
                edges.push(format!("  {id} -> {};", start.id()));
                edges.push(format!("  {id} -> {};", end.id()));
            }
            Self::Repetition(RepetitionExpr { id, term }) => {
                edges.push(format!("  {id} [label=\"Repetition\"]"));
                edges.push(format!("  {id} -> {};", term.id()));
            }
            Self::Primitive(PrimitiveExpr { id, token }) => {
                let lexeme = token.lexeme(source);
                edges.push(format!("  {id} [label=\"Primitive ({lexeme})\"]"));
            }
            Self::Blank(BlankExpr { id }) => {
                edges.push(format!("  {id} [label=\"Blank\"]"));
            }
        });

        let edges = edges.join("\n");

        format!(
            "digraph {graph_name} {{\n\
            {edges}\n\
            }}"
        )
    }
}
