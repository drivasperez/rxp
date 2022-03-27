use std::cell::RefCell;
use typed_arena::Arena;

use crate::{
    expr::{ChoiceExpr, PrimitiveExpr, RepetitionExpr, SequenceExpr},
    scanner::Token,
    Expr,
};

pub enum TransitionKind {
    Epsilon,
    Literal(Token),
}

pub struct Transition<'a> {
    kind: TransitionKind,
    state: &'a State<'a>,
}

pub struct State<'a> {
    transitions: Vec<Transition<'a>>,
}

impl<'a> State<'a> {
    pub fn transit(&mut self, kind: TransitionKind, state: &'a State<'a>) {
        self.transitions.push(Transition { kind, state })
    }
}

pub struct Nfa<'a> {
    start: Option<State<'a>>,
    end: Option<State<'a>>,
}

pub struct Compiler<'a> {
    arena: Arena<State<'a>>,
}

impl<'a> Compiler<'a> {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
        }
    }

    pub fn compile_nfa(&self, regex: Expr) -> Nfa<'a> {
        // TODO: Make unique structs for the kinds of exprs.
        // Pass them to the various helpers.
        match &regex {
            Expr::Blank(_) => panic!(),
            Expr::Choice(e) => self.compile_choice(e),
            Expr::Sequence(e) => self.compile_sequence(e),
            Expr::Repetition(e) => self.compile_repetition(e),
            Expr::Primitive(e) => self.compile_literal(e),
        }
    }

    fn compile_literal(&self, regex: &PrimitiveExpr) -> Nfa<'a> {
        todo!()
    }

    fn compile_sequence(&self, regex: &SequenceExpr) -> Nfa<'a> {
        todo!()
    }

    fn compile_repetition(&self, regex: &RepetitionExpr) -> Nfa<'a> {
        todo!()
    }

    fn compile_choice(&self, regex: &ChoiceExpr) -> Nfa<'a> {
        todo!()
    }
}
