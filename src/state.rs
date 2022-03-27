use std::cell::RefCell;
use typed_arena::Arena;

use crate::{scanner::Token, Expr, ExprKind};

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
        match &regex.kind {
            ExprKind::Blank => panic!(),
            ExprKind::Choice(_, _) => self.compile_choice(regex),
            ExprKind::Sequence(_, _) => self.compile_sequence(regex),
            ExprKind::Repetition(_) => self.compile_repetition(regex),
            ExprKind::Primitive(_) => self.compile_literal(regex),
        }
    }

    fn compile_literal(&self, regex: Expr) -> Nfa<'a> {
        todo!()
    }

    fn compile_sequence(&self, regex: Expr) -> Nfa<'a> {
        todo!()
    }

    fn compile_repetition(&self, regex: Expr) -> Nfa<'a> {
        todo!()
    }

    fn compile_choice(&self, regex: Expr) -> Nfa<'a> {
        todo!()
    }
}
