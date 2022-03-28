use crate::gen_id;
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

#[derive(Default)]
pub struct State<'a> {
    transitions: RefCell<Vec<Transition<'a>>>,
}
impl<'a> State<'a> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<'a> State<'a> {
    pub fn transit(&self, kind: TransitionKind, state: &'a State<'a>) {
        self.transitions
            .borrow_mut()
            .push(Transition { kind, state })
    }
}

pub struct Nfa<'a> {
    start: &'a State<'a>,
    end: &'a State<'a>,
}

impl<'a> Nfa<'a> {
    pub fn new(start: &'a State<'a>, end: &'a State<'a>) -> Self {
        Self { start, end }
    }
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

    fn new_state(&'a self) -> &'a State {
        self.arena.alloc(State::default())
    }

    pub fn compile_nfa(&'a self, expr: &Expr) -> Nfa<'a> {
        match &expr {
            Expr::Blank(_) => panic!(),
            Expr::Choice(e) => self.compile_choice(e),
            Expr::Sequence(e) => self.compile_sequence(e),
            Expr::Repetition(e) => self.compile_repetition(e),
            Expr::Primitive(e) => self.compile_literal(e),
        }
    }

    fn compile_literal(&'a self, expr: &PrimitiveExpr) -> Nfa<'a> {
        let start = self.new_state();
        let end = self.new_state();
        let nfa = Nfa::new(start, end);
        nfa.start.transit(TransitionKind::Literal(expr.token), end);

        nfa
    }

    fn compile_sequence(&'a self, expr: &SequenceExpr) -> Nfa<'a> {
        let left = self.compile_nfa(&*expr.start);
        let right = self.compile_nfa(&*expr.end);
        let nfa = Nfa::new(left.start, right.end);
        left.end.transit(TransitionKind::Epsilon, right.start);

        nfa
    }

    fn compile_repetition(&'a self, regex: &RepetitionExpr) -> Nfa<'a> {
        let left = self.compile_nfa(&*regex.term);
        let nfa = Nfa::new(self.new_state(), self.new_state());
        nfa.start.transit(TransitionKind::Epsilon, left.start);
        nfa.start.transit(TransitionKind::Epsilon, nfa.end);
        left.end.transit(TransitionKind::Epsilon, left.start);
        left.end.transit(TransitionKind::Epsilon, nfa.end);

        nfa
    }

    fn compile_choice(&'a self, expr: &ChoiceExpr) -> Nfa<'a> {
        let left = self.compile_nfa(&*expr.a);
        let right = self.compile_nfa(&*expr.b);

        let nfa = Nfa::new(self.new_state(), self.new_state());
        nfa.start.transit(TransitionKind::Epsilon, left.start);
        left.end.transit(TransitionKind::Epsilon, nfa.end);
        nfa.start.transit(TransitionKind::Epsilon, right.start);
        right.end.transit(TransitionKind::Epsilon, nfa.end);

        nfa
    }
}
