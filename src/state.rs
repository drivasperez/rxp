use crate::{gen_id, ToGraphviz};
use std::cell::RefCell;
use std::collections::{HashSet, VecDeque};
use typed_arena::Arena;
use unicode_segmentation::UnicodeSegmentation;

use crate::{
    expr::{ChoiceExpr, PrimitiveExpr, RepetitionExpr, SequenceExpr},
    scanner::Token,
    Expr,
};

#[derive(Clone, Debug)]
pub enum TransitionKind {
    Epsilon,
    Literal(Token),
}

#[derive(Clone, Debug)]
pub struct Transition<'a> {
    kind: TransitionKind,
    state: &'a State<'a>,
}

#[derive(Clone, Debug)]
pub struct State<'a> {
    id: usize,
    transitions: RefCell<Vec<Transition<'a>>>,
}

impl<'a> Default for State<'a> {
    fn default() -> Self {
        Self {
            id: gen_id(),
            transitions: RefCell::default(),
        }
    }
}

impl<'a> State<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn matches(&'a self, s: &str, regex_source: &str) -> bool {
        let mut current_states = vec![self];
        let mut next_states = Vec::new();

        fn step<'a>(
            transition: &'a Transition<'a>,
            regex_source: &str,
            grapheme: &str,
            next_states: &mut Vec<&'a State<'a>>,
        ) {
            match transition.kind {
                TransitionKind::Literal(token) => {
                    if token.lexeme(regex_source) == grapheme {
                        next_states.push(transition.state);
                    }
                }
                TransitionKind::Epsilon => {
                    // XXX: Bug here. Getting epsilon shouldn't advance cursor
                    // in the string. Should just follow epsilons in this iteration of the
                    // loop, pushing only the next non-epsilon into next_states.
                    step(transition, regex_source, grapheme, next_states);
                }
            };
        }

        for grapheme in s.graphemes(true) {
            for state in current_states.drain(..) {
                let state_transitions = state.transitions.borrow();
                for transition in state_transitions.iter() {
                    step(transition, regex_source, grapheme, &mut next_states);
                }
            }

            std::mem::swap(&mut current_states, &mut next_states);
        }

        for state in current_states {
            if state.transitions.borrow().is_empty() {
                return true;
            }
        }

        false
    }
}

impl<'a> State<'a> {
    pub fn transit(&self, kind: TransitionKind, state: &'a State<'a>) {
        self.transitions
            .borrow_mut()
            .push(Transition { kind, state })
    }
}

pub struct NfaFragment<'a> {
    start: &'a State<'a>,
    end: &'a State<'a>,
}

impl<'a> NfaFragment<'a> {
    pub fn new(start: &'a State<'a>, end: &'a State<'a>) -> Self {
        NfaFragment { start, end }
    }
}

pub struct Compiler<'a> {
    arena: Arena<State<'a>>,
}

impl<'a> ToGraphviz for State<'a> {
    fn graphviz(&self, graph_name: &str, source: &str) -> String {
        let mut edges = Vec::new();

        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();

        queue.push_back(self);

        while let Some(state) = queue.pop_front() {
            if visited.contains(&state.id) {
                continue;
            }
            let id = state.id;
            let transitions = state.transitions.borrow();
            let shape = if transitions.is_empty() {
                "doublecircle"
            } else {
                "circle"
            };
            edges.push(format!("{id} [shape={shape} label=\"\"]"));
            for t in transitions.iter() {
                let label = match t.kind {
                    TransitionKind::Literal(token) => token.lexeme(source),
                    TransitionKind::Epsilon => "\u{03B5}",
                };
                edges.push(format!("{id} -> {} [label=\"{label}\"]", t.state.id));
                queue.push_back(t.state);
            }
            visited.insert(state.id);
        }

        let edges = edges.join("\n");

        format!(
            "digraph {graph_name} {{\n\
                rankdir=LR;\n\
            {edges}\n\
            }}"
        )
    }
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

    pub fn compile(&'a self, expr: &Expr) -> &'a State {
        self.compile_nfa(expr).start
    }

    fn compile_nfa(&'a self, expr: &Expr) -> NfaFragment<'a> {
        match &expr {
            Expr::Blank(_) => panic!(),
            Expr::Choice(e) => self.compile_choice(e),
            Expr::Sequence(e) => self.compile_sequence(e),
            Expr::Repetition(e) => self.compile_repetition(e),
            Expr::Primitive(e) => self.compile_literal(e),
        }
    }

    fn compile_literal(&'a self, expr: &PrimitiveExpr) -> NfaFragment<'a> {
        let start = self.new_state();
        let end = self.new_state();
        let frag = NfaFragment::new(start, end);
        frag.start.transit(TransitionKind::Literal(expr.token), end);

        frag
    }

    fn compile_sequence(&'a self, expr: &SequenceExpr) -> NfaFragment<'a> {
        let left = self.compile_nfa(&*expr.start);
        let right = self.compile_nfa(&*expr.end);
        let frag = NfaFragment::new(left.start, right.end);
        left.end.transit(TransitionKind::Epsilon, right.start);

        frag
    }

    fn compile_repetition(&'a self, regex: &RepetitionExpr) -> NfaFragment<'a> {
        let left = self.compile_nfa(&*regex.term);
        let frag = NfaFragment::new(self.new_state(), self.new_state());
        frag.start.transit(TransitionKind::Epsilon, left.start);
        frag.start.transit(TransitionKind::Epsilon, frag.end);
        left.end.transit(TransitionKind::Epsilon, left.start);
        left.end.transit(TransitionKind::Epsilon, frag.end);

        frag
    }

    fn compile_choice(&'a self, expr: &ChoiceExpr) -> NfaFragment<'a> {
        let left = self.compile_nfa(&*expr.a);
        let right = self.compile_nfa(&*expr.b);

        let frag = NfaFragment::new(self.new_state(), self.new_state());
        frag.start.transit(TransitionKind::Epsilon, left.start);
        left.end.transit(TransitionKind::Epsilon, frag.end);
        frag.start.transit(TransitionKind::Epsilon, right.start);
        right.end.transit(TransitionKind::Epsilon, frag.end);

        frag
    }
}
