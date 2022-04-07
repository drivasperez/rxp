use crate::gen_id;
use std::cell::RefCell;
use std::collections::{HashSet, VecDeque};
use typed_arena::Arena;
use unicode_segmentation::UnicodeSegmentation;

use crate::{
    expr::{ChoiceExpr, PrimitiveExpr, RepetitionExpr, SequenceExpr},
    scanner::Token,
    Expr,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TransitionKind<'a> {
    Epsilon,
    Literal(Token<'a>),
}

#[derive(Clone, Debug)]
pub struct Transition<'a> {
    kind: TransitionKind<'a>,
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

    pub fn matches(&'a self, s: &str) -> bool {
        let mut current_states = VecDeque::new();
        let mut next_states = VecDeque::new();

        current_states.push_back(self);

        for grapheme in s.graphemes(true) {
            let mut visited = HashSet::new();
            while let Some(state) = current_states.pop_front() {
                let transitions = state.transitions.borrow();
                if visited.contains(&state.id) {
                    continue;
                }
                for transition in transitions.iter() {
                    match transition.kind {
                        TransitionKind::Epsilon => {
                            // Epsilon transition.
                            // We need to just follow this without advancing the grapheme.
                            current_states.push_back(transition.state);
                        }
                        TransitionKind::Literal(token) => {
                            if token.lexeme() == grapheme {
                                // If character matched, push into next_states, which will
                                // be the queue for the next character.
                                next_states.push_back(transition.state)
                            }
                            // Otherwise, do nothing, this branch does not match and can be
                            // dropped.
                        }
                    }
                }
                visited.insert(state.id);
            }
            // next_states is now current_states.
            // Previous current_states is empty so we can reuse it as next_states.
            std::mem::swap(&mut current_states, &mut next_states);
        }

        // If current_states contains a match, or an epsilon transition to a match, then we matched.
        let mut visited = HashSet::new();
        while let Some(state) = current_states.pop_front() {
            let transitions = state.transitions.borrow();
            if transitions.is_empty() {
                // NFA State with no transitions is a match.
                return true;
            }
            if visited.contains(&state.id) {
                // Avoid cycles.
                continue;
            }
            for transition in transitions.iter() {
                if let TransitionKind::Epsilon = transition.kind {
                    // Follow any remaining epsilon transitions.
                    current_states.push_back(transition.state);
                }
            }
            visited.insert(state.id);
        }

        // No match was found.
        false
    }
}

impl<'a> State<'a> {
    pub fn transit(&self, kind: TransitionKind<'a>, state: &'a State<'a>) {
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

impl<'a> State<'a> {
    pub fn graphviz(&self, graph_name: &str) -> String {
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
                    TransitionKind::Literal(token) => token.lexeme(),
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

    pub fn compile(&'a self, expr: &Expr<'a>) -> &'a State {
        self.compile_nfa(expr).start
    }

    fn compile_nfa(&'a self, expr: &Expr<'a>) -> NfaFragment<'a> {
        match &expr {
            Expr::Blank(_) => panic!(),
            Expr::Choice(e) => self.compile_choice(e),
            Expr::Sequence(e) => self.compile_sequence(e),
            Expr::Repetition(e) => self.compile_repetition(e),
            Expr::Primitive(e) => self.compile_literal(e),
        }
    }

    fn compile_literal(&'a self, expr: &PrimitiveExpr<'a>) -> NfaFragment<'a> {
        let start = self.new_state();
        let end = self.new_state();
        let frag = NfaFragment::new(start, end);
        frag.start.transit(TransitionKind::Literal(expr.token), end);

        frag
    }

    fn compile_sequence(&'a self, expr: &SequenceExpr<'a>) -> NfaFragment<'a> {
        let left = self.compile_nfa(&*expr.start);
        let right = self.compile_nfa(&*expr.end);
        let frag = NfaFragment::new(left.start, right.end);
        left.end.transit(TransitionKind::Epsilon, right.start);

        frag
    }

    fn compile_repetition(&'a self, regex: &RepetitionExpr<'a>) -> NfaFragment<'a> {
        let left = self.compile_nfa(&*regex.term);
        let frag = NfaFragment::new(self.new_state(), self.new_state());
        frag.start.transit(TransitionKind::Epsilon, left.start);
        frag.start.transit(TransitionKind::Epsilon, frag.end);
        left.end.transit(TransitionKind::Epsilon, left.start);
        left.end.transit(TransitionKind::Epsilon, frag.end);

        frag
    }

    fn compile_choice(&'a self, expr: &ChoiceExpr<'a>) -> NfaFragment<'a> {
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::{Parser, Scanner};

    macro_rules! match_regex {
        ($regex:tt, $test:tt, $matches:expr) => {{
            let scanner = Scanner::new($regex);
            let mut parser = Parser::new(&scanner);
            let re = parser.parse().unwrap();
            let compiler = Compiler::new();
            let nfa = compiler.compile(&re);
            assert_eq!($matches, nfa.matches($test));
        }};
    }

    #[test]
    fn single_char() {
        match_regex!("a", "a", true);
        match_regex!("a", "ab", false);
        match_regex!("a", "ba", false);
    }

    #[test]
    fn sequence() {
        match_regex!("ab", "ab", true);
        match_regex!("ab", "ba", false);
        match_regex!("abcdefghijk", "abcdefghijk", true);
        match_regex!("ab", "aaaaaab", false);
    }

    #[test]
    fn closure() {
        match_regex!("a*", "a", true);
        match_regex!("a*", "aaaaaaaaa", true);
        match_regex!("ab*", "ab", true);
        match_regex!("ab*", "abb", true);
        match_regex!("ab*", "abbbbbb", true);
        match_regex!("ab*", "aba", false);
        match_regex!("ab*", "abababababab", false);
        match_regex!("(ab)*", "abababababab", true);
    }

    #[test]
    fn choice() {
        match_regex!("a|b", "a", true);
        match_regex!("a|b", "b", true);
        match_regex!("a|b", "c", false);
        match_regex!("a|b", "ac", false);
        match_regex!("a|b", "ca", false);
        match_regex!("hello|goodbye", "hello", true);
        match_regex!("hello|goodbye", "goodbye", true);
    }

    #[test]
    fn mixed() {
        match_regex!("(hello)*|goodbye", "hellohellohellohello", true);
        match_regex!("(hello)*|goodbye", "hellogoodbyehellohello", false);
        match_regex!("((hello)*|goodbye)*", "hellogoodbyehellohello", true);
    }
}
