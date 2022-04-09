use std::cell::{Cell, RefCell};
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
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

impl<'a> State<'a> {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            transitions: RefCell::default(),
        }
    }

    /// Match the NFA state machine against a candidate string.
    /// Returns true if the NFA ends in a matching state.
    /// Returns false if the NFA fails to match the string.
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
        epsilon_closure(current_states)
            .iter()
            .any(|x| x.transitions.borrow().is_empty())
    }

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
            edges.push(format!("{id} [shape={shape}]"));
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

impl<'a> State<'a> {
    fn transit(&self, kind: TransitionKind<'a>, state: &'a State<'a>) {
        self.transitions
            .borrow_mut()
            .push(Transition { kind, state })
    }
}

/// The epsilon closure of a set of states is the set of states accessible
/// from those states by making only epsilon transitions.
fn epsilon_closure<'a, T>(states: T) -> Vec<&'a State<'a>>
where
    T: IntoIterator<Item = &'a State<'a>>,
{
    let mut v: Vec<&State> = Vec::new();
    let mut queue: VecDeque<&State> = states.into_iter().collect();
    let mut visited = HashSet::new();

    while let Some(state) = queue.pop_front() {
        let transitions = state.transitions.borrow();
        if visited.contains(&state.id) {
            // Avoid cycles.
            continue;
        }
        for transition in transitions.iter() {
            if let TransitionKind::Epsilon = transition.kind {
                // Follow any remaining epsilon transitions.
                queue.push_back(transition.state);
            }
        }
        v.push(state);
        visited.insert(state.id);
    }
    v
}

#[derive(Debug)]
pub struct NfaFragment<'a> {
    start: &'a State<'a>,
    end: &'a State<'a>,
}

impl<'a> NfaFragment<'a> {
    pub fn new(start: &'a State<'a>, end: &'a State<'a>) -> Self {
        NfaFragment { start, end }
    }
}

#[derive(Default)]
pub struct Compiler<'a> {
    id_counter: Cell<usize>,
    arena: Arena<State<'a>>,
}

impl<'a> Compiler<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    fn new_state(&'a self) -> &'a State {
        let id = self.id_counter.get();
        self.id_counter.set(id + 1);
        self.arena.alloc(State::new(id))
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
        let entry = self.new_state();
        let left = self.compile_nfa(&*regex.term);
        let exit = self.new_state();
        let frag = NfaFragment::new(entry, exit);
        frag.start.transit(TransitionKind::Epsilon, left.start);
        frag.start.transit(TransitionKind::Epsilon, frag.end);
        left.end.transit(TransitionKind::Epsilon, left.start);
        left.end.transit(TransitionKind::Epsilon, frag.end);

        frag
    }

    fn compile_choice(&'a self, expr: &ChoiceExpr<'a>) -> NfaFragment<'a> {
        let entry = self.new_state();
        let left = self.compile_nfa(&*expr.a);
        let right = self.compile_nfa(&*expr.b);
        let exit = self.new_state();

        let frag = NfaFragment::new(entry, exit);
        frag.start.transit(TransitionKind::Epsilon, left.start);
        left.end.transit(TransitionKind::Epsilon, frag.end);
        frag.start.transit(TransitionKind::Epsilon, right.start);
        right.end.transit(TransitionKind::Epsilon, frag.end);

        frag
    }
}

#[derive(Debug)]
pub struct DfaTransition<'a> {
    lexeme: &'a str,
    state: &'a DfaState<'a>,
}

#[derive(Debug)]
pub struct DfaState<'a> {
    id: usize,
    transitions: RefCell<Vec<DfaTransition<'a>>>,
    nfa_states: Vec<&'a State<'a>>,
}

impl<'a> DfaState<'a> {
    pub fn new(id: usize, dfa_states: Vec<&'a State<'a>>) -> Self {
        Self {
            id,
            transitions: RefCell::default(),
            nfa_states: dfa_states,
        }
    }

    /// Returns true if this state is an accepting state.
    /// A DFA state is accepting if one of its constituent NFA states is accepting.
    fn is_accepting(&self) -> bool {
        epsilon_closure(self.nfa_states.iter().copied())
            .iter()
            .any(|x| x.transitions.borrow().is_empty())
    }

    fn transit(&self, lexeme: &'a str, state: &'a DfaState<'a>) {
        self.transitions
            .borrow_mut()
            .push(DfaTransition { lexeme, state })
    }

    /// move(T, a) is the set of states to which there is a transition on input symbol `a` for some NFA
    /// state in T.
    fn find_possible_transitions(&self) -> HashMap<&'a str, Vec<&'a State<'a>>> {
        let mut map: HashMap<&'a str, Vec<&'a State<'a>>> = HashMap::new();

        for state in &self.nfa_states {
            let transitions = state.transitions.borrow();
            for transition in transitions.iter() {
                if let TransitionKind::Literal(token) = transition.kind {
                    map.entry(token.lexeme())
                        .or_default()
                        .push(transition.state)
                }
            }
        }

        map
    }

    pub fn matches(&self, s: &str) -> bool {
        let mut current_state = self;

        for grapheme in s.graphemes(true) {
            if let Some(transition) = current_state
                .transitions
                .borrow()
                .iter()
                .find(|transition| transition.lexeme == grapheme)
            {
                current_state = transition.state;
            } else {
                return false;
            }
        }

        current_state.is_accepting()
    }

    pub fn graphviz(&self, graph_name: &str) -> String {
        fn id_to_label(id: usize) -> char {
            (id as u8 + 65) as char
        }
        let mut edges = Vec::new();

        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();

        queue.push_back(self);

        while let Some(state) = queue.pop_front() {
            if visited.contains(&state.id) {
                continue;
            }
            let id = id_to_label(state.id);
            let shape = if state.is_accepting() {
                "doublecircle"
            } else {
                "circle"
            };
            let transitions = state.transitions.borrow();
            edges.push(format!("{id} [shape={shape}]"));
            for t in transitions.iter() {
                let label = t.lexeme;
                edges.push(format!(
                    "{id} -> {} [label=\"{label}\"]",
                    id_to_label(t.state.id)
                ));
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

trait WithIds {
    fn ids(&self) -> BTreeSet<usize>;
}

impl WithIds for Vec<&State<'_>> {
    fn ids(&self) -> BTreeSet<usize> {
        self.iter().map(|s| s.id).collect()
    }
}

#[derive(Default)]
pub struct DfaCompiler<'a> {
    states: Arena<DfaState<'a>>,
    state_map: RefCell<HashMap<BTreeSet<usize>, &'a DfaState<'a>>>,
    id_counter: Cell<usize>,
}

impl<'a> DfaCompiler<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    fn new_state(&'a self, states: Vec<&'a State<'a>>) -> &'a DfaState<'a> {
        let ids = states.ids();

        self.state_map.borrow_mut().entry(ids).or_insert_with(|| {
            let id = self.id_counter.get();
            self.id_counter.set(id + 1);
            self.states.alloc(DfaState::new(id, states))
        })
    }

    pub fn create_dfa(&'a self, nfa: &'a State<'a>) -> &'a DfaState<'a> {
        // Begin with state 0 and calculate eps-closure.
        let initial_states = epsilon_closure(vec![nfa]);

        let initial_dfa_state = self.new_state(initial_states);
        let mut state_queue = VecDeque::from([initial_dfa_state]);
        let mut visited = HashSet::new();

        while let Some(state) = state_queue.pop_front() {
            let possible_choices = state.find_possible_transitions();
            for (key, mvs) in possible_choices {
                let closure = epsilon_closure(mvs);

                if closure.ids() == state.nfa_states.ids() {
                    state.transit(key, state);
                } else {
                    let closure_ids = closure.ids();
                    let new_state = self.new_state(closure);
                    state.transit(key, new_state);
                    if !visited.contains(&closure_ids) {
                        state_queue.push_back(new_state);
                        visited.insert(closure_ids);
                    }
                }
            }
        }

        initial_dfa_state
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
            let dfa_compiler = DfaCompiler::new();
            let dfa = dfa_compiler.create_dfa(nfa);
            assert_eq!($matches, dfa.matches($test));
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
