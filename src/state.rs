use crate::graphviz::{DiGraph, RankDir, Shape, Style};
use std::cell::{Cell, RefCell};
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
use typed_arena::Arena;
use unicode_segmentation::UnicodeSegmentation;

use crate::expr::OneOrMoreExpr;
use crate::{
    expr::{ChoiceExpr, PrimitiveExpr, RepetitionExpr, SequenceExpr},
    scanner::Token,
    Expr,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TransitionKind<'a> {
    Literal(Token<'a>),
    Digit,
}

#[derive(Clone, Debug)]
pub struct Transition<'a> {
    kind: Option<TransitionKind<'a>>,
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

                // We matched with input left to go. That's fine.
                if transitions.is_empty() {
                    return true;
                }
                for transition in transitions.iter() {
                    match transition.kind {
                        None => {
                            // Epsilon transition.
                            // We need to just follow this without advancing the grapheme.
                            current_states.push_back(transition.state);
                        }
                        Some(TransitionKind::Literal(token)) => {
                            if token.lexeme() == grapheme {
                                // If character matched, push into next_states, which will
                                // be the queue for the next character.
                                next_states.push_back(transition.state)
                            }
                            // Otherwise, do nothing, this branch does not match and can be
                            // dropped.
                        }
                        Some(TransitionKind::Digit) => {
                            if grapheme.bytes().all(|b| b.is_ascii_digit()) {
                                next_states.push_back(transition.state)
                            }
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
        let mut digraph = DiGraph::new(graph_name);
        digraph.rankdir(RankDir::LeftRight);

        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();

        queue.push_back(self);

        digraph
            .vertex(
                "START",
                Style::new()
                    .label("")
                    .shape(Shape::None)
                    .height(0.0)
                    .width(0.0),
            )
            .edge("START", self.id, None);

        while let Some(state) = queue.pop_front() {
            if visited.contains(&state.id) {
                continue;
            }
            let id = state.id;
            let transitions = state.transitions.borrow();
            let shape = if transitions.is_empty() {
                Shape::DoubleCircle
            } else {
                Shape::Circle
            };
            digraph.vertex(id, Style::new().shape(shape));
            for t in transitions.iter() {
                let label = match t.kind {
                    None => "\u{03B5}",
                    Some(TransitionKind::Literal(token)) => token.lexeme(),
                    Some(TransitionKind::Digit) => "\\\\d",
                };
                digraph.edge(id, t.state.id, Style::new().label(label));
                queue.push_back(t.state);
            }
            visited.insert(state.id);
        }

        digraph.to_string()
    }
}

impl<'a> State<'a> {
    fn transit(&self, kind: Option<TransitionKind<'a>>, state: &'a State<'a>) {
        self.transitions
            .borrow_mut()
            .push(Transition { kind, state });
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
            if transition.kind.is_none() {
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
            Expr::Blank(_) => self.compile_blank(),
            Expr::Choice(e) => self.compile_choice(e),
            Expr::Sequence(e) => self.compile_sequence(e),
            Expr::Repetition(e) => self.compile_repetition(e),
            Expr::OneOrMore(e) => self.compile_one_or_more(e),
            Expr::Primitive(e) => self.compile_literal(e),
            Expr::Digit(_) => self.compile_digit(),
        }
    }

    fn compile_blank(&'a self) -> NfaFragment<'a> {
        let start = self.new_state();
        let end = self.new_state();
        let frag = NfaFragment::new(start, end);
        frag.start.transit(None, end);

        frag
    }

    fn compile_literal(&'a self, expr: &PrimitiveExpr<'a>) -> NfaFragment<'a> {
        let start = self.new_state();
        let end = self.new_state();
        let frag = NfaFragment::new(start, end);
        frag.start
            .transit(Some(TransitionKind::Literal(expr.token)), end);

        frag
    }

    fn compile_digit(&'a self) -> NfaFragment<'a> {
        let start = self.new_state();
        let end = self.new_state();
        let frag = NfaFragment::new(start, end);
        frag.start.transit(Some(TransitionKind::Digit), end);

        frag
    }

    // left.start -a-> left.end -eps-> new -eps-> right.start -b-> right.end
    // left.start -a-> right.start -b-> right.end
    // left.start -a-> right.start -b-> right.end -eps->
    fn compile_sequence(&'a self, expr: &SequenceExpr<'a>) -> NfaFragment<'a> {
        let left = self.compile_nfa(&*expr.start);
        let right = self.compile_nfa(&*expr.end);

        left.end.transit(None, right.start);

        NfaFragment::new(left.start, right.end)
    }

    fn compile_repetition(&'a self, regex: &RepetitionExpr<'a>) -> NfaFragment<'a> {
        let entry = self.new_state();
        let left = self.compile_nfa(&*regex.term);
        let exit = self.new_state();
        let frag = NfaFragment::new(entry, exit);
        frag.start.transit(None, left.start);
        frag.start.transit(None, frag.end);
        left.end.transit(None, left.start);
        left.end.transit(None, frag.end);

        frag
    }

    fn compile_one_or_more(&'a self, regex: &OneOrMoreExpr<'a>) -> NfaFragment<'a> {
        let initial = self.compile_nfa(&*regex.term);
        let repetition = {
            let entry = self.new_state();
            let left = self.compile_nfa(&*regex.term);
            let exit = self.new_state();
            let frag = NfaFragment::new(entry, exit);
            frag.start.transit(None, left.start);
            frag.start.transit(None, frag.end);
            left.end.transit(None, left.start);
            left.end.transit(None, frag.end);

            frag
        };

        let frag = NfaFragment::new(initial.start, repetition.end);
        initial.end.transit(None, repetition.start);

        frag
    }

    fn compile_choice(&'a self, expr: &ChoiceExpr<'a>) -> NfaFragment<'a> {
        let entry = self.new_state();
        let left = self.compile_nfa(&*expr.a);
        let right = self.compile_nfa(&*expr.b);
        let exit = self.new_state();

        let frag = NfaFragment::new(entry, exit);
        frag.start.transit(None, left.start);
        left.end.transit(None, frag.end);
        frag.start.transit(None, right.start);
        right.end.transit(None, frag.end);

        frag
    }
}

#[derive(Debug)]
pub struct DfaTransition<'a> {
    kind: TransitionKind<'a>,
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

    fn transit(&self, kind: TransitionKind<'a>, state: &'a DfaState<'a>) {
        self.transitions
            .borrow_mut()
            .push(DfaTransition { kind, state })
    }

    /// move(T, a) is the set of states to which there is a transition on input symbol `a` for some NFA
    /// state in T.
    fn find_possible_transitions(&self) -> HashMap<TransitionKind<'a>, Vec<&'a State<'a>>> {
        let mut map: HashMap<_, Vec<&'a State<'a>>> = HashMap::new();

        for state in &self.nfa_states {
            let transitions = state.transitions.borrow();
            for transition in transitions.iter() {
                if let Some(kind) = transition.kind {
                    map.entry(kind).or_default().push(transition.state)
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
                .find(|transition| match transition.kind {
                    TransitionKind::Literal(token) => token.lexeme() == grapheme,
                    TransitionKind::Digit => grapheme.bytes().all(|b| b.is_ascii_digit()),
                })
            {
                current_state = transition.state;
            } else {
                break;
            }
        }

        current_state.is_accepting()
    }

    pub fn graphviz(&self, graph_name: &str) -> String {
        let mut digraph = DiGraph::new(graph_name);
        digraph.rankdir(RankDir::LeftRight);

        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();

        queue.push_back(self);

        digraph
            .vertex(
                "START",
                Style::new()
                    .label("")
                    .shape(Shape::None)
                    .height(0.0)
                    .width(0.0),
            )
            .edge("START", self.id, None);

        while let Some(state) = queue.pop_front() {
            if visited.contains(&state.id) {
                continue;
            }
            let id = state.id;
            let shape = if state.is_accepting() {
                Shape::DoubleCircle
            } else {
                Shape::Circle
            };

            let transitions = state.transitions.borrow();
            let node_label = state
                .nfa_states
                .chunks(4)
                .map(|x| {
                    x.iter()
                        .map(|x| x.id.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                })
                .collect::<Vec<_>>()
                .join("\n");

            digraph.vertex(
                id,
                Style::new().label(format!("{{{node_label}}}")).shape(shape),
            );
            for t in transitions.iter() {
                let label = match t.kind {
                    TransitionKind::Literal(tok) => tok.lexeme(),
                    TransitionKind::Digit => "\\\\d",
                };
                digraph.edge(id, t.state.id, Style::new().label(label));
                queue.push_back(t.state);
            }
            visited.insert(state.id);
        }

        digraph.to_string()
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
        match_regex!("a", "ab", true);
        match_regex!("a", "ba", false);
    }

    #[test]
    fn digit() {
        match_regex!("\\d", "0", true);
        match_regex!("\\d", "1", true);
        match_regex!("\\d", "2", true);
        match_regex!("\\d", "3", true);
        match_regex!("\\d", "4", true);
        match_regex!("\\d", "5", true);
        match_regex!("\\d", "6", true);
        match_regex!("\\d", "7", true);
        match_regex!("\\d", "8", true);
        match_regex!("\\d", "9", true);
        match_regex!("\\d", "a", false);
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
        match_regex!("ab*", "aba", true);
        match_regex!("ab*", "abababababab", true);
        match_regex!("ab*", "aabababababab", true);
        match_regex!("(ab)*", "abababababab", true);
    }

    #[test]
    fn choice() {
        match_regex!("a|b", "a", true);
        match_regex!("a|b", "b", true);
        match_regex!("a|b", "c", false);
        match_regex!("a|b", "ac", true);
        match_regex!("a|b", "ca", false);
        match_regex!("hello|goodbye", "hello", true);
        match_regex!("hello|goodbye", "goodbye", true);
        match_regex!("hello|", "hello", true);
        match_regex!("hello|", "", true);
        match_regex!("|", "", true);
    }

    #[test]
    fn one_or_more() {
        match_regex!("a+", "", false);
        match_regex!("a+", "a", true);
        match_regex!("a+", "aaa", true);
        match_regex!("(abc)+", "abc", true);
        match_regex!("(abc)+", "abcabc", true);
        match_regex!("a(abc)+", "a", false);
        match_regex!("a(abc)+", "aabc", true);
        match_regex!("a(abc)+", "aabcabc", true);
    }

    #[test]
    fn hmm() {
        match_regex!("(a|b*)*", "a", true);
    }

    #[test]
    fn mixed() {
        match_regex!("(hello)*|g\\dodbye", "hellohellohellohello", true);
        match_regex!("(hello)*|goodbye", "hellogoodbyehellohello", true);
        match_regex!("(hello)+|goodbye", "hellgoodbyehellohello", false);
        match_regex!("((hello)*|goodbye)*", "hellogoodbyehellohello", true);
        match_regex!("((hello)*|g\\dodbye)*", "hellog3odbyehellohello", true);
    }
}
