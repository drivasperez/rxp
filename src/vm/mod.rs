use unicode_segmentation::UnicodeSegmentation;

use crate::expr::Expr;

mod compile;

#[derive(Debug, PartialEq, Eq)]
enum Instruction<'a> {
    Char(&'a str),
    Match,
    Jmp(usize),
    Split(usize, usize),
    Digit,
}

#[derive(Debug)]
struct Thread {
    pc: usize,
    sp: usize,
}

#[derive(Debug)]
pub struct VirtualMachine<'a> {
    instructions: Vec<Instruction<'a>>,
}

impl<'a> VirtualMachine<'a> {
    #[cfg(test)]
    fn new(instructions: Vec<Instruction<'a>>) -> Self {
        Self { instructions }
    }

    pub fn from_expr(expr: &'a Expr<'a>) -> Self {
        let instructions = compile::compile(expr);

        Self { instructions }
    }

    pub fn matches(&self, input: &str) -> bool {
        let input = input.graphemes(true).collect::<Vec<_>>();
        let mut threads = vec![Thread { pc: 0, sp: 0 }];

        while let Some(thread) = threads.pop() {
            let Thread { mut pc, mut sp } = thread;
            loop {
                match self.instructions[pc] {
                    Instruction::Char(c) => {
                        if sp >= input.len() || input[sp] != c {
                            // Did not match, kill this thread.
                            break;
                        }
                        // Advance program and input counter.
                        pc += 1;
                        sp += 1;
                    }
                    Instruction::Digit => {
                        if sp >= input.len() || !input[sp].chars().all(|x| x.is_ascii_digit()) {
                            break;
                        }
                        pc += 1;
                        sp += 1;
                    }
                    // If we matched and there is no more input, we're good.
                    // TODO: Most implementations would permit remaining input this and
                    // introduce $^ to adjust behaviour, but don't have that in NFA/DFA
                    // implementations and want to be consistent.
                    Instruction::Match => return sp >= input.len(),
                    Instruction::Jmp(addr) => {
                        // Jump to addr by changing program counter.
                        pc = addr;
                    }
                    Instruction::Split(a1, a2) => {
                        // Jump this thread to a1.
                        pc = a1;
                        // Spawn a new thread starting at a2
                        threads.push(Thread { sp, pc: a2 });
                    }
                }
            }
        }

        return false;
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::{Parser, Scanner};

    #[test]
    fn aplus_bplus() {
        let instructions = vec![
            Instruction::Char("a"),
            Instruction::Split(0, 2),
            Instruction::Char("b"),
            Instruction::Split(2, 4),
            Instruction::Match,
        ];

        let vm = VirtualMachine::new(instructions);

        assert!(vm.matches("aabb"));
        assert!(!vm.matches("abcd"))
    }

    macro_rules! match_regex {
        ($regex:tt, $test:tt, $matches:expr) => {{
            let scanner = Scanner::new($regex);
            let mut parser = Parser::new(&scanner);
            let re = parser.parse().unwrap();
            let vm = VirtualMachine::from_expr(&re);

            assert_eq!($matches, vm.matches($test));
        }};
    }

    #[test]
    fn single_char() {
        match_regex!("a", "a", true);
        match_regex!("a", "ab", false);
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
    fn mixed() {
        match_regex!("(hello)*|g\\dodbye", "hellohellohellohello", true);
        match_regex!("(hello)*|goodbye", "hellogoodbyehellohello", false);
        match_regex!("((hello)*|goodbye)*", "hellogoodbyehellohello", true);
        match_regex!("((hello)*|g\\dodbye)*", "hellog3odbyehellohello", true);
    }
}
