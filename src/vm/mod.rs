use std::collections::VecDeque;

use unicode_segmentation::UnicodeSegmentation;

use crate::expr::Expr;

pub mod compile;

#[derive(Debug, PartialEq, Eq)]
enum Instruction<'a> {
    Char(&'a str),
    Match,
    Jmp(usize),
    Split(usize, usize),
    Digit,
}

impl std::fmt::Display for Instruction<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Instruction::Char(c) => format!("CHAR '{c}'"),
                Instruction::Match => "MATCH".to_string(),
                Instruction::Jmp(dst) => format!("JMP {dst}"),
                Instruction::Split(d1, d2) => format!("SPL {d1}, {d2}"),
                Instruction::Digit => "DIGIT".to_string(),
            }
        )
    }
}

#[derive(Debug)]
struct Thread {
    pc: usize,
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
        let compiler = compile::Compiler::new();
        let instructions = compiler.compile(expr);

        Self { instructions }
    }

    pub fn matches(&self, input: &str) -> bool {
        let mut graphemes = input.graphemes(true).rev().collect::<Vec<_>>();

        let mut current_threads = VecDeque::with_capacity(self.instructions.len());
        let mut next_threads = VecDeque::with_capacity(self.instructions.len());

        current_threads.push_back(Thread { pc: 0 });

        while let Some(ch) = graphemes.pop() {
            while let Some(thread) = current_threads.pop_front() {
                let Thread { pc } = thread;
                match self.instructions[thread.pc] {
                    Instruction::Char(c) => {
                        if ch == c {
                            next_threads.push_back(Thread { pc: pc + 1 });
                        }
                    }
                    Instruction::Digit => {
                        if ch.chars().all(|n| n.is_ascii_digit()) {
                            next_threads.push_back(Thread { pc: pc + 1 });
                        }
                    }
                    Instruction::Match => return true,
                    Instruction::Jmp(dst) => {
                        current_threads.push_back(Thread { pc: dst });
                    }
                    Instruction::Split(d1, d2) => {
                        current_threads.push_back(Thread { pc: d1 });
                        current_threads.push_back(Thread { pc: d2 });
                    }
                }
            }
            std::mem::swap(&mut current_threads, &mut next_threads);
        }

        while let Some(thread) = current_threads.pop_front() {
            let Thread { pc } = thread;
            match self.instructions[pc] {
                Instruction::Char(_) | Instruction::Digit => continue,
                Instruction::Match => return true,
                Instruction::Jmp(dst) => {
                    current_threads.push_back(Thread { pc: dst });
                }
                Instruction::Split(d1, d2) => {
                    current_threads.push_back(Thread { pc: d1 });
                    current_threads.push_back(Thread { pc: d2 });
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
        assert!(vm.matches("abcd"))
    }

    macro_rules! match_regex {
        ($regex:tt, $test:tt, $matches:expr) => {{
            let scanner = Scanner::new($regex);
            let mut parser = Parser::new(&scanner);
            let re = parser.parse().unwrap();
            println!("Regex: {}", $regex);
            let vm = VirtualMachine::from_expr(&re);

            assert_eq!($matches, vm.matches($test));
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
