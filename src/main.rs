use anyhow::anyhow;
use std::str::FromStr;

use rex::{Compiler, DfaCompiler, Parser, Scanner};
use structopt::StructOpt;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Phase {
    Tokens,
    Ast,
    Nfa,
    Dfa,
}

impl FromStr for Phase {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
        match s.to_lowercase().as_str() {
            "tokens" => Ok(Self::Tokens),
            "ast" => Ok(Self::Ast),
            "nfa" => Ok(Self::Nfa),
            "dfa" => Ok(Self::Dfa),
            other => Err(anyhow!("Expected scanner, dfa, nfa or parser, got {other}")),
        }
    }
}

#[derive(StructOpt)]
/// A utility for evaluating and exploring regular expressions.
enum Opt {
    /// Emits a graphviz visualisation of a regular expression at various phases of compilation.
    Dot {
        /// The phase that should be visualised. Options: "scanner", "parser", "nfa".
        phase: Phase,
        /// The regex to be visualised.
        regex: String,
    },
    /// Tests a regular expression against a given test string.
    Test {
        /// The regular expression to be tested.
        regex: String,
        /// The string to be tested against the regular expression.
        test_string: String,
        /// Whether the regex should be compiled to a DFA (Deterministic Finite Automaton) before
        /// testing.
        #[structopt(long)]
        dfa: bool,
    },
}

fn test_expression(
    regex_string: &str,
    test_string: &str,
    construct_dfa: bool,
) -> anyhow::Result<bool> {
    let scanner = Scanner::new(regex_string);
    let mut parser = Parser::new(&scanner);
    let regex = parser.parse()?;
    let compiler = Compiler::new();
    let nfa = compiler.compile(&regex);

    let res = if !construct_dfa {
        nfa.matches(test_string)
    } else {
        let dfa_compiler = DfaCompiler::new();
        let dfa = dfa_compiler.create_dfa(nfa);
        dfa.matches(test_string)
    };

    Ok(res)
}

fn visualise_expression(regex_string: &str, phase: Phase) -> anyhow::Result<String> {
    let scanner = Scanner::new(regex_string);
    if phase == Phase::Tokens {
        return Ok(scanner.graphviz("Scanner"));
    }

    let mut parser = Parser::new(&scanner);
    let regex = parser.parse().unwrap();

    if phase == Phase::Ast {
        return Ok(regex.graphviz("Parser"));
    }

    let compiler = Compiler::new();
    let nfa = compiler.compile(&regex);

    if phase == Phase::Nfa {
        return Ok(nfa.graphviz("Nfa"));
    }

    let dfa_compiler = DfaCompiler::new();
    let dfa = dfa_compiler.create_dfa(nfa);

    if phase == Phase::Dfa {
        return Ok(dfa.graphviz("Dfa"));
    }

    Err(anyhow!("Haven't implemented that yet."))
}

fn main() -> anyhow::Result<()> {
    match Opt::from_args() {
        Opt::Dot { phase, regex } => println!("{}", visualise_expression(&regex, phase)?),
        Opt::Test {
            regex,
            test_string,
            dfa,
        } => println!("{}", test_expression(&regex, &test_string, dfa)?),
    }

    Ok(())
}
