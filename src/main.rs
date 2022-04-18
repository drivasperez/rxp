use color_eyre::eyre::{eyre, Error, Result};

use std::str::FromStr;

use rxp::{vm_graphviz, Compiler, DfaCompiler, Parser, Scanner, VirtualMachine};
use structopt::StructOpt;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Phase {
    Tokens,
    Ast,
    Nfa,
    Dfa,
    Vm,
}

impl FromStr for Phase {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
        match s.to_lowercase().as_str() {
            "tokens" => Ok(Self::Tokens),
            "ast" => Ok(Self::Ast),
            "nfa" => Ok(Self::Nfa),
            "dfa" => Ok(Self::Dfa),
            "vm" => Ok(Self::Vm),
            other => Err(eyre!("Expected scanner, dfa, nfa or parser, got {other}")),
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
        /// Whether the regex should be compiled to bytecode and evaluated with a virtual machine.
        #[structopt(long)]
        vm: bool,
    },
}

fn test_expression(regex_string: &str, test_string: &str, construct_dfa: bool) -> Result<bool> {
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

fn test_vm_expression(regex_string: &str, _test_string: &str) -> Result<bool> {
    let scanner = Scanner::new(regex_string);
    let mut parser = Parser::new(&scanner);
    let regex = parser.parse()?;
    let vm = VirtualMachine::from_expr(&regex);

    dbg!(vm);

    return Ok(true);
}

fn visualise_vm(regex_string: &str) -> Result<String> {
    let scanner = Scanner::new(regex_string);
    let mut parser = Parser::new(&scanner);
    let regex = parser.parse()?;

    Ok(vm_graphviz(&regex))
}

fn visualise_expression(regex_string: &str, phase: Phase) -> Result<String> {
    let scanner = Scanner::new(regex_string);
    if phase == Phase::Tokens {
        return Ok(scanner.graphviz("Scanner"));
    }

    let mut parser = Parser::new(&scanner);
    let regex = parser.parse()?;

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

    Err(eyre!("Haven't implemented that yet."))
}

fn main() -> Result<()> {
    color_eyre::install()?;
    match Opt::from_args() {
        Opt::Dot { phase, regex } => {
            if phase == Phase::Vm {
                println!("{}", visualise_vm(&regex)?);
            } else {
                println!("{}", visualise_expression(&regex, phase)?);
            }
        }
        Opt::Test {
            regex,
            test_string,
            dfa,
            vm,
        } => {
            if vm {
                println!("{}", test_vm_expression(&regex, &test_string)?)
            } else {
                println!("{}", test_expression(&regex, &test_string, dfa)?)
            }
        }
    }

    Ok(())
}
