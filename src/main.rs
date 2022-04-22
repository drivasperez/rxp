use color_eyre::eyre::{eyre, Error, Result};

use std::str::FromStr;

use rxp::{
    graphviz::Graphviz, instructions_graphviz, vm_graphviz, Compiler, DfaCompiler, Parser, Scanner,
    VirtualMachine,
};
use structopt::StructOpt;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Phase {
    Tokens,
    Ast,
    Ast2,
    Nfa,
    Dfa,
    Vm,
    Instructions,
}

impl FromStr for Phase {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
        match s.to_lowercase().as_str() {
            "tokens" => Ok(Self::Tokens),
            "ast" => Ok(Self::Ast),
            "ast2" => Ok(Self::Ast2),
            "nfa" => Ok(Self::Nfa),
            "dfa" => Ok(Self::Dfa),
            "vmtree" => Ok(Self::Vm),
            "vm" => Ok(Self::Instructions),
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

fn test_vm_expression(regex_string: &str, test_string: &str) -> Result<bool> {
    let scanner = Scanner::new(regex_string);
    let mut parser = Parser::new(&scanner);
    let regex = parser.parse()?;
    let vm = VirtualMachine::from_expr(&regex);

    Ok(vm.matches(test_string))
}

fn visualise_vm(regex_string: &str) -> Result<String> {
    let scanner = Scanner::new(regex_string);
    let mut parser = Parser::new(&scanner);
    let regex = parser.parse()?;

    Ok(vm_graphviz(&regex))
}

fn visualise_instructions(regex_string: &str) -> Result<String> {
    let scanner = Scanner::new(regex_string);
    let mut parser = Parser::new(&scanner);
    let regex = parser.parse()?;

    Ok(instructions_graphviz(&regex, regex_string))
}

fn visualise_scanner(regex_string: &str) -> String {
    let scanner = Scanner::new(regex_string);
    scanner.graphviz("Scanner")
}

fn visualise_ast(regex_string: &str) -> Result<String> {
    let scanner = Scanner::new(regex_string);
    let mut parser = Parser::new(&scanner);
    let regex = parser.parse()?;
    Ok(regex.graphviz("Parser"))
}

fn visualise_ast2(regex_string: &str) -> Result<String> {
    let mut parser = regex_syntax::ast::parse::Parser::new();
    let ast = parser.parse(regex_string)?;
    Ok(ast.graphviz("Parser"))
}

fn visualise_nfa(regex_string: &str) -> Result<String> {
    let scanner = Scanner::new(regex_string);
    let mut parser = Parser::new(&scanner);
    let regex = parser.parse()?;

    let compiler = Compiler::new();
    let nfa = compiler.compile(&regex);

    Ok(nfa.graphviz("Nfa"))
}

fn visualise_dfa(regex_string: &str) -> Result<String> {
    let scanner = Scanner::new(regex_string);
    let mut parser = Parser::new(&scanner);
    let regex = parser.parse()?;

    let compiler = Compiler::new();
    let nfa = compiler.compile(&regex);

    let dfa_compiler = DfaCompiler::new();
    let dfa = dfa_compiler.create_dfa(nfa);
    Ok(dfa.graphviz("Dfa"))
}

fn main() -> Result<()> {
    color_eyre::install()?;
    match Opt::from_args() {
        Opt::Dot { phase, regex } => match phase {
            Phase::Vm => {
                println!("{}", visualise_vm(&regex)?);
            }
            Phase::Instructions => {
                println!("{}", visualise_instructions(&regex)?)
            }
            Phase::Tokens => {
                println!("{}", visualise_scanner(&regex))
            }
            Phase::Ast => {
                println!("{}", visualise_ast(&regex)?)
            }
            Phase::Ast2 => {
                println!("{}", visualise_ast2(&regex)?);
            }
            Phase::Nfa => {
                println!("{}", visualise_nfa(&regex)?)
            }
            Phase::Dfa => {
                println!("{}", visualise_dfa(&regex)?)
            }
        },
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
