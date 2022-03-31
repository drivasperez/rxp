use anyhow::anyhow;
use std::str::FromStr;

use rex::{Compiler, Parser, Scanner, ToGraphviz};
use structopt::StructOpt;

#[derive(Debug, PartialEq, Eq)]
enum WhichDot {
    Scanner,
    Parser,
    Nfa,
}

impl FromStr for WhichDot {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
        match s.to_lowercase().as_str() {
            "scanner" => Ok(Self::Scanner),
            "parser" => Ok(Self::Parser),
            "nfa" => Ok(Self::Nfa),
            other => Err(anyhow!("Expected scanner, nfa or parser, got {other}")),
        }
    }
}

#[derive(StructOpt)]
struct Opt {
    /// The regex to be evaluated
    regex: String,

    #[structopt(short, long)]
    dot: Vec<WhichDot>,
}

fn main() {
    let Opt { regex, dot } = Opt::from_args();

    let test_string = regex;
    let scanner = Scanner::new(&test_string);
    if dot.contains(&WhichDot::Scanner) {
        let scanner_graph = scanner.graphviz("Scanner", &test_string);
        println!("{scanner_graph}");
    }

    let mut parser = Parser::new(&scanner);
    let regex = parser.parse().unwrap();

    if dot.contains(&WhichDot::Parser) {
        let graph = regex.graphviz("Parser", &test_string);
        println!("{graph}");
    }

    let compiler = Compiler::new();
    let nfa = compiler.compile(&regex);

    if dot.contains(&WhichDot::Nfa) {
        let graph = nfa.graphviz("Nfa", &test_string);
        println!("{graph}");
    }
}
