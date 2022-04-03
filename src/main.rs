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

    test: Option<String>,

    #[structopt(short, long)]
    dot: Vec<WhichDot>,
}

fn main() {
    let Opt { regex, test, dot } = Opt::from_args();

    let regex_string = regex;
    let scanner = Scanner::new(&regex_string);
    if dot.contains(&WhichDot::Scanner) {
        let scanner_graph = scanner.graphviz("Scanner", &regex_string);
        println!("{scanner_graph}");
    }

    let mut parser = Parser::new(&scanner);
    let regex = parser.parse().unwrap();

    if dot.contains(&WhichDot::Parser) {
        let graph = regex.graphviz("Parser", &regex_string);
        println!("{graph}");
    }

    let compiler = Compiler::new();
    let nfa = compiler.compile(&regex);

    if dot.contains(&WhichDot::Nfa) {
        let graph = nfa.graphviz("Nfa", &regex_string);
        println!("{graph}");
    }

    if let Some(s) = test {
        println!("{}", nfa.matches(&s, &regex_string));
    }
}

// fn main() {
//     let regex_string = "(hel)*";
//     let test_string = "helhelhelhe";

//     let scanner = Scanner::new(&regex_string);
//     let mut parser = Parser::new(&scanner);
//     let regex = parser.parse().unwrap();

//     let compiler = Compiler::new();
//     let compiled = compiler.compile(&regex);

//     println!("{}", compiled.matches(test_string, regex_string));
// }
