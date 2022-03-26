use anyhow::anyhow;
use std::str::FromStr;

use rex::{Parser, Scanner, ToGraphviz};
use structopt::StructOpt;

#[derive(Debug)]
enum WhichDot {
    Scanner,
    Parser,
}

impl FromStr for WhichDot {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, <Self as FromStr>::Err> {
        match s.to_lowercase().as_str() {
            "scanner" => Ok(Self::Scanner),
            "parser" => Ok(Self::Parser),
            other => Err(anyhow!("Expected scanner or parser, got {other}")),
        }
    }
}

#[derive(StructOpt)]
struct Opt {
    /// The regex to be evaluated
    regex: String,

    #[structopt(short, long)]
    dot: Option<WhichDot>,
}

fn main() {
    let Opt { regex, dot } = Opt::from_args();

    let test_string = regex;
    let scanner = Scanner::new(&test_string);
    if let Some(WhichDot::Scanner) = dot {
        let scanner_graph = scanner.graphviz("Scanner", &test_string);
        println!("{scanner_graph}");
        return;
    }

    let mut parser = Parser::new(&scanner);
    let regex = parser.parse().unwrap();

    if let Some(WhichDot::Parser) = dot {
        let graph = regex.graphviz("Parser", &test_string);
        println!("{graph}");
        return;
    }
}
