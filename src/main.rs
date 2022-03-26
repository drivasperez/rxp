use rex::{Parser, Scanner, ToGraphviz};
use structopt::StructOpt;

#[derive(StructOpt)]
struct Opt {
    /// The regex to be evaluated
    regex: String,

    #[structopt(short, long)]
    dot: bool,
}

fn main() {
    let Opt { regex, dot } = Opt::from_args();

    let test_string = regex;
    let scanner = Scanner::new(&test_string);
    let scanner_graph = scanner.graphviz("Scanner");

    // let mut parser = Parser::new(&test_string);
    // let regex = parser.parse().unwrap();
    // let graph = regex.graphviz("Parser");

    if dot {
        println!("{scanner_graph}");
    }
}
