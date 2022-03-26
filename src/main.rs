use rex::{Parser, ToGraphviz};
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

    let test_string: Vec<char> = regex.chars().collect();
    let mut parser = Parser::new(&test_string);
    let regex = parser.parse().unwrap();
    let graph = regex.graphviz("G");

    if dot {
        println!("{graph}");
    }
}
