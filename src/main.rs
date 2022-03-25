use rex::{Parser, Regex, ToGraphviz};
fn main() {
    let test_string: Vec<char> = "(hello)*1a(a|b)*".chars().collect();
    let mut parser = Parser::new(&test_string);
    let graph = parser.parse().unwrap().graphviz("G");

    println!("{graph}");
}
