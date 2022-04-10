use criterion::{criterion_group, criterion_main, Criterion};
use rxp::{Compiler, DfaCompiler, Parser, Scanner};

fn test_nfa(c: &mut Criterion) {
    c.bench_function("nfa", |b| {
        b.iter(|| {
            let scanner = Scanner::new("(a|b)a(a|b)(a|b)");
            let mut parser = Parser::new(&scanner);
            let regex = parser.parse().unwrap();
            let compiler = Compiler::new();

            let nfa = compiler.compile(&regex);
            nfa.matches("ababaaaabaaabbaab")
        });
    });
}

fn test_dfa(c: &mut Criterion) {
    c.bench_function("dfa", |b| {
        b.iter(|| {
            let scanner = Scanner::new("(a|b)a(a|b)(a|b)");
            let mut parser = Parser::new(&scanner);
            let regex = parser.parse().unwrap();
            let compiler = Compiler::new();

            let nfa = compiler.compile(&regex);
            let dfa_compiler = DfaCompiler::new();
            let dfa = dfa_compiler.create_dfa(nfa);
            dfa.matches("ababaaaabaaabbaab")
        })
    });
}

fn test_compiled_nfa(c: &mut Criterion) {
    let scanner = Scanner::new("(a|b)a(a|b)(a|b)");
    let mut parser = Parser::new(&scanner);
    let regex = parser.parse().unwrap();
    let compiler = Compiler::new();

    let nfa = compiler.compile(&regex);

    c.bench_function("nfa_compiled", |b| {
        b.iter(|| nfa.matches("ababaaaabaaabbaab"))
    });
}
fn test_compiled_dfa(c: &mut Criterion) {
    let scanner = Scanner::new("(a|b)a(a|b)(a|b)");
    let mut parser = Parser::new(&scanner);
    let regex = parser.parse().unwrap();
    let compiler = Compiler::new();

    let nfa = compiler.compile(&regex);
    let dfa_compiler = DfaCompiler::new();
    let dfa = dfa_compiler.create_dfa(nfa);

    c.bench_function("dfa_compiled", |b| {
        b.iter(|| dfa.matches("ababaaaabaaabbaab"))
    });
}

criterion_group!(
    benches,
    test_nfa,
    test_dfa,
    test_compiled_nfa,
    test_compiled_dfa
);
criterion_main!(benches);
