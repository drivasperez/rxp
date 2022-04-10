use criterion::{criterion_group, criterion_main, Criterion};
use rxp::{Compiler, DfaCompiler, Parser, Scanner};

fn fibonacci(n: u64) -> u64 {
    let mut a = 0;
    let mut b = 1;

    match n {
        0 => b,
        _ => {
            for _ in 0..n {
                let c = a + b;
                a = b;
                b = c;
            }
            b
        }
    }
}

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

criterion_group!(benches, test_nfa, test_dfa);
criterion_main!(benches);
