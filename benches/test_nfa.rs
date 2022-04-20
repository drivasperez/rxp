use criterion::{criterion_group, criterion_main, Criterion};
use rxp::{Compiler, DfaCompiler, Parser, Scanner, VirtualMachine};

const REGEX: &str = "(a+|b)a*(a|b|c|hello)(a|b|c|blah|blah|a)";

fn test_nfa(c: &mut Criterion) {
    c.bench_function("nfa", |b| {
        b.iter(|| {
            let scanner = Scanner::new(REGEX);
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
            let scanner = Scanner::new(REGEX);
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

fn test_vm(c: &mut Criterion) {
    c.bench_function("vm", |b| {
        b.iter(|| {
            let scanner = Scanner::new(REGEX);
            let mut parser = Parser::new(&scanner);
            let regex = parser.parse().unwrap();
            let vm = VirtualMachine::from_expr(&regex);

            vm.matches("ababaaaabaaabbaab")
        })
    });
}

fn test_compiled_nfa(c: &mut Criterion) {
    let scanner = Scanner::new(REGEX);
    let mut parser = Parser::new(&scanner);
    let regex = parser.parse().unwrap();
    let compiler = Compiler::new();

    let nfa = compiler.compile(&regex);

    c.bench_function("nfa_compiled", |b| {
        b.iter(|| nfa.matches("ababaaaabaaabbaab"))
    });
}

fn test_compiled_dfa(c: &mut Criterion) {
    let scanner = Scanner::new(REGEX);
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

fn test_compiled_vm(c: &mut Criterion) {
    let scanner = Scanner::new(REGEX);
    let mut parser = Parser::new(&scanner);
    let regex = parser.parse().unwrap();
    let vm = VirtualMachine::from_expr(&regex);

    c.bench_function("vm_compiled", |b| {
        b.iter(|| vm.matches("ababaaaabaaabbaab"))
    });
}

criterion_group!(
    benches,
    test_nfa,
    test_dfa,
    test_vm,
    test_compiled_nfa,
    test_compiled_dfa,
    test_compiled_vm
);
criterion_main!(benches);
