use super::Instruction;
use crate::expr::{ChoiceExpr, Expr, OneOrMoreExpr, PrimitiveExpr, RepetitionExpr, SequenceExpr};

enum RelInstruction<'a> {
    Char(&'a str),
    Digit,
    Jmp(isize),
    Split(isize, isize),
}

pub(super) fn compile<'a>(expr: &'a Expr<'a>) -> Vec<Instruction<'a>> {
    let mut instrs: Vec<Instruction> = _compile(expr)
        .iter()
        .enumerate()
        .map(|(i, instr)| match instr {
            RelInstruction::Char(c) => Instruction::Char(c),
            RelInstruction::Digit => Instruction::Digit,
            RelInstruction::Jmp(offset) => {
                let addr = (i as isize) + offset;
                Instruction::Jmp(addr as usize)
            }
            RelInstruction::Split(o1, o2) => {
                let a1 = (i as isize) + o1;
                let a2 = (i as isize) + o2;

                Instruction::Split(a1 as usize, a2 as usize)
            }
        })
        .collect();

    instrs.push(Instruction::Match);
    instrs
}

fn _compile<'a>(expr: &'a Expr<'a>) -> Vec<RelInstruction<'a>> {
    match expr {
        Expr::Choice(exp) => compile_choice_expr(exp),
        Expr::Sequence(exp) => compile_sequence(exp),
        Expr::Repetition(exp) => compile_repetition(exp),
        Expr::OneOrMore(exp) => compile_oneormore(exp),
        Expr::Primitive(exp) => compile_primitive(exp),
        Expr::Digit(_) => compile_digit(),
        Expr::Blank(_) => vec![],
    }
}

fn compile_primitive<'a>(exp: &'a PrimitiveExpr<'a>) -> Vec<RelInstruction<'a>> {
    let PrimitiveExpr { token, .. } = exp;

    vec![RelInstruction::Char(token.lexeme())]
}

fn compile_digit<'a>() -> Vec<RelInstruction<'a>> {
    vec![RelInstruction::Digit]
}

fn compile_choice_expr<'a>(exp: &'a ChoiceExpr<'a>) -> Vec<RelInstruction<'a>> {
    let ChoiceExpr { a, b, .. } = exp;

    let a_instrs = _compile(&*a);
    let b_instrs = _compile(&*b);

    let mut instrs = Vec::new();

    // Split 1, ?
    // A instr
    // A instr
    // A instr
    // Jmp ?
    // B instr
    // B instr
    // Match lol

    instrs.push(RelInstruction::Split(1, a_instrs.len() as isize + 2));
    instrs.extend(a_instrs);
    instrs.push(RelInstruction::Jmp(b_instrs.len() as isize + 1));
    instrs.extend(b_instrs);

    instrs
}

fn compile_sequence<'a>(exp: &'a SequenceExpr<'a>) -> Vec<RelInstruction<'a>> {
    let SequenceExpr { start, end, .. } = exp;

    let mut start_instrs = _compile(start);
    let end_instrs = _compile(end);

    start_instrs.extend(end_instrs);

    start_instrs
}

fn compile_repetition<'a>(exp: &'a RepetitionExpr<'a>) -> Vec<RelInstruction<'a>> {
    let RepetitionExpr { term, .. } = exp;
    let term_instrs = _compile(term);
    let stride = term_instrs.len() as isize + 1;

    let mut instrs = Vec::new();
    instrs.push(RelInstruction::Split(1, stride + 1));
    instrs.extend(term_instrs);
    instrs.push(RelInstruction::Jmp(-stride));

    instrs
}

fn compile_oneormore<'a>(exp: &'a OneOrMoreExpr<'a>) -> Vec<RelInstruction<'a>> {
    let OneOrMoreExpr { term, .. } = exp;
    let term_instrs = _compile(term);
    let stride = term_instrs.len() as isize;

    let mut instrs = Vec::new();
    instrs.extend(term_instrs);
    instrs.push(RelInstruction::Split(-stride, 1));

    instrs
}

#[cfg(test)]
mod test {
    use crate::{Parser, Scanner};

    use super::*;

    #[test]
    fn compiles() {
        let scanner = Scanner::new("a+b+");
        let mut parser = Parser::new(&scanner);
        let regex = parser.parse().unwrap();

        let instrs = compile(&regex);

        assert_eq!(
            instrs,
            vec![
                Instruction::Char("a"),
                Instruction::Split(0, 2),
                Instruction::Char("b"),
                Instruction::Split(2, 4),
                Instruction::Match,
            ]
        )
    }
}
