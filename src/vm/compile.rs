use super::Instruction;
use crate::expr::{
    BlankExpr, ChoiceExpr, DigitExpr, Expr, OneOrMoreExpr, PrimitiveExpr, RepetitionExpr,
    SequenceExpr,
};

enum RelInstruction<'a> {
    Char(&'a str),
    Digit,
    Jmp(isize),
    Split(isize, isize),
}

enum InstrNode<T> {
    Instr(T),
    Block(InstrTree<T>),
}

struct InstrTree<T> {
    id: usize,
    instrs: Vec<InstrNode<T>>,
}

impl<T> InstrTree<T> {
    pub fn len(&self) -> usize {
        self.instrs.iter().fold(0, |acc, next| match next {
            InstrNode::Instr(_) => acc + 1,
            InstrNode::Block(block) => acc + block.len(),
        })
    }

    pub fn flatten(self) -> Vec<T> {
        self.instrs.into_iter().fold(Vec::new(), |mut acc, next| {
            match next {
                InstrNode::Instr(instr) => acc.push(instr),
                InstrNode::Block(block) => acc.extend(block.flatten()),
            };
            acc
        })
    }
}

pub(super) fn compile<'a>(expr: &'a Expr<'a>) -> Vec<Instruction<'a>> {
    let mut instrs: Vec<Instruction> = _compile(expr)
        .flatten()
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

fn _compile<'a>(expr: &'a Expr<'a>) -> InstrTree<RelInstruction<'a>> {
    match expr {
        Expr::Choice(exp) => compile_choice_expr(exp),
        Expr::Sequence(exp) => compile_sequence(exp),
        Expr::Repetition(exp) => compile_repetition(exp),
        Expr::OneOrMore(exp) => compile_oneormore(exp),
        Expr::Primitive(exp) => compile_primitive(exp),
        Expr::Digit(exp) => compile_digit(exp),
        Expr::Blank(exp) => compile_blank(exp),
    }
}

fn compile_primitive<'a>(exp: &'a PrimitiveExpr<'a>) -> InstrTree<RelInstruction<'a>> {
    let PrimitiveExpr { token, id } = exp;

    InstrTree {
        id: *id,
        instrs: vec![InstrNode::Instr(RelInstruction::Char(token.lexeme()))],
    }
}

fn compile_digit<'a>(exp: &'a DigitExpr) -> InstrTree<RelInstruction<'a>> {
    let DigitExpr { id } = exp;
    InstrTree {
        id: *id,
        instrs: vec![InstrNode::Instr(RelInstruction::Digit)],
    }
}

fn compile_blank<'a>(exp: &'a BlankExpr) -> InstrTree<RelInstruction<'a>> {
    let BlankExpr { id } = exp;
    InstrTree {
        id: *id,
        instrs: vec![],
    }
}

fn compile_choice_expr<'a>(exp: &'a ChoiceExpr<'a>) -> InstrTree<RelInstruction<'a>> {
    let ChoiceExpr { a, b, id } = exp;

    let a_instrs = _compile(&*a);
    let b_instrs = _compile(&*b);

    let mut instrs = Vec::new();

    // Split 1, 5
    // A instr
    // A instr
    // A instr
    // Jmp ?
    // B instr
    // B instr
    // MATCH

    instrs.push(InstrNode::Instr(RelInstruction::Split(
        1,
        a_instrs.len() as isize + 2,
    )));
    instrs.push(InstrNode::Block(a_instrs));
    instrs.push(InstrNode::Instr(RelInstruction::Jmp(
        b_instrs.len() as isize + 1,
    )));
    instrs.push(InstrNode::Block(b_instrs));

    InstrTree { id: *id, instrs }
}

fn compile_sequence<'a>(exp: &'a SequenceExpr<'a>) -> InstrTree<RelInstruction<'a>> {
    let SequenceExpr { id, start, end } = exp;

    let mut start_instrs = _compile(start);
    let end_instrs = _compile(end);

    InstrTree {
        id: *id,
        instrs: vec![InstrNode::Block(start_instrs), InstrNode::Block(end_instrs)],
    }
}

fn compile_repetition<'a>(exp: &'a RepetitionExpr<'a>) -> InstrTree<RelInstruction<'a>> {
    let RepetitionExpr { term, id } = exp;
    let term_instrs = _compile(term);
    let stride = term_instrs.len() as isize + 1;

    // spl 1, 4
    // instr a
    // instr a
    // jmp -3
    // MATCH

    let mut instrs = Vec::new();
    instrs.push(InstrNode::Instr(RelInstruction::Split(1, stride + 1)));
    instrs.push(InstrNode::Block(term_instrs));
    instrs.push(InstrNode::Instr(RelInstruction::Jmp(-stride)));

    InstrTree { id: *id, instrs }
}

fn compile_oneormore<'a>(exp: &'a OneOrMoreExpr<'a>) -> InstrTree<RelInstruction<'a>> {
    let OneOrMoreExpr { term, id } = exp;
    let term_instrs = _compile(term);
    let stride = term_instrs.len() as isize;

    // instr a
    // instr a
    // split -2, 1
    // MATCH

    let mut instrs = Vec::new();
    instrs.push(InstrNode::Block(term_instrs));
    instrs.push(InstrNode::Instr(RelInstruction::Split(-stride, 1)));

    InstrTree { id: *id, instrs }
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
