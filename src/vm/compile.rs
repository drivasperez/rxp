use super::Instruction;
use crate::expr::{
    BlankExpr, ChoiceExpr, DigitExpr, Expr, OneOrMoreExpr, PrimitiveExpr, RepetitionExpr,
    SequenceExpr,
};

#[derive(Debug)]
enum RelInstruction<'a> {
    Char(&'a str),
    Digit,
    Jmp(isize),
    Split(isize, isize),
}

#[derive(Debug)]
enum InstrNode<T: std::fmt::Debug> {
    Instr(T),
    Block(InstrTree<T>),
}

#[derive(Debug)]
pub struct InstrTree<T: std::fmt::Debug> {
    id: usize,
    title: String,
    instrs: Vec<InstrNode<T>>,
}

impl<T: std::fmt::Debug> InstrTree<T> {
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

fn graphviz_instr_row(line_number: usize, value: &str) -> String {
    format!(
        r#"
    <tr>
      <td port="{line_number}" colspan="1">{line_number}</td>
      <td colspan="5">{value}</td>
    </tr>
    "#
    )
}

fn graphviz_block_row(id: usize, title: &str) -> String {
    format!(
        r#"
    <tr>
      <td height="50" port="out{id}" colspan="6">{title}</td>
    </tr>
    "#
    )
}

impl<T: std::fmt::Debug + std::fmt::Display> InstrTree<T> {
    pub fn graphviz(&self, graph_name: &str) -> String {
        let (tables, edges): (Vec<_>, Vec<_>) = self.all_graphviz_tables().into_iter().unzip();

        let tables = tables.join("\n");
        let edges = edges.join("\n");

        format!(
            r#"
        digraph {graph_name} {{
            rankdir=LR
            {tables}
            {edges}
        }}
        "#
        )
    }

    fn all_graphviz_tables(&self) -> Vec<(String, String)> {
        let mut tables = Vec::new();

        tables.push(self.node());
        for instr in &self.instrs {
            if let InstrNode::Block(block) = instr {
                tables.push(block.node());
                tables.extend(block.all_graphviz_tables());
            }
        }

        tables
    }

    fn graphviz_table(&self) -> (String, String) {
        let id = self.id;
        let title = &self.title;
        let rows = self
            .instrs
            .iter()
            .map(|inst| match inst {
                InstrNode::Instr(instr) => graphviz_instr_row(0, &instr.to_string()),
                InstrNode::Block(block) => graphviz_block_row(block.id, &block.title),
            })
            .collect::<Vec<_>>()
            .join("\n");

        let edges = self
            .instrs
            .iter()
            .filter_map(|instr| {
                if let InstrNode::Block(block) = instr {
                    Some(format!("{id} -> {}", block.id))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        let table = format!(
            r#"
        <table style="rounded" border="1" cellborder="0" columns="*" rows="*" cellspacing="0" cellpadding="4">
          <tr>
            <td port="title" colspan="5">{title}</td>
            <td port="id" colspan="1">{id}</td>
          </tr>
          {rows}
        </table>
        "#
        );

        (table, edges)
    }

    fn node(&self) -> (String, String) {
        let id = self.id;
        let (table, edges) = self.graphviz_table();
        let table = format!(
            r#"{id} [shape="none" label=<
            {table}
            >]"#
        );

        (table, edges)
    }
}

fn make_abs_tree<'a>(
    tree: InstrTree<RelInstruction<'a>>,
    start: usize,
) -> InstrTree<Instruction<'a>> {
    InstrTree {
        id: tree.id,
        title: tree.title,
        instrs: tree
            .instrs
            .into_iter()
            .enumerate()
            .map(|(i, node)| (i + start, node))
            .map(|(i, node)| {
                dbg!(i, &node);
                match node {
                    InstrNode::Instr(instr) => InstrNode::Instr(match instr {
                        RelInstruction::Char(c) => Instruction::Char(c),
                        RelInstruction::Digit => Instruction::Digit,
                        RelInstruction::Jmp(offset) => {
                            let dst = i as isize + offset;
                            Instruction::Jmp(dst as usize)
                        }
                        RelInstruction::Split(o1, o2) => {
                            let d1 = i as isize + o1;
                            let d2 = i as isize + o2;

                            Instruction::Split(d1 as usize, d2 as usize)
                        }
                    }),
                    InstrNode::Block(block) => InstrNode::Block(make_abs_tree(block, i)),
                }
            })
            .collect(),
    }
}

pub fn vm_graphviz(expr: &Expr) -> String {
    let tree = make_abs_tree(_compile(expr), 0);

    tree.graphviz("Tree")
}

pub(super) fn compile<'a>(expr: &'a Expr<'a>) -> Vec<Instruction<'a>> {
    let mut instrs = make_abs_tree(_compile(expr), 0).flatten();
    println!("{instrs:?}");
    // let mut instrs: Vec<Instruction> = _compile(expr)
    //     .flatten()
    //     .iter()
    //     .enumerate()
    //     .map(|(i, instr)| match instr {
    //         RelInstruction::Char(c) => Instruction::Char(c),
    //         RelInstruction::Digit => Instruction::Digit,
    //         RelInstruction::Jmp(offset) => {
    //             let addr = (i as isize) + offset;
    //             Instruction::Jmp(addr as usize)
    //         }
    //         RelInstruction::Split(o1, o2) => {
    //             let a1 = (i as isize) + o1;
    //             let a2 = (i as isize) + o2;

    //             Instruction::Split(a1 as usize, a2 as usize)
    //         }
    //     })
    //     .collect();

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
        title: "Primitive".to_string(),
        instrs: vec![InstrNode::Instr(RelInstruction::Char(token.lexeme()))],
    }
}

fn compile_digit<'a>(exp: &'a DigitExpr) -> InstrTree<RelInstruction<'a>> {
    let DigitExpr { id } = exp;
    InstrTree {
        id: *id,
        title: "digit".to_string(),
        instrs: vec![InstrNode::Instr(RelInstruction::Digit)],
    }
}

fn compile_blank<'a>(exp: &'a BlankExpr) -> InstrTree<RelInstruction<'a>> {
    let BlankExpr { id } = exp;
    InstrTree {
        id: *id,
        title: "blank".to_string(),
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

    InstrTree {
        id: *id,
        title: "Choice".to_string(),
        instrs,
    }
}

fn compile_sequence<'a>(exp: &'a SequenceExpr<'a>) -> InstrTree<RelInstruction<'a>> {
    let SequenceExpr { id, start, end } = exp;

    let mut start_instrs = _compile(start);
    let end_instrs = _compile(end);

    InstrTree {
        id: *id,
        title: "Sequence".to_string(),
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

    InstrTree {
        id: *id,
        instrs,
        title: "Repetition".to_string(),
    }
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

    InstrTree {
        id: *id,
        title: "OneOrMore".to_string(),
        instrs,
    }
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

        println!("Oh no!");

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
