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
    Instr(usize, T),
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
            InstrNode::Instr(_, _) => acc + 1,
            InstrNode::Block(block) => acc + block.len(),
        })
    }

    fn initial_port(&self) -> Option<String> {
        let node = self.instrs.get(0)?;
        let port = match node {
            InstrNode::Instr(ln, _) => format!("line{ln}"),
            InstrNode::Block(block) => format!("block{}", block.id),
        };

        Some(port)
    }

    pub fn starting_line(&self) -> Option<usize> {
        match self.instrs.first()? {
            InstrNode::Instr(ln, _) => Some(*ln),
            InstrNode::Block(block) => block.starting_line(),
        }
    }

    pub fn ending_line(&self) -> Option<usize> {
        match self.instrs.last()? {
            InstrNode::Instr(ln, _) => Some(*ln),
            InstrNode::Block(block) => block.ending_line(),
        }
    }

    pub fn flatten(self) -> Vec<T> {
        self.instrs.into_iter().fold(Vec::new(), |mut acc, next| {
            match next {
                InstrNode::Instr(_line_number, instr) => acc.push(instr),
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
      <td port="line{line_number}" colspan="1">{line_number}</td>
      <td colspan="5">{value}</td>
    </tr>
    "#
    )
}

fn graphviz_block_row(id: usize, title: &str) -> String {
    format!(
        r#"
    <tr>
      <td height="50" port="block{id}" colspan="6">{title}</td>
    </tr>
    "#
    )
}

impl<T: std::fmt::Debug + std::fmt::Display> InstrTree<T> {
    pub fn graphviz(&self, graph_name: &str) -> String {
        let (tables, mut edges): (Vec<_>, Vec<_>) = self.all_graphviz_tables().into_iter().unzip();

        let tables = tables.join("\n");
        // TODO: Generate edges in a way that doesn't produce duplicates
        edges.sort_unstable();
        edges.dedup();
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
                InstrNode::Instr(line_num, instr) => {
                    graphviz_instr_row(*line_num, &instr.to_string())
                }
                InstrNode::Block(block) => {
                    let start = block
                        .starting_line()
                        .map(|n| n.to_string())
                        .unwrap_or_default();
                    let end = block
                        .ending_line()
                        .map(|n| n.to_string())
                        .unwrap_or_default();

                    let range = if start == end {
                        start
                    } else {
                        format!("{start}...{end}")
                    };
                    graphviz_block_row(block.id, &range)
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        let edges = self
            .instrs
            .iter()
            .filter_map(|instr| {
                if let InstrNode::Block(block) = instr {
                    Some(format!(
                        "{id}:block{} -> {}:{};",
                        block.id,
                        block.id,
                        block.initial_port()?
                    ))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        let table = format!(
            r#"
        <table style="rounded" border="1" cellborder="0" columns="*" rows="*" cellspacing="0" cellpadding="8">
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
            r#"{id} [shape="none" fontname="Monospace" label=<
            {table}
            >]"#
        );

        (table, edges)
    }
}

fn correct_line_numbers<'a, T: std::fmt::Debug>(tree: InstrTree<T>, start: usize) -> InstrTree<T> {
    InstrTree {
        id: tree.id,
        title: tree.title,
        instrs: tree
            .instrs
            .into_iter()
            .fold(
                (Vec::new(), start),
                |(mut acc, line_num), next| match next {
                    InstrNode::Instr(old_ln, t) => {
                        acc.push(InstrNode::Instr(old_ln + line_num, t));
                        (acc, line_num + 1)
                    }
                    InstrNode::Block(block) => {
                        let new_len = line_num + block.len();
                        acc.push(InstrNode::Block(correct_line_numbers(block, line_num)));
                        (acc, new_len)
                    }
                },
            )
            .0,
    }
}

fn make_abs_tree<'a>(tree: InstrTree<RelInstruction<'a>>) -> InstrTree<Instruction<'a>> {
    InstrTree {
        id: tree.id,
        title: tree.title,
        instrs: tree
            .instrs
            .into_iter()
            .map(|node| match node {
                InstrNode::Instr(line_num, instr) => InstrNode::Instr(
                    line_num,
                    match instr {
                        RelInstruction::Char(c) => Instruction::Char(c),
                        RelInstruction::Digit => Instruction::Digit,
                        RelInstruction::Jmp(offset) => {
                            let dst = line_num as isize + offset;
                            Instruction::Jmp(dst as usize)
                        }
                        RelInstruction::Split(o1, o2) => {
                            let d1 = line_num as isize + o1;
                            let d2 = line_num as isize + o2;

                            Instruction::Split(d1 as usize, d2 as usize)
                        }
                    },
                ),
                InstrNode::Block(block) => InstrNode::Block(make_abs_tree(block)),
            })
            .collect(),
    }
}

pub fn vm_graphviz(expr: &Expr) -> String {
    let tree = _compile(expr);
    let tree = correct_line_numbers(tree, 0);
    let tree = make_abs_tree(tree);

    tree.graphviz("Tree")
}

pub(super) fn compile<'a>(expr: &'a Expr<'a>) -> Vec<Instruction<'a>> {
    let tree = _compile(expr);
    let tree = correct_line_numbers(tree, 0);
    let tree = make_abs_tree(tree);
    let mut instrs = tree.flatten();

    // TODO: Add Match to tree rather than here for better visualisation.
    instrs.push(Instruction::Match);
    instrs
}

pub fn instructions_graphviz(expr: &Expr, graph_name: &str) -> String {
    let rows: Vec<String> = compile(expr)
        .into_iter()
        .enumerate()
        .map(|(i, instr)| graphviz_instr_row(i, &instr.to_string()))
        .collect();

    let rows = rows.join("\n");
    format!(
        r#"
        digraph "{graph_name}" {{
            rankdir=LR
            table [shape="none" fontname="Monospace" label=<
        <table style="rounded" border="1" cellborder="0" columns="*" rows="*" cellspacing="0" cellpadding="8">
          <tr>
            <td port="title" colspan="6">{graph_name}</td>
          </tr>
          {rows}
        </table>
            >]
        }}
    "#
    )
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
        instrs: vec![InstrNode::Instr(0, RelInstruction::Char(token.lexeme()))],
    }
}

fn compile_digit<'a>(exp: &'a DigitExpr) -> InstrTree<RelInstruction<'a>> {
    let DigitExpr { id } = exp;
    InstrTree {
        id: *id,
        title: "digit".to_string(),
        instrs: vec![InstrNode::Instr(0, RelInstruction::Digit)],
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

    instrs.push(InstrNode::Instr(
        0,
        RelInstruction::Split(1, a_instrs.len() as isize + 2),
    ));
    instrs.push(InstrNode::Block(a_instrs));
    instrs.push(InstrNode::Instr(
        0,
        RelInstruction::Jmp(b_instrs.len() as isize + 1),
    ));
    instrs.push(InstrNode::Block(b_instrs));

    InstrTree {
        id: *id,
        title: "Choice".to_string(),
        instrs,
    }
}

fn compile_sequence<'a>(exp: &'a SequenceExpr<'a>) -> InstrTree<RelInstruction<'a>> {
    let SequenceExpr { id, start, end } = exp;

    let start_instrs = _compile(start);
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

    // 0: spl 1, 3
    // 1: instr a
    // 2: jmp 0
    // 3: MATCH

    let mut instrs = Vec::new();
    instrs.push(InstrNode::Instr(0, RelInstruction::Split(1, stride + 1)));
    instrs.push(InstrNode::Block(term_instrs));
    instrs.push(InstrNode::Instr(0, RelInstruction::Jmp(-stride)));

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
    instrs.push(InstrNode::Instr(0, RelInstruction::Split(-stride, 1)));

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
