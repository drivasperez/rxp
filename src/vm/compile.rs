use super::Instruction;
use crate::expr::{
    BlankExpr, ChoiceExpr, DigitExpr, Expr, OneOrMoreExpr, PrimitiveExpr, RepetitionExpr,
    SequenceExpr,
};

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

pub fn vm_graphviz(expr: &Expr) -> String {
    let mut compiler = Compiler::new();
    let tree = compiler._compile(expr);

    tree.graphviz("Tree")
}

#[derive(Default)]
pub struct Compiler {
    line_number: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
    }

    fn current_ln(&self) -> usize {
        self.line_number
    }

    fn next_ln(&mut self) -> usize {
        let ln = self.line_number;
        self.line_number += 1;
        ln
    }

    pub(super) fn compile<'a>(mut self, expr: &'a Expr<'a>) -> Vec<Instruction<'a>> {
        let tree = self._compile(expr);
        let mut instrs = tree.flatten();

        // TODO: Add Match to tree rather than here for better visualisation.
        instrs.push(Instruction::Match);
        instrs
    }

    fn _compile<'a>(&mut self, expr: &'a Expr<'a>) -> InstrTree<Instruction<'a>> {
        match expr {
            Expr::Choice(exp) => self.compile_choice_expr(exp),
            Expr::Sequence(exp) => self.compile_sequence(exp),
            Expr::Repetition(exp) => self.compile_repetition(exp),
            Expr::OneOrMore(exp) => self.compile_oneormore(exp),
            Expr::Primitive(exp) => self.compile_primitive(exp),
            Expr::Digit(exp) => self.compile_digit(exp),
            Expr::Blank(exp) => self.compile_blank(exp),
        }
    }

    fn compile_primitive<'a>(&mut self, exp: &'a PrimitiveExpr<'a>) -> InstrTree<Instruction<'a>> {
        let PrimitiveExpr { token, id } = exp;

        InstrTree {
            id: *id,
            title: "Primitive".to_string(),
            instrs: vec![InstrNode::Instr(
                self.next_ln(),
                Instruction::Char(token.lexeme()),
            )],
        }
    }

    fn compile_digit<'a>(&mut self, exp: &'a DigitExpr) -> InstrTree<Instruction<'a>> {
        let DigitExpr { id } = exp;
        InstrTree {
            id: *id,
            title: "digit".to_string(),
            instrs: vec![InstrNode::Instr(self.next_ln(), Instruction::Digit)],
        }
    }

    fn compile_blank<'a>(&mut self, exp: &'a BlankExpr) -> InstrTree<Instruction<'a>> {
        let BlankExpr { id } = exp;
        InstrTree {
            id: *id,
            title: "blank".to_string(),
            instrs: vec![],
        }
    }

    fn compile_choice_expr<'a>(&mut self, exp: &'a ChoiceExpr<'a>) -> InstrTree<Instruction<'a>> {
        let ChoiceExpr { a, b, id } = exp;

        let mut instrs = Vec::new();

        // Split 1, 5
        // A instr
        // A instr
        // A instr
        // Jmp ?
        // B instr
        // B instr
        // MATCH

        let spl_line = self.next_ln();
        let spl_1 = self.current_ln();
        let a_instrs = self._compile(&*a);

        instrs.push(InstrNode::Instr(
            spl_line,
            Instruction::Split(spl_1, self.current_ln() + 1),
        ));
        instrs.push(InstrNode::Block(a_instrs));
        let jmp_line = self.next_ln();

        let b_instrs = self._compile(&*b);
        instrs.push(InstrNode::Instr(
            jmp_line,
            Instruction::Jmp(self.current_ln()),
        ));
        instrs.push(InstrNode::Block(b_instrs));

        InstrTree {
            id: *id,
            title: "Choice".to_string(),
            instrs,
        }
    }

    fn compile_sequence<'a>(&mut self, exp: &'a SequenceExpr<'a>) -> InstrTree<Instruction<'a>> {
        let SequenceExpr { id, start, end } = exp;

        let start_instrs = self._compile(start);
        let end_instrs = self._compile(end);

        InstrTree {
            id: *id,
            title: "Sequence".to_string(),
            instrs: vec![InstrNode::Block(start_instrs), InstrNode::Block(end_instrs)],
        }
    }

    fn compile_repetition<'a>(
        &mut self,
        exp: &'a RepetitionExpr<'a>,
    ) -> InstrTree<Instruction<'a>> {
        let RepetitionExpr { term, id } = exp;

        // 0: spl 1, 3
        // 1: instr a
        // 2: jmp 0
        // 3: MATCH

        let mut instrs = Vec::new();

        let spl_line = self.next_ln();
        let spl_1 = spl_line + 1;

        let term_instrs = self._compile(term);
        let block_len = term_instrs.len();
        instrs.push(InstrNode::Instr(
            spl_line,
            Instruction::Split(spl_1, self.current_ln() + 1),
        ));
        instrs.push(InstrNode::Block(term_instrs));
        instrs.push(InstrNode::Instr(
            self.next_ln(),
            Instruction::Jmp(self.current_ln() - (block_len + 1)),
        ));

        InstrTree {
            id: *id,
            instrs,
            title: "Repetition".to_string(),
        }
    }

    fn compile_oneormore<'a>(&mut self, exp: &'a OneOrMoreExpr<'a>) -> InstrTree<Instruction<'a>> {
        let OneOrMoreExpr { term, id } = exp;

        // instr a
        // instr a
        // split -2, 1
        // MATCH

        let mut instrs = Vec::new();

        let term_instrs = self._compile(term);
        let stride = term_instrs.len();
        instrs.push(InstrNode::Block(term_instrs));
        instrs.push(InstrNode::Instr(
            self.next_ln(),
            Instruction::Split(self.current_ln() - 1 - stride, self.current_ln()),
        ));

        InstrTree {
            id: *id,
            title: "OneOrMore".to_string(),
            instrs,
        }
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

pub fn instructions_graphviz(expr: &Expr, graph_name: &str) -> String {
    let compiler = Compiler::new();
    let rows: Vec<String> = compiler
        .compile(expr)
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

#[cfg(test)]
mod test {
    use crate::{Parser, Scanner};

    use super::*;

    #[test]
    fn compiles() {
        let scanner = Scanner::new("a+b+");
        let mut parser = Parser::new(&scanner);
        let regex = parser.parse().unwrap();

        let compiler = Compiler::new();
        let instrs = compiler.compile(&regex);

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
