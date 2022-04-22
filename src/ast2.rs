use std::collections::BTreeMap;

use regex_syntax::ast::{
    Alternation, Ast, Class, ClassAscii, ClassBracketed, ClassPerl, ClassSet, ClassSetBinaryOp,
    ClassSetRange, ClassUnicode, Concat, Group, Literal, Repetition, RepetitionRange,
};

use crate::graphviz::{DiGraph, Style};

impl crate::graphviz::Graphviz for Ast {
    fn graphviz(&self, graph_name: &str) -> String {
        let mut ids = BTreeMap::new();
        let mut id_counter = 0;

        visit(self, &mut |ast| {
            ids.insert(id_counter, ast);
            id_counter += 1;
        });

        let get_id = |ast: &Ast| {
            ids.iter()
                .find_map(|(id, candidate)| if ast == *candidate { Some(id) } else { None })
                .unwrap()
        };

        let mut digraph = DiGraph::new(graph_name);

        visit(self, &mut |ast| match ast {
            Ast::Empty(_) => {
                digraph.vertex(
                    get_id(ast),
                    Style::new().label("Empty").fontname("Monospace"),
                );
            }
            Ast::Flags(flags) => {
                digraph.vertex(
                    get_id(ast),
                    Style::new()
                        .label(format!("{flags:?}"))
                        .fontname("Monospace"),
                );
            }
            Ast::Literal(Literal { c, .. }) => {
                digraph.vertex(
                    get_id(ast),
                    Style::new().label(format!("{c}")).fontname("Monospace"),
                );
            }
            Ast::Dot(_) => {
                digraph.vertex(get_id(ast), Style::new().label("Dot").fontname("Monospace"));
            }
            Ast::Assertion(assertion) => {
                digraph.vertex(
                    get_id(ast),
                    Style::new()
                        .label(format!("{assertion:?}"))
                        .fontname("Monospace"),
                );
            }
            Ast::Class(class) => {
                let label = match class {
                    Class::Unicode(ClassUnicode {
                        span,
                        negated,
                        kind,
                    }) => match kind {
                        regex_syntax::ast::ClassUnicodeKind::OneLetter(c) => {
                            if *negated {
                                format!("^{c}")
                            } else {
                                c.to_string()
                            }
                        }
                        regex_syntax::ast::ClassUnicodeKind::Named(named) => {
                            format!("NamedClass ({named})")
                        }
                        regex_syntax::ast::ClassUnicodeKind::NamedValue { op, name, value } => {
                            format!("NamedValue ({name}:{value})")
                        }
                    },
                    Class::Perl(p) => match p.kind {
                        regex_syntax::ast::ClassPerlKind::Digit => format!("Class (Digit)"),
                        regex_syntax::ast::ClassPerlKind::Space => format!("Class (Space)"),
                        regex_syntax::ast::ClassPerlKind::Word => format!("Class (Word)"),
                    },
                    Class::Bracketed(ClassBracketed {
                        span,
                        negated,
                        kind,
                    }) => {
                        match kind {
                            ClassSet::Item(item) => match item {
                                regex_syntax::ast::ClassSetItem::Empty(_) => format!("Class []"),
                                regex_syntax::ast::ClassSetItem::Literal(Literal {
                                    c,
                                    span,
                                    kind,
                                }) => {
                                    let b = match kind {
                                        regex_syntax::ast::LiteralKind::Verbatim => "Verbatim",
                                        regex_syntax::ast::LiteralKind::Punctuation => "Punctuation",
                                        regex_syntax::ast::LiteralKind::Octal => "Octal",
                                        regex_syntax::ast::LiteralKind::HexFixed(h) => match h {
                                            regex_syntax::ast::HexLiteralKind::X => "HexFixed:X",
                                            regex_syntax::ast::HexLiteralKind::UnicodeShort => "HexFixed:UnicodeShort",
                                            regex_syntax::ast::HexLiteralKind::UnicodeLong => "HexFixed:UnicodeLong",
                                        },
                                        regex_syntax::ast::LiteralKind::HexBrace(br) => match br {
                                            regex_syntax::ast::HexLiteralKind::X => "HexBrace:X",
                                            regex_syntax::ast::HexLiteralKind::UnicodeShort => "HexBrace:UnicodeShort",
                                            regex_syntax::ast::HexLiteralKind::UnicodeLong => "HexBrace:UnicodeLong",
                                        },
                                        regex_syntax::ast::LiteralKind::Special(sp) => match sp {
                                            regex_syntax::ast::SpecialLiteralKind::Bell => "Bell",
                                            regex_syntax::ast::SpecialLiteralKind::FormFeed => "FormFeed",
                                            regex_syntax::ast::SpecialLiteralKind::Tab => "Tab",
                                            regex_syntax::ast::SpecialLiteralKind::LineFeed => "LineFeed",
                                            regex_syntax::ast::SpecialLiteralKind::CarriageReturn => "CarriageReturn",
                                            regex_syntax::ast::SpecialLiteralKind::VerticalTab => "VerticalTab",
                                            regex_syntax::ast::SpecialLiteralKind::Space => "Space",
                                        },
                                    };
                                    format!("ClassLiteral:{b} ({c})")
                                }
                                regex_syntax::ast::ClassSetItem::Range(ClassSetRange {
                                    start,
                                    end,
                                    ..
                                }) => format!("Class [{}-{}]", start.c, end.c),
                                regex_syntax::ast::ClassSetItem::Ascii(ClassAscii {
                                    kind,
                                    negated,
                                    ..
                                }) => {
                                    let mut label = match kind {
                                        regex_syntax::ast::ClassAsciiKind::Alnum => {
                                            format!("ClassAscii (Alnum)")
                                        }
                                        regex_syntax::ast::ClassAsciiKind::Alpha => {
                                            format!("ClassAscii (Alpha)")
                                        }
                                        regex_syntax::ast::ClassAsciiKind::Ascii => {
                                            format!("ClassAscii (Ascii)")
                                        }
                                        regex_syntax::ast::ClassAsciiKind::Blank => {
                                            format!("ClassAscii (Blank)")
                                        }
                                        regex_syntax::ast::ClassAsciiKind::Cntrl => {
                                            format!("ClassAscii (Cntrl)")
                                        }
                                        regex_syntax::ast::ClassAsciiKind::Digit => {
                                            format!("ClassAscii (Digit)")
                                        }
                                        regex_syntax::ast::ClassAsciiKind::Graph => {
                                            format!("ClassAscii (Graph)")
                                        }
                                        regex_syntax::ast::ClassAsciiKind::Lower => {
                                            format!("ClassAscii (Lower)")
                                        }
                                        regex_syntax::ast::ClassAsciiKind::Print => {
                                            format!("ClassAscii (Print)")
                                        }
                                        regex_syntax::ast::ClassAsciiKind::Punct => {
                                            format!("ClassAscii (Punct)")
                                        }
                                        regex_syntax::ast::ClassAsciiKind::Space => {
                                            format!("ClassAscii (Space)")
                                        }
                                        regex_syntax::ast::ClassAsciiKind::Upper => {
                                            format!("ClassAscii (Upper)")
                                        }
                                        regex_syntax::ast::ClassAsciiKind::Word => {
                                            format!("ClassAscii (Word)")
                                        }
                                        regex_syntax::ast::ClassAsciiKind::Xdigit => {
                                            format!("ClassAscii (Xdigit)")
                                        }
                                    };

                                    if *negated {
                                        label = format!("{label} (Negated)");
                                    }

                                    label
                                }
                                regex_syntax::ast::ClassSetItem::Unicode(ClassUnicode {
                                    span,
                                    negated,
                                    kind,
                                }) => {
                                    let mut kind = match kind {
                                        regex_syntax::ast::ClassUnicodeKind::OneLetter(l) => {
                                            format!("ClassUnicodeOneLetter ({l})")
                                        }
                                        regex_syntax::ast::ClassUnicodeKind::Named(name) => {
                                            format!("ClassUnicodeNamed ({name})")
                                        }
                                        regex_syntax::ast::ClassUnicodeKind::NamedValue {
                                            op,
                                            name,
                                            value,
                                        } => {
                                            let x = match op {
                                                regex_syntax::ast::ClassUnicodeOpKind::Equal => "=",
                                                regex_syntax::ast::ClassUnicodeOpKind::Colon => ":",
                                                regex_syntax::ast::ClassUnicodeOpKind::NotEqual => {
                                                    "!="
                                                }
                                            };
                                            format!("ClassUnicodeNamedValue ({name} {x} {value})")
                                        }
                                    };
                                    if *negated {
                                        kind = format!("{kind} (Negated)");
                                    }
                                    kind
                                }
                                regex_syntax::ast::ClassSetItem::Perl(ClassPerl {
                                    kind,
                                    negated,
                                    ..
                                }) => {
                                    let mut kind = match kind {
                                        regex_syntax::ast::ClassPerlKind::Digit => {
                                            format!("ClassPerl (Digit)")
                                        }
                                        regex_syntax::ast::ClassPerlKind::Space => {
                                            format!("ClassPerl (Space)")
                                        }
                                        regex_syntax::ast::ClassPerlKind::Word => {
                                            format!("ClassPerl (Word)")
                                        }
                                    };

                                    if *negated {
                                        kind = format!("{kind} (Negated)");
                                    }
                                    kind
                                }
                                regex_syntax::ast::ClassSetItem::Bracketed(br) => {
                                    format!("ClassSetItem (Bracketed)")
                                }
                                regex_syntax::ast::ClassSetItem::Union(un) => {
                                    format!("ClassSetItem (Union)")
                                }
                            },
                            ClassSet::BinaryOp(ClassSetBinaryOp {
                                span,
                                kind,
                                lhs,
                                rhs,
                            }) => {
                                let kind = match kind {
                                regex_syntax::ast::ClassSetBinaryOpKind::Intersection => "Intersection",
                                regex_syntax::ast::ClassSetBinaryOpKind::Difference => "Difference",
                                regex_syntax::ast::ClassSetBinaryOpKind::SymmetricDifference => "SymmetricDifference",
                            };
                                format!("ClassSet BinaryOp {kind}")
                            }
                        }
                    }
                };
                digraph.vertex(get_id(ast), Style::new().label(label).fontname("Monospace"));
            }
            Ast::Repetition(Repetition {
                op,
                greedy,
                ast: new_ast,
                ..
            }) => {
                let id = get_id(ast);
                let mut label = match &op.kind {
                    regex_syntax::ast::RepetitionKind::ZeroOrOne => "ZeroOrOne".to_string(),
                    regex_syntax::ast::RepetitionKind::ZeroOrMore => "ZeroOrMore".to_string(),
                    regex_syntax::ast::RepetitionKind::OneOrMore => "OneOrMore".to_string(),
                    regex_syntax::ast::RepetitionKind::Range(r) => match r {
                        RepetitionRange::Exactly(n) => format!("Exactly {n}"),
                        RepetitionRange::AtLeast(n) => format!("AtLeast {n}"),
                        RepetitionRange::Bounded(n, m) => format!("Bounded {n}-{m}"),
                    },
                };

                if *greedy {
                    label = format!("{label} (Greedy)");
                }

                digraph.vertex(
                    get_id(ast),
                    Style::new().label(&label).fontname("Monospace"),
                );
                digraph.edge(id, get_id(new_ast), None);
            }
            Ast::Group(Group {
                kind, ast: new_ast, ..
            }) => {
                let id = get_id(ast);
                let label = match kind {
                    regex_syntax::ast::GroupKind::CaptureIndex(idx) => {
                        format!("CaptureIndex ({idx})")
                    }
                    regex_syntax::ast::GroupKind::CaptureName(name) => {
                        format!("CaptureName ({})", name.name)
                    }
                    regex_syntax::ast::GroupKind::NonCapturing(_) => format!("NonCapturing"),
                };
                digraph.vertex(
                    get_id(ast),
                    Style::new().label(&label).fontname("Monospace"),
                );
                digraph.edge(id, get_id(new_ast), None);
            }
            Ast::Alternation(Alternation { asts, .. }) => {
                let id = get_id(ast);
                digraph.vertex(
                    get_id(ast),
                    Style::new().label("Alternation").fontname("Monospace"),
                );
                for a in asts {
                    digraph.edge(id, get_id(a), None);
                }
            }
            Ast::Concat(Concat { asts, .. }) => {
                let id = get_id(ast);
                digraph.vertex(
                    get_id(ast),
                    Style::new().label("Concat").fontname("Monospace"),
                );
                for a in asts {
                    digraph.edge(id, get_id(a), None);
                }
            }
        });

        digraph.to_string()
    }
}

fn visit<'a>(ast: &'a Ast, func: &mut impl FnMut(&'a Ast)) {
    func(ast);
    match ast {
        Ast::Repetition(Repetition { ast, .. }) | Ast::Group(Group { ast, .. }) => visit(ast, func),
        Ast::Alternation(Alternation { asts, .. }) | Ast::Concat(Concat { asts, .. }) => {
            for ast in asts {
                visit(ast, func);
            }
        }
        _ => {}
    }
}
