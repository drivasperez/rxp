use crate::ToGraphviz;
use unicode_segmentation::GraphemeCursor;

#[derive(Copy, Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    Pipe,
    Star,
    GraphemeCluster,
    BackSlash,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub length: usize,
}

impl Token {
    /// Given a token and a source buffer, return a slice corresponding to the token's lexeme.
    /// Panics if the lexeme is not valid UTF-8; a good way of achieving that is passing a source
    /// buffer that was not used to generate the token.
    pub fn lexeme<'a>(&self, source: &'a str) -> &'a str {
        let bytes = &source.as_bytes()[self.start..self.start + self.length];

        std::str::from_utf8(bytes).unwrap()
    }
}

pub struct Scanner<'a> {
    source: &'a str,
}

impl<'a> Scanner<'a> {
    /// Creates a new scanner wrapping a string slice.
    pub fn new(source: &'a str) -> Self {
        Self { source }
    }

    /// Produces an iterator of tokens from the scanner.
    pub fn tokens(&'a self) -> Tokens<'a> {
        let cursor = GraphemeCursor::new(0, self.source.len(), true);

        Tokens {
            scanner: self,
            cursor,
        }
    }

    pub fn source(&'a self) -> &'a str {
        self.source
    }
}

impl ToGraphviz for Scanner<'_> {
    fn graphviz(&self, graph_name: &str, _: &str) -> String {
        let mut edges = Vec::new();

        for (i, token) in self.tokens().enumerate() {
            if let TokenKind::GraphemeCluster = &token.kind {
                let lexeme = token.lexeme(self.source);
                edges.push(format!("  {i} [label=\"{lexeme}\"]"));
            } else {
                let kind = &token.kind;
                edges.push(format!("  {i} [label=\"{kind:?}\"]"));
            }

            if i != 0 {
                edges.push(format!("  {} -> {}", i - 1, i))
            }
        }

        let edges = edges.join("\n");
        format!(
            "digraph {graph_name} {{\n\
                rankdir=LR;\n
            {edges}\n\
            }}"
        )
    }
}

pub struct Tokens<'a> {
    scanner: &'a Scanner<'a>,
    cursor: GraphemeCursor,
}

impl Tokens<'_> {
    fn next_token(&mut self) -> Option<Token> {
        let start = self.cursor.cur_cursor();
        let next_boundary = self.cursor.next_boundary(self.scanner.source, 0).unwrap()?;
        let length = next_boundary - start;
        let lexeme = &self.scanner.source[start..next_boundary];

        let kind = match lexeme {
            "(" => TokenKind::LeftParen,
            ")" => TokenKind::RightParen,
            "*" => TokenKind::Star,
            "|" => TokenKind::Pipe,
            "\\" => TokenKind::BackSlash,
            _ => TokenKind::GraphemeCluster,
        };

        Some(Token {
            kind,
            start,
            length,
        })
    }
}

impl std::iter::Iterator for Tokens<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn ascii_lexeme() {
        let token = Token {
            kind: TokenKind::LeftParen,
            start: 1,
            length: 1,
        };

        let source = "a(abcde";

        assert_eq!(token.lexeme(source), "(");
    }

    #[test]
    fn unicode_lexeme() {
        let token = Token {
            kind: TokenKind::GraphemeCluster,
            start: 1,
            length: 4,
        };
        let source = "a💖cde";
        assert_eq!(token.lexeme(source), "💖");
    }

    #[test]
    fn ascii_tokens() {
        let source = "ab)c";
        let tokens: Vec<_> = Scanner::new(source).tokens().collect();

        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenKind::GraphemeCluster,
                    start: 0,
                    length: 1
                },
                Token {
                    kind: TokenKind::GraphemeCluster,
                    start: 1,
                    length: 1
                },
                Token {
                    kind: TokenKind::RightParen,
                    start: 2,
                    length: 1
                },
                Token {
                    kind: TokenKind::GraphemeCluster,
                    start: 3,
                    length: 1
                },
            ]
        )
    }

    #[test]
    fn unicode_tokens() {
        let source = "ab💖*";
        let tokens: Vec<_> = Scanner::new(source).tokens().collect();

        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenKind::GraphemeCluster,
                    start: 0,
                    length: 1
                },
                Token {
                    kind: TokenKind::GraphemeCluster,
                    start: 1,
                    length: 1
                },
                Token {
                    kind: TokenKind::GraphemeCluster,
                    start: 2,
                    length: 4
                },
                Token {
                    kind: TokenKind::Star,
                    start: 6,
                    length: 1
                },
            ]
        )
    }

    #[test]
    fn peeking() {
        let source = "a(*|";
        let scanner = Scanner::new(source);
        let mut tokens = scanner.tokens().peekable();

        assert_eq!(
            tokens.next(),
            Some(Token {
                start: 0,
                length: 1,
                kind: TokenKind::GraphemeCluster
            })
        );

        assert_eq!(
            tokens.peek(),
            Some(&Token {
                start: 1,
                length: 1,
                kind: TokenKind::LeftParen
            })
        );

        assert_eq!(
            tokens.peek(),
            Some(&Token {
                start: 1,
                length: 1,
                kind: TokenKind::LeftParen
            })
        );

        assert_eq!(
            tokens.next(),
            Some(Token {
                start: 1,
                length: 1,
                kind: TokenKind::LeftParen
            })
        );

        assert_eq!(
            tokens.next(),
            Some(Token {
                start: 2,
                length: 1,
                kind: TokenKind::Star
            })
        );
    }
}
