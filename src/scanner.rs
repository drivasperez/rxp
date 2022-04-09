use unicode_segmentation::Graphemes;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Copy, Debug, Clone, PartialEq, Eq)]
pub enum Token<'a> {
    LeftParen,
    RightParen,
    Pipe,
    Star,
    GraphemeCluster(&'a str),
    BackSlash,
}

impl<'a> Token<'a> {
    /// Given a token and a source buffer, return a slice corresponding to the token's lexeme.
    /// Panics if the lexeme is not valid UTF-8; a good way of achieving that is passing a source
    /// buffer that was not used to generate the token.
    pub fn lexeme(&self) -> &'a str {
        match self {
            Self::LeftParen => "(",
            Self::RightParen => ")",
            Self::Pipe => "|",
            Self::Star => "*",
            Self::GraphemeCluster(s) => s,
            Self::BackSlash => "\\",
        }
    }

    pub fn kind(&self) -> &'static str {
        match self {
            Self::LeftParen => "LeftParen",
            Self::RightParen => "RightParen",
            Self::Pipe => "Pipe",
            Self::Star => "Star",
            Self::GraphemeCluster(_) => "GraphemeCluster",
            Self::BackSlash => "BackSlash",
        }
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
        Tokens {
            graphemes: self.source.graphemes(true),
        }
    }

    pub fn source(&'a self) -> &'a str {
        self.source
    }
}

impl Scanner<'_> {
    pub fn graphviz(&self, graph_name: &str) -> String {
        let mut edges = Vec::new();

        for (i, token) in self.tokens().enumerate() {
            if let Token::GraphemeCluster(_) = &token {
                let lexeme = token.lexeme();
                edges.push(format!("  {i} [label=\"{lexeme}\"]"));
            } else {
                let kind = &token.kind();
                edges.push(format!("  {i} [label={kind:?}]"));
            }

            if i != 0 {
                edges.push(format!("  {} -> {};", i - 1, i))
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
    graphemes: Graphemes<'a>,
}

impl<'a> Tokens<'a> {
    fn next_token(&mut self) -> Option<Token<'a>> {
        self.graphemes.next().map(|lexeme| match lexeme {
            "(" => Token::LeftParen,
            ")" => Token::RightParen,
            "*" => Token::Star,
            "|" => Token::Pipe,
            "\\" => Token::BackSlash,
            other => Token::GraphemeCluster(other),
        })
    }
}

impl<'a> std::iter::Iterator for Tokens<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod test {
    // use super::*;

    // #[test]
    // fn ascii_lexeme() {
    //     let source = "a(abcde";
    //     let token = Token {
    //         kind: TokenKind::LeftParen,
    //     };

    //     assert_eq!(token.lexeme(source), "(");
    // }

    // #[test]
    // fn unicode_lexeme() {
    //     let token = Token {
    //         kind: TokenKind::GraphemeCluster,
    //         start: 1,
    //         length: 4,
    //     };
    //     let source = "a💖cde";
    //     assert_eq!(token.lexeme(source), "💖");
    // }

    // #[test]
    // fn ascii_tokens() {
    //     let source = "ab)c";
    //     let tokens: Vec<_> = Scanner::new(source).tokens().collect();

    //     assert_eq!(
    //         tokens,
    //         vec![
    //             Token {
    //                 kind: TokenKind::GraphemeCluster,
    //                 start: 0,
    //                 length: 1
    //             },
    //             Token {
    //                 kind: TokenKind::GraphemeCluster,
    //                 start: 1,
    //                 length: 1
    //             },
    //             Token {
    //                 kind: TokenKind::RightParen,
    //                 start: 2,
    //                 length: 1
    //             },
    //             Token {
    //                 kind: TokenKind::GraphemeCluster,
    //                 start: 3,
    //                 length: 1
    //             },
    //         ]
    //     )
    // }

    // #[test]
    // fn unicode_tokens() {
    //     let source = "ab💖*";
    //     let tokens: Vec<_> = Scanner::new(source).tokens().collect();

    //     assert_eq!(
    //         tokens,
    //         vec![
    //             Token {
    //                 kind: TokenKind::GraphemeCluster,
    //                 start: 0,
    //                 length: 1
    //             },
    //             Token {
    //                 kind: TokenKind::GraphemeCluster,
    //                 start: 1,
    //                 length: 1
    //             },
    //             Token {
    //                 kind: TokenKind::GraphemeCluster,
    //                 start: 2,
    //                 length: 4
    //             },
    //             Token {
    //                 kind: TokenKind::Star,
    //                 start: 6,
    //                 length: 1
    //             },
    //         ]
    //     )
    // }

    // #[test]
    // fn peeking() {
    //     let source = "a(*|";
    //     let scanner = Scanner::new(source);
    //     let mut tokens = scanner.tokens().peekable();

    //     assert_eq!(
    //         tokens.next(),
    //         Some(Token {
    //             start: 0,
    //             length: 1,
    //             kind: TokenKind::GraphemeCluster
    //         })
    //     );

    //     assert_eq!(
    //         tokens.peek(),
    //         Some(&Token {
    //             start: 1,
    //             length: 1,
    //             kind: TokenKind::LeftParen
    //         })
    //     );

    //     assert_eq!(
    //         tokens.peek(),
    //         Some(&Token {
    //             start: 1,
    //             length: 1,
    //             kind: TokenKind::LeftParen
    //         })
    //     );

    //     assert_eq!(
    //         tokens.next(),
    //         Some(Token {
    //             start: 1,
    //             length: 1,
    //             kind: TokenKind::LeftParen
    //         })
    //     );

    //     assert_eq!(
    //         tokens.next(),
    //         Some(Token {
    //             start: 2,
    //             length: 1,
    //             kind: TokenKind::Star
    //         })
    //     );
    // }
}
