use std::iter::Peekable;
use std::str::Chars;

pub fn lex<A: AsRef<str>>(input: A) -> Vec<Token> {
    PoshLexer::from(input.as_ref()).parse_full(false)
}

#[derive(Debug, PartialEq, Eq)]
pub enum QuoteType {
    None,
    Single,
    Double,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    String(String, QuoteType),

    RedirectOutput {
        file_descriptor: char,
        target: String,
        append: bool,
    },

    RedirectInput {
        file_descriptor: char,
        target: String,
    },

    HereDocument {
        file_descriptor: char,
        delimiter: String,
    },

    Pipe,

    And,
    Or,

    Semicolon,
    Ampersand,

    Keyword(Keyword),
    Whitespace(char),
}

trait Lexer {
    fn parse_whitespace(&mut self) -> Option<Token>;
    fn parse_string(&mut self) -> Option<Token>;
    fn parse_single_quoted_string(&mut self) -> Option<Token>;
    fn parse_double_quoted_string(&mut self) -> Option<Token>;
    fn parse_any_string(&mut self) -> Option<Token>;
    fn parse_redirect_output(&mut self) -> Option<Token>;
    fn parse_redirect_input(&mut self) -> Option<Token>;
    fn parse_here_document(&mut self) -> Option<Token>;
    fn parse_semicolon(&mut self) -> Option<Token>;
    fn parse_pipe(&mut self) -> Option<Token>;
    fn parse_or(&mut self) -> Option<Token>;
    fn parse_ampersand(&mut self) -> Option<Token>;
    fn parse_and(&mut self) -> Option<Token>;
    fn parse_keyword(&mut self) -> Option<Token>;

    fn swallow_whitespace(&mut self);

    fn parse(&mut self) -> Option<Token> {
        self.parse_whitespace()
            .or_else(|| self.parse_semicolon())
            .or_else(|| self.parse_or())
            .or_else(|| self.parse_pipe())
            .or_else(|| self.parse_and())
            .or_else(|| self.parse_ampersand())
            .or_else(|| self.parse_keyword())
            .or_else(|| self.parse_redirect_output())
            .or_else(|| self.parse_here_document())
            .or_else(|| self.parse_redirect_input())
            .or_else(|| self.parse_any_string())
    }

    fn parse_full(&mut self, filter_whitespace: bool) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(token) = self.parse() {
            if !filter_whitespace && !matches!(token, Token::Whitespace(_)) {
                tokens.push(token);
            }
        }

        tokens
    }

    fn parse_char(&mut self, target: char) -> Option<char>;
    fn parse_char_if(&mut self, predicate: impl FnOnce(char) -> bool) -> Option<char>;
    fn parse_literal(&mut self, literal: &str) -> Option<String>;
    fn parse_until(&mut self, predicate: impl Fn(char) -> bool) -> Option<String>;
}

#[derive(Debug)]
struct PoshLexer<'a> {
    chars: Peekable<Chars<'a>>,
    prev: Peekable<Chars<'a>>,
}

impl<'a> PoshLexer<'a> {
    fn from(chars: &'a str) -> Self {
        let chars = chars.chars().peekable();
        Self {
            prev: chars.clone(),
            chars,
        }
    }

    fn reset(&mut self) {
        self.chars = self.prev.clone();
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn next(&mut self) -> Option<char> {
        self.chars.next()
    }

    fn bail<T>(&mut self) -> Option<T> {
        self.reset();
        None
    }
}

impl Lexer for PoshLexer<'_> {
    fn parse_literal(&mut self, literal: &str) -> Option<String> {
        self.prev = self.chars.clone();

        let mut literal_iter = literal.chars();
        let mut parsed = String::new();

        while let Some(c) = literal_iter.next() {
            if matches!(self.next(), Some(n) if n == c) {
                parsed.push(c);
            } else {
                return self.bail();
            }
        }
        Some(parsed)
    }

    fn parse_char_if(&mut self, predicate: impl FnOnce(char) -> bool) -> Option<char> {
        match self.peek() {
            Some(&c) if predicate(c) => {
                self.next();
                Some(c)
            }

            _ => None,
        }
    }

    fn parse_until(&mut self, predicate: impl Fn(char) -> bool) -> Option<String> {
        let mut s = String::new();

        let mut is_escaped = false;

        while let Some(c) = self.parse_char_if(|c| !predicate(c) || is_escaped) {
            is_escaped = !is_escaped && c == '\\';

            if !is_escaped {
                s.push(c);
            }
        }

        if s.is_empty() {
            None
        } else {
            Some(s)
        }
    }

    fn parse_char(&mut self, target: char) -> Option<char> {
        self.parse_char_if(|c| c == target)
    }

    fn parse_whitespace(&mut self) -> Option<Token> {
        self.parse_char_if(is_whitespace).map(Token::Whitespace)
    }

    fn parse_semicolon(&mut self) -> Option<Token> {
        self.parse_char(';').map(|_| Token::Semicolon)
    }

    fn parse_pipe(&mut self) -> Option<Token> {
        self.parse_char('|').map(|_| Token::Pipe)
    }

    fn parse_or(&mut self) -> Option<Token> {
        self.parse_literal("||").map(|_| Token::Or)
    }

    fn parse_ampersand(&mut self) -> Option<Token> {
        self.parse_char('&').map(|_| Token::Ampersand)
    }

    fn parse_and(&mut self) -> Option<Token> {
        self.parse_literal("&&").map(|_| Token::And)
    }

    fn parse_string(&mut self) -> Option<Token> {
        // If the first character is an invalid start of a string,
        // exit early
        match self.peek() {
            Some(&c) if is_command_delimiter(c) => return None,
            None | Some('"' | '\'') => return None,

            _ => {}
        }

        let mut parsed = String::new();
        let mut is_escaped = false;
        let mut nested_command_level = 0u8;
        let mut nested_arithmetic_level = 0u8;

        while let Some(&c) = self.peek() {
            match c {
                c if is_command_delimiter(c)
                    && !is_escaped
                    && nested_command_level + nested_arithmetic_level == 0 =>
                {
                    break;
                }

                '\\' if !is_escaped => {
                    self.next();
                    is_escaped = true;
                }

                '$' if !is_escaped => {
                    self.next();
                    parsed.push(c);

                    if let Some('(') = self.peek() {
                        let c = self.next().unwrap();
                        parsed.push(c);
                        if let Some('(') = self.peek() {
                            let c = self.next().unwrap();
                            parsed.push(c);
                            nested_arithmetic_level += 1;
                        } else {
                            nested_command_level += 1;
                        }
                    }
                }

                ')' if !is_escaped => {
                    self.next();
                    parsed.push(c);
                    if nested_arithmetic_level > 0 && matches!(self.peek(), Some(')')) {
                        let c = self.next().unwrap();
                        parsed.push(c);
                        nested_arithmetic_level -= 1;
                    } else {
                        nested_command_level -= 1;
                    }
                }

                c => {
                    self.next();
                    is_escaped = false;
                    parsed.push(c);
                }
            }
        }

        if nested_command_level + nested_arithmetic_level != 0 {
            None
        } else {
            Some(Token::String(parsed, QuoteType::None))
        }
    }

    fn parse_single_quoted_string(&mut self) -> Option<Token> {
        // FIXME: this should not be required, figure out why it is
        if !matches!(self.peek(), Some(&c) if c == '\'') {
            return None;
        }

        self.parse_char('\'')
            .and_then(|_| Some(self.parse_until(|c| c == '\'')))
            .and_then(|s| {
                self.parse_char('\'');
                s
            })
            .map(|s| Token::String(s, QuoteType::Single))
            .or_else(|| self.bail())
    }

    fn parse_double_quoted_string(&mut self) -> Option<Token> {
        self.parse_char('"')
            .and_then(|_| self.parse_until(|c| c == '"'))
            .and_then(|s| {
                self.parse_char('"');
                Some(s)
            })
            .map(|s| Token::String(s, QuoteType::Double))
            .or_else(|| self.bail())

        // // We know that the next token is a double quote here
        // self.next();

        // let mut parsed = String::new();
        // let mut is_escaped = false;

        // for c in self.chars.by_ref() {
        //     match c {
        //         '\\' if !is_escaped => is_escaped = true,
        //         '"' if !is_escaped => break,
        //         c => {
        //             is_escaped = false;
        //             parsed.push(c);
        //         }
        //     }
        // }

        // Some(Token::String(parsed, QuoteType::Double))
    }

    fn parse_any_string(&mut self) -> Option<Token> {
        self.parse_string()
            .or_else(|| self.parse_single_quoted_string())
            .or_else(|| self.parse_double_quoted_string())
    }

    fn parse_redirect_output(&mut self) -> Option<Token> {
        self.prev = self.chars.clone();
        let fd = self
            .parse_char_if(is_valid_file_descriptor_for_redirection)
            .unwrap_or('0');
        self.parse_char('>').or_else(|| self.bail())?;
        let append = self.parse_char('>').is_some();
        self.swallow_whitespace();
        self.parse_any_string()
            .map(|t| match t {
                Token::String(s, _) => Token::RedirectOutput {
                    file_descriptor: fd,
                    target: s,
                    append,
                },
                t => t,
            })
            .or_else(|| self.bail())
    }

    fn parse_redirect_input(&mut self) -> Option<Token> {
        self.prev = self.chars.clone();
        let fd = self
            .parse_char_if(is_valid_file_descriptor_for_redirection)
            .unwrap_or('0');
        self.parse_char('<').or_else(|| self.bail())?;
        self.swallow_whitespace();
        self.parse_any_string()
            .map(|t| match t {
                Token::String(s, _) => Token::RedirectInput {
                    file_descriptor: fd,
                    target: s,
                },
                t => t,
            })
            .or_else(|| self.bail())
    }

    fn parse_here_document(&mut self) -> Option<Token> {
        self.prev = self.chars.clone();
        let fd = self
            .parse_char_if(is_valid_file_descriptor_for_redirection)
            .unwrap_or('0');
        self.parse_literal("<<").or_else(|| self.bail())?;
        self.swallow_whitespace();
        self.parse_any_string()
            .map(|t| {
                match t {
                    Token::String(s, _) => Token::HereDocument {
                        file_descriptor: fd,
                        delimiter: s,
                    },
                    t => t,
                }
            })
            .or_else(|| self.bail())
    }

    fn parse_keyword(&mut self) -> Option<Token> {
        use self::Keyword::*;
        use Token::Keyword;

        self.parse_literal("!")
            .map(|_| Keyword(Not))
            .or_else(|| self.parse_literal("{").map(|_| Keyword(LBrace)))
            .or_else(|| self.parse_literal("}").map(|_| Keyword(RBrace)))
            .or_else(|| self.parse_literal("case").map(|_| Keyword(Case)))
            .or_else(|| self.parse_literal("done").map(|_| Keyword(Done)))
            .or_else(|| self.parse_literal("do").map(|_| Keyword(Do)))
            .or_else(|| self.parse_literal("elif").map(|_| Keyword(Elif)))
            .or_else(|| self.parse_literal("else").map(|_| Keyword(Else)))
            .or_else(|| self.parse_literal("esac").map(|_| Keyword(Esac)))
            .or_else(|| self.parse_literal("fi").map(|_| Keyword(Fi)))
            .or_else(|| self.parse_literal("for").map(|_| Keyword(For)))
            .or_else(|| self.parse_literal("if").map(|_| Keyword(If)))
            .or_else(|| self.parse_literal("in").map(|_| Keyword(In)))
            .or_else(|| self.parse_literal("then").map(|_| Keyword(Then)))
            .or_else(|| self.parse_literal("until").map(|_| Keyword(Until)))
            .or_else(|| self.parse_literal("while").map(|_| Keyword(While)))
            .or_else(|| self.bail())
    }

    fn swallow_whitespace(&mut self) {
        while let Some(&c) = self.peek() {
            if !is_whitespace(c) {
                break;
            }
            self.next();
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Keyword {
    Not,
    LBrace,
    RBrace,
    Case,
    Do,
    Done,
    Elif,
    Else,
    Esac,
    Fi,
    For,
    If,
    In,
    Then,
    Until,
    While,
}

fn is_valid_file_descriptor_for_redirection(c: char) -> bool {
    matches!(c, '0'..='9')
}

fn is_command_delimiter(c: char) -> bool {
    matches!(c, ';' | '&' | '|' | '>' | '<' | '^' | ' ' | '\n' | '\t')
}

fn is_whitespace(c: char) -> bool {
    matches!(c, ' ' | '\r' | '\n' | '\t')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_unquoted_string() {
        let mut input = PoshLexer::from("foobarbaz");
        let parsed = input.parse_string();
        let expected = Token::String("foobarbaz".to_string(), QuoteType::None);
        assert_eq!(Some(expected), parsed);

        let mut input = PoshLexer::from(r#"foo\ bar\ baz"#);
        let parsed = input.parse_string();
        let expected = Token::String("foo bar baz".to_string(), QuoteType::None);
        assert_eq!(Some(expected), parsed);

        let mut input = PoshLexer::from(r#"foo\ bar baz"#);
        let parsed = input.parse_string();
        let expected = Token::String("foo bar".to_string(), QuoteType::None);
        assert_eq!(Some(expected), parsed);
        assert!(input.parse_literal(" baz").is_some());
        assert!(input.next().is_none());
    }

    #[test]
    fn parse_quoted_string() {
        let mut input = PoshLexer::from("'foo bar'");
        let parsed = input.parse_single_quoted_string();
        let expected = Token::String("foo bar".to_string(), QuoteType::Single);
        assert_eq!(Some(expected), parsed);

        // Escaping quotes is not allowed in single quoted string
        // let mut input = PoshLexer::from(r#"'foo \'bar\''"#);
        // let parsed = input.parse_single_quoted_string();
        // // This is what POSIX (dash) prints from 'foo \'bar\''
        // // let expected = Token::String("fooar'".to_string(), QuoteType::Single);
        // let expected = Token::String("foo 'bar''".to_string(), QuoteType::Single);
        // assert_eq!(Some(expected), parsed);

        let mut input = PoshLexer::from(r#"'foo "bar"'"#);
        let parsed = input.parse_single_quoted_string();
        let expected = Token::String(r#"foo "bar""#.to_string(), QuoteType::Single);
        assert_eq!(Some(expected), parsed);

        let mut input = PoshLexer::from(r#""foo bar""#);
        let parsed = input.parse_double_quoted_string();
        let expected = Token::String("foo bar".to_string(), QuoteType::Double);
        assert_eq!(Some(expected), parsed);

        let mut input = PoshLexer::from(r#""foo \"bar\"""#);
        let parsed = input.parse_double_quoted_string();
        let expected = Token::String(r#"foo "bar""#.to_string(), QuoteType::Double);
        assert_eq!(Some(expected), parsed);

        let mut input = PoshLexer::from(r#""foo 'bar'""#);
        let parsed = input.parse_double_quoted_string();
        let expected = Token::String("foo 'bar'".to_string(), QuoteType::Double);
        assert_eq!(Some(expected), parsed);
    }

    #[test]
    fn parse_redirect_input() {
        let mut input = PoshLexer::from("<foo.txt");
        let parsed = input.parse_redirect_input();
        let expected = Token::RedirectInput {
            file_descriptor: '0',
            target: "foo.txt".to_string(),
        };
        assert_eq!(Some(expected), parsed);

        let mut input = PoshLexer::from("<'foo.txt'");
        let parsed = input.parse_redirect_input();
        let expected = Token::RedirectInput {
            file_descriptor: '0',
            target: "foo.txt".to_string(),
        };
        assert_eq!(Some(expected), parsed);

        let mut input = PoshLexer::from(r#"3<"foo.txt""#);
        let parsed = input.parse_redirect_input();
        let expected = Token::RedirectInput {
            file_descriptor: '3',
            target: "foo.txt".to_string(),
        };
        assert_eq!(Some(expected), parsed);
    }

    #[test]
    fn parse_redirect_output() {
        let mut input = PoshLexer::from("> foo.txt");
        let parsed = input.parse_redirect_output();
        let expected = Token::RedirectOutput {
            file_descriptor: '0',
            target: "foo.txt".to_string(),
            append: false,
        };
        assert_eq!(Some(expected), parsed);

        let mut input = PoshLexer::from(">>'foo.txt'");
        let parsed = input.parse_redirect_output();
        let expected = Token::RedirectOutput {
            file_descriptor: '0',
            target: "foo.txt".to_string(),
            append: true,
        };
        assert_eq!(Some(expected), parsed);

        let mut input = PoshLexer::from("1>'foo.txt'");
        let parsed = input.parse_redirect_output();
        let expected = Token::RedirectOutput {
            file_descriptor: '1',
            target: "foo.txt".to_string(),
            append: false,
        };
        assert_eq!(Some(expected), parsed);

        let mut input = PoshLexer::from(r#"3>> "foo.txt""#);
        let parsed = input.parse_redirect_output();
        let expected = Token::RedirectOutput {
            file_descriptor: '3',
            target: "foo.txt".to_string(),
            append: true,
        };
        assert_eq!(Some(expected), parsed);
    }

    #[test]
    fn parse_here_document() {
        let mut input = PoshLexer::from("<<EOF");
        let parsed = input.parse_here_document();
        let expected = Token::HereDocument {
            file_descriptor: '0',
            delimiter: "EOF".to_string(),
        };
        assert_eq!(Some(expected), parsed);

        let mut input = PoshLexer::from("9<< EOF");
        let parsed = input.parse_here_document();
        let expected = Token::HereDocument {
            file_descriptor: '9',
            delimiter: "EOF".to_string(),
        };
        assert_eq!(Some(expected), parsed);

        let mut input = PoshLexer::from("<< 'EOF'");
        let parsed = input.parse_here_document();
        let expected = Token::HereDocument {
            file_descriptor: '0',
            delimiter: "EOF".to_string(),
        };
        assert_eq!(Some(expected), parsed);

        let mut input = PoshLexer::from(r#"2<<"EOF""#);
        let parsed = input.parse_here_document();
        let expected = Token::HereDocument {
            file_descriptor: '2',
            delimiter: "EOF".to_string(),
        };
        assert_eq!(Some(expected), parsed);
    }

    #[test]
    fn parse_keyword() {
        let mut input = PoshLexer::from("done");
        let parsed = input.parse_keyword();
        let expected = Token::Keyword(Keyword::Done);
        assert_eq!(Some(expected), parsed);
    }

    #[test]
    fn parse_string_substitutions() {
        let mut input = PoshLexer::from("$(( $(( 1 + $(cat foo) )) + $VALUE ))");
        let parsed = input.parse_string();
        let expected = Token::String(
            "$(( $(( 1 + $(cat foo) )) + $VALUE ))".to_string(),
            QuoteType::None,
        );
        assert_eq!(Some(expected), parsed);

        let mut input = PoshLexer::from("$(cat <$(cat foo ))");
        let parsed = input.parse_string();
        let expected = Token::String("$(cat <$(cat foo ))".to_string(), QuoteType::None);
        assert_eq!(Some(expected), parsed);
    }

    #[test]
    fn lex_basic() {
        use Token::*;

        let tokens = lex(r#"foo | rev && echo 'yeah!'; echo ";& foo\"bar\"""#);
        let expected = vec![
            String("foo".to_string(), QuoteType::None),
            Pipe,
            String("rev".to_string(), QuoteType::None),
            And,
            String("echo".to_string(), QuoteType::None),
            String("yeah!".to_string(), QuoteType::Single),
            Semicolon,
            String("echo".to_string(), QuoteType::None),
            String(r#";& foo"bar""#.to_string(), QuoteType::Double),
        ];
        assert_eq!(expected, tokens);
    }

    #[test]
    fn lex_complex() {
        use super::Keyword::*;
        use Token::*;

        let input = r#"! { echo $(( 1 + $(cat foo))); echo bar>baz; echo baz 2>"quux"; false; } && echo oof | rev; <file cat >> 1"#;
        let tokens = lex(input);

        let expected = vec![
            Keyword(Not),
            Keyword(LBrace),
            String("echo".to_string(), QuoteType::None),
            String("$(( 1 + $(cat foo)))".to_string(), QuoteType::None),
            Semicolon,
            String("echo".to_string(), QuoteType::None),
            String("bar".to_string(), QuoteType::None),
            RedirectOutput {
                file_descriptor: '0',
                target: "baz".to_string(),
                append: false,
            },
            Semicolon,
            String("echo".to_string(), QuoteType::None),
            String("baz".to_string(), QuoteType::None),
            RedirectOutput {
                file_descriptor: '2',
                target: "quux".to_string(),
                append: false,
            },
            Semicolon,
            String("false".to_string(), QuoteType::None),
            Semicolon,
            Keyword(RBrace),
            And,
            String("echo".to_string(), QuoteType::None),
            String("oof".to_string(), QuoteType::None),
            Pipe,
            String("rev".to_string(), QuoteType::None),
            Semicolon,
            RedirectInput {
                file_descriptor: '0',
                target: "file".to_string(),
            },
            String("cat".to_string(), QuoteType::None),
            RedirectOutput {
                file_descriptor: '0',
                target: "1".to_string(),
                append: true,
            },
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn foo() {
        let mut input = PoshLexer::from("johnny bravo");

        let parsed = input.parse_literal("johnny");
        assert_eq!(Some("johnny".to_string()), parsed);

        let parsed = input.parse_whitespace();
        assert!(parsed.is_some());

        let parsed = input.parse_literal("doey");
        assert!(parsed.is_none());
        assert_eq!(input.peek(), Some(&'b'));

        let parsed = input.parse_literal("bravo");
        assert_eq!(Some("bravo".to_string()), parsed);

        assert!(input.peek().is_none());

        let mut input = PoshLexer::from("foobar");

        let parsed = input.parse_literal("fof");
        assert!(parsed.is_none());
        assert_eq!(input.peek(), Some(&'f'));

        let parsed = input.parse_literal("");
        assert_eq!(parsed, Some("".to_string()));
        assert_eq!(input.peek(), Some(&'f'));

        let parsed = input.parse_literal("foobar");
        assert!(parsed.is_some());
        assert!(input.peek().is_none());
    }

    #[test]
    fn cmd_sub() {
        let mut input = PoshLexer::from("echo foo |rev");

        let parsed = input.parse_full(false);

        let expected = vec![
            Token::String("echo".to_string(), QuoteType::None),
            Token::String("foo".to_string(), QuoteType::None),
            Token::Pipe,
            Token::String("rev".to_string(), QuoteType::None),
        ];

        assert_eq!(expected, parsed);
    }

    #[test]
    fn easy() {
        let mut input = PoshLexer::from("|||");

        let parsed = input.parse();
        assert_eq!(Some(Token::Or), parsed);

        let parsed = input.parse();
        assert_eq!(Some(Token::Pipe), parsed);

        assert_eq!(None, input.next());
    }
}
