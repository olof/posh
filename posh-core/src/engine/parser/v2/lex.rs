use std::iter::Peekable;
use std::str::Chars;

pub fn lex<A: AsRef<str>>(input: A) -> Vec<Token> {
    let mut chars = input.as_ref().chars().peekable();
    let mut tokens = Vec::new();

    while chars.peek().is_some() {
        match chars.parse() {
            Some(Token::Whitespace(_)) => continue,
            None => break,

            Some(token) => tokens.push(token),
        }
    }

    tokens
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    String(String),
    SingleQuotedString(String),
    DoubleQuotedString(String),

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
            .or_else(|| self.parse_string())
            .or_else(|| self.parse_single_quoted_string())
            .or_else(|| self.parse_double_quoted_string())
    }
}

impl Lexer for Peekable<Chars<'_>> {
    fn parse_whitespace(&mut self) -> Option<Token> {
        match self.peek() {
            Some(&c) if is_whitespace(c) => {
                let c = self.next().unwrap();
                Some(Token::Whitespace(c))
            }

            _ => None,
        }
    }

    fn parse_semicolon(&mut self) -> Option<Token> {
        match self.peek() {
            Some(';') => {
                self.next();
                Some(Token::Semicolon)
            }

            _ => None,
        }
    }

    fn parse_pipe(&mut self) -> Option<Token> {
        match self.peek() {
            Some('|') => {
                self.next();
                Some(Token::Pipe)
            }

            _ => None,
        }
    }

    fn parse_or(&mut self) -> Option<Token> {
        let initial = self.clone();

        match self.peek() {
            Some('|') => {
                self.next();

                match self.peek() {
                    Some('|') => {
                        self.next();
                        Some(Token::Or)
                    }

                    _ => {
                        *self = initial;
                        None
                    }
                }
            }

            _ => None,
        }
    }

    fn parse_ampersand(&mut self) -> Option<Token> {
        match self.peek() {
            Some('&') => {
                self.next();
                Some(Token::Ampersand)
            }

            _ => None,
        }
    }

    fn parse_and(&mut self) -> Option<Token> {
        let initial = self.clone();

        match self.peek() {
            Some('&') => {
                self.next();

                match self.peek() {
                    Some('&') => {
                        self.next();
                        Some(Token::And)
                    }

                    _ => {
                        *self = initial;
                        None
                    }
                }
            }

            _ => None,
        }
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
            Some(Token::String(parsed))
        }
    }

    fn parse_single_quoted_string(&mut self) -> Option<Token> {
        // If the first character is not a single quote, exit early
        if !matches!(self.peek(), Some('\'')) {
            return None;
        }

        // We know that the next token is a single quote here
        self.next();

        let mut parsed = String::new();
        let mut is_escaped = false;

        for c in self.by_ref() {
            match c {
                '\\' if !is_escaped => is_escaped = true,
                '\'' if !is_escaped => break,
                c => {
                    is_escaped = false;
                    parsed.push(c);
                }
            }
        }

        Some(Token::SingleQuotedString(parsed))
    }

    fn parse_double_quoted_string(&mut self) -> Option<Token> {
        // If the first character is not a double quote, exit early
        if !matches!(self.peek(), Some('"')) {
            return None;
        }

        // We know that the next token is a double quote here
        self.next();

        let mut parsed = String::new();
        let mut is_escaped = false;

        for c in self.by_ref() {
            match c {
                '\\' if !is_escaped => is_escaped = true,
                '"' if !is_escaped => break,
                c => {
                    is_escaped = false;
                    parsed.push(c);
                }
            }
        }

        Some(Token::DoubleQuotedString(parsed))
    }

    fn parse_redirect_output(&mut self) -> Option<Token> {
        if let Some(&c) = self.peek() {
            if c != '>' && !is_valid_file_descriptor_for_redirection(c) {
                return None;
            }
        }

        let initial = self.clone();
        let c = self.next().unwrap();
        let mut fd = '0';
        let mut append = false;

        if is_valid_file_descriptor_for_redirection(c) {
            fd = c;
            if !matches!(self.next(), Some('>')) {
                *self = initial;
                return None;
            }
        }

        if let Some(&'>') = self.peek() {
            append = true;
            self.next();
        }

        self.swallow_whitespace();

        match self
            .parse_string()
            .or_else(|| self.parse_single_quoted_string())
            .or_else(|| self.parse_double_quoted_string())
        {
            Some(
                Token::String(s) | Token::SingleQuotedString(s) | Token::DoubleQuotedString(s),
            ) => Some(Token::RedirectOutput {
                file_descriptor: fd,
                append,
                target: s,
            }),

            _ => None,
        }
    }

    fn parse_redirect_input(&mut self) -> Option<Token> {
        if let Some(&c) = self.peek() {
            if c != '<' && !is_valid_file_descriptor_for_redirection(c) {
                return None;
            }
        }

        let initial = self.clone();
        let c = self.next().unwrap();
        let mut fd = '0';

        if is_valid_file_descriptor_for_redirection(c) {
            fd = c;

            if !matches!(self.next(), Some('<')) {
                *self = initial;
                return None;
            }
        }

        self.swallow_whitespace();

        match self
            .parse_string()
            .or_else(|| self.parse_single_quoted_string())
            .or_else(|| self.parse_double_quoted_string())
        {
            Some(
                Token::String(s) | Token::SingleQuotedString(s) | Token::DoubleQuotedString(s),
            ) => Some(Token::RedirectInput {
                file_descriptor: fd,
                target: s,
            }),

            _ => None,
        }
    }

    fn parse_here_document(&mut self) -> Option<Token> {
        let initial = self.clone();
        let mut fd = '0';

        match self.peek() {
            Some(&c) if is_valid_file_descriptor_for_redirection(c) => {
                fd = c;
                self.next();
            }
            _ => {}
        }

        match self.peek() {
            Some('<') => {
                self.next();
                match self.peek() {
                    Some('<') => {
                        self.next();
                    }
                    _ => {
                        *self = initial;
                        return None;
                    }
                }
            }

            _ => {
                *self = initial;
                return None;
            }
        }

        self.swallow_whitespace();

        let delimiter = match self
            .parse_string()
            .or_else(|| self.parse_single_quoted_string())
            .or_else(|| self.parse_double_quoted_string())
        {
            Some(
                Token::String(s) | Token::SingleQuotedString(s) | Token::DoubleQuotedString(s),
            ) => s,

            _ => {
                *self = initial;
                return None;
            }
        };

        Some(Token::HereDocument {
            file_descriptor: fd,
            delimiter,
        })
    }

    fn parse_keyword(&mut self) -> Option<Token> {
        match self.peek() {
            Some('!') => {
                self.next();
                return Some(Token::Keyword(Keyword::Not));
            }

            Some('{') => {
                self.next();
                return Some(Token::Keyword(Keyword::LBrace));
            }

            Some('}') => {
                self.next();
                return Some(Token::Keyword(Keyword::RBrace));
            }

            _ => {}
        };

        let initial = self.clone();
        match self.parse_string() {
            Some(Token::String(s)) => match s.as_str() {
                "case" => Some(Token::Keyword(Keyword::Case)),
                "do" => Some(Token::Keyword(Keyword::Do)),
                "done" => Some(Token::Keyword(Keyword::Done)),
                "elif" => Some(Token::Keyword(Keyword::Elif)),
                "else" => Some(Token::Keyword(Keyword::Else)),
                "esac" => Some(Token::Keyword(Keyword::Esac)),
                "fi" => Some(Token::Keyword(Keyword::Fi)),
                "for" => Some(Token::Keyword(Keyword::For)),
                "if" => Some(Token::Keyword(Keyword::If)),
                "in" => Some(Token::Keyword(Keyword::In)),
                "then" => Some(Token::Keyword(Keyword::Then)),
                "until" => Some(Token::Keyword(Keyword::Until)),
                "while" => Some(Token::Keyword(Keyword::While)),
                _ => {
                    *self = initial;
                    None
                }
            },
            _ => None,
        }
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
        let mut input = "foobarbaz".chars().peekable();
        let parsed = input.parse_string();
        let expected = Token::String("foobarbaz".to_string());
        assert_eq!(Some(expected), parsed);

        let mut input = r#"foo\ bar\ baz"#.chars().peekable();
        let parsed = input.parse_string();
        let expected = Token::String("foo bar baz".to_string());
        assert_eq!(Some(expected), parsed);
    }

    #[test]
    fn parse_quoted_string() {
        let mut input = "'foo bar'".chars().peekable();
        let parsed = input.parse_single_quoted_string();
        let expected = Token::SingleQuotedString("foo bar".to_string());
        assert_eq!(Some(expected), parsed);

        let mut input = r#"'foo \'bar\''"#.chars().peekable();
        let parsed = input.parse_single_quoted_string();
        let expected = Token::SingleQuotedString("foo 'bar'".to_string());
        assert_eq!(Some(expected), parsed);

        let mut input = r#"'foo "bar"'"#.chars().peekable();
        let parsed = input.parse_single_quoted_string();
        let expected = Token::SingleQuotedString(r#"foo "bar""#.to_string());
        assert_eq!(Some(expected), parsed);

        let mut input = r#""foo bar""#.chars().peekable();
        let parsed = input.parse_double_quoted_string();
        let expected = Token::DoubleQuotedString("foo bar".to_string());
        assert_eq!(Some(expected), parsed);

        let mut input = r#""foo \"bar\"""#.chars().peekable();
        let parsed = input.parse_double_quoted_string();
        let expected = Token::DoubleQuotedString(r#"foo "bar""#.to_string());
        assert_eq!(Some(expected), parsed);

        let mut input = r#""foo 'bar'""#.chars().peekable();
        let parsed = input.parse_double_quoted_string();
        let expected = Token::DoubleQuotedString("foo 'bar'".to_string());
        assert_eq!(Some(expected), parsed);
    }

    #[test]
    fn parse_redirect_input() {
        let mut input = "<foo.txt".chars().peekable();
        let parsed = input.parse_redirect_input();
        let expected = Token::RedirectInput {
            file_descriptor: '0',
            target: "foo.txt".to_string(),
        };
        assert_eq!(Some(expected), parsed);

        let mut input = "<'foo.txt'".chars().peekable();
        let parsed = input.parse_redirect_input();
        let expected = Token::RedirectInput {
            file_descriptor: '0',
            target: "foo.txt".to_string(),
        };
        assert_eq!(Some(expected), parsed);

        let mut input = r#"3<"foo.txt""#.chars().peekable();
        let parsed = input.parse_redirect_input();
        let expected = Token::RedirectInput {
            file_descriptor: '3',
            target: "foo.txt".to_string(),
        };
        assert_eq!(Some(expected), parsed);
    }

    #[test]
    fn parse_redirect_output() {
        let mut input = "> foo.txt".chars().peekable();
        let parsed = input.parse_redirect_output();
        let expected = Token::RedirectOutput {
            file_descriptor: '0',
            target: "foo.txt".to_string(),
            append: false,
        };
        assert_eq!(Some(expected), parsed);

        let mut input = ">>'foo.txt'".chars().peekable();
        let parsed = input.parse_redirect_output();
        let expected = Token::RedirectOutput {
            file_descriptor: '0',
            target: "foo.txt".to_string(),
            append: true,
        };
        assert_eq!(Some(expected), parsed);

        let mut input = "1>'foo.txt'".chars().peekable();
        let parsed = input.parse_redirect_output();
        let expected = Token::RedirectOutput {
            file_descriptor: '1',
            target: "foo.txt".to_string(),
            append: false,
        };
        assert_eq!(Some(expected), parsed);

        let mut input = "3>> 'foo.txt'".chars().peekable();
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
        let mut input = "<<EOF".chars().peekable();
        let parsed = input.parse_here_document();
        let expected = Token::HereDocument {
            file_descriptor: '0',
            delimiter: "EOF".to_string(),
        };
        assert_eq!(Some(expected), parsed);

        let mut input = "9<< EOF".chars().peekable();
        let parsed = input.parse_here_document();
        let expected = Token::HereDocument {
            file_descriptor: '9',
            delimiter: "EOF".to_string(),
        };
        assert_eq!(Some(expected), parsed);

        let mut input = "<< 'EOF'".chars().peekable();
        let parsed = input.parse_here_document();
        let expected = Token::HereDocument {
            file_descriptor: '0',
            delimiter: "EOF".to_string(),
        };
        assert_eq!(Some(expected), parsed);

        let mut input = r#"2<<"EOF""#.chars().peekable();
        let parsed = input.parse_here_document();
        let expected = Token::HereDocument {
            file_descriptor: '2',
            delimiter: "EOF".to_string(),
        };
        assert_eq!(Some(expected), parsed);
    }

    #[test]
    fn parse_keyword() {
        let mut input = "done".chars().peekable();
        let parsed = input.parse_keyword();
        let expected = Token::Keyword(Keyword::Done);
        assert_eq!(Some(expected), parsed);
    }

    #[test]
    fn parse_string_substitutions() {
        let mut input = "$(( $(( 1 + $(cat foo) )) + $VALUE ))".chars().peekable();
        let parsed = input.parse_string();
        let expected = Token::String("$(( $(( 1 + $(cat foo) )) + $VALUE ))".to_string());
        assert_eq!(Some(expected), parsed);

        let mut input = "$(cat <$(cat foo ))".chars().peekable();
        let parsed = input.parse_string();
        let expected = Token::String("$(cat <$(cat foo ))".to_string());
        assert_eq!(Some(expected), parsed);
    }

    #[test]
    fn lex_basic() {
        use Token::*;

        let tokens = lex(r#"foo | rev && echo 'yeah!'; echo ";& foo\"bar\"""#);
        let expected = vec![
            String("foo".to_string()),
            Pipe,
            String("rev".to_string()),
            And,
            String("echo".to_string()),
            SingleQuotedString("yeah!".to_string()),
            Semicolon,
            String("echo".to_string()),
            DoubleQuotedString(r#";& foo"bar""#.to_string()),
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
            String("echo".to_string()),
            String("$(( 1 + $(cat foo)))".to_string()),
            Semicolon,
            String("echo".to_string()),
            String("bar".to_string()),
            RedirectOutput {
                file_descriptor: '0',
                target: "baz".to_string(),
                append: false,
            },
            Semicolon,
            String("echo".to_string()),
            String("baz".to_string()),
            RedirectOutput {
                file_descriptor: '2',
                target: "quux".to_string(),
                append: false,
            },
            Semicolon,
            String("false".to_string()),
            Semicolon,
            Keyword(RBrace),
            And,
            String("echo".to_string()),
            String("oof".to_string()),
            Pipe,
            String("rev".to_string()),
            Semicolon,
            RedirectInput {
                file_descriptor: '0',
                target: "file".to_string(),
            },
            String("cat".to_string()),
            RedirectOutput {
                file_descriptor: '0',
                target: "1".to_string(),
                append: true,
            },
        ];

        assert_eq!(expected, tokens);
    }
}
