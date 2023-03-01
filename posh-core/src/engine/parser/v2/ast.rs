use std::ops::RangeInclusive;

use super::lex::Token;

#[derive(Debug, PartialEq, Eq)]
pub struct SyntaxTree {
    commands: Vec<Command>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Command {
    Simple(SimpleCommand),
    Pipeline(Pipeline),
    CompoundList(CompoundList),
    CompoundCommand(CompoundCommand),
    FunctionDefinition(FunctionDefinition),
}

#[derive(Debug, PartialEq, Eq)]
pub struct SimpleCommand {
    redirections: Vec<Redirection>,
    assignments: Vec<VariableAssignment>,
    name: Word,
    words: Vec<Word>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Redirection {
    Output {
        file_descriptor: Word,
        append: bool,
        target: Word,
    },

    Input {
        file_descriptor: Word,
        target: Word,
    },

    HereDocument {
        file_descriptor: Word,
        delimiter: Word,
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct VariableAssignment {
    lhs: Word,
    rhs: Option<Word>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Word {
    name: String,
    expansions: Vec<Expansion>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expansion {
    Tilde {
        index: usize,
    },

    Glob {
        range: RangeInclusive<usize>,
        recursive: bool,
        pattern: String,
    },

    Brace {
        range: RangeInclusive<usize>,
        pattern: String,
    },
    
    Parameter {
        range: RangeInclusive<usize>,
        name: String,
    },

    Command {
        range: RangeInclusive<usize>,
        tree: SyntaxTree,
    },

    Arithmetic {
        range: RangeInclusive<usize>,
        expression: Word,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Pipeline;

#[derive(Debug, PartialEq, Eq)]
pub struct CompoundList;

#[derive(Debug, PartialEq, Eq)]
pub struct CompoundCommand;

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionDefinition;
