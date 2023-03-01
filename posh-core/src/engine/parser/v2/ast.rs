use super::lex::Token;

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
        file_descriptor: Option<char>,
        append: bool,
        target: String,
    },
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
pub enum Expansion {}

#[derive(Debug, PartialEq, Eq)]
pub struct Pipeline;

#[derive(Debug, PartialEq, Eq)]
pub struct CompoundList;

#[derive(Debug, PartialEq, Eq)]
pub struct CompoundCommand;

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionDefinition;
