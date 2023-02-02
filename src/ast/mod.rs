use std::fmt::{self, Debug};
pub mod parser;

pub trait AstAttr: Debug + PartialEq + Eq {}

#[derive(Debug, PartialEq, Eq)]
pub enum Ast {
    Empty(Empty),
    Concat(Concat),
    Repetition(Repetition),
    Literal(Literal),
    Group(Group),
    Alternation(Alternation),
}

#[derive(PartialEq, Debug, Eq)]
pub struct Alternation {
    span: Span,
    pub asts: Vec<Ast>,
}

impl Alternation {
    pub fn into_ast(mut self) -> Ast {
        match self.asts.len() {
            0 => Ast::Empty(Empty { span: self.span }),
            1 => self.asts.pop().unwrap(),
            _ => Ast::Alternation(self),
        }
    }
}

#[derive(PartialEq, Debug, Eq)]
pub struct Group {
    span: Span,
    pub ast: Box<Ast>,
}

#[derive(PartialEq, Debug, Eq)]
pub struct Empty {
    span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Concat {
    pub span: Span,
    pub asts: Vec<Ast>,
}
impl Concat {
    fn new(span: Span) -> Concat {
        Concat { span, asts: vec![] }
    }

    fn into_ast(mut self) -> Ast {
        match self.asts.len() {
            0 => Ast::Empty(Empty { span: self.span }),
            1 => self.asts.pop().unwrap(),
            _ => Ast::Concat(self),
        }
    }

    pub fn asts(&self) -> &[Ast] {
        self.asts.as_ref()
    }
}

#[derive(PartialEq, Debug, Eq)]
pub struct Literal {
    span: Span,
    pub kind: LiteralKind,
    pub char: char,
}

impl Literal {}

#[derive(PartialEq, Debug, Eq)]
pub enum LiteralKind {
    Verbatim,    // `a` or `0`
    Punctuation, // escaped `\*` or `\[`
}

#[derive(Debug, PartialEq, Eq)]
pub struct Repetition {
    pub op: RepetitionOp,
    pub greedy: bool,
    pub ast: Box<Ast>,
}
impl Repetition {
    pub fn kind(&self) -> RepetitionKind {
        self.op.kind
    }
}

#[derive(PartialEq, Debug, Eq)]
pub struct RepetitionOp {
    span: Span,
    pub kind: RepetitionKind,
}

#[derive(PartialEq, Debug, Eq, Copy, Clone)]
pub enum RepetitionKind {
    ZeroOrOne,
    ZeroOrMore,
    OneOrMore,
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct Position {
    pub offset: usize,
}
impl Position {
    pub fn new(offset: usize) -> Position {
        Position { offset }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Span {
    start: Position,
    end: Position,
}

impl Span {
    pub fn point(pos: Position) -> Span {
        Span {
            start: pos,
            end: pos,
        }
    }

    pub(crate) fn new(start: Position, end: Position) -> Span {
        Span { start, end }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Span({:?}, {:?})", self.start, self.end)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Error {
    kind: ErrorKind,
    pattern: String,
    span: Span,
}

impl Error {
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn pattern(&self) -> &str {
        &self.pattern
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]

pub enum ErrorKind {
    RepetitionMissing,
    GroupUnopened,
    GroupUnclosed,
}
