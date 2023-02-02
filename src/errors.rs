use crate::ast;
use std::fmt;

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    Syntax(String),
    __Nonexhaustive,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Syntax(ref err) => err.fmt(f),
            Error::__Nonexhaustive => unreachable!(),
        }
    }
}

impl From<ast::Error> for Error {
    fn from(value: ast::Error) -> Self {
        todo!()
    }
}
