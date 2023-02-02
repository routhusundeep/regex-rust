use crate::ast::parser::Parser;
use crate::automata::compiler::Compiler;
use crate::automata::program::Program;
use crate::errors::Error;
use crate::executor::Executor;
use crate::executor::ExecutorType;

pub struct Regex(Program);

impl Regex {
    pub fn new(pat: &str) -> Result<Regex, Error> {
        let ast = Parser::new(pat.to_owned()).parse()?;
        let c = Compiler::new().compile(&ast)?;
        Ok(Regex(c))
    }

    pub fn matches(self, s: &str) -> bool {
        Executor::new(ExecutorType::Pike).matches(&self.0, s)
    }
}
