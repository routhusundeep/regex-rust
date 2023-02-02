use std::collections::HashSet;
use std::hash::Hash;
use std::vec;

use crate::automata::program;
use crate::automata::program::InstPtr;

pub enum ExecutorType {
    Recursive,
    Pike,
}

pub struct Executor {
    typ: ExecutorType,
}

impl Executor {
    pub fn matches(&self, program: &program::Program, s: &str) -> bool {
        match self.typ {
            ExecutorType::Recursive => RecursiveExecutor::new(program).matches(s),
            ExecutorType::Pike => PikeExecutor::new(program).matches(s),
        }
    }

    pub fn new(typ: ExecutorType) -> Executor {
        Executor { typ: typ }
    }
}

#[derive(PartialEq, Eq, Debug, Hash)]
struct Thread(InstPtr);

struct Threads {
    set: HashSet<Thread>,
}
impl Threads {
    fn new() -> Threads {
        Threads {
            set: HashSet::new(),
        }
    }
}

struct PikeExecutor<'a> {
    program: &'a program::Program,
    stack: Vec<InstPtr>,
}

impl PikeExecutor<'_> {
    pub fn new<'a>(program: &'a program::Program) -> PikeExecutor<'a> {
        PikeExecutor {
            program: program,
            stack: vec![],
        }
    }

    pub fn add(&mut self, nlist: &mut Threads, ip: InstPtr) {
        self.stack.push(ip);
        while let Some(ip) = self.stack.pop() {
            self.add_step(nlist, ip);
        }
    }

    fn add_step(&mut self, nlist: &mut Threads, ip: usize) {
        if nlist.set.contains(&Thread(ip)) {
            return;
        }
        nlist.set.insert(Thread(ip));
        match self.program.insts[ip] {
            program::Inst::Match => {}
            program::Inst::Char(_) => {}
            program::Inst::Split(program::InstSplit { goto1, goto2 }) => {
                self.stack.push(goto1);
                self.stack.push(goto2);
            }
            program::Inst::Save(_) => unreachable!("not reachable"),
        }
    }

    pub fn exec(&mut self, clist: &mut Threads, nlist: &mut Threads, s: &str) -> bool {
        self.add(clist, 0);

        for schar in s.chars() {
            for t in clist.set.iter() {
                let i = self.program.insts.get(t.0).expect("should not be empty");
                match *i {
                    program::Inst::Match => return false,
                    program::Inst::Char(program::InstChar { c, goto }) => {
                        if schar == c {
                            self.add(nlist, goto);
                        }
                    }
                    program::Inst::Split(_) => {}
                    program::Inst::Save(_) => unreachable!("not reachable"),
                };
            }
            clist.set.clear();
            core::mem::swap(clist, nlist);
        }
        for t in &clist.set {
            if self.program.insts[t.0] == program::Inst::Match {
                return true;
            }
        }
        false
    }

    pub fn matches(&mut self, s: &str) -> bool {
        self.exec(&mut Threads::new(), &mut Threads::new(), s)
    }
}

struct RecursiveExecutor<'a> {
    program: &'a program::Program,
}

impl RecursiveExecutor<'_> {
    pub fn new<'a>(program: &'a program::Program) -> RecursiveExecutor<'a> {
        RecursiveExecutor { program: program }
    }

    pub fn matches(&self, s: &str) -> bool {
        self.m(self.program.start, &s[..])
    }

    fn m(&self, ptr: InstPtr, s: &str) -> bool {
        let i = self.program.insts.get(ptr).expect("should be present");
        match *i {
            program::Inst::Match => s.len() == 0,
            program::Inst::Char(program::InstChar { c, goto }) => self.m_char(c, goto, s),
            program::Inst::Split(program::InstSplit { goto1, goto2 }) => {
                self.m_split(goto1, goto2, s)
            }
            program::Inst::Save(_) => unreachable!("not reachable"),
        }
    }

    fn m_char(&self, c: char, goto: InstPtr, s: &str) -> bool {
        s.chars().next().eq(&Some(c)) && self.m(goto, &s[1..])
    }

    fn m_split(&self, goto1: InstPtr, goto2: InstPtr, s: &str) -> bool {
        self.m(goto1, s) || self.m(goto2, s)
    }
}

#[cfg(test)]
mod test {
    use super::{Executor, ExecutorType};
    use crate::ast::Ast;
    use crate::automata::compiler::Compiler;
    use crate::{ast::parser::Parser, automata::program::Program};

    fn p(s: &str) -> Ast {
        Parser::new(s.to_string())
            .parse()
            .expect("should be valid ast")
    }

    fn c(s: &str) -> Program {
        Compiler::new().compile(&p(s)).expect("should be valid")
    }

    #[test]
    fn recursive() {
        executor(Executor::new(ExecutorType::Recursive));
        executor(Executor::new(ExecutorType::Pike));
    }

    fn executor(ex: Executor) {
        let p = &c("ab");
        assert_eq!(true, ex.matches(p, "ab"));
        assert_eq!(false, ex.matches(p, "abc"));
        assert_eq!(false, ex.matches(p, "a"));

        let p = &c("a|b");
        assert_eq!(true, ex.matches(p, "a"));
        assert_eq!(true, ex.matches(p, "b"));
        assert_eq!(false, ex.matches(p, "ab"));

        let p = &c("a*b");
        assert_eq!(true, ex.matches(p, "b"));
        assert_eq!(true, ex.matches(p, "ab"));
        assert_eq!(true, ex.matches(p, "aaaaaaab"));
        assert_eq!(false, ex.matches(p, "abb"));

        let p = &c("a+b");
        assert_eq!(false, ex.matches(p, "b"));
        assert_eq!(true, ex.matches(p, "ab"));
        assert_eq!(true, ex.matches(p, "aaaaaaab"));
        assert_eq!(false, ex.matches(p, "abb"));

        let p = &c("a?b");
        assert_eq!(true, ex.matches(p, "b"));
        assert_eq!(true, ex.matches(p, "ab"));
        assert_eq!(false, ex.matches(p, "aaab"));
        assert_eq!(false, ex.matches(p, "abb"));

        let p = &c("a(bc|cd)*e");
        assert_eq!(true, ex.matches(p, "ae"));
        assert_eq!(true, ex.matches(p, "abce"));
        assert_eq!(true, ex.matches(p, "acde"));
        assert_eq!(true, ex.matches(p, "abccdbce"));
        assert_eq!(false, ex.matches(p, "abccd"));
        assert_eq!(false, ex.matches(p, "abccdef"));
    }
}
