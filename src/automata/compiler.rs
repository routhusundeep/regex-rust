use std::{result, vec};

use crate::{
    ast,
    automata::program::{Inst, InstChar, InstPtr, InstSplit, Program},
    errors::Error,
};

#[derive(Debug, Eq, PartialEq)]
enum MaybeInst {
    Compiled(Inst),
    Uncompiled(InstHole),
    Split,
    Split1(InstPtr),
    Split2(InstPtr),
}

impl MaybeInst {
    fn unwrap(self) -> Inst {
        match self {
            MaybeInst::Compiled(inst) => inst,
            _ => unreachable!(
                "must be called on a compiled instruction, \
                 instead it was called on: {:?}",
                self
            ),
        }
    }

    fn fill_split_goto1(&mut self, goto1: InstPtr) {
        let half_filled = match *self {
            MaybeInst::Split => goto1,
            _ => unreachable!(
                "must be called on Split instruction, \
                 instead it was called on: {:?}",
                self
            ),
        };
        *self = MaybeInst::Split1(half_filled);
    }

    fn fill_split_goto2(&mut self, goto2: InstPtr) {
        let half_filled = match *self {
            MaybeInst::Split => goto2,
            _ => unreachable!(
                "must be called on Split instruction, \
                 instead it was called on: {:?}",
                self
            ),
        };
        *self = MaybeInst::Split2(half_filled);
    }

    fn fill(&mut self, goto: usize) {
        let maybe_inst = match *self {
            MaybeInst::Uncompiled(ref hole) => MaybeInst::Compiled(hole.fill(goto)),
            MaybeInst::Split => MaybeInst::Split1(goto),
            MaybeInst::Split1(goto1) => MaybeInst::Compiled(Inst::Split(InstSplit {
                goto1: goto1,
                goto2: goto,
            })),
            MaybeInst::Split2(goto2) => MaybeInst::Compiled(Inst::Split(InstSplit {
                goto1: goto,
                goto2: goto2,
            })),
            _ => unreachable!("compiled cant be filled"),
        };
        *self = maybe_inst
    }
}

#[derive(Debug, Eq, PartialEq)]
enum InstHole {
    Char { c: char },
}
impl InstHole {
    fn fill(&self, goto: usize) -> Inst {
        match *self {
            InstHole::Char { c } => Inst::Char(InstChar { c: c, goto }),
        }
    }
}

struct Patch {
    hole: Hole,
    entry: InstPtr,
}

enum Hole {
    None,
    One(InstPtr),
    Many(Vec<Hole>),
}
impl Hole {
    fn dup_one(&self) -> (Hole, Hole) {
        match *self {
            Hole::None => unreachable!("cannot dup none"),
            Hole::One(pc) => (Hole::One(pc), Hole::One(pc)),
            Hole::Many(_) => unreachable!("cannot dup many"),
        }
    }

    fn new(holes: Vec<Hole>) -> Hole {
        match holes.len() {
            0 => Hole::None,
            1 => holes.into_iter().next().expect("exists"),
            _ => Hole::Many(holes),
        }
    }
}

type ResultOrEmpty = result::Result<Option<Patch>, Error>;

pub struct Compiler {
    insts: Vec<MaybeInst>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler { insts: vec![] }
    }

    pub fn compile(mut self, ast: &ast::Ast) -> result::Result<Program, Error> {
        let mut e = 0;
        match self.c(ast)? {
            Some(Patch { hole, entry }) => {
                e = entry;
                self.fill_to_next(hole);
            }
            None => {}
        }

        self.push_compiled(Inst::Match);
        let insts = self
            .insts
            .into_iter()
            .map(|i| i.unwrap())
            .collect::<Vec<_>>();
        Ok(Program {
            insts: insts,
            start: e,
        })
    }

    fn push_compiled(&mut self, inst: Inst) {
        self.insts.push(MaybeInst::Compiled(inst));
    }

    fn fill_split(&mut self, hole: Hole, goto1: Option<InstPtr>, goto2: Option<InstPtr>) -> Hole {
        match hole {
            Hole::None => Hole::None,
            Hole::One(pc) => match (goto1, goto2) {
                (None, None) => unreachable!("atleast one split holes must be filled"),
                (None, Some(goto2)) => {
                    self.insts[pc].fill_split_goto2(goto2);
                    Hole::One(pc)
                }
                (Some(goto1), None) => {
                    self.insts[pc].fill_split_goto1(goto1);
                    Hole::One(pc)
                }
                (Some(goto1), Some(goto2)) => todo!(),
            },
            Hole::Many(holes) => {
                let mut new_holes = vec![];
                for hole in holes {
                    new_holes.push(self.fill_split(hole, goto1, goto2));
                }
                Hole::new(new_holes)
            }
        }
    }

    fn fill_to_next(&mut self, hole: Hole) {
        let next = self.insts.len();
        self.fill(hole, next);
    }

    fn push_split_hole(&mut self) -> Hole {
        let hole = self.insts.len();
        self.insts.push(MaybeInst::Split);
        Hole::One(hole)
    }

    fn pop_split_hole(&mut self) -> ResultOrEmpty {
        self.insts.pop();
        Ok(None)
    }

    fn c(&mut self, ast: &ast::Ast) -> ResultOrEmpty {
        match *ast {
            ast::Ast::Empty(_) => self.c_empty(),
            ast::Ast::Concat(ref concat) => self.c_concat(concat.asts.as_ref()),
            ast::Ast::Repetition(ref repetition) => self.c_repetition(
                repetition.kind(),
                repetition.greedy,
                repetition.ast.as_ref(),
            ),
            ast::Ast::Literal(ref literal) => self.c_char(literal.char),
            ast::Ast::Group(ref group) => self.c_group(group.ast.as_ref()),
            ast::Ast::Alternation(ref alteration) => self.c_alternation(alteration.asts.as_ref()),
        }
    }

    fn c_concat(&mut self, concat: &[ast::Ast]) -> ResultOrEmpty {
        let mut iter = concat.iter();
        let Patch { mut hole, entry } = loop {
            match iter.next() {
                Some(e) => {
                    if let Some(p) = self.c(e)? {
                        break p;
                    }
                }
                None => return self.c_empty(),
            }
        };
        for e in iter {
            if let Some(p) = self.c(e)? {
                self.fill(hole, p.entry);
                hole = p.hole;
            }
        }
        Ok(Some(Patch { hole, entry }))
    }

    fn c_char(&mut self, c: char) -> ResultOrEmpty {
        let inst = MaybeInst::Uncompiled(InstHole::Char { c });
        let instrptr = self.insts.len();
        self.insts.push(inst);
        Ok(Some(Patch {
            hole: Hole::One(instrptr),
            entry: instrptr,
        }))
    }

    fn c_group(&mut self, ast: &ast::Ast) -> ResultOrEmpty {
        self.c(ast)
    }

    fn c_alternation(&mut self, alt: &[ast::Ast]) -> ResultOrEmpty {
        let mut holes: Vec<Hole> = vec![];
        let first_split_entry = self.insts.len();
        let mut prev = (Hole::None, false);

        for ast in &alt[0..alt.len() - 1] {
            if prev.1 {
                let next = self.insts.len();
                self.fill_split(prev.0, None, Some(next));
            } else {
                self.fill_to_next(prev.0);
            }
            let split = self.push_split_hole();

            if let Some(Patch { hole, entry }) = self.c(&ast)? {
                holes.push(hole);
                prev = (self.fill_split(split, Some(entry), None), false)
            } else {
                let (split1, split2) = split.dup_one();
                holes.push(split1);
                prev = (split2, true);
            }
        }
        if let Some(Patch { hole, entry }) = self.c(&alt[alt.len() - 1])? {
            holes.push(hole);
            if prev.1 {
                self.fill_split(prev.0, None, Some(entry));
            } else {
                self.fill(prev.0, entry);
            }
        } else {
            holes.push(prev.0);
        }
        Ok(Some(Patch {
            hole: Hole::new(holes),
            entry: first_split_entry,
        }))
    }

    fn c_empty(&mut self) -> ResultOrEmpty {
        Ok(None)
    }

    fn fill(&mut self, hole: Hole, goto: InstPtr) {
        match hole {
            Hole::None => {}
            Hole::One(pc) => self.insts[pc].fill(goto),
            Hole::Many(holes) => {
                for hole in holes {
                    self.fill(hole, goto)
                }
            }
        }
    }

    fn c_repetition(
        &mut self,
        kind: ast::RepetitionKind,
        greedy: bool,
        ast: &ast::Ast,
    ) -> ResultOrEmpty {
        match kind {
            ast::RepetitionKind::ZeroOrOne => self.c_zero_or_one(ast),
            ast::RepetitionKind::ZeroOrMore => self.c_zero_or_more(ast),
            ast::RepetitionKind::OneOrMore => self.c_one_or_more(ast),
        }
    }

    fn c_zero_or_one(&mut self, ast: &ast::Ast) -> result::Result<Option<Patch>, Error> {
        let split_entry = self.insts.len();
        let split = self.push_split_hole();
        match self.c(ast)? {
            Some(Patch { hole, entry }) => {
                let split_hole = self.fill_split(split, Some(entry), None);
                return Ok(Some(Patch {
                    hole: Hole::new(vec![split_hole, hole]),
                    entry: split_entry,
                }));
            }
            None => return self.pop_split_hole(),
        }
    }

    fn c_zero_or_more(&mut self, ast: &ast::Ast) -> result::Result<Option<Patch>, Error> {
        let split_entry = self.insts.len();
        let split = self.push_split_hole();
        match self.c(ast)? {
            Some(Patch { hole, entry }) => {
                self.fill(hole, split_entry);
                let split_hole = self.fill_split(split, Some(entry), None);
                return Ok(Some(Patch {
                    hole: split_hole,
                    entry: split_entry,
                }));
            }
            None => return self.pop_split_hole(),
        }
    }

    fn c_one_or_more(&mut self, ast: &ast::Ast) -> result::Result<Option<Patch>, Error> {
        match self.c(ast)? {
            Some(Patch { hole, entry }) => {
                self.fill_to_next(hole);
                let split = self.push_split_hole();
                let split_hole = self.fill_split(split, Some(entry), None);
                return Ok(Some(Patch {
                    hole: split_hole,
                    entry: entry,
                }));
            }
            None => Ok(None),
        }
    }
}

#[cfg(test)]
mod test {
    use super::{Compiler, Error};
    use crate::ast::Ast;
    use crate::automata::program::Inst;
    use crate::{ast::parser::Parser, automata::program, automata::program::Program};
    use pretty_assertions::assert_eq;
    use std::result::Result;

    fn p(s: &str) -> Ast {
        Parser::new(s.to_string())
            .parse()
            .expect("should be valid ast")
    }

    fn c(s: &str) -> Result<Program, Error> {
        Compiler::new().compile(&p(s))
    }

    fn i_split(goto1: usize, goto2: usize) -> program::Inst {
        Inst::Split(program::InstSplit { goto1, goto2 })
    }
    fn i_char(c: char, goto: usize) -> program::Inst {
        Inst::Char(program::InstChar { c, goto })
    }
    fn i_match() -> program::Inst {
        Inst::Match
    }

    fn program(v: Vec<program::Inst>) -> std::result::Result<Program, Error> {
        Ok(Program { insts: v, start: 0 })
    }

    #[test]
    fn concat() {
        assert_eq!(
            c("ab"),
            program(vec![i_char('a', 1), i_char('b', 2), i_match()])
        );
    }

    #[test]
    fn alternate() {
        assert_eq!(
            c("a|b|c"),
            program(vec![
                i_split(1, 2),
                i_char('a', 5),
                i_split(3, 4),
                i_char('b', 5),
                i_char('c', 5),
                i_match(),
            ])
        );
        assert_eq!(
            c("a||c"),
            program(vec![
                i_split(1, 2),
                i_char('a', 4),
                i_split(4, 3),
                i_char('c', 4),
                i_match(),
            ])
        );
    }

    #[test]
    fn repeat_zero_or_many() {
        assert_eq!(
            c("a*"),
            program(vec![i_split(1, 2), i_char('a', 0), i_match()])
        );
        assert_eq!(
            c("a(bc)*"),
            program(vec![
                i_char('a', 1),
                i_split(2, 4),
                i_char('b', 3),
                i_char('c', 1),
                i_match(),
            ])
        );
    }

    #[test]
    fn repeat_zero_or_one() {
        assert_eq!(
            c("a?"),
            program(vec![i_split(1, 2), i_char('a', 2), i_match()])
        );
    }

    #[test]
    fn repeat_one_or_many() {
        assert_eq!(
            c("a+"),
            program(vec![i_char('a', 1), i_split(0, 2), i_match()])
        );
    }
}
