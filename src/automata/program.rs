pub type InstPtr = usize;

#[derive(Debug, Eq, PartialEq)]
pub enum Inst {
    Match,
    Char(InstChar),
    Split(InstSplit),
    Save(InstSave),
}

#[derive(Debug, Eq, PartialEq)]
pub struct InstSave {
    pub slot: usize,
    pub goto: InstPtr,
}

#[derive(Debug, Eq, PartialEq)]
pub struct InstChar {
    pub c: char,
    pub goto: InstPtr,
}

#[derive(Debug, Eq, PartialEq)]
pub struct InstSplit {
    pub goto1: InstPtr,
    pub goto2: InstPtr,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Program {
    pub insts: Vec<Inst>,
    pub start: InstPtr,
}
