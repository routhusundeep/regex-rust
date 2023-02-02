use crate::ast::{self, Ast, Position, Span};
use std::{borrow::Borrow, cell::Cell, result};

use super::Literal;

type Result<T> = result::Result<T, ast::Error>;

pub struct Parser {
    p: String,
    pos: Cell<Position>,
    stack: Vec<GroupState>,
}

enum GroupState {
    Group {
        concat: ast::Concat,
        group: ast::Group,
    },
    Alternation {
        alt: ast::Alternation,
    },
}

impl Parser {
    pub fn new(p: String) -> Self {
        Parser {
            p: p,
            pos: Cell::new(Position::new(0)),
            stack: vec![],
        }
    }

    fn pattern(&self) -> &str {
        self.p.borrow()
    }

    fn pos(&self) -> Position {
        self.pos.get()
    }

    fn span(&self) -> Span {
        Span::point(self.pos())
    }

    fn span_char(&self) -> Span {
        Span::new(
            self.pos(),
            Position {
                offset: self.offset() + self.char().len_utf8(),
            },
        )
    }

    fn error(&self, span: Span, kind: ast::ErrorKind) -> ast::Error {
        ast::Error {
            kind,
            pattern: self.pattern().to_string(),
            span,
        }
    }

    fn offset(&self) -> usize {
        self.pos().offset
    }

    fn bump(&self) -> bool {
        if self.is_eof() {
            return false;
        }
        self.pos
            .set(Position::new(self.pos().offset + self.char().len_utf8()));
        self.pattern()[self.offset()..].chars().next().is_some()
    }

    fn is_eof(&self) -> bool {
        self.offset() == self.p.len()
    }

    fn char(&self) -> char {
        self.char_at(self.pos().offset)
    }

    fn char_at(&self, i: usize) -> char {
        self.pattern()[i..]
            .chars()
            .next()
            .unwrap_or_else(|| panic!("expected char at offset {}", i))
    }

    pub fn parse(&mut self) -> Result<Ast> {
        let mut concat = ast::Concat::new(self.span());
        loop {
            if self.is_eof() {
                break;
            }

            match self.char() {
                '(' => concat = self.push_group(concat)?,
                ')' => concat = self.pop_group(concat)?,
                '|' => concat = self.push_alternate(concat)?,
                '?' => concat = self.parse_repetition(concat, ast::RepetitionKind::ZeroOrOne)?,
                '*' => concat = self.parse_repetition(concat, ast::RepetitionKind::ZeroOrMore)?,
                '+' => concat = self.parse_repetition(concat, ast::RepetitionKind::OneOrMore)?,
                _ => concat.asts.push(Ast::Literal(self.parse_primitive()?)),
            }
        }

        let ast = self.pop_end(concat)?;
        Ok(ast)
    }

    fn parse_primitive(&self) -> Result<Literal> {
        let l = Literal {
            span: self.span_char(),
            kind: ast::LiteralKind::Verbatim,
            char: self.char(),
        };
        self.bump();
        Ok(l)
    }

    fn parse_repetition(
        &self,
        mut concat: ast::Concat,
        kind: ast::RepetitionKind,
    ) -> Result<ast::Concat> {
        assert!(self.char() == '?' || self.char() == '*' || self.char() == '+');

        let start = self.pos();
        let ast = concat
            .asts
            .pop()
            .ok_or_else(|| self.error(self.span(), ast::ErrorKind::RepetitionMissing))?;

        let mut greedy = true;
        if self.bump() && self.char() == '?' {
            greedy = false;
            self.bump();
        }
        concat.asts.push(Ast::Repetition(ast::Repetition {
            op: ast::RepetitionOp {
                span: Span::new(start, self.pos()),
                kind,
            },
            greedy,
            ast: Box::new(ast),
        }));
        Ok(concat)
    }

    fn push_group(&mut self, concat: ast::Concat) -> Result<ast::Concat> {
        assert!(self.char() == '(');
        self.stack.push(GroupState::Group {
            concat: concat,
            group: ast::Group {
                span: self.span_char(),
                ast: Box::new(Ast::Empty(ast::Empty { span: self.span() })),
            },
        });
        self.bump();
        Ok(ast::Concat::new(self.span()))
    }

    fn pop_group(&mut self, mut group_concat: ast::Concat) -> Result<ast::Concat> {
        use self::GroupState::*;

        assert!(self.char() == ')');
        let err = Err(self.error(self.span(), ast::ErrorKind::GroupUnopened));
        let (mut before_concat, mut group, alt) = match self.stack.pop() {
            None => return err,
            Some(Group { concat, group }) => (concat, group, None),
            Some(Alternation { alt }) => match self.stack.pop() {
                Some(Group { concat, group }) => (concat, group, Some(alt)),
                Some(Alternation { alt: _ }) => return err,
                None => return err,
            },
        };
        group_concat.span.end = self.pos();
        self.bump();
        group.span.end = self.pos();
        group.span.end = self.pos();

        match alt {
            Some(mut alt) => {
                alt.span.end = group_concat.span.end;
                alt.asts.push(group_concat.into_ast());
                group.ast = Box::new(alt.into_ast());
            }
            None => {
                group.ast = Box::new(group_concat.into_ast());
            }
        }

        before_concat.asts.push(Ast::Group(group));
        Ok(before_concat)
    }

    fn push_alternate(&mut self, mut concat: ast::Concat) -> Result<ast::Concat> {
        assert_eq!(self.char(), '|');
        concat.span.end = self.pos();
        match self.stack.last_mut() {
            Some(GroupState::Alternation { ref mut alt }) => {
                alt.asts.push(concat.into_ast());
            }
            None | _ => {
                let alt = ast::Alternation {
                    span: Span::new(concat.span.start, self.pos()),
                    asts: vec![concat.into_ast()],
                };
                self.stack.push(GroupState::Alternation { alt: alt })
            }
        };

        self.bump();
        Ok(ast::Concat {
            span: self.span(),
            asts: vec![],
        })
    }

    fn pop_end(&mut self, mut concat: ast::Concat) -> Result<Ast> {
        concat.span.end = self.pos();
        let ast = match self.stack.pop() {
            Some(GroupState::Alternation { mut alt }) => {
                alt.span.end = self.pos();
                alt.asts.push(concat.into_ast());
                Ok(Ast::Alternation(alt))
            }
            Some(GroupState::Group { concat: _, group }) => {
                return Err(self.error(group.span, ast::ErrorKind::GroupUnclosed))
            }
            None => Ok(concat.into_ast()),
        };
        match self.stack.pop() {
            Some(GroupState::Alternation { alt: _ }) => unreachable!(),
            Some(GroupState::Group { concat: _, group }) => {
                Err(self.error(group.span, ast::ErrorKind::GroupUnclosed))
            }
            None => ast,
        }
    }
}

#[cfg(test)]
mod tests {

    use std::{ops::Range, vec};

    use super::Parser;
    use crate::ast::{self, Ast, Position, Span};
    use pretty_assertions::assert_eq;

    fn p(s: &str) -> super::Parser {
        Parser::new(s.to_string())
    }

    fn pp(s: &str) -> super::Result<Ast> {
        p(s).parse()
    }

    fn span(range: Range<usize>) -> Span {
        let start = Position::new(range.start);
        let end = Position::new(range.end);
        Span::new(start, end)
    }

    fn lit_one(c: char, p: usize) -> Ast {
        Ast::Literal(ast::Literal {
            span: span(p..p + 1),
            kind: ast::LiteralKind::Verbatim,
            char: c,
        })
    }

    fn empty(p: usize) -> Ast {
        Ast::Empty(ast::Empty { span: span(p..p) })
    }

    fn rep(s: Range<usize>, kind: ast::RepetitionKind, greedy: bool, ast: Ast) -> Ast {
        Ast::Repetition(ast::Repetition {
            op: ast::RepetitionOp {
                span: span(s),
                kind: kind,
            },
            greedy: true,
            ast: Box::new(ast),
        })
    }

    fn concat(r: Range<usize>, asts: Vec<Ast>) -> Ast {
        Ast::Concat(ast::Concat {
            span: span(r),
            asts: asts,
        })
    }

    fn alt(r: Range<usize>, asts: Vec<Ast>) -> Ast {
        Ast::Alternation(ast::Alternation {
            span: span(r),
            asts: asts,
        })
    }

    fn group(r: Range<usize>, ast: Ast) -> Ast {
        Ast::Group(ast::Group {
            span: span(r),
            ast: Box::new(ast),
        })
    }

    #[test]
    fn parse_primitive() {
        assert_eq!(
            p(r"a").parse_primitive(),
            Ok(ast::Literal {
                span: span(0..1),
                kind: ast::LiteralKind::Verbatim,
                char: 'a',
            })
        );
        assert_eq!(
            p(r"☃").parse_primitive(),
            Ok(ast::Literal {
                span: span(0..3),
                kind: ast::LiteralKind::Verbatim,
                char: '☃',
            })
        );
    }

    #[test]
    fn parse_repetition() {
        use ast::RepetitionKind::{OneOrMore, ZeroOrMore, ZeroOrOne};
        assert_eq!(pp(r"a*"), Ok(rep(1..2, ZeroOrMore, true, lit_one('a', 0))));
        assert_eq!(pp(r"a+"), Ok(rep(1..2, OneOrMore, true, lit_one('a', 0))));
        assert_eq!(pp(r"a?"), Ok(rep(1..2, ZeroOrOne, true, lit_one('a', 0))));
        assert_eq!(
            pp(r"a*b"),
            Ok(concat(
                0..3,
                vec![
                    rep(1..2, ZeroOrMore, true, lit_one('a', 0)),
                    lit_one('b', 2),
                ]
            ))
        );
        assert_eq!(
            pp(r"a(b)*c"),
            Ok(concat(
                0..6,
                vec![
                    lit_one('a', 0),
                    rep(4..5, ZeroOrMore, true, group(1..4, lit_one('b', 2))),
                    lit_one('c', 5),
                ]
            ))
        );
    }

    #[test]
    fn test_group() {
        assert_eq!(pp(r"(a)"), Ok(group(0..3, lit_one('a', 1))));
        assert_eq!(
            pp(r"a(b(c))a"),
            Ok(concat(
                0..8,
                vec![
                    lit_one('a', 0),
                    group(
                        1..7,
                        concat(2..6, vec![lit_one('b', 2), group(3..6, lit_one('c', 4))])
                    ),
                    lit_one('a', 7)
                ]
            ))
        );
        assert_eq!(
            pp(r"(a|b)"),
            Ok(group(
                0..5,
                alt(1..4, vec![lit_one('a', 1), lit_one('b', 3)])
            ),)
        );
    }

    #[test]
    fn test_alternation() {
        assert_eq!(
            pp(r"a|b"),
            Ok(alt(0..3, vec![lit_one('a', 0), lit_one('b', 2)]))
        );
        assert_eq!(
            pp(r"a|b|c"),
            Ok(alt(
                0..5,
                vec![lit_one('a', 0), lit_one('b', 2), lit_one('c', 4)]
            ))
        );
        assert_eq!(
            pp(r"ab|bc"),
            Ok(alt(
                0..5,
                vec![
                    concat(0..2, vec![lit_one('a', 0), lit_one('b', 1)]),
                    concat(3..5, vec![lit_one('b', 3), lit_one('c', 4)])
                ]
            ))
        );
        assert_eq!(
            pp(r"a||b"),
            Ok(alt(0..4, vec![lit_one('a', 0), empty(2), lit_one('b', 3),]))
        );
    }

    #[test]
    fn test_adhoc_complex() {
        use ast::RepetitionKind::{OneOrMore, ZeroOrMore, ZeroOrOne};
        assert_eq!(
            pp(r"a*b(a?|c(d|e)+)fg"),
            Ok(concat(
                0..17,
                vec![
                    rep(1..2, ZeroOrMore, true, lit_one('a', 0)),
                    lit_one('b', 2),
                    group(
                        3..15,
                        alt(
                            4..14,
                            vec![
                                rep(5..6, ZeroOrOne, true, lit_one('a', 4)),
                                concat(
                                    7..14,
                                    vec![
                                        lit_one('c', 7),
                                        rep(
                                            13..14,
                                            OneOrMore,
                                            true,
                                            group(
                                                8..13,
                                                alt(
                                                    9..12,
                                                    vec![lit_one('d', 9), lit_one('e', 11),]
                                                )
                                            )
                                        )
                                    ]
                                )
                            ]
                        ),
                    ),
                    lit_one('f', 15),
                    lit_one('g', 16),
                ]
            ))
        );
    }
}
