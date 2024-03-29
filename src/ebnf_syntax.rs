use std::rc::{Rc, Weak};

#[derive(Clone, Debug)]
pub enum Operator {
    Alternation,
    Concatenation,
    Exception,
    OptionalL,
    OptionalR,
    RepetitionL,
    RepetitionR,
    GroupingL,
    GroupingR,
}
 #[derive(Clone, Debug)]
pub enum AtomicKind {
    Literal,
    Identifier,
}

#[derive(Clone, Debug)]
pub enum SingleKind {
    Repetition,
    Grouping,
    Optional,
}

#[derive(Clone, Debug)]
pub enum DualKind {
    Alternation,
    Concatenation,
    Exception,
}

#[derive(Clone, Debug)]
pub enum Rule {
    Atomic(String, AtomicKind),
    Ref(Weak<Rule>),
    Single(Rc<Rule>, SingleKind),
    Dual(Rc<Rule>, DualKind, Rc<Rule>),
}

#[derive(Clone, Debug)]
pub enum Token {
    Op(Operator),
    Rl(Rc<Rule>),
    Invalid,
}