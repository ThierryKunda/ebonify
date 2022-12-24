use std::rc::{Rc, Weak};

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

pub enum Rule {
    Literal(String),
    Identifier(String),
    Ref(Weak<Rule>),
    Alternation(Rc<Rule>, Rc<Rule>),
    AlterRef(Weak<Rule>,Weak<Rule>),
    AlterRefL(Weak<Rule>, Rc<Rule>),
    AlterRefR(Rc<Rule>, Weak<Rule>),
    Concatenation(Rc<Rule>, Rc<Rule>),
    ConcatRef(Weak<Rule>, Weak<Rule>),
    ConcatRefL(Weak<Rule>, Rc<Rule>),
    ConcatRefR(Rc<Rule>, Weak<Rule>),
    Exception(Rc<Rule>, Rc<Rule>),
    ExceptRef(Weak<Rule>, Weak<Rule>),
    ExceptRefL(Weak<Rule>, Rc<Rule>),
    ExceptRefR(Rc<Rule>, Weak<Rule>),
    Optional(Rc<Rule>),
    OptRef(Weak<Rule>),
    Repetition(Rc<Rule>),
    RepetRef(Weak<Rule>),
    Grouping(Rc<Rule>),
    GrpRef(Weak<Rule>),
}

pub enum Token {
    Op(Operator),
    Rl(Rc<Rule>),
    Invalid,
}