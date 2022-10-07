use std::collections::HashMap;
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

pub enum Rule<'a> {
    Literal(String),
    Identifier(String),
    Alternation(Box<Rule<'a>>, Box<Rule<'a>>),
    AlterRef(Box<&'a Rule<'a>>, Box<&'a Rule<'a>>),
    AlterRefL(Box<&'a Rule<'a>>, Box<Rule<'a>>),
    AlterRefR(Box<Rule<'a>>, Box<&'a Rule<'a>>),
    Concatenation(Box<Rule<'a>>, Box<Rule<'a>>),
    ConcatRef(Box<&'a Rule<'a>>, Box<&'a Rule<'a>>),
    ConcatRefL(Box<&'a Rule<'a>>, Box<Rule<'a>>),
    ConcatRefR(Box<Rule<'a>>, Box<&'a Rule<'a>>),
    Exception(Box<Rule<'a>>, Box<Rule<'a>>),
    ExceptRef(Box<&'a Rule<'a>>, Box<&'a Rule<'a>>),
    ExceptRefL(Box<&'a Rule<'a>>, Box<Rule<'a>>),
    ExceptRefR(Box<Rule<'a>>, Box<&'a Rule<'a>>),
    Optional(Box<Rule<'a>>),
    OptRef(Box<&'a Rule<'a>>),
    Repetition(Box<&'a Rule<'a>>),
    RepetRef(Box<&'a Rule<'a>>),
    GrpRef(Box<&'a Rule<'a>>),
}

pub enum Token<'a> {
    Op(Operator),
    Rl(Rule<'a>),
    Invalid,
}