use std::rc::{Rc, Weak};

#[derive(Debug)]
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
 #[derive(Debug)]
pub enum AtomicKind {
    Literal,
    Identifier,
}

#[derive(Debug)]
pub enum SingleKind {
    Repetition,
    Grouping,
    Optional,
}

#[derive(Debug)]
pub enum DualKind {
    Alternation,
    Concatenation,
    Exception,
}

#[derive(Debug)]
pub enum Rule {
    Atomic(String, AtomicKind),
    Ref(Weak<Rule>),
    Single(Rc<Rule>, SingleKind),
    Dual(Rc<Rule>, DualKind, Rc<Rule>),
}

#[derive(Debug)]
pub enum Token {
    Op(Operator),
    Rl(Rc<Rule>),
    Invalid,
}

impl Token {
    pub fn show(&self) {
        match self {
            Token::Op(op) => match op {
                Operator::Alternation => print!(" | "),
                Operator::Concatenation => print!(" , "),
                Operator::Exception => print!(" - "),
                Operator::OptionalL => print!(" [ "),
                Operator::OptionalR => print!(" ] "),
                Operator::RepetitionL => print!(" {{ "),
                Operator::RepetitionR => print!(" }} "),
                Operator::GroupingL => print!(" ( "),
                Operator::GroupingR => print!(" ) "),
            },
            Token::Rl(rl) => rl.show(),
            Token::Invalid => print!("INVALID"),
        }
    }
}

fn println_shift(txt: &str, shift: u8) {
    for _ in 1..shift {
        print!("  ");
    }
    println!("{}", txt);
}