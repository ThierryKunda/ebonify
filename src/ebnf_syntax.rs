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
pub enum Rule {
    Literal(String),
    Identifier(String),
    Ref(Weak<Rule>),
    Alternation(Rc<Rule>, Rc<Rule>),
    Concatenation(Rc<Rule>, Rc<Rule>),
    Exception(Rc<Rule>, Rc<Rule>),
    Optional(Rc<Rule>),
    Repetition(Rc<Rule>),
    Grouping(Rc<Rule>),
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

impl Rule {
    pub fn show(&self) {
        match self {
            Rule::Literal(lit) => {
                print!("\"{}\"", lit);
            },
            Rule::Identifier(lit) => {
                print!("{}", lit);
            },
            _ => (),
        }
            
    }
}