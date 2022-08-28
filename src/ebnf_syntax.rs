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
    Dual(Box<Rule>, Operator, Box<Rule>),
    Single(Operator, Box<Rule>, Operator),

}

pub enum Token {
    Op(Operator),
    Rl(Rule),
    Invalid,
}

pub struct EBNF {
    identifier: Rule,
    rules: Vec<Rule>
}