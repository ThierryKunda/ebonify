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
    N,
    Literal(String),
    Identifier(String),
    Operation(Box<Rule>, Operator),
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