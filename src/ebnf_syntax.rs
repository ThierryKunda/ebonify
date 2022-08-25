pub enum Operator {
    Alternation(String),
    Optional(String, String),
    RepetitionL(String),
    RepetitionR(String),
    GroupingL(String),
    GroupingR(String),
    TerminalStringL(String),
    TerminalStrinR(String),
    CommentL(String),
    CommentR(String),
    Exception(String),
}

pub struct Identifier {
    symbol: String,
}

pub enum Rule {
    N,
    Single(Identifier),
    Capsule(Identifier, Operator, Identifier),
    CapsCompo(Operator, Box<Rule>, Operator),
}

pub struct Association {
    identifier: Identifier,
    rule: Rule,
}

pub struct EBNF {
    rules: Vec<Rule>
}