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
            Token::Rl(rl) => rl.show(0),
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
    pub fn show(&self, shift: u8) {
        match self {
            Rule::Literal(lit) => {
                print!("\"{}\"", lit);
            },
            Rule::Identifier(lit) => {
                print!("{}", lit);
            },
            Rule::AlterRefL(sub_1, sub_2) => match sub_1.upgrade() {
                Some(s1) => {
                    print!("| + + + ");
                    sub_2.show(shift+3);
                    println!();
                    for _ in 1..3 {
                        println_shift("*", shift+2);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    s1.show(shift+1);
                },
                None => {
                    print!("| + + + ");
                    sub_2.show(shift+3);
                    println!();
                    for _ in 1..3 {
                        println_shift("*", shift+2);
                    }
                    println_shift("/NO REFERENCE\\", shift+1);
                },
            },
            Rule::ConcatRefL(sub_1, sub_2) => match sub_1.upgrade() {
                Some(s1) => {
                    print!(", + + + ");
                    sub_2.show(shift+3);
                    println!();
                    for _ in 1..3 {
                        println_shift("*", shift+2);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    s1.show(shift+1);
                },
                None => {
                    print!(", + + + ");
                    sub_2.show(shift+3);
                    println!();
                    for _ in 1..3 {
                        println_shift("*", shift+2);
                    }
                    println_shift("/NO REFERENCE\\", shift+1);
                },
            },
            Rule::ExceptRefL(sub_1, sub_2) => match sub_1.upgrade() {
                Some(s1) => {
                    print!("- + + + ");
                    sub_2.show(shift+3);
                    println!();
                    for _ in 1..3 {
                        println_shift("*", shift+2);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    s1.show(shift+1);
                },
                None => {
                    print!("- + + + ");
                    sub_2.show(shift+3);
                    println!();
                    for _ in 1..3 {
                        println_shift("+", shift+3);
                    }
                    println_shift("/NO REFERENCE\\", shift+1);
                },
            },
            Rule::AlterRefR(sub_1, sub_2) => match sub_2.upgrade() {
                Some(s2) => {
                    print!("| * * * ");
                    s2.show(shift+3);
                    println!();
                    for _ in 1..3 {
                        println_shift("+", shift+3);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    sub_1.show(shift+1);
                },
                None => {
                    println!("| * * * /NO REFERENCE\\");
                    for _ in 1..3 {
                        println_shift("+", shift+3);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    sub_1.show(shift+1);
                },
            },
            Rule::ConcatRefR(sub_1, sub_2) => match sub_2.upgrade() {
                Some(s2) => {
                    print!(", * * * ");
                    s2.show(shift+3);
                    println!();
                    for _ in 1..3 {
                        println_shift("+", shift+3);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    sub_1.show(shift+1);
                },
                None => {
                    println!(", * * * /NO REFERENCE\\");
                    for _ in 1..3 {
                        println_shift("+", shift+3);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    sub_1.show(shift+1);
                },
            },
            Rule::ExceptRefR(sub_1, sub_2) => match sub_2.upgrade() {
                Some(s2) => {
                    print!("- * * * ");
                    s2.show(shift+3);
                    println!();
                    for _ in 1..3 {
                        println_shift("+", shift+3);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    sub_1.show(shift+1);
                },
                None => {
                    println!("- * * * /NO REFERENCE\\");
                    for _ in 1..3 {
                        println_shift("+", shift+3);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    sub_1.show(shift+1);
                },
            },
            Rule::AlterRef(sub_1, sub_2) => match (sub_1.upgrade(), sub_2.upgrade()) {
                (Some(s1), Some(s2)) => {
                    print!("| * * * ");
                    s2.show(shift+4);
                    println!();
                    for _ in 1..3 {
                        println_shift("*", shift+2);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    s1.show(shift+1);
                },
                (Some(s1), None) => {
                    println!("| * * * /NO REFERENCE\\");
                    for _ in 1..3 {
                        println_shift("*", shift+2);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    s1.show(shift+1);
                },
                (None, Some(s2)) => {
                    print!("| * * * ");
                    s2.show(shift+4);
                    println!();
                    for _ in 1..3 {
                        println_shift("*", shift+2);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    println!("/NO REFERENCE\\");
                },
                (None, None) => {
                    println!("| * * * /NO REFERENCE\\");
                    for _ in 1..3 {
                        println_shift("*", shift+2);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    println!("/NO REFERENCE\\");
                },
            },
            Rule::ConcatRef(sub_1, sub_2) => match (sub_1.upgrade(), sub_2.upgrade()) {
                (Some(s1), Some(s2)) => {
                    print!(", * * * ");
                    s2.show(shift+4);
                    println!();
                    for _ in 1..3 {
                        println_shift("*", shift+2);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    s1.show(shift+1);
                },
                (Some(s1), None) => {
                    println!(", * * * /NO REFERENCE\\");
                    for _ in 1..3 {
                        println_shift("*", shift+2);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    s1.show(shift+1);
                },
                (None, Some(s2)) => {
                    print!(", * * * ");
                    s2.show(shift+4);
                    println!();
                    for _ in 1..3 {
                        println_shift("*", shift+2);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    println!("/NO REFERENCE\\");
                },
                (None, None) => {
                    println!(", * * * /NO REFERENCE\\");
                    for _ in 1..3 {
                        println_shift("*", shift+2);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    println!("/NO REFERENCE\\");
                },
            },
            Rule::ExceptRef(sub_1, sub_2) => match (sub_1.upgrade(), sub_2.upgrade()) {
                (Some(s1), Some(s2)) => {
                    print!("- * * * ");
                    s2.show(shift+4);
                    println!();
                    for _ in 1..3 {
                        println_shift("*", shift+2);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    s1.show(shift+1);
                },
                (Some(s1), None) => {
                    println!("- * * * /NO REFERENCE\\");
                    for _ in 1..3 {
                        println_shift("*", shift+2);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    s1.show(shift+1);
                },
                (None, Some(s2)) => {
                    print!("- * * * ");
                    s2.show(shift+4);
                    println!();
                    for _ in 1..3 {
                        println_shift("*", shift+2);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    println!("/NO REFERENCE\\");
                },
                (None, None) => {
                    println!("- * * * /NO REFERENCE\\");
                    for _ in 1..3 {
                        println_shift("*", shift+2);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    println!("/NO REFERENCE\\");
                },
            },
            Rule::Ref(sub_tree) => match sub_tree.upgrade() {
                Some(el) => {
                    print!("- ~~~~~> ");
                    el.show(shift+4);
                    println!();
                },
                None => println!("- ~~~~> /NO REFERENCE\\"),
            },
            Rule::RepetRef(sub_tree) => match sub_tree.upgrade() {
                Some(el) => {
                    print!("{{}} * * * ");
                    el.show(shift+4);
                    println!();
                },
                None => println!("{{}} * * * /NO REFERENCE\\"),
            },
            Rule::GrpRef(sub_tree) => match sub_tree.upgrade() {
                Some(el) => {
                    print!("() * * * ");
                    el.show(shift+4);
                    println!();
                },
                None => println!("() * * * /NO REFERENCE\\"),
            },
            Rule::OptRef(sub_tree) => match sub_tree.upgrade() {
                Some(el) => {
                    print!("[] * * * ");
                    el.show(shift+4);
                    println!();
                },
                None => println!("[] * * * /NO REFERENCE\\"),
            },
            Rule::Alternation(sub_1, sub_2) => {
                print!("| + + + ");
                    sub_2.show(shift+4);
                    println!();
                    for _ in 1..3 {
                        println_shift("+", shift+3);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    sub_1.show(shift+1);
            },
            Rule::Concatenation(sub_1, sub_2) => {
                print!(", + + + ");
                    sub_2.show(shift+4);
                    println!();
                    for _ in 1..3 {
                        println_shift("+", shift+2);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    sub_1.show(shift);
            },
            Rule::Exception(sub_1, sub_2) => {
                print!("- + + + ");
                    sub_2.show(shift+4);
                    println!();
                    for _ in 1..3 {
                        println_shift("+", shift+3);
                    }
                    for _ in 1..shift+1 {
                        print!("  ");
                    }
                    sub_1.show(shift+1);
            },
            Rule::Repetition(el) => {
                print!("{{ }} + + + ");
                el.show(shift+4);
                println!();
            },
            Rule::Grouping(el) => {
                print!("( ) + + + ");
                el.show(shift+4);
                println!();
            },
            Rule::Optional(el) => {
                print!("[ ] + + + ");
                el.show(shift+4);
                println!();
            },
        }
            
    }
}