use crate::{ebnf_syntax::*, pre_teatment::{brackets_paired}};

pub fn tree_without_grouping(rule: Rule) -> Rule {
    match rule {
        Rule::Literal(_) | Rule::Identifier(_) => rule,
        Rule::RepetRef(_) | Rule::OptRef(_) => rule,
        Rule::AlterRef(_, _) | Rule::ConcatRef(_, _) | Rule::ExceptRef(_, _) => rule,
        Rule::GrpRef(sub_tree) => match *sub_tree {
            Rule::Literal(lit) => Rule::Literal(lit.to_string()),
            Rule::Identifier(id) => Rule::Identifier(id.to_string()),
            _ => Rule::Identifier(String::from("Invalid")),
        },
        Rule::Grouping(sub_tree) => tree_without_grouping(*sub_tree), 
        Rule::Repetition(sub_tree) => Rule::Repetition(Box::new(tree_without_grouping(*sub_tree))),
        Rule::Optional(sub_tree) => Rule::Optional(Box::new(tree_without_grouping(*sub_tree))),
        Rule::Alternation(lst, rst) => Rule::Alternation(
            Box::new(tree_without_grouping(*lst)), Box::new(tree_without_grouping(*rst))),
        Rule::Concatenation(lst, rst) => Rule::Concatenation(
            Box::new(tree_without_grouping(*lst)), Box::new(tree_without_grouping(*rst))),
        Rule::Exception(lst, rst) => Rule::Exception(
            Box::new(tree_without_grouping(*lst)), Box::new(tree_without_grouping(*rst))),
            Rule::AlterRefL(left, rst) => Rule::AlterRefL(left, Box::new(tree_without_grouping(*rst))),
            Rule::AlterRefR(lst, right) => Rule::AlterRefR(Box::new(tree_without_grouping(*lst)), right),
            Rule::ConcatRefL(left, rst) => Rule::ConcatRefL(left, Box::new(tree_without_grouping(*rst))),
            Rule::ConcatRefR(lst, right) => Rule::ConcatRefR(Box::new(tree_without_grouping(*lst)), right),
            Rule::ExceptRefL(left, rst) => Rule::ExceptRefL(left, Box::new(tree_without_grouping(*rst))),
            Rule::ExceptRefR(lst, right) => Rule::ExceptRefR(Box::new(tree_without_grouping(*lst)), right),
    }
}

pub fn create_rule_tree<'a>(rule: &'a Vec<Token<'a>>) -> Rule<'a> {
    let rule_as_ref = tokens_as_ref(rule);
    create_rule_tree_by_ref(rule_as_ref)
}

pub fn create_rule_tree_by_ref<'a>(rule: Vec<&'a Token<'a>>) -> Rule<'a> {
    if rule.len() == 1 {
        match rule.first().unwrap() {
            Token::Rl(rl) => match rl {
                Rule::Literal(lit) => return Rule::Literal(lit.to_string()),
                Rule::Identifier(id) => return Rule::Identifier(id.to_string()),
                _ => return Rule::Identifier("Invalid".to_string()),
            },
            _ => return Rule::Identifier("Invalid".to_string()),
        };
    }
    if rule.len() == 3 {
        match (rule.first().unwrap(), rule.get(1).unwrap(), rule.last().unwrap()) {
            (Token::Op(op1), Token::Rl(rl), Token::Op(_)) => match op1 {
                Operator::OptionalL => return Rule::OptRef(Box::new(rl)),
                Operator::RepetitionL => return Rule::RepetRef(Box::new(rl)),
                Operator::GroupingL => return Rule::GrpRef(Box::new(rl)),
                _ => return Rule::Identifier("Invalid".to_string()),
            },
            (Token::Rl(rl1), Token::Op(op), Token::Rl(rl2)) => match op {
                Operator::Alternation => return Rule::AlterRef(Box::new(rl1), Box::new(rl2)),
                Operator::Concatenation => return Rule::ConcatRef(Box::new(rl1), Box::new(rl2)),
                Operator::Exception => return Rule::ExceptRef(Box::new(rl1), Box::new(rl2)),
                _ => return Rule::Identifier("Invalid".to_string()),
            },
            _ => return Rule::Identifier("Invalid".to_string()),
        };
    }
    if least_prior_is_unary(&rule) {
        match rule.first().unwrap() {
            Token::Op(Operator::RepetitionL) => return Rule::Repetition(Box::new(create_rule_tree_by_ref(get_rule_without_first_last(rule)))),
            Token::Op(Operator::OptionalL) => return Rule::Optional(Box::new(create_rule_tree_by_ref(get_rule_without_first_last(rule)))),
            Token::Op(Operator::GroupingL) => return Rule::Grouping(Box::new(create_rule_tree_by_ref(get_rule_without_first_last(rule)))),
            _ => Rule::Identifier("Invalid".to_string()),
        };
    }
    let i = get_least_prior_binary_index(&rule);
    match i {
        Some(idx) => {
            let el = *rule.get(idx).unwrap();
            let (left_part, right_part) = split_rule_by_index(&rule, idx);
            match el {
                Token::Op(Operator::Alternation) => return Rule::Alternation(Box::new(create_rule_tree_by_ref(left_part)), Box::new(create_rule_tree_by_ref(right_part))),
                Token::Op(Operator::Concatenation) => return Rule::Concatenation(Box::new(create_rule_tree_by_ref(left_part)), Box::new(create_rule_tree_by_ref(right_part))),
                Token::Op(Operator::Exception) => return Rule::Exception(Box::new(create_rule_tree_by_ref(left_part)), Box::new(create_rule_tree_by_ref(right_part))),
                _ => return Rule::Identifier("Invalid".to_string()),
            }
        }
        None => Rule::Identifier("Invalid".to_string()),
    };
    Rule::Identifier("Invalid".to_string())
}

pub fn with_priority_parentheses<'a, 'b>(rule: &'a mut Vec<&'b Token<'b>>) -> Vec<&'b Token<'b>> {
    let mut new_rule: Vec<&Token> = Vec::new();
    match rule.len() {
        1 => new_rule.push(rule.first().unwrap()), // with_priority_parentheses(a) -> a
        3 => {
            let (first, mid, last) = (
                *rule.first().unwrap(),
                *rule.get(1).unwrap(),
                *rule.last().unwrap()
            );
            match (first, mid, last) {
                (Token::Op(_), Token::Rl(_), Token::Op(_)) => {
                    // with_priority_parentheses(< a >) -> < a >
                    new_rule.push(first);
                    new_rule.push(mid);
                    new_rule.push(last);
                },
                (Token::Rl(_), Token::Op(op), Token::Rl(_)) => match op {
                    // with_priority_parentheses(a . b) -> ( a . b )
                    Operator::Alternation | Operator::Concatenation | Operator::Exception => {
                        new_rule.push(&Token::Op(Operator::GroupingL));
                        new_rule.push(first);
                        new_rule.push(mid);
                        new_rule.push(last);
                        new_rule.push(&Token::Op(Operator::GroupingR));
                    },
                    _ => rule.push(&Token::Invalid),
                },
                _ => (),
            }
        },
        5 => {
            let (first, second, third, fourth, last) = (
                *rule.get(0).unwrap(),
                *rule.get(1).unwrap(),
                *rule.get(2).unwrap(),
                *rule.get(3).unwrap(),
                *rule.get(4).unwrap()
            );
            match (first, third, last) {
                (Token::Op(_), Token::Op(_), Token::Op(_)) => {
                    // with_priority_parentheses(< a . b >) -> < a . b >
                    new_rule.append(&mut vec![first, second, third, fourth, last]);
                },
                (Token::Rl(_), Token::Rl(_), Token::Rl(_)) => {
                    // with_priority_parentheses(a . b * c) -> ( a . b ) * c if . >> *
                    //                                         a . ( b * c ) else
                    match (second, fourth) {
                        (Token::Op(op1), Token::Op(op2)) => {
                            if has_highter_priority_to(op1, op2) {
                                new_rule.push(&Token::Op(Operator::GroupingL));
                                new_rule.append(&mut vec![first, second, third]);
                                new_rule.push(&Token::Op(Operator::GroupingR));
                                new_rule.append(&mut vec![fourth, last]);
                            } else {
                                new_rule.append(&mut vec![first, second]);
                                new_rule.push(&Token::Op(Operator::GroupingL));
                                new_rule.append(&mut vec![third, fourth, last]);
                                new_rule.push(&Token::Op(Operator::GroupingR));
                            }
                        },
                        _ => new_rule.push(&Token::Invalid),
                    }
                }
                _ => new_rule.push(&Token::Invalid),
            }
        }
        _ => if least_prior_is_unary(rule) {
            // with_priority_parentheses(< A >) -> < with_priority_parentheses(A) >
            let mut sub_rule: Vec<&Token> = Vec::new();
            new_rule.push(*rule.first().unwrap());
            for i in 1..rule.len()-1 {
                sub_rule.push(*rule.get(i).unwrap());
            }
            for el in with_priority_parentheses(&mut sub_rule) {
                new_rule.push(el);
            }
            rule.push(*rule.last().unwrap());
        } else {
            let idx1 = get_least_prior_binary_index(rule);
            match idx1 {
                Some(i) => {
                    let op1 = *rule.get(i).unwrap();
                    let (mut left, mut right) = split_rule_by_index(rule, i);
                    let idx2 = get_least_prior_binary_index(&right);
                    match idx2 {
                        Some(j) => {
                            // with_priority_parentheses(A . B * C) -> with_priority_parentheses(A . B) * with_priority_parentheses(C) if . >> *
                            //                                         with_priority_parentheses(A) . with_priority_parentheses(B * C)
                            let op2 = *rule.get(j).unwrap();
                            let (mut mid, mut end) = split_rule_by_index(&right, i);
                            match (op1, op2) {
                                (Token::Op(o1), Token::Op(o2)) => if has_highter_priority_to(o1, o2) {
                                    new_rule.push(&Token::Op(Operator::GroupingL));
                                    for el in with_priority_parentheses(&mut left) {
                                        new_rule.push(el);
                                    }
                                    new_rule.push(op1);
                                    for el in with_priority_parentheses(&mut mid) {
                                        new_rule.push(el);
                                    }
                                    new_rule.push(&Token::Op(Operator::GroupingR));
                                    new_rule.push(op2);
                                    for el in with_priority_parentheses(&mut end) {
                                        new_rule.push(el);
                                    }

                                } else {
                                    for el in with_priority_parentheses(&mut left) {
                                        new_rule.push(el);
                                    }
                                    new_rule.push(op1);
                                    new_rule.push(&Token::Op(Operator::GroupingL));
                                    for el in with_priority_parentheses(&mut mid) {
                                        new_rule.push(el);
                                    }
                                    new_rule.push(op2);
                                    for el in with_priority_parentheses(&mut end) {
                                        new_rule.push(el);
                                    }
                                    new_rule.push(&Token::Op(Operator::GroupingR));
                                },
                                _ => new_rule.push(&Token::Invalid),
                            }
                        },
                        None => {
                            // with_priority_parentheses(A . B) -> with_priority_parentheses(A) . with_priority_parentheses(B)
                            new_rule.append(&mut with_priority_parentheses(&mut left));
                            new_rule.push(op1);
                            new_rule.append(&mut with_priority_parentheses(&mut right));
                        }
                    }

                },
                None => new_rule.push(&Token::Invalid),
            }
        },

    }
    
    new_rule
}

pub fn split_rule_by_index<'a, 'b, 'c>(rule: &Vec<&'b Token<'c>>, split_pos: usize) -> (Vec<&'b Token<'c>>, Vec<&'b Token<'c>>) {
    let mut left_part: Vec<&Token> = Vec::new();
    let mut right_part: Vec<&Token> = Vec::new();
    let rl_size = rule.len();
    let split_pos_cp = split_pos.clone();
    for i in 0..split_pos {
        left_part.push(rule.get(i).unwrap());
    }
    for i in split_pos_cp+1..rl_size {
        right_part.push(rule.get(i).unwrap());
    }
    (left_part, right_part)
}

pub fn copy_rule<'a, 'b>(rule: &'a Vec<&'b Token<'b>>) -> Vec<&'b Token<'b>> {
    let mut new_rule: Vec<&'b Token<'b>> = Vec::new();
    for tk in rule {
        new_rule.push(tk);
    }
    new_rule
}

pub fn get_rule_without_first_last<'a>(rule: Vec<&'a Token<'a>>) -> Vec<&'a Token<'a>>{
    let mut new_rule: Vec<&Token> = Vec::new();
    for i in 1..rule.len()-1 {
        new_rule.push(rule.get(i).unwrap());
    }
    return new_rule;
}

pub fn get_least_prior_binary_index<'a, 'b>(rule: &'a Vec<&'b Token<'b>>) -> Option<usize> {
    if rule.len() < 3 {
        return None;
    }
    if rule.len() == 3 {
        return Some(1);
    }
    match rule.first().unwrap() {
        Token::Op(_) => (),
        Token::Rl(_) => return Some(1),
        Token::Invalid => return None,
    }
    let mut test_stack: Vec<&'b Token<'b>> = Vec::new();
    test_stack.push(rule.first().unwrap());
    let mut i = 1;
    while !test_stack.is_empty() && i < rule.len()-1 {
        match (test_stack.last().unwrap(), rule.get(i).unwrap()) {
            (Token::Op(last_of_stack), Token::Op(current)) => {
                match (last_of_stack, current) {
                    (Operator::RepetitionL, Operator::RepetitionR) |
                    (Operator::OptionalL, Operator::OptionalR) |
                    (Operator::GroupingL, Operator::GroupingR) => { test_stack.pop(); },
                    (_, Operator::Alternation) | (_, Operator::Concatenation) | (_, Operator::Exception) => (),
                    _ => test_stack.push(rule.get(i).unwrap()),
                }
            }
            _ => ()
        }
        i += 1;
    }
    for j in i..rule.len()-1 {
        match rule.get(j).unwrap() {
            Token::Op(op) => match op {
                Operator::Alternation | Operator::Concatenation | Operator::Exception => return Some(j),
                _ => (),
            }
            _ => ()
        }
    }
    return None;
}

pub fn tokens_as_ref<'a>(rule: &'a Vec<Token<'a>>) -> Vec<&'a Token<'a>> {
    let mut tokens: Vec<&'a Token<'a>> = Vec::new();
    for tk in rule {
        tokens.push(tk);
    }
    tokens
}

pub fn has_highter_priority_to(op1: &Operator, op2: &Operator) -> bool {
    match (op1, op2) {
        (&Operator::Exception, &Operator::Exception) => false,
        (&Operator::Exception, _) => true,
        (&Operator::Concatenation, &Operator::Concatenation) => false,
        (&Operator::Concatenation, _) => true,
        (&Operator::Alternation, &Operator::Alternation) => false,
        (&Operator::Alternation, _) => true,
        _ => false,
    }
}

pub fn least_prior_is_unary<'a, 'b>(rule: &'a Vec<&'b Token<'b>>) -> bool {
    // We borrow the rule without the "extremities"
    let mut test_vec: Vec<&Token> = Vec::new();
    for i in 1..rule.len()-1 {
        test_vec.push(rule.get(i).unwrap());
    }
    return valid_dual_ref_operators(&test_vec);
}

pub fn valid_dual_ref_operators(rule: &Vec<&Token>) -> bool {
    let mut operators_list: Vec<&Operator> = Vec::new();
    for token in rule {
        match token {
            Token::Op(op) => match op {
                Operator::OptionalL => operators_list.push(op),
                Operator::OptionalR => operators_list.push(op),
                Operator::RepetitionL => operators_list.push(op),
                Operator::RepetitionR => operators_list.push(op),
                Operator::GroupingL => operators_list.push(op),
                Operator::GroupingR => operators_list.push(op),
                _ => ()
            },
            _ => (),
        }
    }
    let mut operators_test: Vec<&Operator> = Vec::new();
    match operators_list.first() {
        Some(v) => operators_test.push(v),
        None => return true,
    }
    let size = operators_list.len();
    for i in 1..size {
        let current_op = operators_list.get(i);
        let last_test_op = operators_test.last();
        match (last_test_op, current_op) {
            (Some(op1), Some(op2)) => {
                if brackets_paired(op1, op2) {
                    operators_test.pop();
                } else {
                    let o = *op1;
                    operators_test.push(o);
                }
            },
            (Some(op1), None) => {
                let o = *op1;
                operators_test.push(o)
            },
            (None, _) => return operators_test.len() == 0,
        }
    }
operators_test.len() == 0
}