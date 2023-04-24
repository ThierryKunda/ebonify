use std::ops::Deref;

use crate::ebnf_syntax::*;
use crate::parsing::pre_processing::*;
use crate::parsing::validation::*;

#[test]
fn split_lines_from_file_test() {
    let file_content = split_lines_from_file("tests_samples/separation.txt");
    let lines: Vec<String>;
    match file_content {
        Ok(v) => {
            assert!(v.len() != 0);
            lines = v;
        },
        Err(_) => return,
    }
    assert_eq!(lines, vec!["Line 1 : ABCD;", "Line 2 : EFGH;", "Line n : ...;"]);
}

#[test]
fn split_members_test() {
    let content_test = "float = integer, '.', integer;".to_string();
    let expected_res = (String::from("float"), String::from("integer, '.', integer;"));
    assert_eq!(split_members_aux(content_test), expected_res);
}

#[test]
fn tokenize_test() {
    let some_literal_1 = "'a'".to_string();
    let some_literal_2 = "\"a\"".to_string();
    let ident = "var".to_string();
    let quote_invalid = "\"mystring".to_string();
    let altern = "|".to_string();
    let opening_repet = "{".to_string();

    let lit1_token = tokenize(some_literal_1);
    let lit2_token = tokenize(some_literal_2);
    let var_token = tokenize(ident);
    let extra_quote_token = tokenize(quote_invalid);
    let altern_token = tokenize(altern);
    let op_repet_token = tokenize(opening_repet);

    match &lit1_token {
        Token::Rl(rl) => match rl.deref() {
            Rule::Atomic(lit, AtomicKind::Literal) => assert_eq!(lit, &String::from("a")),
            _ => assert!(false),
        },
        _ => assert!(false),
    }

    match &lit2_token {
        Token::Rl(rl) => match rl.deref() {
            Rule::Atomic(lit, AtomicKind::Literal) => assert_eq!(lit, &String::from("a")),
            _ => assert!(false),
        },
        _ => assert!(false),
    }

    match var_token {
        Token::Rl(rl) => match rl.deref() {
            Rule::Atomic(idt, AtomicKind::Identifier) => assert_eq!(idt, &String::from("var")),
            _ => assert!(false),
        },
        _ => assert!(false),
    }
    match extra_quote_token {
        Token::Invalid => assert!(true),
        _ => assert!(false),
    }
    match altern_token {
        Token::Op(op) => match op {
            Operator::Alternation => assert!(true),
            _ => assert!(false),
        },
        _ => assert!(false),
    }
    match op_repet_token {
        Token::Op(op) => match op {
            Operator::RepetitionL => assert!(true),
            _ => assert!(false),
        },
        _ => assert!(false),
    }
}
#[test]
fn brackets_paired_test() {
    assert!(brackets_paired(&Operator::GroupingL, &Operator::GroupingR));
    assert!(brackets_paired(&Operator::OptionalR, &Operator::OptionalL) == false);
}

#[test]
fn valid_dual_operators_test() {
    let tokens_0: Vec<Token> = Vec::new();
    let tokens_1: Vec<Token> = tokenize_rule(vec![String::from("{"), String::from("}")]);
    let tokens_2: Vec<Token> = tokenize_rule(vec![String::from("]"), String::from("[")]);
    let tokens_3: Vec<Token> = tokenize_rule(vec![String::from("("), String::from("["), String::from(")")]);
    let tokens_4: Vec<Token> = tokenize_rule(vec![String::from("{"), String::from("("), String::from(")"), String::from("}")]);

    let tokens_ref_1: Vec<&Token> = tokens_1.iter().collect();
    let tokens_ref_2: Vec<&Token> = tokens_2.iter().collect();
    let tokens_ref_3: Vec<&Token> = tokens_3.iter().collect();
    let tokens_ref_4: Vec<&Token> = tokens_4.iter().collect();

    assert!(valid_dual_operators(&tokens_0));
    assert!(valid_dual_operators(&tokens_1));
    assert!(!valid_dual_operators(&tokens_2));
    assert!(!valid_dual_operators(&tokens_3));
    assert!(valid_dual_ref_operators(&tokens_ref_1));
    assert!(!valid_dual_ref_operators(&tokens_ref_2));
    assert!(!valid_dual_ref_operators(&tokens_ref_3));
    assert!(valid_dual_ref_operators(&tokens_ref_4));
}

#[test]
fn valid_single_operators_test() {
    let tokens_0: Vec<Token> = tokenize_rule(vec![String::from("abcd")]);
    let tokens_1: Vec<Token> = tokenize_rule(vec![String::from("{"), String::from("abcd"), String::from("|"), String::from("abcd"), String::from("}")]);
    let tokens_2: Vec<Token> = tokenize_rule(vec![String::from("{"), String::from("bad"), String::from("|"), String::from("|"), String::from("}")]);
    let tokens_3: Vec<Token> = tokenize_rule(vec![String::from("{"), String::from("bad"), String::from(","), String::from("|"), String::from("}")]);

    assert!(valid_single_operators(&tokens_as_ref(&tokens_0)));
    assert!(valid_single_operators(&tokens_as_ref(&tokens_1)));
    assert!(valid_single_operators(&tokens_as_ref(&tokens_2)) == false);
    assert!(valid_single_operators(&tokens_as_ref(&tokens_3)) == false);

}
#[test]
fn valid_following_operators_test() {
    let tokens_0: Vec<Token> = tokenize_rule(vec![String::from("abcd")]);
    let tokens_1: Vec<Token> = tokenize_rule(vec![String::from("{"), String::from("abcd"), String::from("|"), String::from("abcd"), String::from("}")]);
    let tokens_2: Vec<Token> = tokenize_rule(vec![String::from("{"), String::from("|"), String::from("bad"), String::from("}")]);
    let tokens_3: Vec<Token> = tokenize_rule(vec![String::from("["), String::from("bad"), String::from("|"), String::from("]")]);

    assert!(valid_following_operators(&tokens_0));
    assert!(valid_following_operators(&tokens_1));
    assert!(valid_following_operators(&tokens_2) == false);
    assert!(valid_following_operators(&tokens_3) == false);
}

