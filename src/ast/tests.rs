use std::ops::Deref;
use std::rc::Rc;

use crate::{
    utils::*,
    ebnf_syntax::*,
    pre_treatment::{
        tokenize_rule_from_str,
        tokens_as_ref,
        rules_equals,
        tokenize_rule
    },
};
use super::{tokens::*, rule::*};

#[test]
fn rules_equals_test() {
    let tokens_0: Vec<Token> = tokenize_rule_from_str(String::from("abcd"));
    let tokens_1: Vec<Token> = tokenize_rule_from_str(String::from("abcd"));
    let tokens_2: Vec<Token> = tokenize_rule_from_str(String::from("{ abcd | ok }"));
    let tokens_3: Vec<Token> = tokenize_rule_from_str(String::from("{ abcd | err }"));
    let tokens_4: Vec<Token> = tokenize_rule_from_str(String::from("abcd|(efg)"));
    let tokens_5: Vec<Token> = tokenize_rule_from_str(String::from("abcd|(efg)"));
    let tokens_6: Vec<Token> = tokenize_rule_from_str(String::from("ab,ok,cd"));
    let tokens_7: Vec<Token> = tokenize_rule_from_str(String::from("ab,ok,cd"));

    let t0 = tokens_as_ref(&tokens_0);
    let t1 = tokens_as_ref(&tokens_1);
    let t2 = tokens_as_ref(&tokens_2);
    let t3 = tokens_as_ref(&tokens_3);
    let t4 = tokens_as_ref(&tokens_4);
    let t5 = tokens_as_ref(&tokens_5);
    let t6 = tokens_as_ref(&tokens_6);
    let t7 = tokens_as_ref(&tokens_7);

    assert!(rules_equals(&t0, &t1));
    assert!(!rules_equals(&t2, &t3));
    assert!(rules_equals(&t4, &t5));
    assert!(rules_equals(&t6, &t7));
}

#[test]
fn has_highter_priority_to_test() {
    let tokens_0: Vec<Token> = tokenize_rule(vec![String::from("{"), String::from("123"), String::from("}")]);
    let tokens_1: Vec<Token> = tokenize_rule(vec![String::from("a"), String::from("|"), String::from("b"), String::from("|"), String::from("c")]);
    let tokens_2: Vec<Token> = tokenize_rule(vec![String::from("a"), String::from(","), String::from("b"), String::from("|"), String::from("c")]);

    match (tokens_0.first().unwrap(), tokens_0.last().unwrap()) {
        (Token::Op(op1), Token::Op(op2)) => assert!(!has_highter_priority_to(op1, op2)),
        _ => assert!(false),
    }

    match (tokens_1.get(1).unwrap(), tokens_1.get(3).unwrap()) {
        (Token::Op(op1), Token::Op(op2)) => assert!(!has_highter_priority_to(op1, op2)),
        _ => assert!(false),
    }
    
    match (tokens_2.get(1).unwrap(), tokens_2.get(3).unwrap()) {
        (Token::Op(op1), Token::Op(op2)) => assert!(has_highter_priority_to(op1, op2)),
        _ => assert!(false),
    }
}

#[test]
fn get_least_prior_binary_index_test() {
    let tokens_0: Vec<Token> = tokenize_rule_from_str(String::from("abc"));
    let tokens_1: Vec<Token> = tokenize_rule_from_str(String::from("{abc}"));
    let tokens_2: Vec<Token> = tokenize_rule_from_str(String::from("abc|123"));
    let tokens_3: Vec<Token> = tokenize_rule_from_str(String::from("{abc,zzz},123"));
    let tokens_4: Vec<Token> = tokenize_rule_from_str(String::from("a|b|c"));
    let tokens_5: Vec<Token> = tokenize_rule_from_str(String::from("a,b|c"));
    let tokens_6: Vec<Token> = tokenize_rule_from_str(String::from("a,b-c|d"));
    let tokens_7: Vec<Token> = tokenize_rule_from_str(String::from("(a,b)-(c,d)|(e,f)"));
    
    let tokens_ref_0: Vec<&Token> = tokens_as_ref(&tokens_0);
    let tokens_ref_1: Vec<&Token> = tokens_as_ref(&tokens_1);
    let tokens_ref_2: Vec<&Token> = tokens_as_ref(&tokens_2);
    let tokens_ref_3: Vec<&Token> = tokens_as_ref(&tokens_3);
    let tokens_ref_4: Vec<&Token> = tokens_as_ref(&tokens_4);
    let tokens_ref_5: Vec<&Token> = tokens_as_ref(&tokens_5);
    let tokens_ref_6: Vec<&Token> = tokens_as_ref(&tokens_6);
    let tokens_ref_7: Vec<&Token> = tokens_as_ref(&tokens_7);

    let index_0 = get_least_prior_binary_index(&tokens_ref_0);
    let index_1 = get_least_prior_binary_index(&tokens_ref_1);
    let index_2 = get_least_prior_binary_index(&tokens_ref_2);
    let index_3 = get_least_prior_binary_index(&tokens_ref_3);
    let index_4 = get_least_prior_binary_index(&tokens_ref_4);
    let index_5 = get_least_prior_binary_index(&tokens_ref_5);
    let index_6= get_least_prior_binary_index(&tokens_ref_6);
    let index_7= get_least_prior_binary_index(&tokens_ref_7);

    assert_eq!(index_0, None);
    assert_eq!(index_1, None);
    assert_eq!(index_2, Some(1));
    assert_eq!(index_3, Some(5));
    assert_eq!(index_4, Some(1));
    assert_eq!(index_5, Some(3));
    assert_eq!(index_6, Some(5));
    assert_eq!(index_7, Some(11));
}


#[test]
fn with_priority_parentheses_test() {
    let tokens_0: Vec<Token> = tokenize_rule_from_str(String::from("abc"));
    let tokens_1: Vec<Token> = tokenize_rule_from_str(String::from("abc | 123"));
    let tokens_2: Vec<Token> = tokenize_rule_from_str(String::from("[abc|def]"));
    let tokens_3: Vec<Token> = tokenize_rule_from_str(String::from("a,b|c"));
    let tokens_4: Vec<Token> = tokenize_rule_from_str(String::from("a|b,c"));
    let tokens_5: Vec<Token> = tokenize_rule_from_str(String::from("{a-b|c}"));
    let tokens_6: Vec<Token> = tokenize_rule_from_str(String::from("(a|b)-c"));
    let tokens_7: Vec<Token> = tokenize_rule_from_str(String::from("a|b|c"));
    let tokens_8: Vec<Token> = tokenize_rule_from_str(String::from("a|b|c|d"));
    let tokens_9: Vec<Token> = tokenize_rule_from_str(String::from("a|b,c|d"));
    let tokens_10: Vec<Token> = tokenize_rule_from_str(String::from("{a,b}|[c-d]"));
    
    let tokens_ref_0: Vec<&Token> = tokens_as_ref(&tokens_0);
    let tokens_ref_1: Vec<&Token> = tokens_as_ref(&tokens_1);
    let tokens_ref_2: Vec<&Token> = tokens_as_ref(&tokens_2);
    let tokens_ref_3: Vec<&Token> = tokens_as_ref(&tokens_3);
    let tokens_ref_4: Vec<&Token> = tokens_as_ref(&tokens_4);
    let tokens_ref_5: Vec<&Token> = tokens_as_ref(&tokens_5);
    let tokens_ref_6: Vec<&Token> = tokens_as_ref(&tokens_6);
    let tokens_ref_7: Vec<&Token> = tokens_as_ref(&tokens_7);
    let tokens_ref_8: Vec<&Token> = tokens_as_ref(&tokens_8);
    let tokens_ref_9: Vec<&Token> = tokens_as_ref(&tokens_9);
    let tokens_ref_10: Vec<&Token> = tokens_as_ref(&tokens_10);

    let t0 = with_priority_parentheses( tokens_ref_0);
    let t1 = with_priority_parentheses( tokens_ref_1);
    let t2 = with_priority_parentheses(tokens_ref_2);
    let t3 = with_priority_parentheses(tokens_ref_3);
    let t4 = with_priority_parentheses( tokens_ref_4);
    let t5 = with_priority_parentheses(tokens_ref_5);
    let t6 = with_priority_parentheses( tokens_ref_6);
    let t7 = with_priority_parentheses( tokens_ref_7);
    let t8 = with_priority_parentheses( tokens_ref_8);
    let t9 = with_priority_parentheses( tokens_ref_9);
    let t10 = with_priority_parentheses( tokens_ref_10);

    let expected_0: Vec<Token> = tokenize_rule_from_str(String::from("abc"));
    let expected_1: Vec<Token> = tokenize_rule_from_str(String::from("(abc | 123)"));
    let expected_2: Vec<Token> = tokenize_rule_from_str(String::from("[abc|def]"));
    let expected_3: Vec<Token> = tokenize_rule_from_str(String::from("(a,b)|c"));
    let expected_4: Vec<Token> = tokenize_rule_from_str(String::from("a|(b,c)"));
    let expected_5: Vec<Token> = tokenize_rule_from_str(String::from("{(a-b)|c}"));
    let expected_6: Vec<Token> = tokenize_rule_from_str(String::from("(a|b)-(c)"));
    let expected_7: Vec<Token> = tokenize_rule_from_str(String::from("a|(b|c)"));
    let expected_8: Vec<Token> = tokenize_rule_from_str(String::from("a|(b|(c|d))"));
    let expected_9: Vec<Token> = tokenize_rule_from_str(String::from("a|((b,c)|d)"));
    let expected_10: Vec<Token> = tokenize_rule_from_str(String::from("{a,b}|([c-d])"));
    
    let tokens_exp_ref_0 = tokens_as_ref(&expected_0);
    let tokens_exp_ref_1 = tokens_as_ref(&expected_1);
    let tokens_exp_ref_2 = tokens_as_ref(&expected_2);
    let tokens_exp_ref_3 = tokens_as_ref(&expected_3);
    let tokens_exp_ref_4 = tokens_as_ref(&expected_4);
    let tokens_exp_ref_5 = tokens_as_ref(&expected_5);
    let tokens_exp_ref_6 = tokens_as_ref(&expected_6);
    let tokens_exp_ref_7 = tokens_as_ref(&expected_7);
    let tokens_exp_ref_8 = tokens_as_ref(&expected_8);
    let tokens_exp_ref_9 = tokens_as_ref(&expected_9);
    let tokens_exp_ref_10 = tokens_as_ref(&expected_10);

    assert!(rules_equals(&t0, &tokens_exp_ref_0));
    assert!(rules_equals(&t1, &tokens_exp_ref_1));
    assert!(rules_equals(&t2, &tokens_exp_ref_2));
    assert!(rules_equals(&t3, &tokens_exp_ref_3));
    assert!(rules_equals(&t4, &tokens_exp_ref_4));
    assert!(rules_equals(&t5, &tokens_exp_ref_5));
    assert!(rules_equals(&t6, &tokens_exp_ref_6));
    assert!(rules_equals(&t7, &tokens_exp_ref_7));
    assert!(rules_equals(&t8, &tokens_exp_ref_8));
    assert!(rules_equals(&t9, &tokens_exp_ref_9));
    assert!(rules_equals(&t10, &tokens_exp_ref_10));
}

#[test]
fn create_rule_tree_test() {
    let tokens_0 = tokenize_rule_from_str(String::from("ok"));
    let tokens_1 = tokenize_rule_from_str(String::from("{ yes }"));
    let tokens_2 = tokenize_rule_from_str(String::from("'abc' | 'efg'"));
    let tokens_3 = tokenize_rule_from_str(String::from("[ function | method ]"));
    let tokens_4 = tokenize_rule_from_str(String::from("(foo|bar)-var"));
    let tokens_5 = tokenize_rule_from_str(String::from("'a' | 'b' | 'c'"));

    let tree_0 = create_rule_tree(&tokens_0);
    let tree_1 = create_rule_tree(&tokens_1);
    let tree_2 = create_rule_tree(&tokens_2);
    let tree_3 = create_rule_tree(&tokens_3);
    let tree_4 = create_rule_tree(&tokens_4);
    let tree_5 = create_rule_tree(&tokens_5);
    
    match tree_0 {
        Rule::Atomic(_, AtomicKind::Identifier) => assert!(true),
        _ => assert!(false),
    }
    match tree_1 {
        Rule::Single(sub, SingleKind::Repetition) => match sub.deref() {
            Rule::Ref(r) => match r.upgrade() {
                Some(st) => if let Rule::Atomic(id, AtomicKind::Identifier) = st.deref() {
                    assert_eq!(id, &String::from("yes"));
                },
                None => assert!(false),
            },
            _ => assert!(false),
        }
        _ => assert!(false),
    }
    match tree_2 {
        Rule::Single(_, SingleKind::Grouping) => assert!(true),
        _ => assert!(false),
    }
    match tree_3 {
        Rule::Single(el, SingleKind::Optional) => match el.deref() {
            Rule::Dual(left, DualKind::Alternation, right) => match (left.deref(), right.deref()) {
                (Rule::Ref(_), Rule::Ref(_)) => assert!(true),
                _ => assert!(false)
            },
            _ => assert!(false)
        },
        _ => assert!(false)
    }

    match tree_4 {
        Rule::Dual(a, DualKind::Exception, b) => match (a.deref(), b.deref()) {
            (Rule::Single(sub1, SingleKind::Grouping), Rule::Single(sub2, SingleKind::Grouping)) => match (sub1.deref(), sub2.deref()) {
                (Rule::Dual(left, DualKind::Alternation, right), Rule::Ref(_)) => match (left.deref(), right.deref()) {
                    (Rule::Ref(_), Rule::Ref(_)) => assert!(true),
                    _ => assert!(false),
                },
                _ => assert!(false),
            },
            _ => assert!(false),
        },
        _ => assert!(false),
    }

    match tree_5 {
        Rule::Dual(left, DualKind::Alternation, other) => match (left.deref(), other.deref()) {
        (Rule::Atomic(_, AtomicKind::Literal), Rule::Single(right, SingleKind::Grouping)) => match right.deref() {
            Rule::Dual(_, DualKind::Alternation, _) => assert!(true),
            _ => assert!(false),
        },
        _ => assert!(false),
        },
        _ => assert!(false),
    }

}

#[test]
fn rule_without_grouping_test() {
    let tokens_0 = tokenize_rule_from_str(String::from("ok"));
    let tokens_1 = tokenize_rule_from_str(String::from("{yes}"));
    let tokens_2 = tokenize_rule_from_str(String::from("'abc' | 'def'"));
    let tokens_3 = tokenize_rule_from_str(String::from("( 'abc' | 'def' ) , '1234'"));

    let tree_0 = create_rule_tree(&tokens_0);
    let tree_1 = create_rule_tree(&tokens_1);
    let tree_2 = create_rule_tree(&tokens_2);
    let tree_3 = create_rule_tree(&tokens_3);

    let t0 = tree_without_grouping(Rc::new(tree_0));
    let t1 = tree_without_grouping(Rc::new(tree_1));
    let t2 = tree_without_grouping(Rc::new(tree_2));
    let t3 = tree_without_grouping(Rc::new(tree_3));
    
    match t0.deref() {
        Rule::Atomic(id, AtomicKind::Identifier) => assert_eq!(id, &String::from("ok")),
        _ => assert!(false), 
    }

    match t1.deref() {
        Rule::Single(_, SingleKind::Repetition) => assert!(true),
        _ => assert!(false),
    }
    match t2.deref() {
        Rule::Dual(left, DualKind::Alternation, right) => match (left.deref(), right.deref()) {
            _ => assert!(true),
        },
        _ => assert!(false)
    }

    match t3.deref() {
        Rule::Dual(left, DualKind::Concatenation, right) => match (left.deref(), right.deref()) {
            (Rule::Dual(a, DualKind::Alternation, b), Rule::Ref(_)) => match (a.deref(), b.deref()) {
                (Rule::Ref(_), Rule::Ref(_)) => assert!(true),
                _ => assert!(false),
            },
            _ => assert!(false),
        },
        _ => assert!(false),
    }
}

#[test]
fn are_same_tree_test() {
    let tokens_0 = tokenize_rule_from_str(String::from("ok"));
    let tokens_1 = tokenize_rule_from_str(String::from("{ yes }"));
    let tokens_2 = tokenize_rule_from_str(String::from("('abc'|'def')"));
    let tokens_3 = tokenize_rule_from_str(String::from("(foo|bar)-var"));

    let tokens_a = tokenize_rule_from_str(String::from("ok"));
    let tokens_b = tokenize_rule_from_str(String::from("{ yes }"));
    let tokens_c = tokenize_rule_from_str(String::from("('abc'|'def')"));
    let tokens_d = tokenize_rule_from_str(String::from("(foo|bar)-var"));
    
    let tree_0 = create_rule_tree(&tokens_0);
    let tree_1 = create_rule_tree(&tokens_1);
    let tree_2 = create_rule_tree(&tokens_2);
    let tree_3 = create_rule_tree(&tokens_3);

    let tree_a = create_rule_tree(&tokens_a);
    let tree_b = create_rule_tree(&tokens_b);
    let tree_c = create_rule_tree(&tokens_c);
    let tree_d = create_rule_tree(&tokens_d);

    assert!(are_same_tree(&tree_0, &tree_a, true, false));
    assert!(are_same_tree(&tree_1, &tree_b, true, false));
    assert!(are_same_tree(&tree_2, &tree_c, true, false));
    assert!(are_same_tree(&tree_3, &tree_d, true, false));

}

#[test]
pub fn predicate_single_result_test() {
    let tokens = tokenize_rule_from_str(String::from("( 'abc' | 'def' )"));
    let tree = get_pure_tree(create_definition_tree(&tokens));
    let pr1 = |v: &Rule, res: bool| -> bool {
        match v.deref() {
            Rule::Dual(_, DualKind::Alternation, _) => true,
            _ => res
        }
    };

    let pr2 = |v: &Rule, res: bool| -> bool {
        match v.deref() {
            Rule::Atomic(_, AtomicKind::Identifier) => false,
            _ => res
        }
    };

    // contains at least one alternation
    let res_1 = predicate_single_result(
        &tree,
        &|_| false,
        &|_| false,
        &pr1,
        &|v, a,b| pr1(v, a) || pr1(v, b)

    );
    // Doesn't contain any identifier
    let res_2 = predicate_single_result(
        &tree,
        &|v| if let Rule::Atomic(_, AtomicKind::Identifier) = v { false } else { true },
        &|_| true,
        &pr2,
        &|v, a, b| pr2(v, a) && pr2(v, b)
    );

    assert!(res_1);
    assert!(res_2);
}

#[test]
pub fn counting_single_result_test() {
    let tokens = tokenize_rule_from_str(String::from("(ok - abc | 'def' ,  ok)"));
    let tree = get_pure_tree(create_definition_tree(&tokens));
    let check_if_id = |atom: &Rule| {
        if let Rule::Atomic(id, AtomicKind::Identifier) = atom.deref() {
            AssocRuleCounter::from(vec![(id.to_string(), 1)])
        } else {
            AssocRuleCounter::from(vec![])
        }
    };
    let res = counting_single_result(
        &tree, &check_if_id,
        &|_| AssocRuleCounter::from(vec![]),
        &|_, cnt| cnt,
            &|_, cnt1, cnt2| cnt1 + cnt2
    );
    println!("{:?}", res);
    let expected_res = AssocRuleCounter::from(vec![
        ("ok".to_string(), 2), 
        ("abc".to_string(), 1), 
    ]);
    assert_eq!(res, expected_res);
}

#[test]
pub fn tree_with_id_ref_test() {
    let name = "integer".to_string();
    let tree_from = get_pure_tree(create_definition_tree(&tokenize_rule_from_str("0|1|2".to_string())));
    let tree = get_pure_tree(create_definition_tree(&tokenize_rule_from_str(String::from("integer,'.',integer"))));
    let ref_to = Rc::new(Rule::Ref(Rc::downgrade(&tree_from)));
    let tree_res = tree_with_id_ref((&name, &tree_from), &tree);
    let tree_expected = Rc::new(
        Rule::Dual(
            Rc::clone(&ref_to),
            DualKind::Concatenation,
            Rc::new(
                Rule::Dual(
                    Rc::new(Rule::Atomic(".".to_string(), AtomicKind::Literal)),
                    DualKind::Concatenation,
                    Rc::clone(&ref_to)
                )
            )
        )
    );
    assert!(are_same_tree(&tree_res, &tree_expected, true, true));
}


#[test]
fn grammarize_repetition_test() {
    let rule_0 = 
        create_definition_tree(
            &tokenize_rule_from_str(String::from("{ ok }"))
        )
    ;
    let rule_a = 
        create_definition_tree(
            &tokenize_rule_from_str(String::from("ok | ok, same"))
        )
    ;
    let gram_0 = grammarize_repetition(&rule_0);
    println!("{:?}", gram_0);
    println!("{:?}", rule_a);
    let res = are_same_tree(&gram_0, &rule_a, false, false);
    assert!(res);
}

#[test]
fn grammarize_exception_test() {
    let rule_0 =
        get_pure_tree(create_definition_tree(
            &tokenize_rule_from_str(String::from("'abcd' - 'bc'"))
        ))
    ;
    let rule_1 =
        get_pure_tree(create_definition_tree(
            &tokenize_rule_from_str(String::from("'hdedldldod' - 'd'"))
        ))
    ;

    let rule_2 =
        get_pure_tree(create_definition_tree(
            &tokenize_rule_from_str(String::from("'abcd' - ('a' | 'c')"))
        ))
    ;

    let rule_3 =
        get_pure_tree(create_definition_tree(
            &tokenize_rule_from_str(String::from("('abcd' , 'ef') - ('a' | 'c')"))
        ))
    ;
    
    let rule_a =
        get_pure_tree(create_definition_tree(
            &tokenize_rule_from_str(String::from("'ad'"))
        ))
    ;
    let rule_b =
        get_pure_tree(create_definition_tree(
            &tokenize_rule_from_str(String::from("'hello'"))
        ))
    ;

    let rule_c =
        get_pure_tree(create_definition_tree(
            &tokenize_rule_from_str(String::from("'bcd'|'abd'"))
        ))
    ;

    let rule_d =
        get_pure_tree(create_definition_tree(
            &tokenize_rule_from_str(String::from("'bcdef'|'abdef'"))
        ))
    ;
    
    let gram_0 = grammarize_exception(&rule_0);
    let gram_1 = grammarize_exception(&rule_1);
    let gram_2 = grammarize_exception(&rule_2);
    let gram_3 = grammarize_exception(&rule_3);
    println!("{:?}", rule_0);
    println!("{:?}\n", gram_0);
    println!("{:?}", rule_1);
    println!("{:?}\n", gram_1);
    println!("{:?}", rule_2);
    println!("{:?}\n", gram_2);
    println!("{:?}", rule_3);
    println!("{:?}", gram_3);

    let res_0 = are_same_tree(&gram_0, &rule_a, false, false);
    let res_1 = are_same_tree(&gram_1, &rule_b, false, false);
    let res_2 = are_same_tree(&gram_2, &rule_c, true, false);
    let res_3 = are_same_tree(&gram_3, &rule_d, true, false);
    assert!(res_0);
    assert!(res_1);
    assert!(res_2);
    assert!(res_3);
}


#[test]
fn grammarize_optional_test() {
    let rule_0 =
        get_pure_tree(create_definition_tree(
            &tokenize_rule_from_str(String::from("ok , [fine]"))
        ))
    ;
    let rule_1 =
        get_pure_tree(create_definition_tree(
            &tokenize_rule_from_str(String::from("[yes] - [no]"))
        ))
    ;
    let rule_2 =
        get_pure_tree(create_definition_tree(
            &tokenize_rule_from_str(String::from("[ yes ] - no"))
        ))
    ;

    let rule_a =
    get_pure_tree(create_definition_tree(
        &tokenize_rule_from_str(String::from("ok | (ok , fine)"))
    ))
    ;
    let rule_b =
    get_pure_tree(create_definition_tree(
        &tokenize_rule_from_str(String::from("yes | no | (yes - no)"))
    ))
    ;  
    let rule_c =
    get_pure_tree(create_definition_tree(
        &tokenize_rule_from_str(String::from(" '' | (yes - no)"))
    ))
    ;

    let gram_0 = grammarize_optional(&rule_0);
    let gram_1 = grammarize_optional(&rule_1);
    let gram_2 = grammarize_optional(&rule_2);
    println!("{:?}", gram_0);
    println!("{:?}", gram_1);
    println!("{:?}", gram_2);

    let res_0 = are_same_tree(&gram_0, &rule_a, false, false);
    let res_1 = are_same_tree(&gram_1, &rule_b, false, false);
    let res_2 = are_same_tree(&gram_2, &rule_c, false, false);
    assert!(res_0);
    assert!(res_1);
    assert!(res_2);
}