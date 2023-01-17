use std::{ops::Deref, rc::Rc};

use crate::{pre_treatment::*, ebnf_syntax::{Token, Operator, Rule}, ast::{*, predicate_single_result}};

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

        match lit1_token {
            Token::Rl(rl) => match rl.deref() {
                Rule::Literal(lit) => assert_eq!(lit, &String::from("a")),
                _ => assert!(false),
            },
            _ => assert!(false),
        }

        match lit2_token {
            Token::Rl(rl) => match rl.deref() {
                Rule::Literal(lit) => assert_eq!(lit, &String::from("a")),
                _ => assert!(false),
            },
            _ => assert!(false),
        }

        match var_token {
            Token::Rl(rl) => match rl.deref() {
                Rule::Identifier(idt) => assert_eq!(idt, &String::from("var")),
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
    fn least_prior_is_unary_test() {
        let tokens_0: Vec<Token> = tokenize_rule_from_str(String::from("{ 123 }"));
        let tokens_1: Vec<Token> = tokenize_rule_from_str(String::from("{a,b|c}"));
        let tokens_2: Vec<Token> = tokenize_rule_from_str(String::from("{a,b}|c"));
        let tokens_3: Vec<Token> = tokenize_rule_from_str(String::from("a,{b|c}"));
        
        assert!(least_prior_is_unary(&tokens_as_ref(&tokens_0)));
        assert!(least_prior_is_unary(&tokens_as_ref(&tokens_1)));
        assert!(least_prior_is_unary(&tokens_as_ref(&tokens_2)) == false);
        assert!(least_prior_is_unary(&tokens_as_ref(&tokens_3)) == false);
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
            Rule::Identifier(_) => assert!(true),
            _ => assert!(false),
        }
        match tree_1 {
            Rule::Repetition(sub) => match sub.deref() {
                Rule::Ref(r) => match r.upgrade() {
                    Some(st) => if let Rule::Identifier(id) = st.deref() {
                        assert_eq!(id, &String::from("yes"));
                    },
                    None => assert!(false),
                },
                _ => assert!(false),
            }
            _ => assert!(false),
        }
        match tree_2 {
            Rule::Grouping(_) => assert!(true),
            _ => assert!(false),
        }
        match tree_3 {
            Rule::Optional(el) => match el.deref() {
                Rule::Alternation(left, right) => match (left.deref(), right.deref()) {
                    (Rule::Ref(_), Rule::Ref(_)) => assert!(true),
                    _ => assert!(false)
                },
                _ => assert!(false)
            },
            _ => assert!(false)
        }

        match tree_4 {
            Rule::Exception(a, b) => match (a.deref(), b.deref()) {
                (Rule::Grouping(sub1), Rule::Grouping(sub2)) => match (sub1.deref(), sub2.deref()) {
                    (Rule::Alternation(left, right), Rule::Ref(_)) => match (left.deref(), right.deref()) {
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
           Rule::Alternation(left, other) => match (left.deref(), other.deref()) {
            (Rule::Literal(_), Rule::Grouping(right)) => match right.deref() {
                Rule::Alternation(_, _) => assert!(true),
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
            Rule::Identifier(id) => assert_eq!(id, &String::from("ok")),
            _ => assert!(false), 
        }

        match t1.deref() {
            Rule::Repetition(id) => assert!(true),
            _ => assert!(false),
        }
        match t2.deref() {
            Rule::Alternation(left, right) => match (left.deref(), right.deref()) {
                _ => assert!(true),
            },
            _ => assert!(false)
        }

        match t3.deref() {
            Rule::Concatenation(left, right) => match (left.deref(), right.deref()) {
                (Rule::Alternation(a, b), Rule::Ref(_)) => match (a.deref(), b.deref()) {
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

        assert!(are_same_tree(&tree_0, &tree_a));
        assert!(are_same_tree(&tree_1, &tree_b));
        assert!(are_same_tree(&tree_2, &tree_c));
        assert!(are_same_tree(&tree_3, &tree_d));

    }

    #[test]
    pub fn predicate_single_result_test() {
        let tokens = tokenize_rule_from_str(String::from("( 'abc' | 'def' )"));
        let tree = get_pure_tree(create_definition_tree(&tokens));
        let pr1 = |v: &Rule, res: bool| -> bool {
            match v.deref() {
                Rule::Alternation(_, _) => true,
                _ => res
            }
        };

        let pr2 = |v: &Rule, res: bool| -> bool {
            match v.deref() {
                Rule::Identifier(_) => false,
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
            &|v| if let Rule::Identifier(_) = v { false } else { true },
            &|_| true,
            &pr2,
            &|v, a, b| pr2(v, a) && pr2(v, b)
        );

        assert!(res_1);
        assert!(res_2);
    }