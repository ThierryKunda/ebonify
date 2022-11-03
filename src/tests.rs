use std::ops::Deref;

use crate::{pre_teatment::*, ebnf_syntax::{Token, Operator, Rule}, ast::*};

#[test]
    fn split_lines_test() {
        let file_content = split_lines("tests_samples/separation.txt");
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
        let expected_res = vec!["float", "integer, '.', integer;"];
        assert_eq!(split_members_aux(content_test), expected_res);
    }

    #[test]
    fn tokenize_test() {
        let some_literal = "\"a\"".to_string();
        let ident = "var".to_string();
        let quote_invalid = "\"mystring".to_string();
        let altern = "|".to_string();
        let opening_repet = "{".to_string();

        let a_token = tokenize(some_literal);
        let var_token = tokenize(ident);
        let extra_quote_token = tokenize(quote_invalid);
        let altern_token = tokenize(altern);
        let op_repet_token = tokenize(opening_repet);

        match a_token {
            Token::Rl(rl) => match rl {
                crate::ebnf_syntax::Rule::Literal(lit) => assert_eq!(lit, "a".to_string()),
                _ => assert!(false),
            },
            _ => assert!(false),
        }
        match var_token {
            Token::Rl(rl) => match rl {
                crate::ebnf_syntax::Rule::Identifier(idt) => assert_eq!(idt, "var".to_string()),
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

        let tokens_ref_1: Vec<&Token> = tokens_1.iter().collect();
        let tokens_ref_2: Vec<&Token> = tokens_2.iter().collect();
        let tokens_ref_3: Vec<&Token> = tokens_3.iter().collect();
    
        assert!(valid_dual_operators(&tokens_0));
        assert!(valid_dual_operators(&tokens_1));
        assert!(!valid_dual_operators(&tokens_2));
        assert!(!valid_dual_operators(&tokens_3));
        assert!(valid_dual_ref_operators(&tokens_ref_1));
        assert!(!valid_dual_ref_operators(&tokens_ref_2));
        assert!(!valid_dual_ref_operators(&tokens_ref_3));
    }

    #[test]
    fn valid_single_operators_test() {
        let tokens_0: Vec<Token> = tokenize_rule(vec![String::from("abcd")]);
        let tokens_1: Vec<Token> = tokenize_rule(vec![String::from("{"), String::from("abcd"), String::from("|"), String::from("abcd"), String::from("}")]);
        let tokens_2: Vec<Token> = tokenize_rule(vec![String::from("{"), String::from("bad"), String::from("|"), String::from("|"), String::from("}")]);
        let tokens_3: Vec<Token> = tokenize_rule(vec![String::from("{"), String::from("bad"), String::from(","), String::from("|"), String::from("}")]);

        assert!(valid_single_operators(&tokens_0));
        assert!(valid_single_operators(&tokens_1));
        assert!(valid_single_operators(&tokens_2) == false);
        assert!(valid_single_operators(&tokens_3) == false);

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
    fn least_prior_is_unary_test() {
        let tokens_0: Vec<Token> = tokenize_rule(vec![String::from("{"), String::from("123"), String::from("}")]);
        let tokens_1: Vec<Token> = tokenize_rule(vec![String::from("{"), String::from("a"), String::from(","), String::from("b"), String::from("|"), String::from("c"), String::from("}")]);
        let tokens_2: Vec<Token> = tokenize_rule(vec![String::from("{"), String::from("a"), String::from(","), String::from("b"), String::from("}"), String::from("|"), String::from("c")]);
        let tokens_3: Vec<Token> = tokenize_rule(vec![String::from("a"), String::from(","), String::from("{"), String::from("b"), String::from("|"), String::from("c"), String::from("}")]);
        
        assert!(least_prior_is_unary(&tokens_as_ref(&tokens_0)));
        assert!(least_prior_is_unary(&tokens_as_ref(&tokens_1)));
        assert!(least_prior_is_unary(&tokens_as_ref(&tokens_2)) == false);
        assert!(least_prior_is_unary(&tokens_as_ref(&tokens_3)) == false);
    }
    
    #[test]
    fn get_least_prior_binary_index_test() {
        let tokens_0: Vec<Token> = tokenize_rule(vec![String::from("abc"), String::from("|"), String::from("123")]);
        let tokens_1: Vec<Token> = tokenize_rule(vec![String::from("{"), String::from("abcd"), String::from(","), String::from("zzz"), String::from("}"), String::from(","), String::from("123")]);
        let tokens_2: Vec<Token> = tokenize_rule(vec![String::from("a"), String::from("|"), String::from("b"), String::from("c")]);
        
        let tokens_ref_0: Vec<&Token> = tokens_as_ref(&tokens_0);
        let tokens_ref_1: Vec<&Token> = tokens_as_ref(&tokens_1);
        let tokens_ref_2: Vec<&Token> = tokens_as_ref(&tokens_2);
        
        let index_0 = get_least_prior_binary_index(&tokens_ref_0);
        let index_1 = get_least_prior_binary_index(&tokens_ref_1);
        let index_2 = get_least_prior_binary_index(&tokens_ref_2);

        assert_eq!(index_0, Some(1));
        assert_eq!(index_1, Some(5));
        assert_eq!(index_2, Some(1));
    }
    #[test]
    fn create_rule_tree_test() {
        let tokens_0 = tokenize_rule(vec![String::from("ok")]);
        let tokens_1 = tokenize_rule(vec![String::from("{"), String::from("yes"), String::from("}")]);
        let tokens_2 = tokenize_rule(vec![String::from("\"abc\""), String::from("|"), String::from("\"efg\"")]);
        let tokens_3 = tokenize_rule(vec![String::from("["), String::from("function"), String::from("|"), String::from("method"), String::from("]")]);
        let tokens_4 = tokenize_rule(vec![String::from("("), String::from("foo"), String::from("|"), String::from("bar"), String::from(")"), String::from("-"), String::from("var")]);
        let tokens_5 = tokenize_rule(vec![String::from("\"a\""), String::from("|"), String::from("\"b\""), String::from("|"), String::from("\"c\"")]);

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
            Rule::RepetRef(rl) => match rl.deref() {
                Rule::Identifier(id) => assert_eq!(id, &String::from("yes")),
                _ => assert!(false),
            },
            _ => assert!(false),
        }
        match tree_2 {
            Rule::AlterRef(left, right) => match (left.deref(), right.deref()) {
                (&Rule::Literal(lit1), &Rule::Literal(lit2)) => {
                    assert_eq!(lit1, &String::from("abc"));
                    assert_eq!(lit2, &String::from("efg"));
                },
                _ => assert!(false),
            }
            _ => assert!(false),
        }
        match tree_3 {
            Rule::Optional(el) => match el.deref() {
                Rule::AlterRef(left, right) => match (*left.deref(), *right.deref()) {
                    (Rule::Identifier(id1), Rule::Identifier(id2)) => {
                        assert_eq!(id1, &String::from("function"));
                        assert_eq!(id2, &String::from("method"));
                    },
                    _ => assert!(false),
                },
                _ => assert!(false),
            },
            _ => assert!(false),
        }

        match tree_4 {
            Rule::Exception(a, b) => match (*a,b.deref()) {
                (Rule::Grouping(grp), id) => match (*grp, id) {
                    (Rule::AlterRef(f, b), Rule::Identifier(s3)) => match (f.deref(),b.deref()) {
                        (&Rule::Identifier(s1), &Rule::Identifier(s2)) => {
                            assert_eq!(s1, &String::from("foo"));
                            assert_eq!(s2, &String::from("bar"));
                            assert_eq!(s3, &String::from("var"));
                        },
                        _ => assert!(false),
                    },
                    _ => assert!(false),
                },
                _ => assert!(false),
            },
            Rule::Identifier(s) => {
                println!("Error message : {s}");
                assert!(false);
            },
            _ => assert!(false),
        }

        match tree_5 {
           Rule::Alternation(a, other) => match (a.deref(), *other) {
            (l1, Rule::AlterRef(b, c)) => match (l1, *b, *c) {
                (Rule::Literal(lit1), Rule::Literal(lit2), Rule::Literal(lit3)) => {
                    assert_eq!(lit1, &String::from("a"));
                    assert_eq!(lit2, &String::from("b"));
                    assert_eq!(lit3, &String::from("c"));
                },
                _ => assert!(false),
            },
            _ => assert!(false),
           }
           _ => assert!(false),
        }

    }