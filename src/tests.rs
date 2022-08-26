use crate::{pre_teatment::*, ebnf_syntax::{Token, Operator}};

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