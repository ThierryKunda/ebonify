use crate::{ast::rule::*, parsing::pre_processing::tokenize_rule_from_str};
use super::builders::EbnfTreeBuilder;

#[test]
fn tree_from_file_test() {
    let tree = EbnfTreeBuilder::from_file("tests_samples/lang_mini.ebnf").unwrap();
    let char_rule = get_pure_tree(
        create_definition_tree(
            &tokenize_rule_from_str(String::from("'a'|'b'|'c'|'e'|'f'"))
        ).unwrap()
    );
    let digit_rule = get_pure_tree(
        create_definition_tree(
            &tokenize_rule_from_str(String::from("'0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'"))
        ).unwrap()
    );
    let string_rule = get_pure_tree(
        create_definition_tree(
            &tokenize_rule_from_str(String::from("{ char },  [ { ( char | digit ) } ]"))
        ).unwrap()
    );
    let positive_number_rule = get_pure_tree(
        create_definition_tree(
            &tokenize_rule_from_str(String::from("{ digit }"))
        ).unwrap()
    );
    let integer_rule = get_pure_tree(
        create_definition_tree(
            &tokenize_rule_from_str(String::from("[ \"-\" ], positive_number"))
        ).unwrap()
    );
    let float_rule = get_pure_tree(
        create_definition_tree(
            &tokenize_rule_from_str(String::from("integer, '.', integer"))
        ).unwrap()
    );

    assert!(are_same_tree(tree.rules.get("char").unwrap(), &char_rule, true, false));
    assert!(are_same_tree(tree.rules.get("digit").unwrap(), &digit_rule, true, false));
    assert!(are_same_tree(tree.rules.get("string").unwrap(), &string_rule, true, false));
    assert!(are_same_tree(tree.rules.get("positive_number").unwrap(), &positive_number_rule, true, false));
    assert!(are_same_tree(tree.rules.get("integer").unwrap(), &integer_rule, true, false));
    assert!(are_same_tree(tree.rules.get("float").unwrap(), &float_rule, true, false));
}