use crate::{ast::rule::*, parsing::tokenize_rule_from_str};
use super::builders::EbnfTreeBuilder;

#[test]
fn tree_from_file_test() {
    let tree = EbnfTreeBuilder::from_file("tests_samples/lang_mini.ebnf").unwrap();
    // println!("{:?}", tree.rules.get("char"));
    let char_rule = get_pure_tree(
        create_definition_tree(
            &tokenize_rule_from_str(String::from("'a'|'b'|'c'|'e'|'f'"))
        )
    );
    let digit_rule = get_pure_tree(
        create_definition_tree(
            &tokenize_rule_from_str(String::from("'0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'"))
        )
    );
    let string_rule = get_pure_tree(
        create_definition_tree(
            &tokenize_rule_from_str(String::from("{ char },  [ { ( char | digit ) } ]"))
        )
    );
    let positive_number_rule = get_pure_tree(
        create_definition_tree(
            &tokenize_rule_from_str(String::from("{ digit }"))
        )
    );
    let integer_rule = get_pure_tree(
        create_definition_tree(
            &tokenize_rule_from_str(String::from("[ \"-\" ], positive_number"))
        )
    );
    let float_rule = get_pure_tree(
        create_definition_tree(
            &tokenize_rule_from_str(String::from("integer, '.', integer"))
        )
    );

    assert!(are_same_tree(tree.rules.get("char").unwrap(), &char_rule, true, false));
    assert!(are_same_tree(tree.rules.get("digit").unwrap(), &digit_rule, true, false));
    assert!(are_same_tree(tree.rules.get("string").unwrap(), &string_rule, true, false));
    assert!(are_same_tree(tree.rules.get("positive_number").unwrap(), &positive_number_rule, true, false));
    println!("{:?}", integer_rule);
    assert!(are_same_tree(tree.rules.get("integer").unwrap(), &integer_rule, true, false));
    assert!(are_same_tree(tree.rules.get("float").unwrap(), &float_rule, true, false));
}