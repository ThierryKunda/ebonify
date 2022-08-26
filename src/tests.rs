use crate::pre_teatment::*;

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