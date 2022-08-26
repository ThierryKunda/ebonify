use pre_treatment::*;
pub mod error;
pub mod ebnf_syntax;

fn main() {
    let lines_res = split_lines("tests_samples/lang_mini.ebnf");
        match lines_res {
            Ok(lines) => {
                for l in lines {
                    print!("START {l} END");
                }
            },
            Err(err) => {
                println!("Erreur de traitement : {err}");
            },
        }
}

mod pre_treatment {
    use regex;
    use std::{fs, error::Error};
    use crate::error::PreTreatmentError;

    pub fn split_lines(filepath: &str) -> Result<Vec<String>, PreTreatmentError> {
        let file_content = fs::read_to_string(filepath);
        let content: String;
        match file_content {
            Ok(ct) => {
                content = ct;
            },
            Err(_) => {
                return Err(PreTreatmentError);
            },
        }
        let re = regex::Regex::new(r".+;").unwrap();
        let content_str = content.as_str();
        let matches_iter = re.find_iter(content_str).map(|m| m.as_str());
        let mut v: Vec<String> = Vec::new();
        for mat in matches_iter {
            v.push(mat.to_string())
        }
        return Ok(v);
    }

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
}