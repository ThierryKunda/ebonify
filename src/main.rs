use pre_treatment::*;
use regex;
pub mod ebnf_syntax;

fn main() {
    let mut content_lines = split_lines("tests_samples/lang_mini.ebnf");
    remove_block_comments(&mut content_lines);
    let mut members = split_members(content_lines);
    sanitize(&mut members);
    for l in members {
        if l.len() == 1 {
            println!("!!!VIDE!!!")
        } else {
            for m in l {
                println!("{}", m);
            }
        }
        // println!("----------------------");
    }
}

mod pre_treatment {
    use std::fs;
    pub fn split_lines(filepath: &str) -> Vec<String> {
        let content = fs::read_to_string(filepath).unwrap_or_else(|er| er.to_string());
        let lines_iterator = content.split(';').into_iter();
        let mut lines: Vec<String> = Vec::new();
        lines_iterator.for_each(|el| { lines.push(el.to_string())});
        return lines;
    }

    pub fn remove_block_comments(lines: &mut Vec<String>) -> () {
        let index = lines.len();
        for i in 0..(index-1) {
            match lines.get(i) {
                Some(v) => if v.contains("(*") && v.contains("*)") {
                    lines.remove(i);
                }
                None => ()
            }
        }
    }

    fn sep_members_aux(line: &String) -> Vec<String> {
        let line_split: Vec<&str> = line.split(" ").collect();
        return line_split.iter().map(|e| e.to_string()).collect();
    }

    pub fn split_members(lines: Vec<String>) -> Vec<Vec<String>> {
        return lines.iter().map(|e| sep_members_aux(e)).collect();
    }


    fn sanit_aux(lines: &mut Vec<String>) {
       for i in 0..(lines.len()-1) {
        let m_without_blank = lines.get(i).unwrap().trim().to_string();
        lines.insert(i, m_without_blank);
        lines.remove(i+1);
       }
    }

    pub fn sanitize(lines: &mut Vec<Vec<String>>) -> () {
        // let all_tokens: Vec<String> = Vec::new();
        for l in lines {
            sanit_aux(l);
        }
    }
}