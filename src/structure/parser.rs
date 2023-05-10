use std::rc::Rc;
use super::tree::EbnfTree;
use crate::ebnf_syntax::{Rule, AtomicKind};
use crate::error::ParserError;

enum ParsingStatus {
    BeforeStarting,
    OnGoing,
    Success,
    Failure
}

enum StackElement {
    InputSlice(char),
    Rule(String)
}

enum Action<'a> {
    Shift,
    Reduction(&'a String, &'a String),
    EndOnSuccess,
    EndOnFailure
}

/// Parser structure for LR analysis.
/// 
/// Its goal is to tell any third-part member (public APIs, CLIs, all-in-one softwares, etc) if a sequence of characters belongs to a formal language,
/// using its grammar.
/// 
/// Note : the parser needs an entry point to the grammar (called an `axiom`). If not present, the parser
/// cannot be constructed and the parsing process will not be available.
/// Therefore be careful defining the axiom in the EbnfTree, which will be hold by the parser.
struct Parser<'a> {
    tree: &'a EbnfTree,
    input: String,
    stack: Vec<StackElement>,
    actions: Vec<Action<'a>>,
    status: ParsingStatus,
    axiom: String,
}

impl<'a> Parser<'a> {
    pub fn new(string_to_match: &'a String, tree: &'a EbnfTree) -> Result<Self, ParserError> {
        if let Some(ax) = &tree.axiom {
            Ok(Parser {
                tree,
                input: string_to_match.clone(),
                stack: Vec::new(),
                status: ParsingStatus::BeforeStarting,
                actions: Vec::new(),
                axiom: ax.clone(),
            })
        } else {
            Err(ParserError::new("axiom is undefined"))
        }
    }

    /// Shifts a character to the stack
    fn shift(&mut self) {
        self.stack.push(StackElement::InputSlice(self.input.remove(0)));
    }

    /// Initializes the parsing process
    /// 
    /// If the input is void (string is `""`) the parsing is useless, as any formal langage can produce
    /// this element. It leads to a direct success
    pub fn initialize(&mut self) {
        if self.input == String::from("") {
            self.status = ParsingStatus::Success;
            self.actions.push(Action::EndOnSuccess);
        } else {
            self.status = ParsingStatus::OnGoing;
            self.actions.push(Action::Shift);
            self.shift();
        }
    }

    /// Checks if the parsing is a success -> the sequence of characters belongs to the formal langage.
    /// 
    /// Two conditions must be met :
    /// - The only element contained in the stack is the axiom of the grammar
    /// - There is no more input element
    fn check_success(&self) -> bool {
        if let Some(StackElement::Rule(first)) = self.stack.first() {
            self.stack.len() == 1 && first == &self.axiom && self.input == String::from("")
        } else {
            false
        }
    }
}