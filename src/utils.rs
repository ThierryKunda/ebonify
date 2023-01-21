use std::collections::HashMap;
use std::ops::Add;


pub trait Counter<T> {
    fn total(&self) -> usize;
    fn has_key_value_association(&self) -> bool;
    fn add_one_to_total(&mut self);
    fn remove_one_to_total(&mut self);
    fn add_element(&mut self, element: &T);
    fn remove_element(&mut self, element: &T);
    fn element_already_stored(&self, element: &T) -> Option<bool>;
    fn add_one_to_element(&mut self, element: Option<&T>) -> bool {
        if let Some(v) = element {
            if self.has_key_value_association() {
                self.add_element(v);
            }
            self.add_one_to_total();
            true
        } else {
            false
        }
    }
    fn remove_one_to_element(&mut self, element: Option<&T>) -> bool {
        if let Some(v) = element {
            if self.has_key_value_association() {
                self.remove_element(v);
            }
            self.add_one_to_total();
            true
        } else {
            false
        }
    }
}

#[derive(Clone, Debug)]
pub struct AssocRuleCounter {
    total: usize,
    occurrences: HashMap<String, usize>,
}

impl AssocRuleCounter {
    pub fn new() -> Self {
        AssocRuleCounter {
            total: 0,
            occurrences: HashMap::new(),
        }
    }
}

impl From<Vec<(String, usize)>> for AssocRuleCounter {
    fn from(source_vec: Vec<(String, usize)>) -> Self {
        let mut occurrences = HashMap::new();
        for (name, nb_occ) in source_vec {
            occurrences.insert(name, nb_occ);
        }

        AssocRuleCounter { total: occurrences.iter().fold(0, |acc, (_, occ)| acc + *occ), occurrences: occurrences }
    }
}

impl Add for AssocRuleCounter {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        let mut res = AssocRuleCounter { total: 0, occurrences: HashMap::new() };
        // Get all occurences
        res.occurrences = self.occurrences;
        for occ in rhs.occurrences {
            if let Some(v) = res.occurrences.get_mut(&occ.0) {
                *v += 1;
            } else {
                res.occurrences.insert(occ.0, occ.1);
            }
        }
        // Re-calculate the total amount of occurences of all identified rules
        res.total = res.occurrences.iter().fold(0, |acc, (_, occ)| acc + *occ);
        
        res
    }
}

impl PartialEq for AssocRuleCounter {
    fn eq(&self, other: &Self) -> bool {
        self.total == other.total && self.occurrences == other.occurrences
    }
}

impl Counter<String> for AssocRuleCounter {
    fn total(&self) -> usize {
        self.total
    }

    fn has_key_value_association(&self) -> bool {
        true
    }

    fn add_one_to_total(&mut self) {
        self.total += 1;
    }

    fn remove_one_to_total(&mut self) {
        self.total -= 1;
    }

    fn add_element(&mut self, element: &String) {
        if self.occurrences.contains_key(element) {
            let e = self.occurrences.get_mut(element).unwrap();
            *e += 1;
        } else {
            self.occurrences.insert(element.to_string(), 1);
        }
    }

    fn remove_element(&mut self, element: &String) {
        if self.occurrences.contains_key(element) {
            let e = self.occurrences.get_mut(element).unwrap();
            *e -= 1;
        }
    }

    fn element_already_stored(&self, element: &String) -> Option<bool> {
        if self.has_key_value_association() {
            Some(self.occurrences.contains_key(element))
        } else {
            None
        }
    }
}