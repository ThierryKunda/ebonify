use std::collections::HashMap;
use std::collections::hash_map::Iter;
use std::ops::{Add, Sub};

pub trait Counter<T, I> {
    fn total(&self) -> I;
    fn has_key_value_association(&self) -> bool;
    fn add_any_to_total(&mut self, qty: I);
    fn remove_any_to_total(&mut self, qty: I);
    fn get_associated_count(&self, element: Option<&T>) -> Option<I>;
    fn add_element_with_qty(&mut self, element: &T, qty: I);
    fn remove_element_with_qty(&mut self, element: &T, qty: I);
    fn element_already_stored(&self, element: &T) -> Option<bool>;
    fn add_any_to_element(&mut self, element: Option<&T>, qty: I) -> bool {
        if let Some(v) = element {
            if self.has_key_value_association() {
                self.add_element_with_qty(v, qty);
            } else {
                self.add_any_to_total(qty);
            }
            true
        } else {
            false
        }
    }
    fn remove_any_to_element(&mut self, element: Option<&T>, qty: I) -> bool {
        if let Some(v) = element {
            if self.has_key_value_association() {
                self.remove_element_with_qty(v, qty);
            } else {
                self.remove_any_to_total(qty);
            }
            true
        } else {
            false
        }
    }
}

#[derive(Debug)]
pub struct RuleCounter {
    total: u32
}

impl RuleCounter {
    pub fn new() -> Self {
        RuleCounter { total: 0 }
    }
}

impl From<u32> for RuleCounter {
    fn from(tot: u32) -> Self {
        Self { total: tot }
    }
}

impl Add for RuleCounter {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        RuleCounter { total: self.total + rhs.total }
    }
}

impl Sub for RuleCounter {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        RuleCounter { total: self.total - rhs.total }
    }
}

impl Clone for RuleCounter {
    fn clone(&self) -> Self {
        Self { total: self.total.clone() }
    }
}

impl Counter<usize, u32> for RuleCounter {
    fn total(&self) -> u32 {
        self.total
    }

    fn has_key_value_association(&self) -> bool {
        false
    }

    fn add_any_to_total(&mut self, qty: u32) {
        self.total += qty;
    }

    fn remove_any_to_total(&mut self, qty: u32) {
        if self.total > qty {
            self.total -= qty;
        }
    }

    fn element_already_stored(&self, _element: &usize) -> Option<bool> { None }

    fn add_element_with_qty(&mut self, element: &usize, qty: u32) {}

    fn remove_element_with_qty(&mut self, element: &usize, qty: u32) {}

    fn get_associated_count(&self, element: Option<&usize>) -> Option<u32> {
        None
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

    pub fn update_total(&mut self, to_add: usize) {
        self.total += to_add;
    }

    pub fn iter(&self) -> Iter<'_, String, usize> {
        self.occurrences.iter()
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
        // Re-calculate the total amount of occurences of each rule
        res.total = res.occurrences.iter().fold(0, |acc, (_, occ)| acc + *occ);
        
        res
    }
}

impl PartialEq for AssocRuleCounter {
    fn eq(&self, other: &Self) -> bool {
        self.total == other.total && self.occurrences == other.occurrences
    }
}

impl Counter<String, usize> for AssocRuleCounter {
    fn total(&self) -> usize {
        self.total
    }

    fn has_key_value_association(&self) -> bool {
        true
    }

    fn add_any_to_total(&mut self, qty: usize) {
        self.total += qty;
    }

    fn remove_any_to_total(&mut self, qty: usize) {
        self.total -= qty;
    }

    fn add_any_to_element(&mut self, element: Option<&String>, qty: usize) -> bool {
        if let Some(el) = element {
            self.add_element_with_qty(el, qty);
            self.add_any_to_total(qty);
            true
        } else {
            false
        }
    }

    fn remove_any_to_element(&mut self, element: Option<&String>, qty: usize) -> bool {
        if let Some(el) = element {
            self.remove_element_with_qty(el, qty);
            self.remove_any_to_total(qty);
            true
        } else {
            false
        }
    }
    

    fn element_already_stored(&self, element: &String) -> Option<bool> {
        if self.has_key_value_association() {
            Some(self.occurrences.contains_key(element))
        } else {
            None
        }
    }

    fn add_element_with_qty(&mut self, element: &String, qty: usize) {
        if let Some(v) = self.occurrences.get_mut(element) {
            *v += qty; 
        } else {
            self.occurrences.insert(element.to_string(), qty);
        }
    }

    fn remove_element_with_qty(&mut self, element: &String, qty: usize) {
        if let Some(v) = self.occurrences.get_mut(element) {
            if *v > qty {
                *v -= qty;
            }
        } else {
            self.occurrences.insert(element.to_string(), qty);
        }
    }

    fn get_associated_count(&self, element: Option<&String>) -> Option<usize> {
        if let Some(el) = element {
            if let Some(v) = self.occurrences.get(el) {
                Some(*v)
            } else {
                None
            }
        } else {
            None
        }
    }
}

pub fn diff_str(s1: &String, s2: &String) -> String {
    s1
    .chars()
    .filter(|c| !s2.contains(
        c.to_string().as_str()
    ))
    .collect()
}