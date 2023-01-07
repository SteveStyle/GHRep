use std::collections::HashMap;



/*  extract data from a string with this example format
root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32
 */
fn parse_contents( contents: &str) -> Vec<Rule> {
    let mut rules: Vec<Rule> = Vec::new();
    for line in contents.lines() {
        let mut parts = line.split(": ");
        let name = parts.next().unwrap().to_string();
        let value = parts.next().unwrap().to_string();
        if value.contains(" + ") {
            let mut parts = value.split(" + ");
            let left = parts.next().unwrap().to_string();
            let right = parts.next().unwrap().to_string();
            rules.push(Rule::Rule {
                name,
                op: Op::Add,
                left,
                right,
            });
        } else if value.contains(" - ") {
            let mut parts = value.split(" - ");
            let left = parts.next().unwrap().to_string();
            let right = parts.next().unwrap().to_string();
            rules.push(Rule::Rule {
                name,
                op: Op::Subtract,
                left,
                right,
            });
        } else if value.contains(" * ") {
            let mut parts = value.split(" * ");
            let left = parts.next().unwrap().to_string();
            let right = parts.next().unwrap().to_string();
            rules.push(Rule::Rule {
                name,
                op: Op::Multiply,
                left,
                right,
            });
        } else if value.contains(" / ") {
            let mut parts = value.split(" / ");
            let left = parts.next().unwrap().to_string();
            let right = parts.next().unwrap().to_string();
            rules.push(Rule::Rule {
                name,
                op: Op::Divide,
                left,
                right,
            });
        } else {
            let value = value.parse::<isize>().unwrap();
            rules.push(Rule::Value { name, value });
        }
    }
    rules

}

#[derive(Debug)]
enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
}
    
#[derive(Debug)]
enum Rule {
    Value { name: String, value: isize },
    Rule {
        name: String,
        op: Op,
        left: String,
        right: String,
    },
}

//implement get and set functions for the Rule struct, for name and gen
impl Rule {
    fn get_name(&self) -> String {
        match self {
            Rule::Value { name, value: _ } => name.to_string(),
            Rule::Rule {
                name,
                op: _,
                left: _,
                right: _,
            } => name.to_string(),
        }
    }
}

// implement the MonkeyYell struct containing a vector of Rule structs in a hashmap

pub struct MonkeyYell {
// add a hashmap to the struct
   rules: HashMap<String, Rule>,
}

impl MonkeyYell {
    pub fn new(contents: &str) -> MonkeyYell {
        let vrules = parse_contents(contents);
        let mut rules: HashMap<String, Rule> = HashMap::new();
        for rule in vrules {
            rules.insert(rule.get_name(), rule);
        }

        MonkeyYell { rules }
    }

    // Add a function to get the value of a rule.  
    // If it is a value, return the value.  
    // If it is a rule, get the values of the left and right rules, and apply the operation to them
    pub fn get_value(&self, name: &str) -> isize {
        let rule = self.rules.get(name).unwrap();
        match rule {
            Rule::Value { name: _, value } => *value,
            Rule::Rule {
                name: _,
                op,
                left,
                right,
            } => {
                let left_value = self.get_value(left);
                let right_value = self.get_value(right);
                match op {
                    Op::Add => left_value + right_value,
                    Op::Subtract => left_value - right_value,
                    Op::Multiply => left_value * right_value,
                    Op::Divide => left_value / right_value,
                }
            }
        }
    }

    // add a new version of get_value that returns None if name = 'humn' or if get_value2(left) or get_value2(right) returns None
    pub fn get_value2(&self, name: &str) -> Option<isize> {
        if name == "humn" {
            return None;
        }
        let rule = self.rules.get(name).unwrap();
        match rule {
            Rule::Value { name: _, value } => Some(*value),
            Rule::Rule {
                name: _,
                op,
                left,
                right,
            } => {
 
                let left_value = self.get_value2(left);
                let right_value = self.get_value2(right);
                match (left_value, right_value) {
                    (Some(l), Some(r)) => match op {
                        Op::Add => Some(l + r),
                        Op::Subtract => Some(l - r),
                        Op::Multiply => Some(l * r),
                        Op::Divide => Some(l / r),
                    },
                    _ => None,
                }
            }
        }
    }

    // force_value() takes a name and a value.  If left or right is 'humn' return the value which is passed.  
    // Otherwise, if get_value2(left) returns None, calculate the value for get_value(left) that would make this rule evaluate to the value passed in.  Call force_value() on left passing in the calculated value.
    // Do the same for right.
    pub fn force_value(&self, name: &str, value: isize) -> Option<isize> {
        println!("force_value: name: {}, value: {}", name, value);
        if name == "humn" {
            return Some(value);
        }
        let rule = self.rules.get(name).unwrap();
        match rule {
            Rule::Value { name: _, value: v } => None,
            Rule::Rule {
                name: _,
                op,
                left,
                right,
            } => {
                let left_value = self.get_value2(left);
                let right_value = self.get_value2(right);
                if name == "root" {
                    match(left_value, right_value) {
                        (Some(l), None) => {
                            return self.force_value(right, l);
                        }
                        (None, Some(r)) => {
                            return self.force_value(left, r);
                        }
                        _ => { return None; }
                    }
                }
                match (left_value, right_value) {
                    (Some(l), None) => match op {
                        Op::Add => {
                            return self.force_value(right, value - l);
                        }
                        Op::Subtract => {
                            return self.force_value(right, l - value);
                        }
                        Op::Multiply => {
                            return self.force_value(right, value / l);
                        }
                        Op::Divide => {
                            return self.force_value(right, l / value);
                        }
                    },
                    (None, Some(r)) => match op {
                        Op::Add => {
                            return self.force_value(left, value - r);
                        }
                        Op::Subtract => {
                            return self.force_value(left, value + r);
                        }
                        Op::Multiply => {
                            return self.force_value(left, value / r);
                        }
                        Op::Divide => {
                            return self.force_value(left, value * r);
                        }
                    },
                    _ => None,
                }
            }
        }
    }

    // solve2(), calls force_value() on the root rule and returns the result
    pub fn solve2(&mut self) -> Option<isize> {
        self.force_value("root", 0)
    }
    

}

// add a test module and a test function to test the parse_contents function
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_force_value() {
        let contents = "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32";
        let mut monkey = MonkeyYell::new(contents);
        let result = monkey.solve2();
        assert_eq!(result, Some(301));
    }

    #[test]
    fn test_parse_contents() {
        let contents = "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32";
        let rules = parse_contents(contents);
        assert_eq!(rules.len(), 15);
        assert_eq!(rules[0].get_name(), "root");
        assert_eq!(rules[1].get_name(), "dbpl");
        assert_eq!(rules[2].get_name(), "cczh");
        assert_eq!(rules[3].get_name(), "zczc");
        assert_eq!(rules[4].get_name(), "ptdq");
        assert_eq!(rules[5].get_name(), "dvpt");
        assert_eq!(rules[6].get_name(), "lfqf");
        assert_eq!(rules[7].get_name(), "humn");
        assert_eq!(rules[8].get_name(), "ljgn");
        assert_eq!(rules[9].get_name(), "sjmn");
        assert_eq!(rules[10].get_name(), "sllz");
        assert_eq!(rules[11].get_name(), "pppw");
        assert_eq!(rules[12].get_name(), "lgvd");
        assert_eq!(rules[13].get_name(), "drzm");
        assert_eq!(rules[14].get_name(), "hmdt");
        println!("{:#?}", rules);
    }
}